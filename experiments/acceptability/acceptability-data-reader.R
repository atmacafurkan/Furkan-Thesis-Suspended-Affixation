# general libraries
library(dplyr) # magic
library(magrittr) # piped magic
library(tidyr) # dark magic
library(ggplot2) # plots things
theme_set(theme_bw()) # set ggplot theme
library(readxl) # reads excel files
library(gdata) # some functions here and there

# specific libraries
library(extrafont) # Times font for plot use
library(stringr) # good with regular expressions

# statistics
library(brms) # bayesian regression
library(bayesplot) # some useful functions for brms objects
library(tidybayes) # fairy dust for bayes
bayesplot_theme_set(new = theme_bw()) # set plot view
library(MASS) # contr.sdif
`%notin%` <- Negate(`%in%`)

coloring <- function(df){ # function to color intervals 
  with(df, ifelse(m > 0 & l > 0, 'blue',
                  ifelse(m < 0 & h < 0,'red','black')))
} 

plot_ready <- function(df, namer){ # function to make pretty df for plotting
  if(length(df$parameter) != length(namer)){
    stop("Parameter and namer length mismatch")
  }
  df %<>% tibble::rowid_to_column("num")
  levels(df$parameter) <- namer
  df %<>% arrange(desc(num)) %>% mutate(parameter = factor(parameter,parameter))
  df
}

coef_interval_plot <- function(brms_object, namer){ # function to make pretty brms interval plotting with custom labels
  df <- mcmc_intervals_data(brms_object, pars = vars(starts_with("b_")))
  if(length(df$parameter) != length(namer)){
    stop("Parameter and namer length mismatch")
  }
  df %<>% tibble::rowid_to_column("num")
  levels(df$parameter) <- namer
  df %<>% arrange(desc(num)) %>% mutate(parameter = factor(parameter,parameter))
  df %>% ggplot(aes(m,parameter)) + geom_point(size = 4, color =  with(df, ifelse(m > 0 & l > 0, 'blue',
                                                                                  ifelse(m < 0 & h < 0,'red','black')))) +
    geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1.5,color =  with(df, ifelse(m > 0 & l > 0, 'blue',
                                                                                                       ifelse(m < 0 & h < 0,'red','black')))) + 
    geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0),color =  with(df, ifelse(m > 0 & l > 0, 'blue',
                                                                                  ifelse(m < 0 & h < 0,'red','black')))) + vline_0() +
    xlab("Estimate(log)") + ylab("coefficients")
}

df <- read.csv("./results/results.csv", 
               header = F, 
               comment.char = "#", 
               encoding = "UTF-8" , 
               col.names = paste0("V",seq_len(11)), 
               fill = TRUE)

colnames(df) <- c("subject", 
                  "MD5", 
                  "Controller", 
                  "Order", 
                  "Element", 
                  "experiment", 
                  "item_number",
                  "sentence", 
                  "response", 
                  "CorrectAnswer", 
                  "RT")

df <- drop.levels(subset(df, experiment != "intro" & experiment != "practice"))  %>%
  separate(experiment, c("experiment","condition"), sep="_")

df$Cresponse <- ifelse(str_detect(df$response, "Evet"), 1, 0)

df$condition[is.na(df$condition)] <- 0

df %<>%  dplyr::select(subject,
                       experiment,
                       item = item_number,
                       condition,
                       response,
                       Cresponse,
                       RT) %>% na.omit()

df2 <- read_excel("SA-Survey-Items.xlsx") %>% subset(Problem!="p") %>%
  dplyr::select(item = item_id, Suffix, CONJ, Suffix) 
df2$item %<>% as.factor()
df2$condition <- ifelse(df2$CONJ=="ve", "a", ifelse(df2$CONJ=="veya", "b", 0))
df2 %<>% dplyr::select(-CONJ)
df2$item %<>% drop.levels()


df %<>% left_join(df2)
df$condition %<>% as.factor()
df$subject %<>% as.character() %>% as.factor() %>% as.integer()
df$Suffix %<>% as.factor()
df$conjoiner <- ifelse(df$condition =="a", "ve", ifelse(df$condition =="b", "veya", 0))
df%<>% dplyr::arrange(subject, condition)
df$correct <- ifelse(df$Suffix =="grammatical" & df$Cresponse =="1", 1,
                     ifelse(df$Suffix=="ungrammatical" & df$Cresponse =="0", 1,0)) %>% as.integer()

saveRDS(df, file="./report/init_judgments")

# removing some items and fillers
df %<>% subset(item %notin% c("9","24","118","125","142"))

# remove outlier responses with more than 7s or less than 2s response time
df %<>% subset(RT < 7000) %>% subset(RT > 2000)

# subject accuracy by response time
df %>% subset(condition=="0") %>%
  dplyr::select(subject, correct, RT) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(correct), se = sd(correct)/sqrt(length(correct)), averageRT = mean(RT)) %>%
  ggplot(aes(accuracy,averageRT)) +
  geom_point() +
  scale_y_log10() +
  geom_text(aes(label=subject),hjust=0, vjust=0)

# subject accuracy ecdf
df %>% subset(condition=="0") %>%
  dplyr::select(subject, correct, RT) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(correct)) %>% ggplot(aes(accuracy)) + stat_ecdf(geom = "point")

subject_accuracy <- df %>% subset(condition=="0") %>%
  dplyr::select(subject, correct, RT) %>% 
  group_by(subject) %>%
  summarise(accuracy = mean(correct), se = sd(correct)/sqrt(length(correct)), averageRT = mean(RT)) %>% 
  subset(accuracy < .7)

# remove low accuracy subjects from the data
df %<>% subset(subject %notin% subject_accuracy$subject)

# average acceptability for SA of suffixes
# suffix_acceptability <- df %>% dplyr::select(Suffix, conjoiner, Cresponse) %>% 
#   subset(conjoiner!="0") %>%
#   group_by(Suffix, conjoiner) %>%
#   summarise(average = mean(Cresponse), se = sd(Cresponse) / sqrt(length(Cresponse)))
# 
df %>% dplyr::select(Suffix, conjoiner, Cresponse) %>%
  subset(conjoiner!="0") %>%
  group_by(Suffix, conjoiner) %>%
  summarise(average = mean(Cresponse), se = sd(Cresponse) / sqrt(length(Cresponse))) %>%
  ggplot(aes(Suffix, average, fill = conjoiner)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = average -se, ymax = average + se),
                width = .1, position = position_dodge(0.85), color = "black")

# average acceptability for fillers
# filler_acceptability <- df %>% dplyr::select(item, conjoiner, Cresponse, Suffix) %>%
#   subset(conjoiner =="0") %>%
#   group_by(item, Suffix) %>%
#   summarise(average = mean(Cresponse), se = sd(Cresponse) / sqrt(length(Cresponse)))

# df %>% dplyr::select(item, condition, Cresponse, Suffix) %>%
#   subset(condition =="0") %>%
#   group_by(item, Suffix) %>%
#   summarise(average = mean(Cresponse), se = sd(Cresponse) / sqrt(length(Cresponse))) %>%
#   ggplot(aes(item, average, fill = Suffix)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_errorbar(aes(ymin = average -se, ymax = average + se),
#                 width = .1, position = position_dodge(0.85), color = "black")
saveRDS(df, file ="./report/df_judgments")

df %>% subset(condition=="0") %>%
  dplyr::select(subject, correct, RT) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(correct)) %>% ggplot(aes(accuracy)) + stat_ecdf(geom = "step")

df %>% dplyr::select(subject, RT) %>%
  group_by(subject) %>%
  summarise(average = mean(RT)) %>% 
  ggplot(aes(average)) + stat_ecdf(geom = "step")


# prepare df for regression model
df_model <- df %>% subset(condition !="0") %>% drop.levels()
df_model$Cresponse %<>% as.integer()
df_model$conjoiner %<>% as.factor()
df_model$item %<>% as.integer()
df_model$Cresponse <- ifelse(df_model$Cresponse == 1, 1, -1)
df_model$conjoiner %<>% reorder.factor(new.order = c("veya","ve")) 
contrasts(df_model$Suffix) <- contr.sum(9)
contrasts(df_model$conjoiner) <- contr.sum(2)


# linear mixed effects model
modelim <- brm(Cresponse ~ conjoiner*Suffix + (conjoiner | item) + (conjoiner*Suffix | subject),
               data = df_model, family = bernoulli("logit"), cores = 4, chains = 4, file = "./report/sa_acceptability")
model_results <- fixef(modelim, summary = TRUE, robust = FALSE) %>% as.data.frame() %>% tibble::rownames_to_column("variables")

# labels for y axis
plot_labels <- c("Intercept(VeACC)", "veya", "-(I)msI",
                 "-(I)ncI", "-(ş)Ar", "-CAsInA",
                 "-CI", "-lI", "-lIK",
                 "-sIz", "veya:-(I)msI", "veya:-(I)ncI",
                 "veya:-(ş)Ar", "veya:-CAsInA", "veya:-CI",
                 "veya:-lI", "veya:-lIK", "veya:-sIz")

coef_interval_plot(modelim, plot_labels)


df_model %>% add_residual_draws(modelim) %>%
  median_qi() %>% 
  group_by(conjoiner, Suffix) %>%
  ggplot(aes(sample = .residual)) + geom_qq() + geom_qq_line(color = 'red') + facet_grid(conjoiner~Suffix)


df_model2 <- df_model %>% subset(conjoiner == "ve")

modelim2 <- brm(Cresponse ~ Suffix + (1 | item) + (Suffix | subject), 
                data = df_model2, family = bernoulli("logit"), cores = 4, chains = 4, file = "./report/sa_acceptability2")

model2_results <- fixef(modelim2, summary = TRUE, robust = FALSE) %>% as.data.frame() %>% tibble::rownames_to_column("variables")

plot2_labels <- c("Intercept(ACC)","-(I)msI",
                  "-(I)ncI", "-(ş)Ar", "-CAsInA",
                  "-CI", "-lI", "-lIK",
                  "-sIz")


coef_interval_plot(modelim2, plot2_labels)
