# general libraries
library(dplyr) # magic
library(magrittr) # piped magic
library(tidyr) # dark magic
library(ggplot2) # plots things
theme_set(theme_bw()) # set ggplot theme
library(readxl) # reads excel files
library(gdata) # some functions here and there
library(modelr)

# specific libraries
library(extrafont) # Times font for plot use
library(stringr) # good with regular expressions

# statistics
library(brms) # bayesian regression
library(bayesplot) # plot bayes models as ggplot objects
library(tidybayes) # fairy dust for bayes
bayesplot_theme_set(new = theme_bw()) # set plot view
library(MASS) # contr.sdif

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

`%notin%` <- Negate(`%in%`) # I hate using ! at the very beginning of logical expressions
se <- function(x) sd(x)/sqrt(length(x))

### BEGIN IMPORT AND ORGANIZE ###
df <- read.csv("./results/results.csv", 
                  header = F, 
                  comment.char = "#", 
                  encoding = "UTF-8" , 
                  col.names = paste0("V",seq_len(12)), 
                  fill = TRUE)

colnames(df) <- c("subject", 
                  "MD5", 
                  "controller", 
                  "Order", 
                  "Element", 
                  "experiment",
                  "item",
                  "word_number",
                  "word",
                  "RT",
                  "response_time",
                  "sentence")

df %<>% separate(experiment, c("experiment","condition"), sep="_") %>%
  subset(experiment %in% c("expSA","filler"))
df$condition <- ifelse(df$experiment =="filler", "x", df$condition)

str(df)

# encode vector types
df$word %<>% as.character()
df$subject %<>% as.character() %>% as.factor() %>% as.integer()
df$item %<>% as.character() %>% as.integer()
df$condition %<>% as.factor()
df$word_number %<>% as.character() %>% as.integer()
df$RT %<>% as.character() %>% as.integer()
df$response_time %<>% as.character() %>% as.integer()
df %<>% dplyr::arrange(subject, item, word_number) %>% 
  dplyr::select(subject, item, controller, experiment, condition, word, word_number, RT, response_time)

# create vectors for experiment variables
df$conjoiner <- ifelse(df$experiment=="expSA" & df$condition %in% c("a","c","e","g"), "veya", 
                       ifelse(df$experiment=="expSA" & df$condition %in% c("b","d","f","h"), "ve", NA)) %>% as.factor()
df$cat <- ifelse(df$experiment =='filler', 'filler', 
                       ifelse(df$condition %in% c('a','b'), 'No_SA',
                              ifelse(df$condition %in% c('c','d'), 'One_SA',
                                     ifelse(df$condition %in% c('e','f'), 'Full_SA', 'Contrast')))) %>%
  as.factor() %>% 
  reorder.factor(new.order = c("Contrast","No_SA", "One_SA", "Full_SA"))

# create data frame of question responses
df_responses <- df %>% subset(controller=="Question") %>% dplyr::select(-RT, -word_number, -controller)
df_responses$word <- ifelse(str_detect(df_responses$word, "Evet"), "yes", "no")
df_responses$question <- ifelse(df_responses$experiment=="expSA" & df_responses$item %in% c("1","2","3","4","6","7","8","9","10","11","12"), "yes",
                                ifelse(df_responses$experiment=="expSA" & df_responses$item %in% c("5","13","14","15","16","17","18","19","20","21","22","23","24"), "no",
                                  ifelse(df_responses$experiment=="filler" & df_responses$item %in% c(101:124), "yes", "no")))
df_responses$correct <- ifelse(df_responses$word == df_responses$question, 1, 0)

# create data frame of word reading times 
df_reads <- df %>% subset(controller == "DashedSentence") %>% dplyr::select(-response_time, -controller)
df_reads$word <- gsub('\\.','',df_reads$word)
df_reads$word_length <- nchar(df_reads$word)
df_reads$word_name <- df_reads$word_number %>% as.factor()

### END IMPORT AND ORGANIZE ###
saveRDS(df_responses, file = "./report/init_responses.rds")
saveRDS(df_reads, file ="./report/init_reads.rds")

### BEGIN SANITY CHECKS ###
# accuracy by averageRT
accuracy_by_RT <- df_responses %>% dplyr::select(subject, correct, response_time) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(correct), se = sd(correct)/sqrt(length(correct)), averageRT = mean(response_time)) %>%
  ggplot(aes(accuracy, averageRT, label = subject)) + geom_point() + scale_y_log10() + geom_label()

accuracy_ecdf <- df_responses %>% dplyr::select(subject, correct, response_time) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(correct), se = sd(correct)/sqrt(length(correct)), averageRT = mean(response_time)) %>%
  ggplot(aes(accuracy)) + stat_ecdf(geom = "point")

accuracy_avg_bad <- df_responses %>% dplyr::select(subject, correct, response_time) %>%
  group_by(subject) %>% 
  summarise(accuracy = mean(correct), averageRT = mean(response_time)) %>% subset(accuracy < .7)

# remove bad subjects accuracy from all data frames  
df %<>% subset(subject %notin% accuracy_avg_bad$subject) %>% ungroup()
df_responses %<>% subset(subject %notin% accuracy_avg_bad$subject) %>% ungroup()
df_reads %<>% subset(subject %notin% accuracy_avg_bad$subject) %>% ungroup()

df_responses_join <- df_responses %>% dplyr::select(subject, item, condition, correct)
df_reads %<>% left_join(df_responses_join)


# two items with a typo excluded
df_reads %<>% subset(item %notin% c("17","20"))

# determine outliers
df_reads %<>% subset(experiment =="expSA") %>%
  group_by(subject, item, condition) %>%
  mutate(has_outlier = any(RT < 150 | RT > 3000))

# exclude trials with outliers
df_reads %<>% subset(!has_outlier & correct == "1") %>% dplyr::select(-has_outlier, -correct) %>% ungroup()

saveRDS(df_responses, file = "./report/df_responses.rds")
saveRDS(df_reads, file ="./report/df_reads.rds")
### END SANITY CHECKS ###



### BEGIN PLOTTING ###
# word length histogram
wordlength_histogram <- df_reads %>% dplyr::select(word_length, word) %>% group_by(word_length) %>% summarise(nword = length(word)) %>%
  ggplot(aes(word_length, nword)) + geom_bar(stat = "identity")


# reading time by word length
RT_by_wordlength <- df_reads %>% dplyr::select(word_length, RT) %>%
  group_by(word_length) %>% summarise(averageReaT = mean(RT), se= se(RT)) %>%
  ggplot(aes(word_length, averageReaT)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = averageReaT -se, ymax = averageReaT + se),
                width = .1, position = position_dodge(0.85), color = "black")


# reading time by word length excluding the first conjunct
RT_by_wordlength_no_conj <- df_reads %>% subset(word_name %notin% c("5")) %>% dplyr::select(word_length, RT) %>%
  group_by(word_length) %>% summarise(averageReaT = mean(RT), se= se(RT)) %>%
  ggplot(aes(word_length, averageReaT)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = averageReaT -se, ymax = averageReaT + se),
                width = .1, position = position_dodge(0.85), color = "black")


# average word_length of the first conjunct
length_freq_conj1 <- df_reads %>% subset(word_name %in% c("5") & cat %notin% c("Contrast")) %>% group_by(cat) %>%
    summarise(x = mean(word_length), y = median(word_length), se = se(word_length))


SA_label <- c("Üzerinde",
              "toz",
              "biriken",
              "masayı",
              "yıkamalı-ymış-ım",
              "ve/ya",
              "silmeliymişim", 
              "diye",
              "mırıldandım",
              "kendi",
              "kendime") %>% enc2utf8()

averageRT_sentence <- df_reads %>% dplyr::select(cat, word_name, RT, conjoiner) %>% ungroup() %>%
  subset(word_name %in% c(1:11) & cat !=  "Contrast") %>% 
  group_by(cat, word_name, conjoiner) %>%
  summarise(average_RT = mean(RT), se = se(RT)) %>%
  ggplot(aes(word_name, average_RT, group = cat, color = cat)) +
  theme(text=element_text(size=12),
        legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(face = "bold.italic", angle = 30, hjust = 1)) +
  geom_point(aes(shape = cat), size= 2.4, position = position_dodge(0.35)) + 
  geom_line(aes(linetype = cat), position = position_dodge(0.35)) +
  geom_errorbar(aes(ymin = average_RT -se, ymax = average_RT + se),
                alpha = 0.4,
                width = .3, position = position_dodge(0.35), color = "black") +
  scale_x_discrete(labels = SA_label, name ="") + ylab("average RT (ms)") +
  facet_grid(conjoiner~.)
### END PLOTTING ###


### BEGIN MODELLING ###
# data frame for target word in SA conditions
df_SA_model <- df_reads %>% subset(word_number %in% c("7") & cat !="Contrast") %>% drop.levels()
df_SA_model$word_number %<>% as.factor()
df_SA_model$cat %<>% reorder.factor(new.order = c("No_SA", "One_SA", "Full_SA"))
df_SA_model$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))
contrasts(df_SA_model$cat) <- MASS::contr.sdif(3)
contrasts(df_SA_model$conjoiner) <- contr.sum(2)


# model for target word in SA conditions
SA_target <- brm(RT ~ 1 + cat*conjoiner+ (cat*conjoiner | item) + (cat*conjoiner | subject), 
                 data = df_SA_model,
                 family = lognormal('identity'), chains = 4, cores = 4, iter = 2000,
                 file = "./report/model_SA_target")

SA_target_results <- fixef(SA_target, summary = TRUE, robust = FALSE) %>% as.data.frame() %>%
  tibble::rownames_to_column("variables")

plot_labels <- c("Intercept(NoVeya)","One(to)No","Full(to)One","ve","One(to)No:ve","Full(to)One:ve")

coef_interval_plot(SA_target, plot_labels)

# check model fit
model_fit <- df_SA_model %>% 
  add_residual_draws(SA_target) %>%
  mean_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq(distribution = stats::qlnorm) + geom_qq_line(distribution = stats::qlnorm) + facet_grid(cat~conjoiner)


# data frame for spillover word in SA conditions
df_SA_model_so <- df_reads %>% subset(word_number %in% c("8") & cat !="Contrast") %>% drop.levels()
df_SA_model_so$word_number %<>% as.factor()
df_SA_model_so$cat %<>% reorder.factor(new.order = c("No_SA", "One_SA", "Full_SA"))
df_SA_model_so$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))
contrasts(df_SA_model_so$cat) <- MASS::contr.sdif(3)
contrasts(df_SA_model_so$conjoiner) <- contr.sum(2)

SA_spillover <- brm(RT ~ 1 + cat*conjoiner+ (cat*conjoiner | item) + (cat*conjoiner | subject), 
                 data = df_SA_model_so,
                 family = lognormal('identity'), chains = 4, cores = 4, iter = 2000,
                 file = "./report/model_SA_spillover")

SA_spillover_results <- fixef(SA_spillover, summary = TRUE, robust = FALSE) %>% as.data.frame() %>%
  tibble::rownames_to_column("variables")

coef_interval_plot(SA_spillover, plot_labels)

# data frame for target word in Contrasting conditions
df_SA_model2 <- df_reads %>% subset(word_number %in% c("7") & cat %in% c("Contrast","No_SA")) %>% drop.levels()
df_SA_model2$word_number %<>% as.factor()
df_SA_model2$cat %<>% reorder.factor(new.order = c("No_SA","Contrast"))
df_SA_model2$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))
contrasts(df_SA_model2$cat) <- contr.sum(2)
contrasts(df_SA_model2$conjoiner) <- contr.sum(2)

# model for target word in Contrast conditions
SA_target2 <- brm(RT ~ 1 + cat*conjoiner + (cat*conjoiner | item) + (cat*conjoiner | subject), 
                 data = df_SA_model2,
                 family = lognormal('identity'), chains = 4, cores = 4, iter = 2000,
                 file = "./report/model_SA_target2")

SA_target2_results <- fixef(SA_target2, summary = TRUE, robust = FALSE) %>% as.data.frame() %>%
  tibble::rownames_to_column("variables")

plot2_labels <- c("Intercept(NoVeya)", "Contrast", "ve", "Contrast:ve")

coef_interval_plot(SA_target2, plot2_labels)

# check model fit
model2_fit <- df_SA_model2 %>% 
  add_residual_draws(SA_target2) %>%
  mean_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq(distribution = stats::qlnorm) + geom_qq_line(distribution = stats::qlnorm) + facet_grid(cat~conjoiner)


