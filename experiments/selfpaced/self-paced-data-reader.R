# general libraries
library(dplyr) # magic
library(magrittr) # piped magic
library(tidyr) # dark magic
library(ggplot2) # plots things
theme_set(theme_bw()) # set ggplot theme
library(readxl) # reads excel files
library(gdata) # some functions here and there
library(modelr) # forgot what this does

# specific libraries
library(extrafont) # Times font for plot use
library(stringr) # good with regular expressions

# statistics
library(brms) # bayesian regression
library(bayesplot) # plot bayes models as ggplot objects
library(tidybayes) # fairy dust for bayes
bayesplot_theme_set(new = theme_bw()) # set plot view
library(MASS) # contr.sdif


coef_interval_plot <- function(brms_object, namer){ # function to make pretty brms interval plotting with custom labels
  df <- mcmc_intervals_data(brms_object, pars = vars(starts_with("b_")))
  df %<>% tibble::rowid_to_column("num")
  df %<>% subset(num %notin% c("1")) %>% drop.levels()
  if(length(df$parameter) != length(namer)){
    stop("Parameter and namer length mismatch")
  }
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


df_responses_join <- df_responses %>% dplyr::select(subject, item, condition, correct)
df_reads %<>% left_join(df_responses_join)

# two items with a typo excluded
df_reads %<>% subset(item %notin% c("17","20"))

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


# determine outliers
df_reads %<>% subset(experiment =="expSA") %>%
  group_by(subject, item, condition) %>%
  mutate(has_outlier = any(RT < 100 | RT > 3000)) %>% ungroup()

df_reads %>% dplyr::select(subject, RT, has_outlier, cat, correct) %>% 
  group_by(subject, has_outlier, correct) %>% 
  summarise(averageRT = mean(RT)) %>%
  ggplot(aes(averageRT, colour = has_outlier)) + stat_ecdf(geom="point") + scale_x_log10() + facet_wrap(correct~.)


# exclude trials with outliers
df_reads %<>% subset(!has_outlier & correct == "1") %>% dplyr::select(-has_outlier, -correct) %>% ungroup()

saveRDS(df_responses, file = "./report/df_responses.rds")
saveRDS(df_reads, file ="./report/df_reads.rds")
### END SANITY CHECKS ###




### BEGIN MODELS ###
# data frame for SA models
df_SA_model <- df_reads %>% 
  subset(word_number %in% c(7:9) & cat !="Contrast") %>% drop.levels() %>%
  dplyr::select(subject, item, cat, conjoiner, RT, word_number)

df_SA_model$word_number %<>% dplyr::recode(`7`="critical", `8`="so1", `9`="so2")
df_SA_model %<>% tidyr::pivot_wider(names_from = "word_number", values_from="RT")

df_SA_model$cat %<>% reorder.factor(new.order = c("No_SA", "One_SA", "Full_SA"))
df_SA_model$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))
contrasts(df_SA_model$cat) <- MASS::contr.sdif(3)
contrasts(df_SA_model$conjoiner) <- contr.sum(2)


# models for SA conditions in critical and spillover regions
SA_target_model <- brm(mvbind(critical,so1,so2) ~ cat*conjoiner+ (cat*conjoiner | item) + (cat*conjoiner | subject), 
                 data = df_SA_model,
                 family = lognormal('identity'), chains = 4, cores = 4, iter = 3000, warmup = 2000,
                 file = "./report/SA_target_model")


# data frame for Contrast models
df_SA_model2 <- df_reads %>% 
  subset(word_number %in% c(7:9) & cat %in% c("Contrast","No_SA")) %>% drop.levels() %>%
  dplyr::select(subject, item, cat, conjoiner, RT, word_number)

df_SA_model2$word_number %<>% dplyr::recode(`7`="critical", `8`="so1", `9`="so2")
df_SA_model2 %<>% tidyr::pivot_wider(names_from = "word_number", values_from="RT")

df_SA_model2$cat %<>% reorder.factor(new.order = c("No_SA","Contrast"))
df_SA_model2$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))
contrasts(df_SA_model2$cat) <- contr.sdif(2)
contrasts(df_SA_model2$conjoiner) <- contr.sum(2)


# model for Contrast conditions in critical and spillover regions
SA_contrast_model <- brm(mvbind(critical,so1,so2) ~ cat*conjoiner + (cat*conjoiner | item) + (cat*conjoiner | subject),
                        data = df_SA_model2,
                        family = lognormal('identity'), chains = 4, cores = 4, iter = 3000, warmup = 2000,
                        file = "./report/SA_contrast_model")


 # df for comparing SA and contrast
saVcontrast_df <- df_reads %>% 
  subset(word_number %in% c(7:9) & cat %in% c("Contrast","One_SA")) %>%
  dplyr::select(subject, item, cat, conjoiner, RT, word_number) %>% drop.levels()
saVcontrast_df$word_number %<>% dplyr::recode(`7` = "critical", `8` = "so1", `9`="so2")
saVcontrast_df %<>% tidyr::pivot_wider(names_from = "word_number", values_from = "RT")

saVcontrast_df$cat %<>% reorder.factor(new.order = c("Contrast","One_SA"))
saVcontrast_df$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))
contrasts(saVcontrast_df$cat) <- contr.sum(2)
contrasts(saVcontrast_df$conjoiner) <- contr.sum(2)


# model for comparing SA to Contrast
saVcontrast_model <- brm(mvbind(critical,so1,so2) ~ cat*conjoiner + (cat*conjoiner|subject) + (cat*conjoiner|item),
                        data = saVcontrast_df, 
                        family = lognormal(link = "identity"), chains = 4, cores = 4, iter = 3000, warmup = 2000,
                        file = "./report/saVcontrast_model")













