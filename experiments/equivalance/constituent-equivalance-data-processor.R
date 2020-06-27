# general libraries
library(dplyr) # magic
library(magrittr) # piped magic
library(tidyr) # dark magic
library(ggplot2) # plots things
theme_set(theme_bw()) # set ggplot theme
library(gdata) # some functions here and there

# specific libraries
library(extrafont) # Times font for plot use
library(stringr) # good with regular expressions
library(brms) # bayesian regression
library(bayesplot) # plot options for bayesian models
bayesplot_theme_set(new = theme_bw()) # set plot view
library(tidybayes) # fairy dust for bayes


coef_interval_plot <- function(brms_object, namer){ # function to make pretty brms interval plotting with custom labels
  df <- mcmc_intervals_data(brms_object, pars = vars(starts_with("b_"))) %>%
    subset(parameter !="b_Intercept")
  
  if(length(df$parameter) != length(namer)){
    stop("Parameter and namer length mismatch")
  }
  df$labeller <- namer %>%
    as.factor() %>%
    reorder.factor(new.order = namer)
  
  df %>% ggplot(aes(m,y=factor(labeller, 
                               levels = rev(levels(factor(labeller)))))) +
    geom_point(size = 4, color =  with(df, ifelse(ll > 0, 'blue', ifelse(hh < 0,'red','black')))) +
    geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1.5,color =  with(df, ifelse(ll > 0, 'blue',
                                                                                                       ifelse(hh < 0,'red','black')))) +
    geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0),color =  with(df, ifelse(ll > 0, 'blue',
                                                                                  ifelse(hh < 0,'red','black')))) + vline_0() +
    xlab("Estimate(log)") + ylab("coefficients")
}

ci_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL, t_gamma = 1.96)
{
  stopifnot(!"avgDV" %in% colnames(df))
  subject_var <- substitute(subject) %>% deparse()
  DV <- substitute(DV) %>% deparse()
  group <- substitute(group) %>% deparse()
  
  subj_means <- df %>% group_by(.dots = subject_var) %>% dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T))
  GM <- mean(subj_means$avgDV)
  df %<>% group_by(.dots = subject_var) %>% dplyr::mutate(nDV = !!as.name(DV) - mean(!!as.name(DV), na.rm = T) + GM )
  
  if (is.null(is_proportion)) {
    dv <- df[[DV]]
    dv_unique <- unique(dv)
    if ( is.logical(dv) || (length(dv_unique) == 2 && all(dv_unique %in% c(0,1))) ) {
      is_proportion <- TRUE
    } else {
      is_proportion <- FALSE
    }
  }
  
  var_correction_factor <- n_conditions/(n_conditions-1)
  df %>% group_by(.dots = group) %>% 
    dplyr::summarize(M = mean(nDV, na.rm = T),
                     Var = ifelse(is_proportion, M*(1-M), var(nDV, na.rm = T)) * var_correction_factor,
                     #Var = var(nDV, na.rm = T) * var_correction_factor,
                     N = sum(!is.na(nDV)),
                     SE = sqrt(Var/N),
                     CI = sqrt(Var/N)*t_gamma)
}


`%notin%` <- Negate(`%in%`)
se <- function(x) sd(x)/sqrt(length(x))

df <- readRDS("./report/organised_data.rds") %>% ungroup()
df$correct_asc <- df$correct %>% as.character() %>% dplyr::recode(`0` = "incorrect", `1` = "correct")
df$condition %<>% dplyr::recode(x = "filler")
df$qtype <- with(df, ifelse(disambiguation == "obj" & question_type =="one_subject_correct","yes",
                                  ifelse(disambiguation =="subj" & question_type =="two_subjects_correct","yes",
                                         ifelse(condition =="filler" & question_type == "correct","yes","no")))) %>% as.factor()


### BEGIN SANITY CHECKS ###
# separate RT data, drop outliers
df %<>% group_by(subject, item, condition) %>%
  mutate(has_outlier = any(RT < 100 | RT > 3000))

# ecdf RT, all data (with outlier)
df %>% ggplot(aes(RT, colour = has_outlier)) + stat_ecdf(geom = "step") + scale_x_log10()  
  
# remove outliers  
df %<>% subset(!has_outlier) %>% dplyr::select(-has_outlier) %>% ungroup()


# ecdf subject accuracies, fillers
df %>% subset(condition=="filler") %>%
  dplyr::select(subject, correct) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(correct)) %>%
  ggplot(aes(accuracy)) + stat_ecdf(geom = "point")

# subjects with lower than %80 accuracy
low_accuracy <- df %>% subset(condition=="filler") %>%
  dplyr::select(subject, correct) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(correct)) %>% subset(accuracy < .7)

# remove low accuracy subjects
df %<>% subset(subject %notin% low_accuracy$subject) %>% ungroup()

# # ecdf subject accuracies, fillers
# df %>% subset(condition=="filler") %>%
#   dplyr::select(subject, correct) %>%
#   group_by(subject) %>%
#   summarise(accuracy = mean(correct)) %>%
#   ggplot(aes(accuracy)) + stat_ecdf(geom = "point")
# 
# # accuracy by response time, fillers
# df %>% subset(condition %in% c("filler")) %>%
#   dplyr::select(subject, correct, response_time) %>%
#   group_by(subject) %>%
#   summarise(accuracy = mean(correct), average_time = mean(response_time)) %>%
#   ggplot(aes(accuracy, average_time)) + geom_point() + geom_label(aes(label=subject))

saveRDS(df, "./report/cleaned_data.rds")


accuracy_ci <- df %>% subset(word_name =="8" & condition !="filler") %>%
  mutate(condition2 = paste(condition, qtype, sep = "_")) %>%
  ci_cousineau(8, subject = subject, DV = correct, group = condition2) %>% 
  tidyr::separate(condition2, into= c("parallelism","disambiguation","correct_answer"), sep="_") %>% 
  dplyr::rename(accuracy = M)
accuracy_ci$correct_answer %<>% reorder.factor(new.order = c("yes","no"))

saveRDS(accuracy_ci, "./report/accuracy_ci.rds")


# empty data frame for confidence intervals
RTs_ci <- data.frame(condition = factor(),
                     M = numeric(),
                     Var = numeric(),
                     N = numeric(),
                     SE = numeric(),
                     word_name = factor())


# pavel hoca's function for ci of RTs
for (i in 1:length(unique(df$word_name))){
  l <- df %>% 
    subset(condition !="filler" & correct ==1 & word_name ==i) %>% 
    ci_cousineau(4, subject, RT, condition)
  l$word_name <- i %>% as.factor()
  RTs_ci %<>% rbind(l)
}

RTs_ci %<>% tidyr::separate(condition, into = c("parallelism","disambiguation"), sep="_")
RTs_ci$parallelism %<>% dplyr::recode(even = "par", uneven ="non-par")
RTs_ci$condition <- paste(RTs_ci$parallelism, RTs_ci$disambiguation, sep = "_")

saveRDS(RTs_ci, "./report/sentence_RT_ci.rds")
### END SANITY CHECKS ###




### BEGIN PLOTTING & VISUALIZATION###
# accuracy by response time, conditions
# df %>% subset(condition!="filler") %>%
#   dplyr::select(correct, condition, response_time) %>%
#   group_by(condition) %>%
#   summarise(accuracy = mean(correct), average_time = mean(response_time),
#             se_accuracy = se(correct), se_time = se(response_time)) %>% 
#   ggplot(aes(accuracy, average_time, group=condition, color= condition)) + geom_point() +
#   geom_errorbar(aes(ymin= average_time - se_time, ymax = average_time + se_time), width = .01, color = "black") +
#   geom_errorbarh(aes(xmin = accuracy - se_accuracy, xmax = accuracy + se_accuracy), height = 30)



# words for sentence plot
# SA_label <- c("Bence",
#               "baron",
#               "ve",
#               "cesur",
#               "şövalyeyi",
#               "ödüllendiren",
#               "kral",
#               "onları/ birbirlerini", 
#               "şatoda",
#               "dinleyecek")

# word reading plot with 95% confidence intervals using cousineau standart errors (withing subject confidence intervals)
# RTs_ci %>%
#   dplyr::select(condition, word_name, average_RT = M, ci = CI) %>%
#   ggplot(aes(word_name, average_RT, group = condition, color = condition)) +
#   theme(text=element_text(size=12),
#         legend.position = "bottom", legend.title = element_blank(),
#         axis.text.x = element_text(face = "bold.italic", angle = 30, hjust = 1)) +
#   geom_point(aes(shape=condition),size= 2.4) + 
#   geom_line(position = position_dodge(0.35)) +
#   geom_errorbar(aes(ymin = average_RT -ci, ymax = average_RT + ci, color=condition),
#                 alpha = 0.4,
#                 width = .1) +
#   scale_x_discrete(labels = SA_label, name ="") + ylab("average RT (ms)")
### END PLOTTING ###





### BEGIN MODELS ###
#prepare df_model for models
df_model <- df %>% subset(condition !="filler" & word_name %in% c("5","8","9","10")) %>% drop.levels()
df_model %<>% dplyr::select(word_name, subject, item, 
                            parallelism, disambiguation, qtype,
                            RT, response_time, correct)
df_model$word_name %<>% dplyr::recode(`5`= "conj2",`8` = "critical", `9` = "spillover1", `10` = "spillover2")
df_model %<>% tidyr::pivot_wider(names_from = "word_name", values_from = "RT")


df_model$parallelism %<>% as.factor() %>% reorder.factor(new.order = c("par","non-par"))
df_model$disambiguation %<>% as.factor() %>% reorder.factor(new.order = c("subj","obj"))

contrasts(df_model$parallelism) <- contr.sum(2)
contrasts(df_model$disambiguation) <- contr.sum(2)


# write models
RT_models <- brm(mvbind(critical, spillover1, spillover2) ~ parallelism*disambiguation +
                   (parallelism*disambiguation|item) + 
                   (parallelism*disambiguation|subject),
               data = df_model %>% subset(correct =="1"), 
               family = lognormal("identity"), chains = 4, cores = 4, iter = 3000, warmup = 2000,
               file = "./report/model_RT")

RT_models_df <- RT_models %>% 
  mcmc_intervals_data(pars=vars(starts_with("b_")), prob_outer = 0.95) %>% 
  tidyr::separate(col = "parameter", into= c("x","model","parameter"), sep="\\_") %>% 
  subset(parameter != "Intercept") %>% dplyr::select(-x)

RT_models_df$parameter %<>% dplyr::recode(`parallelism1` = "parallel", 
                                            `disambiguation1` = "subject",
                                            `parallelism1:disambiguation1` = "subj*par") %>%
  reorder.factor(new.order = c("subject","parallel","subj*par"))

RT_models_df %>% ggplot(aes(m, y=factor(parameter, 
                                          levels = rev(levels(factor(parameter)))))) +
  theme(text=element_text(size=10), strip.text.x = element_text(size=10),
        axis.title.x = element_text(size=10), axis.title.y = element_text(size=10)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + vline_0() +
  xlab("Estimate(log)") + ylab("coefficients") + facet_grid(.~model)

# sanity check
# RT_models_samples <- posterior_samples(RT_models, pars = c("b_Intercept","b_parallelism1","b_disambiguation1",
#                                                    "b_parallelism1:disambiguation1"))
# pairs(RT_models_samples)


# set contrast for qtype
df_model$qtype %<>% reorder.factor(new.order = c("no","yes"))
contrasts(df_model$qtype) <- contr.sum(2)

# accuracy model
response_accuracy <- brm(correct ~ parallelism*disambiguation*qtype + 
                           (parallelism*disambiguation*qtype|subject) +
                           (parallelism*disambiguation|item),
                         data = df_model, 
                         family = bernoulli("logit"), chains = 4, cores = 4, iter = 3000, warmup = 2000,
                         file="./report/model_response_accuracy")

response_accuracy_df <- response_accuracy %>%
  mcmc_intervals_data(pars = vars(starts_with("b_")), prob_outer = 0.95) %>%
  tidyr::separate(col = "parameter", into = c("x","parameter"), sep ="\\_") %>%
  subset(parameter != "Intercept") %>% dplyr::select(-x)

response_accuracy_df$parameter %<>% dplyr::recode(`parallelism1` = "parallel",
                                                  `disambiguation1` = "subject",
                                                  `qtype1` = "no",
                                                  `parallelism1:disambiguation1` = "subj*par",
                                                  `disambiguation1:qtype1` = "subj*no",
                                                  `parallelism1:qtype1` = "par*no",
                                                  `parallelism1:disambiguation1:qtype1` = "subj*par*no") %>%
  reorder.factor(new.order= c("subject","parallel","no","subj*par","subj*no","par*no","subj*par*no"))

response_accuracy_df %>% ggplot(aes(m, y=factor(parameter, 
                                                levels = rev(levels(factor(parameter)))))) +
  theme(text=element_text(size=10), strip.text.x = element_text(size=10),
        axis.title.x = element_text(size=10), axis.title.y = element_text(size=10)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + vline_0() +
  xlab("Estimate(log-odds)") + ylab("coefficients")


# sanity check
# response_accuracy_samples <- posterior_samples(response_accuracy, 
#                                                pars = c("b_Intercept","b_parallelism1","b_disambiguation1",
#                                                         "b_qtype1","b_parallelism1:disambiguation1",
#                                                         "b_parallelism1:qtype1","b_disambiguation1:qtype1",
#                                                         "b_parallelism1:disambiguation1:qtype1"))
# pairs(response_accuracy_samples)



noun_RT <- brm(conj2 ~ parallelism + (parallelism|subject) + (parallelism|item),
               data = df_model %>% subset(correct=="1"),
               family = lognormal("identity"), chains = 4, cores = 4, iter = 3000, warmup = 2000,
               file = "./report/model_noun_RT")







