# general libraries
library(dplyr) # magic
library(magrittr) # piped magic
library(tidyr) # dark magic
library(readxl) # reads excel files
library(gdata) # some functions here and there

# withins subjects condifdence interval function
library(papaja)

`%notin%` <- Negate(`%in%`)
se <- function(x) sd(x)/sqrt(length(x))


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


df_responses_SA <- readRDS("./report/df_responses.rds") %>%
  subset(cat!= "Contrast") %>% drop.levels()
df_responses_SA$grouper <- paste(df_responses_SA$cat, df_responses_SA$conjoiner, sep="armut") 

df_responses_Contrast <- readRDS("./report/df_responses.rds") %>%
  subset(cat== "Contrast") %>% drop.levels() 
df_responses_Contrast$grouper <- paste(df_responses_Contrast$cat, df_responses_Contrast$conjoiner, sep="armut") 

accuracy_SA_ci <- df_responses_SA %>% drop.levels() %>%
  ci_cousineau(6, subject, correct, grouper) %>% 
  tidyr::separate(grouper, into=c("category","conjoiner"), sep="armut")

accuracy_Contrast_ci <- df_responses_Contrast %>% drop.levels() %>%
  ci_cousineau(2, subject, correct, grouper) %>% 
  tidyr::separate(grouper, into=c("category","conjoiner"), sep="armut")

accuracy_all_ci <- rbind(accuracy_SA_ci, accuracy_Contrast_ci)

df_reads_SA <- readRDS("./report/df_reads.rds") %>%
  subset(cat!= "Contrast") %>% drop.levels()
df_reads_SA$grouper <- paste(df_reads_SA$cat, df_reads_SA$conjoiner, sep="armut")

df_reads_Contrast <- readRDS("./report/df_reads.rds") %>%
  subset(cat== "Contrast") %>% drop.levels()
df_reads_Contrast$grouper <- paste(df_reads_Contrast$cat, df_reads_Contrast$conjoiner, sep="armut")


# empty data frame for confidence intervals
SA_RTs_ci <- data.frame(grouper = factor(),
                        M = numeric(),
                        Var = numeric(),
                        N = numeric(),
                        SE = numeric(),
                        word_name = factor())


# pavel hoca's function for ci of RTs
for (i in 1:length(unique(df_reads_SA$word_number))){
  l <- df_reads_SA %>% 
    subset(word_name ==i) %>% 
    ci_cousineau(6, subject, RT, grouper)
  l$word_number <- i
  SA_RTs_ci %<>% rbind(l)
}

SA_RTs_ci %<>% tidyr::separate(grouper, into=c("category","conjoiner"), sep="armut")


# empty data frame for confidence intervals
Contrast_RTs_ci <- data.frame(grouper = factor(),
                        M = numeric(),
                        Var = numeric(),
                        N = numeric(),
                        SE = numeric(),
                        word_name = factor())


# pavel hoca's function for ci of RTs
for (i in 1:length(unique(df_reads_Contrast$word_number))){
  l <- df_reads_Contrast %>% 
    subset(word_name ==i) %>% 
    ci_cousineau(6, subject, RT, grouper)
  l$word_number <- i
  Contrast_RTs_ci %<>% rbind(l)
}

Contrast_RTs_ci %<>% tidyr::separate(grouper, into=c("category","conjoiner"), sep="armut")

RTs_all_ci <- rbind(SA_RTs_ci, Contrast_RTs_ci)


saveRDS(accuracy_all_ci,"./report/accuracy_all_ci.rds")
saveRDS(RTs_all_ci, "./report/RTs_all_ci.rds")





