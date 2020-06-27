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


ci_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL, t_gamma = 1.96){
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


df <- readRDS("./report/df_judgments") %>% 
  subset(experiment !="filler") %>% 
  drop.levels()

df$grouper <- paste(df$Suffix, df$conjoiner,sep = "_")
  
  
acceptance_ci <- df %>% 
  ci_cousineau(n_conditions= 18, subject=subject, DV= Cresponse, grouper) %>%
  tidyr::separate(grouper, into =c("suffix","conjoiner"), sep="_")

saveRDS(acceptance_ci, "./report/acceptance_ci.rds")







