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


df <- readRDS("./report/cleaned_data.rds") %>% 
  subset(condition !="filler") %>% 
  drop.levels()


accuracy_ci <- df %>%
  subset(word_name == 8) %>%
  drop.levels() %>% ci_cousineau(4, subject, correct, condition)


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
    subset(correct ==1 & word_name ==i) %>% 
    ci_cousineau(4, subject, RT, condition)
  l$word_name <- i %>% as.factor()
  RTs_ci %<>% rbind(l)
}

# words for sentence plot
SA_label <- c("Bence",
              "baron",
              "ve",
              "cesur",
              "şövalyeyi",
              "ödüllendiren",
              "kral",
              "onları/ birbirlerini", 
              "şatoda",
              "dinleyecek")

# word reading plot with 95% confidence intervals using cousineau standart errors (withing subject confidence intervals)
RTs_ci %>%
  dplyr::select(condition, word_name, average_RT = M, ci = CI) %>%
  ggplot(aes(word_name, average_RT, group = condition, color = condition)) +
  theme(text=element_text(size=12),
        legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(face = "bold.italic", angle = 30, hjust = 1)) +
  geom_point(aes(shape = condition), size= 2.4, position = position_dodge(0.35)) + 
  geom_line(linetype = "dashed", position = position_dodge(0.35)) +
  geom_errorbar(aes(ymin = average_RT -ci, ymax = average_RT + ci), color = "black",
                alpha = 0.4,
                width = .3, position = position_dodge(0.35)) +
  scale_x_discrete(labels = SA_label, name ="") + ylab("average RT (ms)") +
  scale_colour_brewer(palette = "Dark2")








