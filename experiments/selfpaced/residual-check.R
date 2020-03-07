library(dplyr) # magic
library(magrittr) # piped magic
library(purrr)
library(tidyr) # dark magic
library(tidybayes)
library(ggplot2)
library(ggridges)
library(cowplot)
library(rstan)
library(brms)
library(gganimate)
library(gdata)
library(bayesplot)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

theme_set(theme_tidybayes() + panel_border())
bayesplot_theme_set(new = theme_bw())

df_reads <- readRDS("./report/df_reads.rds")

df_SA_model <- df_reads %>% subset(word_number %in% c("5") & cat !="Contrast") %>% drop.levels()
df_SA_model$word_number %<>% as.factor()
df_SA_model$cat %<>% reorder.factor(new.order = c("No_SA", "One_SA", "Full_SA"))
df_SA_model$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))
contrasts(df_SA_model$cat) <- MASS::contr.sdif(3) # %T>% { colnames(.) <- c('ali','yegda') } 
df_SA_model$p_word_length <- df_SA_model$word_length %>% log() %>% scale()  

SA_target <- brm(RT ~ 1 + cat*conjoiner + (cat*conjoiner | item) + (cat*conjoiner | subject), 
                 data = df_SA_model,
                 family = lognormal('identity'), chains = 4, cores = 4, iter = 2000,
                 file = "./report/model_SA_target")


SA_target_results <- fixef(SA_target, summary = TRUE, robust = FALSE) %>% as.data.frame() %>%
  tibble::rownames_to_column("variables")
mcmc_intervals(SA_target, pars =  paste0("b_", SA_target_results$variables))

df_SA_model %>% 
  add_residual_draws(SA_target) %>%
  mean_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq(distribution = stats::qlnorm) + geom_qq_line(distribution = stats::qlnorm) + facet_grid(cat~conjoiner)
  
  
  
  


