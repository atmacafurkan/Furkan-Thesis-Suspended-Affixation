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

df_SA_model <- df_reads %>% subset(word_number %in% c(7:9) & cat !="Contrast") %>% drop.levels()
df_SA_model$word_number %<>% dplyr::recode(`7` = "critical", `8`="so1", `9` = "so2")

df_SA_model$cat %<>% reorder.factor(new.order = c("No_SA", "One_SA", "Full_SA"))
df_SA_model$conjoiner %<>% reorder.factor(new.order = c("veya","ve"))


df_SA_model %<>% dplyr::select(subject, item, cat, conjoiner, word_number, RT) %>%
  tidyr::pivot_wider(names_from = "word_number", values_from = "RT")


SA_target <- readRDS("./report/SA_target_model.rds")


df_SA_model %>% 
  add_residual_draws(SA_target) %>%
  mean_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq(distribution = stats::qlnorm) + geom_qq_line(distribution = stats::qlnorm) + facet_grid(cat~conjoiner)
  
  
  
  




