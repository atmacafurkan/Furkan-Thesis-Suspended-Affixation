library(dplyr)
library(tidyr)
library(magrittr)
library(gdata)
library(anytime)

df <- read.csv("results.csv", 
               header = F, 
               comment.char = "#", 
               encoding = "UTF-8" , 
               col.names = paste0("V",seq_len(9)), 
               fill = TRUE)

colnames(df) <- c("time","md5","controller",
                  "item","element","type",
                  "group","field_name","field_value")


df %<>% subset(type =="debrief") %>% dplyr::select(-item,-element,-type,-group, -controller)

df %<>% pivot_wider(names_from = field_name, values_from = field_value) %>% dplyr::select(time, md5, name, studentid, profname)

df$time %<>% as.character() %>% as.integer()
df$md5 %<>% as.character()
df$name %<>% as.character()
df$studentid %<>% as.character()

df %<>% subset(name !="Furkan Atmaca")

df$actual_time <- anytime(df$time)





