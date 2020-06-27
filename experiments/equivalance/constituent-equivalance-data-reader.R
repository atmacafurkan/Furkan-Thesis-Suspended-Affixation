# general libraries
library(dplyr) # magic
library(magrittr) # piped magic
library(tidyr) # dark magic
library(ggplot2) # plots things
library(cowplot) # some plot options
theme_set(theme_bw()) # set ggplot theme
library(readxl) # reads excel files
library(gdata) # some functions here and there

library(readxl) # reads excel files
library(gdata) # some functions here and there
library(stringr)

library(anytime) # convert time stamps to actual times


`%notin%` <- Negate(`%in%`) # I hate using ! at the very beginning of logical expressions
se <- function(x) sd(x)/sqrt(length(x))

df_items <- readRDS("./report/items_df") %>% dplyr::select(item = item_number, question, question_type)
df_items$item %<>% as.integer()


df_fillers <- readRDS("./report/fillers_df") %>% dplyr::select(item = item_number, question, question_type)
df_fillers$item %<>% as.integer()

df_sentences <- rbind(df_items, df_fillers)

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
df$condition %<>% as.character()
df$condition %<>% dplyr::recode(a="par_obj",b="par_subj",c="non-par_obj",d="non-par_subj") %>% as.factor()
df$word_number %<>% as.character() %>% as.integer()
df$RT %<>% as.character() %>% as.integer()
df$response_time %<>% as.character() %>% as.integer()
df %<>% dplyr::arrange(subject, item, word_number) %>% 
  dplyr::select(subject, item, experiment, condition, word, word_number, RT, response_time,MD5)


# merge sentences with data
df %<>% left_join(df_sentences)


# create data frame of word reading times 
df_reads <- df %>% 
  subset(word %notin% c("Evet (Q'ya basınız)","Hayır (P'ye basınız)")) %>%
  ungroup() %>% dplyr::select(-response_time)

df_reads$word <- gsub('\\.','',df_reads$word)
df_reads$word_length <- nchar(df_reads$word)
df_reads$word_number <- ifelse(!str_detect(df_reads$condition,"non-par") &
                                 df_reads$word_number > 3, df_reads$word_number+1, df_reads$word_number)
df_reads$word_name <- df_reads$word_number %>% as.factor()


# separate responses, calculate correct response for fillers
df_responses <- df %>% subset(word %in% c("Evet (Q'ya basınız)","Hayır (P'ye basınız)")) %>%
  dplyr::select(-word_number, -RT)
df_responses$response <- ifelse(df_responses$word =="Evet (Q'ya basınız)",1,0)
df_responses %<>% dplyr::select(-word) %>% ungroup()

df_responses$question_code <- with(df_responses,
                                   ifelse(str_detect(condition, "obj") & str_detect(question_type, "one"),1,
                                          ifelse(str_detect(condition,"subj") & str_detect(question_type,"two"),1,
                                                 ifelse(condition=="x" & question_type=="correct",1,0))))

df_responses$correct <- ifelse(df_responses$response == df_responses$question_code,1,0)


# conjoin RT and response data
df_all <- left_join(df_reads, df_responses) 
df_all$parallelism <- with(df_all, ifelse(str_detect(condition, "non-par"),"non-par",
                          ifelse(condition =="x","filler","par")))

df_all$disambiguation <- with(df_all, ifelse(str_detect(condition, "obj"),"obj",
                                ifelse(condition=="x","filler","subj")))

saveRDS(df_all,"./report/organised_data.rds")


