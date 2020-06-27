library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)



df <- read.csv("inci_search_20words_periph.txt",sep="\t") %>% dplyr::select(3:5)

colnames(df) <- c("before", "hit", "after")
df$before %<>% as.character()
df$after %<>% as.character()

df$periphery <- paste(df$before, df$after)


df$text_type <- ifelse(str_detect(df$periphery, pattern = regex("kanun|hüküm|fıkra|madde|paragraf|yönetmelik|nüsha", ignore_case = T)), "law", 
                            ifelse(str_detect(df$periphery, pattern = regex("gol|takım|lig|futbol|puan|oyun", ignore_case = T)), "football",
                                  ifelse(str_detect(df$periphery, pattern = regex("sınıf|ders|okul|eğitim|öğrenci", ignore_case = T)), "education","others") 
                                   )
                       )
saveRDS(df,"./report/inci_corpus_text.rds")

df %>% group_by(text_type) %>% summarise(count = n())
