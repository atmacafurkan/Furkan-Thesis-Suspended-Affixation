library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)


df1 <- read.csv2("frequency_list_cı_mid.csv", sep ="\t") %>% mutate(suffix = "-CI")


df2 <- read.csv2("frequency_list_lık_mid.csv", sep = "\t") %>% mutate(suffix = "-lIK")


df3 <- read.csv2("frequency_list_sız_end.csv", sep = "\t") %>% mutate(suffix = "-sIz")


df4 <- read.csv2("frequency_list_sız_mid.csv", sep = "\t") %>% mutate(suffix = "-sIz")


df5 <- read.csv2("frequency_list_with_end.csv", sep = "\t") %>% mutate(suffix = "-lI")


df6 <- read.csv2("frequency_list_with_mid.csv", sep = "\t") %>% mutate(suffix = "-lI")


df <- rbind(df1,df2,df3,df4,df5,df6) %>% 
  dplyr::select(suffix, frequency = Frequency)

saveRDS(df, "../report/suffixfrequency")


df$suffix %<>% as.factor()
df$total <- sum(df$frequency)
df %<>% group_by(suffix) %>% summarise(ratio = sum(frequency) / mean(total))


df %>% ggplot(aes(suffix, ratio, fill = suffix)) + geom_bar(stat = "identity")








