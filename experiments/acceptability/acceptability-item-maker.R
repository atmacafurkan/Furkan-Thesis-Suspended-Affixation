library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)

# import the excel sheet for item creation
df <- read_excel("SA-Survey-Items.xlsx") %>% subset(Problem !="p" & experimental_cat =="exp_item")
df_filler <- read_excel("SA-Survey-Items.xlsx") %>% subset(experimental_cat=="filler") %>% dplyr::select(item_id, sentence = Sentence)


df$noSA_sentence <- paste(df$PreConjunction, x= paste0(df$NP1, df$Suffix1), df$CONJ, y= paste0(df$NP2, df$Suffix2), df$Sentence)
df$SA_sentence <-  paste(df$PreConjunction, df$NP1, df$CONJ, y= paste0(df$NP2, df$Suffix2), df$Sentence)
df %<>% dplyr::select(item_id, noSA_sentence, SA_sentence, Suffix, SA_type, CONJ)
df$Cconj <- ifelse(df$CONJ=="ve", "a","b")


fillers <- sprintf("[[\"filler\",%s], \"AcceptabilityJudgment\",{s: \"%s\"}],", df_filler$item_id, df_filler$sentence)
items <- sprintf("[[\"exp_%s\",%s], \"AcceptabilityJudgment\",{s: \"%s\"}],", df$Cconj, df$item_id, df$SA_sentence)


# export the ibexfarm items to a txt file
filing <-file("acceptability_ibex_items.txt")
writeLines(c(items,fillers), filing)
close(filing)


# sentences as an independent txt file
sentence_items <- sprintf("%s\\_%s \\\\", df$item_id, df$noSA_sentence)
sentence_fillers <- sprintf("%s\\_%s\\\\", df_filler$item_id, df_filler$sentence)


filing2 <- file("acceptability_items_as_sentences.txt")
writeLines(c(sentence_items, sentence_fillers), filing2)
close(filing2)

