library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)

# import the excel sheet for item creation
df <- read_excel("SA-Experiment-Items.xlsx") %>% subset(SA_type !="filler") %>% subset(SA_type=="verbal") 
df_fillers <- read_excel("SA-Experiment-Items.xlsx") %>% subset(SA_type =="filler") 

df$qtype <- ifelse(df$question == df$ques_yes, "correct","incorrect")
df_fillers$qtype <- ifelse(df_fillers$question == df_fillers$ques_yes, "correct","incorrect")


# form sentences from the data frame
df$noSA_ve <-  paste(df$PreSentence, y= paste0(df$Verb1, df$Suffix1_1, df$Suffix1_2),
                         df$CONJ, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)
df$noSA_veya <-  paste(df$PreSentence, y= paste0(df$Verb1, df$Suffix1_1, df$Suffix1_2),
                       df$CONJ2, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)

df$oneSA_ve <-  paste(df$PreSentence, y= paste0(df$Verb1, df$Suffix1_1),
                           df$CONJ, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)
df$oneSA_veya <-  paste(df$PreSentence, y= paste0(df$Verb1, df$Suffix1_1),
                      df$CONJ2, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)

df$fullSA_ve <-  paste(df$PreSentence, df$Verb1,
                            df$CONJ, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)
df$fullSA_veya <-  paste(df$PreSentence, df$Verb1,
                       df$CONJ2, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)

df$contrast_ve <- paste(df$PreSentence, df$Verb1_alt,
                              df$CONJ, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)
df$contrast_veya <- paste(df$PreSentence, df$Verb1_alt,
                        df$CONJ2, z=paste0(df$Verb2, df$Suffix2_1, df$Suffix2_2), df$Sentence)

df %<>% dplyr::select(item_id, noSA_ve, noSA_veya, 
                      oneSA_ve, oneSA_veya, 
                      fullSA_ve, fullSA_veya, 
                      contrast_ve, contrast_veya, 
                      question, qtype)

df_fillers$filler <- paste(df_fillers$PreSentence,
                                    df_fillers$Verb1,
                                    df_fillers$Suffix1_1,
                                    df_fillers$Suffix1_2,
                                    df_fillers$CONJ,
                                    df_fillers$Verb2,
                                    df_fillers$Suffix2_1,
                                    df_fillers$Suffix2_2,
                                    df_fillers$Sentence,
                                    df_fillers$SA1,
                                    df_fillers$SA2)

df_fillers %<>% dplyr::select(item_id, filler, question, qtype) %>% na.omit()


# form the SAsentences according to ibexfarm item format
noSA_ve <- sprintf("[[\"expSA_a\",%s], \"DashedSentence\",
                   {s: \"%s\"},\"Question\",
                   {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                   df$item_id, df$noSA_ve, df$question)
noSA_veya <- sprintf("[[\"expSA_b\",%s], \"DashedSentence\",
                     {s: \"%s\"},\"Question\",
                     {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                     df$item_id, df$noSA_veya, df$question)

oneSA_ve <- sprintf("[[\"expSA_c\",%s], \"DashedSentence\",
                    {s: \"%s\"},\"Question\",
                    {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                    df$item_id, df$oneSA_ve, df$question)
oneSA_veya <- sprintf("[[\"expSA_d\",%s], \"DashedSentence\",
                      {s: \"%s\"},\"Question\",
                      {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],",
                      df$item_id, df$oneSA_veya, df$question)

fullSA_ve <- sprintf("[[\"expSA_e\",%s], \"DashedSentence\",
                     {s: \"%s\"},\"Question\",
                     {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                     df$item_id, df$fullSA_ve, df$question)
fullSA_veya <- sprintf("[[\"expSA_f\",%s], \"DashedSentence\",
                       {s: \"%s\"},\"Question\",
                       {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                       df$item_id, df$fullSA_veya, df$question)

contrast_ve <- sprintf("[[\"expSA_g\", %s],\"DashedSentence\",
                       {s: \"%s\"}, \"Question\",
                       {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                       df$item_id, df$contrast_ve, df$question)
contrast_veya <- sprintf("[[\"expSA_h\", %s],\"DashedSentence\",
                         {s: \"%s\"}, \"Question\",
                         {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                         df$item_id, df$contrast_veya, df$question)

fillers <- sprintf("[[\"filler\",%s],\"DashedSentence\",
                   {s: \"%s\"}, \"Question\",
                   {q: \"%s\", as: [[\"q\",\"Evet (Q'ya basınız)\"], [\"p\",\"Hayır (P'ye basınız)\"]]}],", 
                   df_fillers$item_id, df_fillers$filler, df_fillers$question) 


# export the ibexfarm items to a txt file
filing <-file("self_paced_ibex_items.txt")
writeLines(c(noSA_ve, noSA_veya, 
             oneSA_ve, oneSA_veya, 
             fullSA_ve, fullSA_veya, 
             contrast_ve, contrast_veya, 
             fillers), filing)
close(filing)


# sentences as an independent txt file
sentence_items <- sprintf("%s\\_%s \\\\", df$item_id, df$noSA_ve)
sentence_fillers <- sprintf("%s\\_%s \\\\", df_fillers$item_id, df_fillers$filler)
items_questions <- sprintf("%s\\_%s\\_%s \\\\", df$item_id, df$qtype, df$question)
fillers_questions <- sprintf("%s\\_%s\\_%s \\\\", df_fillers$item_id, df_fillers$qtype, df_fillers$question)


filing2 <- file("self_paced_items_as_sentences.txt")
writeLines(c(sentence_items, sentence_fillers), filing2)
close(filing2)

filing3 <- file("self_paced_questions.txt")
writeLines(c(items_questions, fillers_questions), filing3)
close(filing3)






