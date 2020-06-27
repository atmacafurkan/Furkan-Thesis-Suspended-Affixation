library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)


df <- read_excel("SA-Constituent-Equivalence.xlsx") %>% 
  subset(item=="experimental") %>% na.omit() %>% 
  tibble::rownames_to_column(var = "item_number")

df_filler <- read_excel("SA-Constituent-Equivalence.xlsx") %>% 
  subset(item=="filler") %>% dplyr::select(main, sa_case, question, question_type) %>% na.omit %>%
  tibble::rownames_to_column(var = "item_number")
df_filler$item_number %<>% as.integer() %>% +100

df$even_obj <- with(df, paste(topic_pos, noun1, conj1, noun2, embedded, rcnoun, disamb1, argument, main))

df$even_subj <- with(df, paste(topic_pos, noun1, conj1, noun2, embedded, rcnoun, disamb2, argument, main))

df$uneven_obj <- with(df, paste(topic_pos, noun1, conj1, mod2, noun2, embedded, rcnoun, disamb1, argument, main))

df$uneven_subj <- with(df, paste(topic_pos, noun1, conj1, mod2, noun2, embedded, rcnoun, disamb2, argument, main))


df %<>% dplyr::select(item_number, sa_case, even_obj, even_subj, uneven_obj, uneven_subj, question, question_type)


saveRDS(df,"./report/items_df")
saveRDS(df_filler, "./report/fillers_df")


fillers <- sprintf("[[\"filler\",%s], \"DashedAcceptabilityJudgment\", 
                   {s: \"%s\", 
                   q: \"%s\"}],",
                   df_filler$item_number, df_filler$main, df_filler$question)

expSA_a <- sprintf("[[\"expSA_a\",%s], \"DashedAcceptabilityJudgment\", 
                   {s: \"%s\", 
                   q: \"%s\"}],",
                   df$item_number, df$even_obj, df$question)

expSA_b <- sprintf("[[\"expSA_b\",%s], \"DashedAcceptabilityJudgment\", 
                   {s: \"%s\", 
                   q: \"%s\"}],", 
                    df$item_number, df$even_subj, df$question)

expSA_c <- sprintf("[[\"expSA_c\",%s], \"DashedAcceptabilityJudgment\",
                   {s: \"%s\", 
                   q: \"%s\"}],", 
                    df$item_number, df$uneven_obj, df$question)

expSA_d <- sprintf("[[\"expSA_d\",%s], \"DashedAcceptabilityJudgment\",
                   {s: \"%s\", 
                   q: \"%s\"}],",
                    df$item_number, df$uneven_subj, df$question)


filing <-file("constituent_self_paced_ibex_items.txt")
writeLines(c(expSA_a, expSA_b,
             expSA_c, expSA_d,
             fillers), filing)
close(filing)

# sentences as an independent txt file
sentence_items_a <- sprintf("%s\\_par-subj\\_%s \\\\", df$item_number, df$even_subj)
sentence_items_b <- sprintf("%s\\_nonpar-subj\\_%s \\\\", df$item_number, df$uneven_subj)
sentence_items_c <- sprintf("%s\\_par-obj\\_%s \\\\", df$item_number, df$even_obj)
sentence_items_d <- sprintf("%s\\_nonpar-obj\\_%s \\\\", df$item_number, df$uneven_obj)
sentence_fillers <- sprintf("%s\\_filler\\_%s \\\\", df_filler$item_number, df_filler$main)

df$question_type %<>% dplyr::recode(`two_subjects_correct` = "subject correct", `one_subject_correct` = "object correct")
item_questions <- sprintf("%s\\_%s\\_%s \\\\", df$item_number, df$question_type, df$question)
filler_questions <- sprintf("%s\\_%s\\_%s \\\\", df_filler$item_number, df_filler$question_type, df_filler$question)


filing2 <- file("constituent_self_paced_sentences.txt")
writeLines(c(sentence_items_a, sentence_items_b,
             sentence_items_c, sentence_items_d,
             sentence_fillers), filing2)
close(filing2)

filing3 <- file("constituent_self_paced_questions.txt")
writeLines(c(item_questions, filler_questions), filing3)
close(filing3)





