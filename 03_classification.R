pacman::p_load(tidyverse, tidytext, quanteda, textdata)

rm(list = ls())

# importing data ---------------------------------------------------------------

green <- read.delim("green.txt", header = FALSE)

green <- str_c(green$V1, collapse = " ")

reform <- read.delim("reform.txt", header = FALSE)

reform <- str_c(reform$V1, collapse = " ")

# cleaning and binding manifestos -------------------------------------------------------

green_sentences <- green |> 
  as_tibble_col(column_name = "text") |> 
  unnest_tokens(sentence, text, token = "sentences") |> 
  mutate(
    sentence = str_remove_all(sentence, "[^-[:^punct:]]|[0-9]| -") |>
      str_replace_all("realhoperealchange", "real hope real change") |>
      str_replace_all("realchangereal", "real change") |> 
      str_replace_all("realhopereal", "real hope") |> 
      str_trim(),
    sentence_id = str_c("green_",row_number()),
    party = "green"
  )

reform_sentences <- reform |> 
  as_tibble_col(column_name = "text") |> 
  unnest_tokens(sentence, text, token = "sentences") |> 
  mutate(
    sentence = str_remove_all(sentence, "[^-[:^punct:]]|[0-9]| -") |>
      str_replace_all("contractour", "contract our") |>
      str_replace_all("youwith", "you with") |> 
      str_trim(),
    sentence_id = str_c("reform_",row_number()),
    party = "reform"
  )

full_sentences <- bind_rows(green_sentences, reform_sentences)

# word tf-idf ----------------------------------------------------------------------

green_stops <- tibble(word = c("green","greens","bullet","bn","uk","election",
                               "party","poundbn"), 
                      lexicon = "green_stops")

reform_stops <- tibble(word = c("uk","election","party","reform","pa","swh","hw","poundk","poundm"), 
                       lexicon = "reform_stops")

party_tf_idf <- full_sentences |> 
  unnest_tokens(word, sentence, token = "words") |>
  anti_join(bind_rows(stop_words,green_stops, reform_stops)) |> 
  count(party, word) |> 
  bind_tf_idf(word, party, n)

party_tf_idf |>
  group_by(party) |> 
  slice_max(order_by = tf_idf, n = 50) |> 
  ggplot(aes(x = tf_idf, y = fct_reorder(word, tf_idf), fill = party)) +
  geom_col() +
  facet_wrap(~party, scales = "free") +
  scale_fill_manual(values = c("springgreen4","cyan3")) +
  labs(x = "TF-IDF", y = NULL)

# predictions for logit model --------------------------------------------

predictors <- party_tf_idf |>
  group_by(party) |> 
  slice_max(order_by = tf_idf, n = 50) |> 
  ungroup() |> 
  select(word) |> 
  as_vector()

# logit model -------------------------------------------------------

full_dfm <- full_sentences |> 
  unnest_tokens(word, sentence, token = "words") |>
  anti_join(bind_rows(stop_words, green_stops, reform_stops)) |> 
  count(sentence_id, word) |> 
  cast_dfm(sentence_id, word, n) |> 
  dfm_select(pattern = predictors) |> 
  convert(to = "data.frame") |> 
  mutate(party = as.factor(ifelse(str_detect(doc_id,"^r"),"reform","green")),
         .before = 1)

full_dfm |> head(10)
levels(full_dfm$party)
full_dfm |> 
  count(party) |> 
  mutate(prop = n / sum(n))

dfm_logit <- glm(party ~ . -doc_id, data = full_dfm, family = "binomial") 
summary(dfm_logit)

dfm_probs <- predict(dfm_logit, type = "response")
dfm_preds <- ifelse(dfm_probs > 0.5, "reform", "green")
table(dfm_preds, full_dfm$party)
mean(dfm_preds == full_dfm$party)

# extracting glove embeddings ----------------------------------------------

glove6b <- read_delim("glove.6B.200d.txt", delim = " ", col_names = FALSE, quote = "")
names(glove6b) <- c("word",str_c("v",seq(1,200,1)))
vector_names <- glove6b |> select(v1:v200) |> names()

full_words <- full_sentences |> 
  unnest_tokens(word, sentence, token = "words") |> 
  anti_join(bind_rows(stop_words, green_stops, reform_stops)) 

full_glove <- full_words |> 
  inner_join(glove6b, by = "word") |> 
  group_by(party, sentence_id) |> 
  summarise(across(all_of(vector_names), ~mean(.x)),
            .groups = "drop") |> 
  mutate(party = as.factor(party))

levels(full_glove$party)

# logit with glove embeddings ------------------------------------

glove_logit <- glm(party ~ . -sentence_id, data = full_glove, family = "binomial") 
summary(glove_logit)

glove_probs <- predict(glove_logit, type = "response")
glove_preds <- ifelse(glove_probs > 0.5, "reform", "green")
table(glove_preds, full_glove$party)
mean(glove_preds == full_glove$party)