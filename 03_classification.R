# loading packages
pacman::p_load(tidyverse, tidytext, quanteda, textdata)

# clearing environment
rm(list = ls())

# importing data ---------------------------------------------------------------

# green manifesto
green <- read.delim("green.txt", header = FALSE)

# making a single string
green <- str_c(green$V1, collapse = " ")

# reform manifesto
reform <- read.delim("reform.txt", header = FALSE)

# making a single string
reform <- str_c(reform$V1, collapse = " ")

# cleaning and binding manifestos -------------------------------------------------------

# green manifesto into sentences
green_sentences <- green |> 
  as_tibble_col(column_name = "text") |> # making tibble 
  unnest_tokens(sentence, text, token = "sentences") |> # making sentences
  mutate(
    sentence = str_remove_all(sentence, "[^-[:^punct:]]|[0-9]| -") |> # removing punctuation (except hyphens) and numbers
      str_replace_all("realhoperealchange", "real hope real change") |> # cleaning conjoined words
      str_replace_all("realchangereal", "real change") |> 
      str_replace_all("realhopereal", "real hope") |> 
      str_trim(), # removing whitespace
    sentence_id = str_c("green_",row_number()), # adding id for sentence, including party
    party = "green" # variable for party
  )

# as above for reform party
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

# binding all data into a single tibble 
full_sentences <- bind_rows(green_sentences, reform_sentences)

# word tf-idf ----------------------------------------------------------------------

# bespoke stops for green party
green_stops <- tibble(word = c("green","greens","bullet","bn","uk","election",
                               "party","poundbn"), 
                      lexicon = "green_stops")

# bespoke stops for reform party
reform_stops <- tibble(word = c("uk","election","party","reform","pa","swh","hw","poundk","poundm"), 
                       lexicon = "reform_stops")

# tf-idf calculation
party_tf_idf <- full_sentences |> 
  unnest_tokens(word, sentence, token = "words") |> # turning sentences into word rows
  anti_join(bind_rows(stop_words,green_stops, reform_stops)) |> # removing stop words
  count(party, word) |> # count of words per party manifesto
  bind_tf_idf(word, party, n) # tf-idf where each party manifesto is a document

# plotting tf-idf per party
party_tf_idf |>
  group_by(party) |> 
  slice_max(order_by = tf_idf, n = 50) |> # top 50 tf-idf per party
  ggplot(aes(x = tf_idf, y = fct_reorder(word, tf_idf), fill = party)) +
  geom_col() +
  facet_wrap(~party, scales = "free") +
  scale_fill_manual(values = c("springgreen4","cyan3")) + # party colours
  labs(x = "TF-IDF", y = NULL)

# predictors for logit model --------------------------------------------

# top 50 words by tf-idf for each party 
predictors <- party_tf_idf |>
  group_by(party) |> 
  slice_max(order_by = tf_idf, n = 50) |> 
  ungroup() |> 
  select(word) |> 
  as_vector()

# logit model -------------------------------------------------------

# document-feature-matrix for logit
full_dfm <- full_sentences |> 
  unnest_tokens(word, sentence, token = "words") |> # one row per word each sentence
  anti_join(bind_rows(stop_words, green_stops, reform_stops)) |>  # removing stopwords
  count(sentence_id, word) |>  # count of words per sentence
  cast_dfm(sentence_id, word, n) |> # turning to dfm
  dfm_select(pattern = predictors) |> # selecting only predictors as columns in dfm
  convert(to = "data.frame") |>  # turning back to data frame
  mutate(party = as.factor(ifelse(str_detect(doc_id,"^r"),"reform","green")), # variable for party i.e. outcome variable
         .before = 1)

full_dfm |> head(10)
levels(full_dfm$party)
full_dfm |> 
  count(party) |> 
  mutate(prop = n / sum(n)) # baseline for predictions - around 60% of sentences are from green party

# logit model
dfm_logit <- glm(party ~ . -doc_id, data = full_dfm, family = "binomial") 
summary(dfm_logit)

# logit probabilities
dfm_probs <- predict(dfm_logit, type = "response")

# logit predictions
dfm_preds <- ifelse(dfm_probs > 0.5, "reform", "green")

# confusion table
table(dfm_preds, full_dfm$party)

# percentage correct predictions
mean(dfm_preds == full_dfm$party)

# extracting glove embeddings ----------------------------------------------

# importing glove 200 dimension vectors
glove6b <- read_delim("glove.6B.200d.txt", delim = " ", col_names = FALSE, quote = "")

# renaming
names(glove6b) <- c("word",str_c("v",seq(1,200,1)))

# vector of embedding names
vector_names <- glove6b |> select(v1:v200) |> names()

# turning into data frame where each row is a word in a sentence
full_words <- full_sentences |> 
  unnest_tokens(word, sentence, token = "words") |> 
  anti_join(bind_rows(stop_words, green_stops, reform_stops)) # removing stop words

# calculating sentence mean embedding
full_glove <- full_words |> 
  inner_join(glove6b, by = "word") |> # binding vectors to document words 
  group_by(party, sentence_id) |> # grouping by sentence
  summarise(across(all_of(vector_names), ~mean(.x)), # mean embedding for each document across all 200 dimensions
            .groups = "drop") |> 
  mutate(party = as.factor(party)) # making sure party is factor variable

levels(full_glove$party)

# logit with glove embeddings ------------------------------------

# logit with glove embeddings
glove_logit <- glm(party ~ . -sentence_id, data = full_glove, family = "binomial") 
summary(glove_logit)

# glove logit probabilities
glove_probs <- predict(glove_logit, type = "response")

# glove logit predictions
glove_preds <- ifelse(glove_probs > 0.5, "reform", "green")

# confusion table
table(glove_preds, full_glove$party)

# percentage correct predictions
mean(glove_preds == full_glove$party)