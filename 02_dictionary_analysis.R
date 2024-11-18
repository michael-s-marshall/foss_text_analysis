rm(list = ls())

pacman::p_load(tidyverse, tidytext, quanteda, textdata, patchwork)

# importing data ---------------------------------------------------------------

# green manifesto
green <- read.delim("green.txt", header = FALSE)

# making a single string
green <- str_c(green$V1, collapse = " ")

# reform manifesto
reform <- read.delim("reform.txt", header = FALSE)

# making a single string
reform <- str_c(reform$V1, collapse = " ")

# cleaning green manifesto -------------------------------------------------------

green_pages <- green |> 
  as_tibble_col(column_name = "text") |> # making a tibble 
  unnest_tokens(page, text, token = "regex", pattern = "\\<pb") |> # splitting into pages 
  mutate(
    page = str_remove_all(page, "[^-[:^punct:]]|[0-9]| -") |> # removing punctuation (except hyphens) and removing numbers 
      str_replace_all("realhoperealchange", "real hope real change") |> # removing conjoined words
      str_replace_all("realchangereal", "real change") |> 
      str_replace_all("realhopereal", "real hope") |> 
      str_replace_all("global warming","global_warming") |> # conjoining words for dictionary 
      str_replace_all("greenhouse gas","greenhouse_gas") |> 
      str_replace_all("fossil fuel", "fossil_fuel") |> 
      str_replace_all("fossil fuels","fossil_fuels") |> 
      str_replace_all("green technology","green_technology") |> 
      str_replace_all("net zero","net_zero") |> 
      str_replace_all("green deal", "green_deal") |> 
      str_replace_all("green belt","green_belt") |> 
      str_replace_all("circular economy","circular_economy") |> 
      str_trim(), # removing whitespace
    page_id = row_number() # adding id for page number
  )

# stop words bespoke to green manifesto
green_stops <- tibble(word = c("green","greens","bullet","bn","uk","election",
                               "party","poundbn"), 
                      lexicon = "green_stops")

# one row for each word on each page
green_words <- green_pages |> 
  unnest_tokens(word, page, token = "words") |>
  anti_join(bind_rows(stop_words,green_stops))

# plot of most common words
green_words |> 
  count(word) |> 
  slice_max(order_by = n, n = 30) |> # 30 most common words 
  ggplot(aes(x = n, y = fct_reorder(word,n))) + # reordering by frequency
  geom_col() +
  labs(x = "Count", y = NULL)

# cleaning reform manifesto ------------------------------------------------

reform_pages <- reform |> 
  as_tibble_col(column_name = "text") |> 
  unnest_tokens(page, text, token = "regex", pattern = "\\<pb") |> 
  mutate(
    page = str_remove_all(page, "[^-[:^punct:]]|[0-9]| -") |>
      str_replace_all("contractour", "contract our") |>
      str_replace_all("youwith", "you with") |> 
      str_replace_all("hostile environment","hostile_environment") |> 
      str_replace_all("welfare tourism","welfare_tourism") |> 
      str_replace_all("welfare tourist","welfare_tourist") |> 
      str_replace_all("benefit tourism","benefit_tourism") |> 
      str_replace_all("over population","over_population") |> 
      str_replace_all("british values","british_values") |> 
      str_replace_all("british culture","british_culture") |> 
      str_trim(),
    page_id = row_number()
  )

# bespoke stop words for reform
reform_stops <- tibble(word = c("uk","election","party","reform","pa"), 
                      lexicon = "reform_stops")

reform_words <- reform_pages |> 
  unnest_tokens(word, page, token = "words") |>
  anti_join(bind_rows(stop_words,reform_stops))

# plot of most common words in reform manifesto
reform_words |> 
  count(word) |> 
  slice_max(order_by = n, n = 30) |>
  ggplot(aes(x = n, y = fct_reorder(word,n))) +
  geom_col() +
  labs(x = "Count", y = NULL)

# document feature matrices -----------------------------------------------

# dfm for green manifesto - each document is a page
green_dfm <- green_words |> 
  count(page_id, word) |> 
  cast_dfm(page_id, word, n)

green_dfm

# dfm for reform manifesto - each document is a page
reform_dfm <- reform_words |> 
  count(page_id, word) |> 
  cast_dfm(page_id, word, n)

reform_dfm

# environment dictionary analysis ---------------------------------------------------

# loading dictionaries
source("01_dictionaries.R")

# environment dictionary frequency for green party
green_env <- dfm_lookup(green_dfm, dictionary = environment_dictionary, nomatch = "unmatched")

# plotting environment dictionary frequency for green party
g1 <- green_env |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = environment)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nfrequency", title = "Green Party") +
  coord_cartesian(ylim = c(0,65)) # fixing x-axis to same range for both parties

# environment dictionary frequency for reform party
reform_env <- dfm_lookup(reform_dfm, dictionary = environment_dictionary, nomatch = "unmatched")

# plotting
r1 <- reform_env |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = environment)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nfrequency", title = "Reform") +
  coord_cartesian(ylim = c(0,65)) # fixing x-axis to same range for both parties

# plotting both parties
g1 / r1

# environment dictionary proportion for green party
g2 <- green_env |>
  convert(to = "data.frame") |> 
  mutate(prop = environment / (environment + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nproportion", title = "Green Party") +
  coord_cartesian(ylim = c(0,0.15))

# environment dictionary proportion for reform party
r2 <- reform_env |>
  convert(to = "data.frame") |> 
  mutate(prop = environment / (environment + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nproportion", title = "Reform") +
  coord_cartesian(ylim = c(0,0.15))

# proportion for both parties
g2 / r2

# immigration dictionary analysis ---------------------------------------------------

# immigration dictionary for green party
green_imm <- dfm_lookup(green_dfm, dictionary = immigration_dictionary, nomatch = "unmatched")

# plotting immigration dictionary frequency for green party
g3 <- green_imm |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = immigration)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nfrequency", title = "Green Party") +
  coord_cartesian(ylim = c(0,25))

# immigration dictionary for reform party
reform_imm <- dfm_lookup(reform_dfm, dictionary = immigration_dictionary, nomatch = "unmatched")

# immigration dictionary frequency for reform party
r3 <- reform_imm |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = immigration)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nfrequency", title = "Reform") +
  coord_cartesian(ylim = c(0,25))

# plotting immigration dictionary for both parties
g3 / r3

# plotting immigration proportion for green party
g4 <- green_imm |>
  convert(to = "data.frame") |> 
  mutate(prop = immigration / (immigration + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nproportion", title = "Green Party") +
  coord_cartesian(ylim = c(0,0.15))

# plotting immigration proportion for reform party
r4 <- reform_imm |>
  convert(to = "data.frame") |> 
  mutate(prop = immigration / (immigration + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nproportion", title = "Reform") +
  coord_cartesian(ylim = c(0,0.15))

# plotting immigration proportion for both parties
g4 / r4