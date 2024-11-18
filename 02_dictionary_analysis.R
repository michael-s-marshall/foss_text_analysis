rm(list = ls())

pacman::p_load(tidyverse, tidytext, quanteda, textdata, patchwork)

# importing data ---------------------------------------------------------------

green <- read.delim("green.txt", header = FALSE)

green <- str_c(green$V1, collapse = " ")

reform <- read.delim("reform.txt", header = FALSE)

reform <- str_c(reform$V1, collapse = " ")

# cleaning green manifesto -------------------------------------------------------

green_pages <- green |> 
  as_tibble_col(column_name = "text") |> 
  unnest_tokens(page, text, token = "regex", pattern = "\\<pb") |> 
  mutate(
    page = str_remove_all(page, "[^-[:^punct:]]|[0-9]| -") |>
      str_replace_all("realhoperealchange", "real hope real change") |>
      str_replace_all("realchangereal", "real change") |> 
      str_replace_all("realhopereal", "real hope") |> 
      str_replace_all("global warming","global_warming") |> 
      str_replace_all("greenhouse gas","greenhouse_gas") |> 
      str_replace_all("fossil fuel", "fossil_fuel") |> 
      str_replace_all("fossil fuels","fossil_fuels") |> 
      str_replace_all("green technology","green_technology") |> 
      str_replace_all("net zero","net_zero") |> 
      str_replace_all("green deal", "green_deal") |> 
      str_replace_all("green belt","green_belt") |> 
      str_replace_all("circular economy","circular_economy") |> 
      str_trim(),
    page_id = row_number()
  )

green_stops <- tibble(word = c("green","greens","bullet","bn","uk","election",
                               "party","poundbn"), 
                      lexicon = "green_stops")

green_words <- green_pages |> 
  unnest_tokens(word, page, token = "words") |>
  anti_join(bind_rows(stop_words,green_stops))

green_words |> 
  count(word) |> 
  slice_max(order_by = n, n = 30) |> 
  ggplot(aes(x = n, y = fct_reorder(word,n))) +
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

reform_stops <- tibble(word = c("uk","election","party","reform","pa"), 
                      lexicon = "reform_stops")

reform_words <- reform_pages |> 
  unnest_tokens(word, page, token = "words") |>
  anti_join(bind_rows(stop_words,reform_stops))

reform_words |> 
  count(word) |> 
  slice_max(order_by = n, n = 30) |>
  ggplot(aes(x = n, y = fct_reorder(word,n))) +
  geom_col() +
  labs(x = "Count", y = NULL)

# document feature matrices -----------------------------------------------

green_dfm <- green_words |> 
  count(page_id, word) |> 
  cast_dfm(page_id, word, n)

green_dfm

reform_dfm <- reform_words |> 
  count(page_id, word) |> 
  cast_dfm(page_id, word, n)

reform_dfm

# environment dictionary analysis ---------------------------------------------------

source("01_dictionaries.R")

green_env <- dfm_lookup(green_dfm, dictionary = environment_dictionary, nomatch = "unmatched")

g1 <- green_env |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = environment)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nfrequency", title = "Green Party") +
  coord_cartesian(ylim = c(0,65))

reform_env <- dfm_lookup(reform_dfm, dictionary = environment_dictionary, nomatch = "unmatched")

r1 <- reform_env |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = environment)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nfrequency", title = "Reform") +
  coord_cartesian(ylim = c(0,65))

g1 / r1

g2 <- green_env |>
  convert(to = "data.frame") |> 
  mutate(prop = environment / (environment + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nproportion", title = "Green Party") +
  coord_cartesian(ylim = c(0,0.15))

r2 <- reform_env |>
  convert(to = "data.frame") |> 
  mutate(prop = environment / (environment + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Environment dictionary\nproportion", title = "Reform") +
  coord_cartesian(ylim = c(0,0.15))

g2 / r2

# immigration dictionary analysis ---------------------------------------------------

green_imm <- dfm_lookup(green_dfm, dictionary = immigration_dictionary, nomatch = "unmatched")

g3 <- green_imm |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = immigration)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nfrequency", title = "Green Party") +
  coord_cartesian(ylim = c(0,25))

reform_imm <- dfm_lookup(reform_dfm, dictionary = immigration_dictionary, nomatch = "unmatched")

r3 <- reform_imm |>
  convert(to = "data.frame") |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = immigration)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nfrequency", title = "Reform") +
  coord_cartesian(ylim = c(0,25))

g3 / r3

g4 <- green_imm |>
  convert(to = "data.frame") |> 
  mutate(prop = immigration / (immigration + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nproportion", title = "Green Party") +
  coord_cartesian(ylim = c(0,0.15))

r4 <- reform_imm |>
  convert(to = "data.frame") |> 
  mutate(prop = immigration / (immigration + unmatched)) |> 
  ggplot(aes(x = as.factor(as.numeric(doc_id)), y = prop)) +
  geom_col() +
  labs(x = "Page", y = "Immigration dictionary\nproportion", title = "Reform") +
  coord_cartesian(ylim = c(0,0.15))

g4 / r4
