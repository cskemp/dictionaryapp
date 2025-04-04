library(tidyverse)
library(here)
library(dplyr)
library(maps)
library(ggplot2)

data_full <- read_csv(here("data", "bila_app_stats_6000.csv.gz"))

data_lean <- data_full %>%
  # remove ab for conversation app because this form appears first in alphabetical order, and is confusing
  filter(word != "ab") %>%
  group_by(glottocode) %>%
  mutate(rank_per_glottocode = rank(desc(zeta), ties.method = "first")) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(cutoff = quantile(zeta, probs = c(0.95))) %>%
  mutate(rank_per_word = rank(desc(zeta), ties.method = "first")) %>%
  filter(rank_per_glottocode <= 20 | rank_per_word <= 20) %>%
  mutate(top_flag = zeta > cutoff) %>%
  select(-rank_per_glottocode, -rank_per_word) %>%
  # temporary fix for Central Siberian Yupik -- can remove once glottocodes updated
  mutate(langname = if_else(langname == "Central Siberian Yupik", "Central Alaskan Yupik", langname)) %>%
  arrange(langname, desc(zeta))


plo <- data_lean  %>%
  ungroup() %>%
  arrange(langname) %>%
  arrange(word)

# Process positions
posns <- plo %>%
  select(glottocode, longitude, latitude) %>%
  mutate(longitude = if_else(longitude > 180, longitude - 360, longitude)) %>%
  distinct() # Use distinct() instead of unique() with dplyr pipes

# Get world map data
world <- map_data("world")

wrap_words <- function(topterms) {
  words_text <- paste(topterms$neighbour, collapse = ", ")
  wrapped_words <- str_wrap(words_text, width = 60)
  return(wrapped_words)
}

# Get nns and nls
nns <- read_csv(here("data", "bila_app_nn_6000.csv")) %>%
  mutate(i = -i)  %>%
  group_by(word) %>%
  nest()  %>%
  mutate(top_cases = map_chr(data, wrap_words)) %>%
  select(-data) %>%
  ungroup()

nls <- read_csv(here("data", "bila_app_nl.csv")) %>%
  group_by(langs) %>%
  nest()  %>%
  mutate(top_cases = map_chr(data, wrap_words)) %>%
  select(-data) %>%
  ungroup()


# Create the final list
app_data <- list(
  plo = plo,
  posns = posns,
  world = world,
  words = sort(unique(plo$word)), # Sort here
  languages = sort(unique(plo$langname)),
  nns = nns,
  nls = nls
)

# Save as RDS
saveRDS(app_data, file = here("preprocessed_app_data.rds"))

