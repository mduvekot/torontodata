# https://www.tidytextmining.com/tfidf

library(dplyr)
library(tidytext)
library(torontodata)
library(readr)
library(forcats)
library(ggplot2)
library(torontodata)
library(tidyverse)


# this works with the TO OpenData member_voting_record csv data
###################################################################################################
library(torontodata)
library(tidyverse)

data(member_voting_record)


df <- member_voting_record |>
  select(Agenda.Item.Title) |>
  distinct() |>
  categorize_df("Agenda.Item.Title", climate_terms())

# we didn't mean to match Business Bus.*
`%nin%` <- Negate(`%in%`)

df <- df |>
  filter(match %nin% c("Business"), !is.na(match))

library(tidytext)
sector_words <- df |>
  filter(!is.na(sector)) |>
  select(Agenda.Item.Title, sector) |>
  unnest_tokens(word, Agenda.Item.Title) |>
  count(sector, word, sort = TRUE)

total_words <- sector_words %>% group_by(sector) %>% summarize(total = sum(n))

sector_words <- left_join(sector_words, total_words)

sector_words

sector_words <- sector_words %>%
  bind_tf_idf(word, sector, n)
sector_words

sector_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


freq_by_rank <- sector_words %>%
  group_by(sector) %>%
  mutate(rank = row_number(), term_frequency = n / total) %>%
  ungroup()

freq_by_rank


sector_tf_idf <- sector_words %>%
  bind_tf_idf(word, sector, n)

sector_tf_idf %>%
  group_by(sector) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = sector)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sector, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


# per committee
###################################################################################################
library(torontodata)
library(tidyverse)
library(tidytext)

data(member_voting_record)


df <- member_voting_record |>
  select(Committee, Agenda.Item.Title) |>
  distinct() |>
  categorize_df("Agenda.Item.Title", climate_terms())


# we didn't mean to match Business Bus.*
`%nin%` <- Negate(`%in%`)

df <- df |>
  filter(match %nin% c("Business"), !is.na(match))

head(df)


Committee_words <- df |>
  filter(!is.na(Committee)) |>
  select(Agenda.Item.Title, Committee) |>
  unnest_tokens(word, Agenda.Item.Title) |>
  count(Committee, word, sort = TRUE)

total_words <- Committee_words %>% 
  group_by(Committee) %>% 
  summarize(total = sum(n))

Committee_words <- left_join(Committee_words, total_words)

head(Committee_words)

Committee_words <- Committee_words %>%
  bind_tf_idf(word, Committee, n)

Committee_words

Committee_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

freq_by_rank <- Committee_words %>%
  group_by(Committee) %>%
  mutate(rank = row_number(), term_frequency = n / total) %>%
  ungroup()

freq_by_rank


Committee_tf_idf <- Committee_words %>%
  bind_tf_idf(word, Committee, n)

Committee_tf_idf %>%
  group_by(Committee) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Committee)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Committee, ncol = 6, scales = "free") +
  labs(x = "tf-idf", y = NULL)

