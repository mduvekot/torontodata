# https://www.tidytextmining.com/tfidf

library(dplyr)
library(tidytext)
library(torontodata)
library(readr)
library(forcats)
library(ggplot2)

folder_path <- "~/Desktop/to_council_commitees_2025-08-11"

csv_files <- list.files(
  path = folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

df <- csv_files %>%
  lapply(
    readr::read_csv,
    col_types = readr::cols(
      ...1 = col_integer(),
      nativeTermYear = col_integer(),
      considerStartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      considerTypeCd = col_character(),
      itemStatusCd = col_integer(),
      meetingId = col_integer(),
      publishTypeCd = col_character(),
      wards = col_character(),
      inCamera = col_character(),
      agendaItemId = col_integer(),
      currentInd = col_character(),
      nativeItemStatusCd = col_character(),
      referenceNumber = col_character(),
      keyItemInd = col_character(),
      heldByFirstName = col_character(),
      heldByLastName = col_character(),
      statutory = col_character(),
      urgent = col_character(),
      heldByMemberId = col_character(),
      confidentialReason = col_character(),
      annotation = col_character(),
      url = col_character(),
      sectors = col_character(),
      key_terms = col_character(),
      key_term = col_character(),
      match = col_character()
    )
  ) %>%
  bind_rows()



# redo with tighter matching
df <- df |> 
  select (-c(sectors, key_terms, key_term, match))|> 
  match_key_terms_tidy()

# we didn't mean to match Business Bus.*  
`%nin%` <- Negate(`%in%`)

df <- df |> 
  filter(match %nin% c("Business"), !is.na(match))

sector_words <- df |>
  filter(!is.na(sectors)) |>
  select(agendaItemTitle, sectors) |>
  unnest_tokens(word, agendaItemTitle) |>
  count(sectors, word, sort = TRUE)

total_words <- sector_words %>% group_by(sectors) %>% summarize(total = sum(n))
sector_words <- left_join(sector_words, total_words)
sector_words

sector_words <- sector_words %>%
  bind_tf_idf(word, sectors, n)
sector_words

sector_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


freq_by_rank <- sector_words %>%
  group_by(sectors) %>%
  mutate(rank = row_number(), term_frequency = n / total) %>%
  ungroup()

freq_by_rank


sector_tf_idf <- sector_words %>%
  bind_tf_idf(word, sectors, n)

sector_tf_idf %>%
  group_by(sectors) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = sectors)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sectors, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
