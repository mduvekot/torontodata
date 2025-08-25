library(readr)
member_voting_record_2022_2026 <- read_csv(
  "~/Downloads/member-voting-record-2022-2026.csv",
  col_types = cols(
    `_id` = col_integer(),
    Committee = col_character(),
    `Date/Time` = col_character()
  )
) |>
  dplyr::mutate(
    `Date/Time` = lubridate::parse_date_time(`Date/Time`, "%Y-%m-%d %H:%M %p")
  )

`%nin%` <- Negate(`%in%`)

library(dplyr)
library(purrr)

browseItem <- function(item) {
  url <- paste0("https://secure.toronto.ca/council/agenda-item.do?item=", item)
  print(url)
  browseURL(url)
}


member_voting_record_2022_2026 |>
  group_by(`Agenda Item #`) |>
  summarise(committees = unique(Committee) |> sort() |> list()) |>
  rowwise() |>
  filter("City Council" %nin% committees) |>
  ungroup() |>
  slice_sample(n = 5) |>
  pull(`Agenda Item #`) |>
  map(browseItem)


library(torontodata)
library(purrr)
library(dplyr)
library(stringr)
`%nin%` <- Negate(`%in%`)

colnames(member_voting_record)
data(member_voting_record)

committe_only_climate_votes <- member_voting_record |>
  summarise(
    .by = `Agenda.Item..`,
    committees = unique(Committee) |> sort() |> list()
  ) |>
  rowwise() |>
  filter("City Council" %nin% committees) |>
  ungroup() |>
  # slice_sample(n = 5) |>
  select(`Agenda.Item..`) |>
  left_join(member_voting_record) |>
  categorize_df("Agenda.Item.Title", climate_terms()) |>
  filter(!is.na(sector))

# filter out "Application to Remove a Private Tree"
library(ggplot2)

committe_only_climate_votes |>
  filter(!str_detect(Agenda.Item.Title, "Private Tree")) |>
  filter(!str_detect(Agenda.Item.Title, "Demolition")) |>
  filter(!str_detect(Agenda.Item.Title, "Tree Removal")) |>
  filter(!str_detect(Agenda.Item.Title, "Collective Bargaining")) |>
  filter(!str_detect(Agenda.Item.Title, "Collective Agreement")) |>
  filter(!str_detect(Agenda.Item.Title, "Tender")) |>

  select(`Agenda.Item..`, Committee, sector, Agenda.Item.Title) |>
  distinct() |>
  filter(sector == "Climate change-related terms") |>
  pull(`Agenda.Item..`) |>
  map(browseItem)

ggplot() +
  aes(fill = sector, y = Committee) +
  geom_bar(oriontation = "y", show.legend = FALSE) +
  facet_wrap(~sector, axes = "all_y", ncol = 2)
