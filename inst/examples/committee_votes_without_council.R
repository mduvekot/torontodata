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
