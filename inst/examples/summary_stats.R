library(torontodata)
library(tidyverse)
data(member_voting_record)


# find al the individual agendaItems in the member voting record

member_voting_record |> colnames()

cc_votes <- member_voting_record |>
  select(Committee, `Agenda.Item..`) |>
  distinct() |>
  filter(Committee == "City Council") |>
  select(`Agenda.Item..`)

cat_non_council_votes <-
  member_voting_record |>
  anti_join(cc_votes) |>
  select(Term, Committee, `Agenda.Item..`, Agenda.Item.Title) |>
  distinct() |>
  categorize_df(name_col = "Agenda.Item.Title", terms_tbl = climate_terms())

# How many non-council climate_related agenda items per term?
cat_non_council_votes |>
  filter(!is.na(sector)) |>
  pull(`Agenda.Item..`) |>
  map(browseItem)
