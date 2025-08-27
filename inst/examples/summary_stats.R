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

cat_non_council_votes |>
  filter(!is.na(sector)) |>
  select(`Agenda.Item..`, sector) |>
  distinct() |>
  left_join(member_voting_record) -> df_plot

ggplot(df_plot) +
  aes(x = `Agenda.Item..`, y = Last.Name, fill = Vote) +
  geom_tile() +
  scale_fill_manual(
    values = c(
      "No" = "#ff0000",
      "Yes" = "#afafaf",
      "Absent" = "#efefef",
      "Absent(Interest Declared" = "#efefef"
    )
  ) +
  scale_y_discrete(position = "right") +
  theme_void() +
  theme(
    #axis.text.y.left = element_text(hjust = 1),
    axis.text.y.right = element_text(hjust = 0)
  )
