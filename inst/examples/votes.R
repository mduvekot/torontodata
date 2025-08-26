# Michiel
load(here::here("inst/extdata/df_all.RData"))
load(here::here("inst/extdata/flat_results.RData"))
colnames(flat_results)
colnames(df_all)

votes_df <- left_join(flat_results, df_all)

votes_df |>
  select(councillor, vote_type, sectors, has_match) |>
  filter(has_match == TRUE) |>
  select(-has_match) |>
  # only look at the current councillors
  filter(councillor %in% get_councillors()$Councillor) |>
  mutate(
    vote_type = factor(vote_type, levels = rev(c("no", "yes", "absent")))
  ) |>
  dplyr::summarise(.by = everything(), n = n()) |>
  pivot_wider(names_from = vote_type, values_from = n) |>
  mutate(across(c(yes, no, absent), ~ replace_na(., 0))) |>
  mutate(.by = sectors, score = yes / (yes + no + absent) * 100) |>
  arrange(.by = sectors, desc(score)) |>
  pivot_longer(
    cols = c(yes, no, absent),
    names_to = "vote_type",
    values_to = "n"
  ) |>
  ggplot() +
  aes(x = n, y = reorder(councillor, score), fill = vote_type) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "absent" = "#7f7f7f",
      "yes" = "#0f8faf",
      "no" = "#cf007f"
    )
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap(~sectors, ncol = 3, scales = "free") +
  theme_void() +
  theme(
    text = element_text(
      family = "AtkinsonHyperlegibleNext-Regular",
      size = unit(12, "pt")
    ),
    axis.text.y.left = element_text(hjust = 1),
    strip.text = element_text(
      family = "AtkinsonHyperlegibleNext-Semibold",
      hjust = 0,
      size = rel(1)
    )
  )


# why is Shelley Carroll so bad? is it amendments?
votes_df |>
  filter(
    councillor == "Shelley Carroll",
    sectors == "Climate change-related terms",
    # itemStatusCd == "ADOPTED"
    vote_type == "no"
  ) |>
  select(vote_type, itemStatusCd, agendaItemTitle, url) |>
  pull(url) |>
  unique() |>
  purrr::map(browseURL)


votes_df |>
  filter(referenceNumber == "IE5.1", councillor == "Shelley Carroll") |>
  View()
