library(torontodata)


decisionBodyId <- 2587

df <- fetch_all_agenda_items(decisionBodyId = 2587)
df_matched <- match_key_terms(
  df,
  name_col = "agendaItemTitle",
  lut = key_terms()
)


df_matched |>
  dplyr::select(sectors, match, agendaItemTitle) |>
  dplyr::distinct() |>
  View()

library(ggplot2)
df_matched |>
  dplyr::select(sectors, match, nativeItemStatusCd, agendaItemTitle) |>
  dplyr::distinct() |>
  ggplot() +
  aes(x = sectors, fill = nativeItemStatusCd) +
  geom_bar()
