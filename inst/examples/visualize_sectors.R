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


match_key_terms_tidy <- function(df, name_col, lut) {
  name_sym <- rlang::sym(name_col)

  df |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      matches = list(
        lut |>
          dplyr::mutate(
            match = stringr::str_extract(
              !!name_sym,
              stringr::regex(key_terms, ignore_case = TRUE)
            )
          ) |>
          dplyr::filter(!is.na(match))
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest(matches, keep_empty = TRUE) |>
    dplyr::select(-row_id)
}

df_matched_tidy <- match_key_terms_tidy(
  df,
  name_col = "agendaItemTitle",
  lut = key_terms()
)


library(ggplot2)
df_matched |>
  dplyr::select(sectors, match, nativeItemStatusCd, agendaItemTitle) |>
  dplyr::distinct() |>
  ggplot() +
  aes(x = sectors, fill = nativeItemStatusCd) +
  geom_bar()
