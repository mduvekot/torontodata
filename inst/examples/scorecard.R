# Michiel
scorecard_items <- tibble::tribble(
  ~item,
  "2025.EX23.2",
  "2025.IE21.10",
  "2025.IE21.4",
  "2025.PH20.7",
  "2025.EX22.5",
  "2025.MM28.37",
  "2025.EX21.2",
  "2025.EX21.4",
  "2025.MM26.7",
  "2025.EX20.13",
  "2025.PH18.6",
  "2024.MM24.42",
  "2024.IE18.5",
  "2024.PH16.1",
  "2024.EX17.1",
  "2024.MM22.20",
  "2024.EX17.3",
  "2024.IE16.8",
  "2024.MM20.24",
  "2024.IE14.4",
  "2024.MM19.9",
  "2024.IE14.9",
  "2024.IE14.4",
  "2024.MM17.9",
  "2024.EX13.5",
  "2024.EX11.8",
  "2024.IE10.6",
  "2024.IE10.3",
  "2023.IE9.5",
  "2023.EC6.6",
  "2023.MM7.25",
  "2023.MM6.13",
  "2023.EX4.10",
  "2023.MM6.17",
  "2023.IE3.3",
  "2023.MM2.14",
  "2022.GL32.1"
) |>
  dplyr::mutate(
    item = forcats::fct_inorder(item),
    nativeTermYear = stringr::str_sub(item, 1, 4) |> as.numeric(),
    referenceNumber = stringr::str_remove(item, "\\d{4}\\.")
  )

get_councillors()$Councillor


dplyr::left_join(scorecard_items, df_all, relationship = "many-to-many") |>
  dplyr::select(item, url) |>
  dplyr::filter(!is.na(url)) |>
  dplyr::pull(url) |>
  unique() |>
  purrr::map(browseURL)


dplyr::left_join(
  scorecard_items,
  flat_results,
  relationship = "many-to-many"
) |>
  dplyr::filter(councillor %in% get_councillors()$Councillor) |>
  dplyr::mutate(
    last_name = word(councillor, -1), # Extract last word as last name
    #councillor = factor(councillor, levels = councillor[order(last_name)])
  ) |>
  dplyr::arrange(desc(last_name)) |>
  dplyr::mutate(councillor = forcats::fct_inorder(councillor)) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = item, y = councillor, fill = vote_type) +
  ggplot2::geom_tile(color = "#ffffff") +
  ggplot2::scale_fill_manual(
    values = c(
      "absent" = "#7f7f7f",
      "yes" = "#0f8faf",
      "no" = "#cf007f"
    )
  ) +
  ggplot2::theme(axis.text.x.bottom = element_text(angle = 90))
