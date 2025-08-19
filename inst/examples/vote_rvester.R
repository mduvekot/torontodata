vote_counter <- function(nativeTermYear = 2025, referenceNumber = "IE21.9") {
  url <- paste0(
    "https://secure.toronto.ca/council/agenda-item.do?item=",
    nativeTermYear,
    ".",
    referenceNumber
  )
  print(url)
  html <- rvest::read_html(url)

  tables <- rvest::html_elements(html, "h4.vote-table-header + table") |>
    rvest::html_table()

  table2df <- function(table) {
    table |>
      dplyr::select(-1) |>
      dplyr::rename(votes = 1) |>
      dplyr::mutate(
        vote_type = dplyr::case_when(
          stringr::str_detect(votes, "voted Yes") ~ "yes",
          stringr::str_detect(votes, "voted No") ~ "no",
          stringr::str_detect(votes, "were absent") ~ "absent"
        ),
        votes = stringr::str_remove(votes, "Members that .* are\\s*")
      ) |>
      tidyr::separate_rows(votes, sep = ",\\s*") |>
      dplyr::rename(councillor = votes) |>
      dplyr::mutate(
        nativeTermYear = nativeTermYear,
        referenceNumber = referenceNumber
      )
  }

  purrr::map(tables, table2df)
}

#
# # fmt skip
# urls <- tibble::tribble(
#   ~nativeTermYear,
#   ~referenceNumber,
#   2025,
#   "MA6.3",
#   # 2025, "IE21.9",
#   # 2023, "EC3.5",
#   # 2025, "IE22.1"
# )
#
# urls <- agenda_items |> select(nativeTermYear, referenceNumber)
# urls
# # wrap in a lambda
# results <- urls |>
#   (\(df) {
#     purrr::map2(
#       df$nativeTermYear,
#       df$referenceNumber,
#       vote_counter
#     )
#   })()
#
#
# flat_results <- purrr::list_rbind(purrr::flatten(results))
#
# flat_results
# save(flat_results, file = "~/Desktop/flat_results.RData")
# write_csv(flat_results, "~/Desktop/flat_results.csv")
#
#
# ggplot(flat_results) +
#   aes(x = councillor, fill = vote_type) +
#   geom_bar() +
#   coord_flip()
#
# summarise(.by = c(councillor, vote_type), flat_results, n = n()) |>
#   pivot_wider(names_from = vote_type, values_from = n) |>
#   arrange(desc(no))
#
# summarise(.by = c(councillor, vote_type), flat_results, n = n()) |>
#   pivot_wider(names_from = vote_type, values_from = n) |>
#   arrange(desc(yes))
#
# summarise(.by = c(councillor, vote_type), flat_results, n = n()) |>
#   pivot_wider(names_from = vote_type, values_from = n) |>
#   arrange(desc(absent))
#
# unique(paste(flat_results$nativeTermYear, flat_results$referenceNumber))
#
#
# flat_results |>
#   filter(str_detect(referenceNumber, "IE")) |>
#   count(referenceNumber)
#
#
# flat_results |>
#   mutate(decisionBodyCode = str_extract(referenceNumber, "[A-Z]+(?=\\d)")) |>
#   select(referenceNumber, decisionBodyCode) |>
#   distinct() |>
#   summarise(.by = decisionBodyCode, n = n()) |>
#   arrange(desc(n))
#
#
# save(flat_results, file = here::here("inst/extdata/flat_results.RData"))
