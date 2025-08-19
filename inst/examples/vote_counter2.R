vote_counter2 <- function(nativeTermYear, referenceNumber) {
  nativeTermYear <- 2025
  referenceNumber <- "MM28.37"

  url <- paste0(
    "https://secure.toronto.ca/council/agenda-item.do?item=",
    nativeTermYear,
    ".",
    referenceNumber
  )
  print(url)
  browseURL(url)
  html <- rvest::read_html(url)
  # html
  # cat(as.character(html))
  #
  # figure out how many sections we have
  heading_text <- rvest::html_element(html, "#heading0 button") |>
    rvest::html_text(trim = TRUE)

  section_type <- dplyr::case_when(
    stringr::str_detect(heading_text, "City Council") ~ "council",
    stringr::str_detect(heading_text, "Committee") ~ "committee",
    TRUE ~ "unknown"
  )

  council_tables <- html |>
    rvest::html_node("#collapse0") |>
    rvest::html_nodes("table")

  # Get Committee vote tables
  committee_tables <- html |>
    rvest::html_node("#collapse1") |>
    rvest::html_nodes("table")

  committee_tables

  council_tibbles <- purrr::map(
    council_tables,
    ~ rvest::html_table(.x, fill = TRUE) |> tibble::as_tibble()
  )

  committee_tibbles <- purrr::map(
    committee_tables,
    ~ html_table(.x, fill = TRUE) |> tibble::as_tibble()
  )

  # Parse each table and tag with section
  table2df <- function(table, section) {
    table |>
      dplyr::select(-1) |>
      dplyr::rename(votes = 1) |>
      dplyr::mutate(
        result = stringr::str_replace_all(colnames(table)[1], "Result: ", ""),
        note = colnames(table)[2],
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
        referenceNumber = referenceNumber,
        section = section
      )
  }

  table2df(council_tibbles[[1]], "council")
  table2df(committee_tibbles[[1]], "committee")

  committee_parsed <- purrr::imap(
    committee_tibbles,
    ~ table2df(.x, section = "committee") |>
      dplyr::mutate(section_id = .y)
  )

  council_parsed <- purrr::imap(
    council_tibbles,
    ~ table2df(.x, section = "council") |>
      dplyr::mutate(section_id = .y)
  )

  c(committee_parsed, council_parsed) |>
    purrr::list_rbind()
}


vote_counter2(2023, "IE9.5")
vote_counter2(2025, "MM28.37")
