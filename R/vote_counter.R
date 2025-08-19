vote_counter <- function(nativeTermYear = 2025, referenceNumber = "MM28.37") {
  url <- paste0(
    "https://secure.toronto.ca/council/agenda-item.do?item=",
    nativeTermYear,
    ".",
    referenceNumber
  )

  print(url)
  # browseURL(url)
  html <- rvest::read_html(url)

  table2df <- function(table, section) {
    # to do: an empty list of absentees when Absent: 0 adds entry to the list of councillors
    # also happens when no concillor vots no, as in 2025 EX21.2
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

  # Get all collapsible sections
  cards <- rvest::html_elements(html, "#agenda-item-accordion .card")
  # cards
  # cat(as.character(cards[1]))
  # str(cards)
  # Parse each section
  parsed_sections <- purrr::imap(cards, function(card, index) {
    heading <- rvest::html_element(card, ".card-header button") |>
      rvest::html_text(trim = TRUE)

    section_type <- dplyr::case_when(
      stringr::str_detect(heading, "City Council") ~ "council",
      stringr::str_detect(heading, "Committee") ~ "committee",
      TRUE ~ "unknown"
    )
    # this also finds tables that are notvote tables
    # tables <- rvest::html_elements(card, "table")
    # find only tables after <h4 class="vote-table-header">
    tables <- rvest::html_elements(card, "h4.vote-table-header + table")

    if (length(tables) == 0) {
      message("No vote tables found for ", referenceNumber)
      return(NULL) # or return NULL
    }

    purrr::imap(
      tables,
      ~ table2df(
        rvest::html_table(.x, fill = TRUE) |> tibble::as_tibble(),
        section = section_type
      ) |>
        dplyr::mutate(
          # section_id = paste0("section_", index, "_table_", .y)
          section_id = index,
          idx = .y
        )
    )
  })

  # Flatten and return
  # purrr::list_rbind(purrr::flatten(parsed_sections))  |>
  #   dplyr::select(councillor, vote_type, result, nativeTermYear, referenceNumber, section, idx, note)
  #
  #
  flattened <- purrr::flatten(parsed_sections)

  if (length(flattened) == 0) {
    message("No vote data to return for ", referenceNumber)
    return(tibble::tibble()) # or return NULL if preferred
  }

  purrr::list_rbind(flattened) |>
    dplyr::select(
      councillor,
      vote_type,
      result,
      nativeTermYear,
      referenceNumber,
      section,
      idx,
      note
    )
}


vote_counter()

dplyr::sample_n(agenda_items, 10) |>
  dplyr::select(nativeTermYear, referenceNumber) |>
  purrr::pmap(vote_counter) -> foo

purrr::list_rbind(foo)
