utils::globalVariables(c(
  "_id",
  "Term",
  "First Name",
  "Last Name",
  "Committee",
  "Date/Time",
  "Agenda Item #",
  "Agenda Item Title",
  "Motion Type",
  "result"
))

#' @title make member voting record
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details creates a closure
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_member_voting_record <- make_member_voting_record()
#' get_member_voting_record(2023, "BU3.1")
#'  }
#' }
#' @rdname make_member_voting_record
#' @export
#' @importFrom rvest read_html html_element html_text html_elements html_table
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_detect str_remove str_remove_all str_extract
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr case_when select rename mutate filter row_number
#' @importFrom purrr map imap map_chr flatten list_rbind
#' @importFrom tidyr separate_rows extract
#'
make_member_voting_record <- function() {
  cache <- new.env(parent = emptyenv())

  function(
    nativeTermYear = 2025,
    referenceNumber = "IE21.4",
    use_cache = TRUE
  ) {
    key <- paste0(nativeTermYear, ".", referenceNumber)

    if (use_cache && exists(key, envir = cache)) {
      message("Using cached vote data for ", key)
      return(cache[[key]])
    }

    # --- your full member_voting_record logic goes here ---
    url <- paste0(
      "https://secure.toronto.ca/council/agenda-item.do?item=",
      nativeTermYear,
      ".",
      referenceNumber
    )

    html <- tryCatch(
      rvest::read_html(url),
      error = function(e) {
        message("Failed to fetch URL: ", e$message)
        return(tibble::tibble())
      }
    )

    if (is.null(html)) {
      return(tibble::tibble())
    }

    get_term <- Vectorize(function(year) {
      start_years <- seq(1994, 2026, by = 4)
      term_start <- max(start_years[start_years <= year])
      paste0(term_start, "-", term_start + 4)
    })

    parse_vote_time <- function(x) {
      if (stringr::str_detect(x, "\\d{1,2}:\\d{2}")) {
        lubridate::parse_date_time(
          x,
          "%b-%d-%Y %H:%M %p",
          tz = "America/Toronto"
        )
      } else {
        lubridate::parse_date_time(x, "%b-%d-%Y", tz = "America/Toronto")
      }
    }

    agenda_title <- html |>
      rvest::html_element("h3.heading") |>
      rvest::html_text(trim = TRUE) |>
      stringr::str_remove("^\\S+\\s+-\\s+")

    cards <- rvest::html_elements(html, "#agenda-item-accordion .card")

    extract_votes_from_card <- function(card, section_id) {
      heading <- rvest::html_element(card, ".card-header button") |>
        rvest::html_text(trim = TRUE)

      committee_name <- dplyr::case_when(
        stringr::str_detect(heading, "City Council") ~ "City Council",
        stringr::str_detect(heading, "Committee consideration") ~
          stringr::str_remove(heading, " consideration.*"),
        TRUE ~ "Unknown"
      )

      headers <- rvest::html_elements(card, "h4.vote-table-header")
      motion_types <- headers |>
        rvest::html_elements("span.normal-text") |>
        rvest::html_text(trim = TRUE) |>
        stringr::str_remove_all("[()]")

      timestamps <- headers |>
        rvest::html_elements("small.text-right") |>
        rvest::html_text(trim = TRUE) |>
        purrr::map(parse_vote_time)

      tables <- rvest::html_elements(card, "h4.vote-table-header + table")

      purrr::imap(tables, function(tbl, idx) {
        df <- rvest::html_table(tbl, fill = TRUE) |> tibble::as_tibble()

        rows <- rvest::html_elements(tbl, "tbody tr")
        cells <- purrr::map_chr(
          rows,
          ~ rvest::html_element(.x, "td") |> rvest::html_text(trim = TRUE)
        )

        yes_count <- stringr::str_extract(
          cells[stringr::str_detect(cells, "^Total members that voted Yes:")],
          "\\d+"
        ) |>
          as.integer()

        no_count <- stringr::str_extract(
          cells[stringr::str_detect(cells, "^Total members that voted No:")],
          "\\d+"
        ) |>
          as.integer()

        result_text <- stringr::str_remove(colnames(df)[1], "Result: ")
        result <- paste0(result_text, ", ", yes_count, "-", no_count)

        note <- colnames(df)[2]
        vote_time <- timestamps[[idx]]
        motion_type <- motion_types[[idx]]

        df |>
          dplyr::select(-1) |>
          dplyr::rename(votes = 1) |>
          dplyr::mutate(
            vote_type = dplyr::case_when(
              stringr::str_detect(votes, "voted Yes") ~ "Yes",
              stringr::str_detect(votes, "voted No") ~ "No",
              stringr::str_detect(votes, "were absent") ~ "Absent"
            ),
            votes = stringr::str_remove(votes, "Members that .* are\\s*")
          ) |>
          tidyr::separate_rows(votes, sep = ",\\s*") |>
          dplyr::rename(councillor = votes) |>
          dplyr::filter(councillor != "", !is.na(councillor)) |>
          dplyr::mutate(
            councillor = stringr::str_remove(councillor, "\\s*\\(.*\\)$"),
            result = result,
            note = note,
            `Date/Time` = vote_time,
            `Motion Type` = motion_type,
            `Agenda Item Title` = agenda_title,
            `Agenda Item #` = paste0(nativeTermYear, ".", referenceNumber),
            nativeTermYear = nativeTermYear,
            referenceNumber = referenceNumber,
            Term = get_term(nativeTermYear),
            Committee = committee_name,
            idx = idx
          ) |>
          tidyr::extract(
            councillor,
            into = c("First Name", "Last Name"),
            regex = "^([^ ]+)\\s+(.*)$"
          )
      })
    }

    parsed <- purrr::imap(cards, extract_votes_from_card)
    flattened <- purrr::flatten(parsed)

    if (length(flattened) == 0) {
      return(tibble::tibble())
    }

    votes <- purrr::list_rbind(flattened) |>
      dplyr::mutate(`_id` = dplyr::row_number()) |>
      dplyr::select(
        `_id`,
        Term,
        `First Name`,
        `Last Name`,
        Committee,
        `Date/Time`,
        `Agenda Item #`,
        `Agenda Item Title`,
        `Motion Type`,
        Vote = vote_type,
        Result = result,
        `Vote Description` = note,
        idx
      )

    cache[[key]] <- votes
    votes
  }
}

# member_voting_record <- make_member_voting_record()

# nativeTermYear <- 2025
# referenceNumber <- "IE21.4"
# agendaItemNumber <- paste0(nativeTermYear, ".", referenceNumber)

# member_voting_record(nativeTermYear, referenceNumber, use_cache = FALSE) |>
#   dplyr::arrange(`Vote Description`) |>
#   print(n = Inf)

# member_voting_record_2022_2026 |>
#   dplyr::filter(`Agenda Item #` == agendaItemNumber) |>
#   dplyr::arrange(`Vote Description`) |>
#   print(n = Inf)
