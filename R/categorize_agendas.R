#' @title categorize agenda
#' @description categorizes all agenda items for a decisionBodyId
#' @param decisionBodyId integer
#' @return list
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  results <- categorize_agendas(2467)
#'  results <- categorize_agendas(2566)
#'  results <- categorize_agendas(2945)
#'  results <- categorize_agendas(2946)
#' 
#' decisionBodyId = 2945
#' fetch_individual_meeting_agenda_item_titles(2945) 
#' meetingId = 26585
#' meetingId = 27110
#'  View(results$categorized)
#'  View(results$unmatched)
#'
#'  }
#' }
#' @rdname categorize_agendas
#' @export
#' @importFrom purrr map list_rbind map_lgl
#' @importFrom dplyr mutate filter
#' @importFrom stringr str_detect regex
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
categorize_agendas <- function(decisionBodyId) {
  # Fetch meetings
  # decisionBodyId = 2945
  # decisionBodyId <- 29
  meetings <- fetch_multiple_meeting(decisionBodyId = decisionBodyId)
  meetingIds <- meetings$meetingId # |> tail(3)

  # decisionBodyId <- 2944
  #   fetch_individual_meeting_agenda_item_titles()
  # Fetch agenda item titles
  # agenda_raw <<- purrr::map(
  #   meetingIds,
  #   fetch_individual_meeting_agenda_item_titles
  # ) |>
  #   purrr::list_rbind()

  # meetingIds <- c(26585)
agenda_raw <- purrr::map(
  meetingIds,
  fetch_individual_meeting_agenda_item_titles
)

# Filter out NULLs
agenda_raw <- purrr::compact(agenda_raw)

# If all results were NULL, return an empty tibble with expected columns
if (length(agenda_raw) == 0) {
  agenda_raw <- tibble::tibble(
    nativeTermYear = integer(),
    referenceNumber = character(),  # Add other expected columns here
    agendaItemTitle = character(),
    sector_info = tibble::tibble(
            key_term = NA,
            sector = NA
          ) 
  )
} else {
  agenda_raw <- purrr::list_rbind(agenda_raw)
}

  # categorize by sector
  agenda_categorized <- agenda_raw |>
    dplyr::mutate(
      # store the results in a tibble, then unnest to make key_term and sector columns
      sector_info = purrr::map(
        .data[["agendaItemTitle"]],
        function(text) {
          terms_tbl <- key_terms()
          detected <- stringr::str_detect(
            text,
            # stringr::regex(terms_tbl$key_terms, ignore_case = FALSE) is too greedy
            stringr::regex(
              paste0("\\b", terms_tbl$key_terms, "\\b"),
              ignore_case = FALSE
            )
          )
          #if (!any(detected)) return(NULL)
          if (!any(detected)) return(tibble::tibble(
            key_term = NA,
            sector = NA
          ))
          tibble::tibble(
            key_term = terms_tbl$key_terms[detected],
            sector = terms_tbl$sectors[detected]
          )
        }
      )
    ) |>
    tidyr::unnest(.data$sector_info)

  
  # unmatched items
  # agenda_unmatched <- agenda_raw |>
  #   dplyr::mutate(
  #     sector_info = purrr::map(
  #       .data$agendaItemTitle,
  #       function(text) {
  #         terms_tbl <- key_terms()
  #         detected <- stringr::str_detect(
  #           text,
  #           stringr::regex(terms_tbl$key_terms, ignore_case = FALSE)
  #         )
  #         if (!any(detected)) return(NULL)
  #         tibble::tibble(
  #           key_term = terms_tbl$key_terms[detected],
  #           sector = terms_tbl$sectors[detected]
  #         )
  #       }
  #     )
  #   ) |>
  #   dplyr::filter(purrr::map_lgl(.data$sector_info, is.null)) |> 
  #   dplyr::rename(sector = sector_info) |> 
  #   dplyr::mutate(key_term = NA) |>
  #   dplyr::select(c("nativeTermYear","referenceNumber","agendaItemTitle","key_term","sector"))

  # list(
  #   categorized = agenda_categorized
  #   #unmatched = agenda_unmatched
  # )
  agenda_categorized
}
