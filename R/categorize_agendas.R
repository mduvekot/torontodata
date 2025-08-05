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
#'  View(results$categorized)
#'  View(results$unmatched)
#'
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{list_c}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}
#'  \code{\link[stringr]{str_detect}}, \code{\link[stringr]{modifiers}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[tidyr]{unnest}}
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
  # decisionBodyId = 2566
  meetings <- fetch_multiple_meeting(decisionBodyId = decisionBodyId)
  meetingIds <- meetings$meetingId # |> tail(3)

  # Fetch agenda item titles
  agenda_raw <- purrr::map(
    meetingIds,
    fetch_individual_meeting_agenda_item_titles
  ) |>
    purrr::list_rbind()

  # categorize by sector
  agenda_categorized <- agenda_raw |>
    dplyr::mutate(
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
          if (!any(detected)) return(NULL)
          tibble::tibble(
            key_term = terms_tbl$key_terms[detected],
            sector = terms_tbl$sectors[detected]
          )
        }
      )
    ) |>
    tidyr::unnest(.data$sector_info)

  # unmatched items
  agenda_unmatched <- agenda_raw |>
    dplyr::mutate(
      sector_info = purrr::map(
        .data$agendaItemTitle,
        function(text) {
          terms_tbl <- key_terms()
          detected <- stringr::str_detect(
            text,
            stringr::regex(terms_tbl$key_terms, ignore_case = FALSE)
          )
          if (!any(detected)) return(NULL)
          tibble::tibble(
            key_term = terms_tbl$key_terms[detected],
            sector = terms_tbl$sectors[detected]
          )
        }
      )
    ) |>
    dplyr::filter(purrr::map_lgl(.data$sector_info, is.null))

  list(
    categorized = agenda_categorized,
    unmatched = agenda_unmatched
  )
}
