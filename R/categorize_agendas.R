#' @title categorize agenda
#' @description categorizes all agenda items for a decisionBodyId
#' @param decisionBodyId integer
#' @return list
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  results <- categorize_agendas(2468)
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
  fetcher <- make_fetch_meetings()
  meetings <- fetcher(decisionBodyId = decisionBodyId)
  meetingIds <- meetings$meetingId

  agenda_raw <- purrr::map(
    meetingIds,
    fetch_individual_meeting_agenda_item_titles
  )

  # Filter out NULLs
  agenda_raw <- purrr::compact(agenda_raw)

  agenda_raw <- purrr::list_rbind(agenda_raw)
  result <- categorize_df(agenda_raw)
  return(result)
}
