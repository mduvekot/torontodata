#' @title fetch_agenda_item_status
#' @description FUNCTION_DESCRIPTION
#' @param meetingId integer
#' @return dataframe
#' @details fetches the date that an item will be discussed in a council meeting
#'  with fetch_individual_meeting() and updates the dataframe with the result of
#'  a query that attempts to get the council data by scraping the website for
#'  the agendaItem.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fetch_agenda_item_status(meetingId = 25775)
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname fetch_agenda_item_status
#' @export
#' @importFrom dplyr mutate
#'
fetch_agenda_item_status <- function(meetingId = 25775) {
  # an example of an agendaItem that was adopted by the Board ff Health
  # https://secure.toronto.ca/council/agenda-item.do?item=2025.HL23.2

  meeting <- fetch_individual_meeting(meetingId)

  meetingAgenda <- meeting$sections$agendaItems[[1]] |>
    # https://secure.toronto.ca/council/agenda-item.do?item=2025.HL23.2
    dplyr::mutate(
      url = paste0(
        "https://secure.toronto.ca/council/agenda-item.do?item=",
        .data$nativeTermYear,
        ".",
        .data$referenceNumber
      )
    )
  meetingAgenda |>
    dplyr::rowwise() |>
    dplyr::mutate(councildate = fetch_agenda_item_council_date(.data$url))
}
