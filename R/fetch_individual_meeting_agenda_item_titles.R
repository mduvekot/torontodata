#' Fetch agenda item titles from a meeting
#'
#' @param meetingId Numeric ID of the meeting
#'
#' @return  a dataframe with nativeTermYear, referenceNumber, agendaItemTitle
#' @examples
#' # example code
#' fetch_individual_meeting_agenda_item_titles(25793)
#' fetch_individual_meeting_agenda_item_titles(meetingId = 25775)
#'
#' @export
# fetch_individual_meeting_agenda_item_titles <- function(meetingId = 25775) {
#   meeting_data <- fetch_individual_meeting(meetingId = meetingId)
#   purrr::map(
#     meeting_data$sections$agendaItems,
#     ~ dplyr::pull(.x, agendaItemTitle)
#   ) |>
#     purrr::flatten_chr()
# }

# fetch_individual_meeting_agenda_item_titles <- function(meetingId = 25793) {
#   meetingId <- dplyr::first(meetingIds)
#   meeting_data <- fetch_individual_meeting(meetingId = meetingId)
#
#   meeting_data$sections$agendaItems[[1]] |>
#     dplyr::select(nativeTermYear, referenceNumber, agendaItemTitle)
# }

fetch_individual_meeting_agenda_item_titles <- function(meetingId) {
  meeting_data <- fetch_individual_meeting(meetingId = meetingId)

  # Try to grab the first agenda item safely
  item <- tryCatch(
    meeting_data[["sections"]][["agendaItems"]][[1]],
    error = function(e) NULL
  )

  if (is.null(item) || !inherits(item, "data.frame") || nrow(item) == 0) {
    return(NULL) # or tibble(), or data.frame() depending on your downstream needs
  }

  # Return selected columns
  dplyr::select(
    item,
    .data$nativeTermYear,
    .data$referenceNumber,
    .data$agendaItemTitle
  )
}
