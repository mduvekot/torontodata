#' @title fetch individual meeting
#' @description FUNCTION_DESCRIPTION
#' @param meetingId PARAM_DESCRIPTION, Default: 25849
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @description fetches the Record from URLS like "https://secure.toronto.ca/council/api/individual/meeting/25849.json"
#' @param meetingId integer, Default: 25849
#' @return list
#' @details fetch JSON data from the toronto council API for a given meeetingId,
#'   as you might see in URLs like https://secure.toronto.ca/council/#/committees/2468/25777
#'   where 2468 is the decisionBodyId and 25777 the meetingId
#'   returns the Record, which contains a list of 5 items,
#'   the one you want is most likely the sections, which contains agendaItems
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fetch_individual_meeting(meetingId = 25849)
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname fetch_individual_meeting
#' @export
fetch_individual_meeting <- function(meetingId = 25849) {
  Sys.sleep(.3)
  base_url <- "https://secure.toronto.ca/council/api/individual/meeting/"
  endpoint <- paste0(base_url, meetingId, ".json")
  json_data <- jsonlite::fromJSON(endpoint)
  json_data$Record
}
