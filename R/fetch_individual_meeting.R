#' @title fetch individual meeting
#' @description fetches the Record from URLS like "https://secure.toronto.ca/council/api/individual/meeting/25849.json"
#' @param meetingId integer, Default: 25849
#' @return list
#' @details DETAILS
#' @example fetch_individual_meeting(meetingId = 25849)
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname fetch_individual_meeting
#' @export
#' @importFrom jsonlite fromJSON
fetch_individual_meeting <- function(meetingId = 25849) {
  Sys.sleep(.3)
  base_url <- "https://secure.toronto.ca/council/api/individual/meeting/"
  endpoint <- paste0(base_url, meetingId, ".json")
  json_data <- jsonlite::fromJSON(endpoint)
  json_data$Record
}
