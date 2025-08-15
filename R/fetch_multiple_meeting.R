#' @title fetch_multiple_meeting
#' @description FUNCTION_DESCRIPTION
#' @param decisionBodyId integer, Default: 2566
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  meetings_2562 <- make_fetch_meetings()(decisionBodyId = 2562) # creates a new instance with its own cache
#'  fetcher <- make_fetch_meetings() # store the closure
#'  fetcher(2562)
#'  fetcher(2566)
#'
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname fetch_multiple_meeting
#' @export
#' @importFrom jsonlite fromJSON
#'
# without caching
# fetch_multiple_meeting <- function(decisionBodyId = 2566) {
#   # https://secure.toronto.ca/council/api/multiple/meeting.json?decisionBodyId=2566
#   meeting_list <- jsonlite::fromJSON(
#     paste0(
#       "https://secure.toronto.ca/council/api/multiple/meeting.json?decisionBodyId=",
#       decisionBodyId
#     )
#   )
#   meeting_list$Records
# }

# with closure-based caching
make_fetch_meetings <- function() {
  cache <- list()

  function(decisionBodyId) {
    key <- as.character(decisionBodyId)

    if (!is.null(cache[[key]])) {
      message("Using cached data for decisionBodyId: ", decisionBodyId)
      return(cache[[key]])
    }

    url <- paste0(
      "https://secure.toronto.ca/council/api/multiple/meeting.json?decisionBodyId=",
      decisionBodyId
    )

    meeting_list <- jsonlite::fromJSON(url)
    cache[[key]] <<- meeting_list$Records
    meeting_list$Records
  }
}
