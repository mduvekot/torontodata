#' @title make_fetch_meetings
#' @description create a function that can store cached results
#' @return function
#' @details  `make_fetch_meetings()` returns a closure that fetches meeting records from the
#' City of Toronto's Council API for a given `decisionBodyId`. The returned function
#' caches results internally, so repeated calls with the same `decisionBodyId` avoid
#' redundant API requests and return previously fetched data.
#'
#' This design improves performance when working with multiple decision bodies,
#' especially in iterative workflows or interactive sessions. The cache is stored
#' in memory and scoped to the lifetime of the closure instance.
#'
#' To use the function:
#' ```
#' fetcher <- make_fetch_meetings()
#' meetings_2566 <- fetcher(2566)  # First call — fetches from API
#' meetings_2566_again <- fetcher(2566)  # Second call — uses cached result
#' ```
#'
#' You can also use it inline:
#' ```
#' meetings_2562 <- make_fetch_meetings()(2562)
#' ```
#'
#' Note: Each call to `make_fetch_meetings()` creates a new cache. To reuse cached
#' results across calls, store the returned function in a variable.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  meetings_2562 <- make_fetch_meetings()(decisionBodyId = 2562) # new instance
#'  fetcher <- make_fetch_meetings() # store closure in a variable
#'  fetcher(2562)
#'  fetcher(2566)
#'
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname make_fetch_meetings
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
