#' Fetch a list of decision bodies from the Toronto Council API
#'
#' This function retrieves a list of decision-making bodies for a given term.
#' Results are cached by default to avoid repeated API calls.
#'
#' @param termId Integer. The council term (e.g., 8 for 2022–2026).
#' @param use_cache Logical. If TRUE (default), returns cached results if available.
#' @return A data frame of decision bodies.
#' @export
#' @examples
#' fetch_multiple_decisionbody_list(use_cache = TRUE)
#' fetch_multiple_decisionbody_list(use_cache = FALSE)
#'
fetch_multiple_decisionbody_list <- local({
  cache <- list()

  function(termId = 8, use_cache = TRUE) {
    if (use_cache && !is.null(cache[[as.character(termId)]])) {
      return(cache[[as.character(termId)]])
    }

    url <- paste0(
      "https://secure.toronto.ca/",
      "council/api/multiple/",
      "decisionbody-list.json?termId=",
      termId
    )

    print(url)

    json <- jsonlite::fromJSON(url)
    result <- json$Records

    cache[[as.character(termId)]] <<- result
    result
  }
})
