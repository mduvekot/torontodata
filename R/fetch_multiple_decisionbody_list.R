#' fetch a list of decision bodies from the Toronto Council API
#'
#' This function retrieves a list of decision-making bodies for a given term, i.e. (8 is 2022-2026)
#'
#' @param term_id the current term as an integer,
#' @return a data frame of decison bodies
#' @export
#' @examples
#' fetch_multiple_decisionbody_list()
fetch_multiple_decisionbody_list <- function(term_id = 8) {
  json <-
    jsonlite::fromJSON(paste0(
      "https://secure.toronto.ca/",
      "council/api/multiple/",
      "decisionbody-list.json?termId=",
      term_id
    ))
  json$Records
}
