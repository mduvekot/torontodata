#' lookup the decisionbodyId  of a city Committee (including Council) by name
#'
#' This function retrieves a an interger value for a decisionBody
#'
#' @param name, the exact name of the decisionBody
#' @return an inter value for the decisionBodyId
#' @export
#' @importFrom rlang .data
#' @examples
#' lookup_decision_body_id()
lookup_decision_body_id <- function(name = "City Council") {
  fetch_multiple_decisionbody_list() |>
    dplyr::filter(as.character(name) == as.character(.data$decisionBodyName)) |>
    dplyr::pull(.data$decisionBodyId)
}
