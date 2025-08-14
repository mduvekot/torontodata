#' @title lookup decision body
#' @description look up the decisionBodyId of a Committee (including City Council) by name
#' @param name PARAM_DESCRIPTION, Default: 'City Council'
#' @return integer
#' @details looks up the decisionBodyId in fetch_multiple_decisionbody_list()
#' @examples
#' \dontrun{
#' if(interactive()){
#'  lookup_decision_body_id("Executive Committee")
#'  }
#' }
#' @seealso
#'  \code{\link[torontodata:fetch_multiple_decisionbody_list]{fetch_multiple_decisionbody_list}}
#' @rdname lookup_decision_body_id
#' @export
#'
#'
lookup_decision_body_id <- function(name = "City Council") {
  fetch_multiple_decisionbody_list() |>
    dplyr::filter(as.character(name) == as.character(.data$decisionBodyName)) |>
    dplyr::pull(.data$decisionBodyId)
}
