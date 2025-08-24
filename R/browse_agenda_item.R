#' @title browse aganda item
#' @description opens an agneda item in a browser
#' @param nativeTermYear int
#' @param referenceNumber char
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  browse_agenda_item(2022, "BA198.1")
#'  }
#' }
#' @seealso
#'  \code{\link[utils]{browseURL}}
#' @rdname browse_agenda_item
#' @export
#' @importFrom utils browseURL
browse_agenda_item <- function(nativeTermYear, referenceNumber) {
  url <- paste0(
    "https://secure.toronto.ca/council/agenda-item.do?item=",
    nativeTermYear,
    ".",
    referenceNumber
  )
  utils::browseURL(url)
}
