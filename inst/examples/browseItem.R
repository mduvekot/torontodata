#' @title browse item
#' @description opens a browser for an agenda Item # of the form year.referenceNumber
#' @param item string
#' @return NULL
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname browseItem
#' @export
#'
browseItem <- function(item) {
  url <- paste0("https://secure.toronto.ca/council/agenda-item.do?item=", item)
  print(url)
  browseURL(url)
}
