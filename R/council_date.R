#' @title council date
#' @description returns the date that follows the string
#'  *It will be considered by City Council on
#'  in agend item webspages with a URL of the form
#'  https://secure.toronto.ca/council/agenda-item.do?item=nativeTermYear,referenceNumber
#' @param nativeTermYear integer
#' @param referenceNumber chraacter
#' @return charcater vector
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # council_date(nativeTermYear = 2025,  referenceNumber = "IE23.2")
#'  }
#' }
#' @seealso
#'  \code{\link[rvest]{read_html}}, \code{\link[rvest]{html_element}}, \code{\link[rvest]{html_text}}
#'  \code{\link[stringr]{str_detect}}
#' @rdname council_date
#' @export
#' @importFrom rvest read_html html_elements html_text
#' @importFrom stringr str_detect
council_date <- function(
  nativeTermYear = 2025,
  referenceNumber = "IE23.2"
) {
  url <- paste0(
    "https://secure.toronto.ca/council/agenda-item.do?item=",
    nativeTermYear,
    ".",
    referenceNumber
  )
  #browseURL(url)
  html <- rvest::read_html(url)
  elements <- html |>
    rvest::html_elements(
      xpath = "/html/body/main/div/section/div/div[3]/ul/li"
    ) |>
    rvest::html_text(trim = TRUE)
  elements
  # does elements contain the string  "It will be considered by City Council"
  considered <- stringr::str_detect(
    string = elements,
    pattern = "It will be considered by City Council"
  )
  #find the index of the element for which considered is TRUE
  if (sum(considered) > 0) {
    idx <- which(considered == TRUE)
    # get the date
    date <- sub(
      ".*It will be considered by City Council on ",
      "",
      elements[idx]
    )
    # remove the period at the end of the string
    date <- sub("\\.$", "", date)
  } else {
    date = NA
  }
  date
}
