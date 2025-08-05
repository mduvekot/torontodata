#' @title fetch agenda council date
#' @description scrapes a website to fetch that date that an item will be
#'  discussed in Council
#' @param url a valid url,
#'  Default: 'https://secure.toronto.ca/council/agenda-item.do?item=2023.IE1.1'
#' @return date
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fetch_agenda_item_council_date(
#'   "https://secure.toronto.ca/council/agenda-item.do?item=2025.EX24.2"
#'    )
#'  }
#' }
#' @seealso
#'  \code{\link[rvest]{read_html}},
#'  \code{\link[rvest]{html_element}},
#'  \code{\link[rvest]{html_text}}
#'  \code{\link[stringr]{str_detect}}
#' @rdname fetch_agenda_item_council_date
#' @export
#' @importFrom rvest read_html html_elements html_text
#' @importFrom stringr str_detect
fetch_agenda_item_council_date <- function(
  url = "https://secure.toronto.ca/council/agenda-item.do?item=2025.EX24.2"
) {
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
