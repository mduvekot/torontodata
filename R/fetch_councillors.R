#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_councillors <- fetch_councillors()
#' get_councillors()|> print (n = Inf)
#' get_councillors()$Councillor
#' get_councillors()$last_name
#'  }
#' }
#' @seealso
#'  \code{\link[rvest]{read_html}}, \code{\link[rvest]{html_table}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{c("rowwise", "rowwise")}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_split}}
#' @rdname fetch_councillors
#' @export
#' @importFrom rvest read_html html_table
#' @importFrom dplyr mutate rowwise ungroup arrange
#' @importFrom stringr str_replace_all str_split
#'
fetch_councillors <- function() {
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      message("Using cached councillor data")
      return(cache)
    }

    url <- "https://www.toronto.ca/city-government/council/members-of-council/"
    html <- rvest::read_html(url)
    tables <- rvest::html_table(html)

    result <- tables[[1]] |>
      dplyr::mutate(
        Councillor = stringr::str_replace_all(Councillor, "Councillor ", "")
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        first_name = stringr::str_split(Councillor, " ")[[1]][1],
        last_name = paste(
          stringr::str_split(Councillor, " ")[[1]][-1],
          collapse = " "
        ),
        Councillor = factor(Councillor, levels = Councillor[order(last_name)])
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(last_name)
    cache <<- result
    result
  }
}

# # closure-based caching function
# get_councillors <- fetch_councillors()
# get_councillors()|> print (n = Inf)

# get_councillors()$Councillor
# get_councillors()$last_name
