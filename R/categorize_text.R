#' @title catgeorize text
#' @description catagorizes a text using key_terms()
#' @param text text
#' @return tibble
#' @details DETAILS
#' @examples
#' categorize_text("Climate change is real")
#' @seealso
#'  \code{\link[stringr]{str_detect}}, \code{\link[stringr]{modifiers}}
#'  \code{\link[tibble]{tibble}}
#' @rdname categorize_text
#' @export
#' @importFrom stringr str_detect regex
#' @importFrom tibble tibble
categorize_text <- function(text) {
  terms_tbl <- key_terms()
  detected <- stringr::str_detect(
    text,
    # stringr::regex(terms_tbl$key_terms, ignore_case = FALSE) is too greedy
    stringr::regex(
      paste0("\\b", terms_tbl$key_terms, "\\b"),
      ignore_case = FALSE
    )
  )
  if (!any(detected)) return(NULL)
  tibble::tibble(
    key_term = terms_tbl$key_terms[detected],
    sector = terms_tbl$sectors[detected]
  )
}
