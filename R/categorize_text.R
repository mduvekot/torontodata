#' @title categorize text
#' @description categorizes a text using key_terms()
#' @param text text
#' @param terms_tbl tibble with two columnsl key_terms and sector. Default: key_terms()
#' @return tibble
#' @details DETAILS
#' @examples
#' categorize_text(text = "The Toronto Atmospheric Fund's (TAF) mandate
#' includes the promotion of global climate stabilization through the
#' reduction of greenhouse gas emissions, local air quality, energy
#' conservation and efficiency, public understanding of global warming
#' and its implications for the urban environment, and related research
#' and technology development. ")
#' @seealso
#'  \code{\link[stringr]{str_detect}}, \code{\link[stringr]{modifiers}}
#'  \code{\link[tibble]{tibble}}
#' @rdname categorize_text
#' @export
#' @importFrom stringr str_detect regex
#' @importFrom tibble tibble
categorize_text <- function(text, terms_tbl = key_terms()) {
  terms_tbl <- terms_tbl |>
    dplyr::mutate(pattern = paste0("\\b", key_terms, "\\b"))

  detected <- stringr::str_detect(
    text,
    # stringr::regex(terms_tbl$key_terms, ignore_case = FALSE) is too greedy
    # pattern = stringr::regex(paste0("\\b", terms_tbl$key_terms, "\\b"),ignore_case = FALSE)
    pattern = terms_tbl$pattern
  )

  if (!any(detected)) {
    return(NULL)
  }

  tibble::tibble(
    key_term = terms_tbl$key_terms[detected],
    pattern = terms_tbl$pattern[detected],
    sector = terms_tbl$sectors[detected],
    match = stringr::str_extract(
      string = text,
      pattern = terms_tbl$pattern[detected]
    ),
    keyword_count = NULL
  ) |>
    dplyr::add_count(match, name = "keyword_count") |>
    dplyr::arrange(dplyr::desc(match)) |>
    dplyr::arrange(dplyr::desc(.data$keyword_count))
}
