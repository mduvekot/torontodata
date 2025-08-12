#' @title matchkey terms
#' @description use a lookup table to match key tersma snf sectors to agendaItemTitle
#' @param df data frame
#' @param name_col character, Default: 'agendaItemTitle'
#' @param lut data frame, Default: key_terms()
#' @return data frame
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname match_key_terms
#' @export
match_key_terms <- function(
  df,
  name_col = "agendaItemTitle",
  lut = key_terms()
) {
  if (!name_col %in% names(df)) {
    stop("The specified name column does not exist in the dataframe.")
  }

  # Initialize list to collect all match rows
  all_matches <- list()

  for (i in seq_len(nrow(df))) {
    text <- df[[name_col]][i]
    row_data <- df[i, , drop = FALSE]

    for (j in seq_len(nrow(lut))) {
      key <- paste0("\\b", lut$key_terms[j], "\\b")
      sectors <- lut$sectors[j]

      print(key)

      if (grepl(key, text, ignore.case = FALSE)) {
        match_str <- regmatches(text, regexpr(key, text, ignore.case = FALSE))
        match_row <- cbind(
          row_data,
          key_term = key,
          sectors = sectors,
          match = match_str
        )
        all_matches[[length(all_matches) + 1]] <- match_row
      }
    }

    # If no matches, add a row with NAs
    if (!any(sapply(all_matches, function(x) identical(x[[name_col]], text)))) {
      all_matches[[length(all_matches) + 1]] <- cbind(
        row_data,
        key_term = NA,
        sectors = NA,
        match = NA
      )
    }
  }

  # Combine all match rows into a single dataframe
  do.call(rbind, all_matches)
}

#' @title match_key_terms - tidy
#' @description updatye a dtaframe with matched key_terms
#' @param df daatrframe
#' @param name_col name of a column that contains words to be macthed, Default: 'agendaItemTitle'
#' @param lut a look up tableas a tibble of dataframe PARAM_DESCRIPTION, Default: key_terms()
#' @return data frame
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' df <- tibble::tribble(
#' ~agendaItemTitle,
#' "Trees are cool",
#' "Streets are hot",
#' "Diesel Buses are big polluters")
#' match_key_terms_tidy(df)
#'  }
#' }
#' @rdname match_key_terms_tidy
#' @export
#' @importFrom dplyr rowwise mutate filter select ungroup
#' @importFrom stringr str_c str_extract
#' @importFrom tidyr unnest

match_key_terms_tidy <- function(
  df,
  name_col = "agendaItemTitle",
  lut = key_terms()
) {
  if (!name_col %in% names(df)) {
    stop("The specified name column does not exist in the dataframe.")
  }
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      matches = list(
        lut |>
          dplyr::filter(
            stringr::str_detect(
              !!rlang::sym(name_col),
              stringr::str_c("\\b", .data$key_terms, "\\b")
            )
          ) |>
          dplyr::mutate(
            # too broad
            # key_term = stringr::str_c("\\b", .data$key_terms, "\\b"),
            key_term = stringr::str_c(
              "\\b",
              stringr::str_replace(.data$key_terms, "\\.\\*", "\\\\w*\\\\b")
            ),
            match = stringr::str_extract(!!rlang::sym(name_col), .data$key_term)
          )
      )
    ) |>
    tidyr::unnest(.data$matches, keep_empty = TRUE) |>
    dplyr::select(
      dplyr::everything(),
      # .data$key_term,
      # .data$sectors,
      # .data$match
    ) |>
    dplyr::ungroup()
}

# df <- tibble::tribble(
#   ~agendaItemTitle, ~foo,
#   "Trees are cool", "hey",
#   "Streets are hot", "don't forget",
#   "Diesel Buses are big polluters", "about me"
# )
# match_key_terms_tidy(df)
