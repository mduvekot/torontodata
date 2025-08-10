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
      key <- lut$key_terms[j]
      sectors <- lut$sectors[j]

      if (grepl(key, text, ignore.case = TRUE)) {
        match_str <- regmatches(text, regexpr(key, text, ignore.case = TRUE))
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
