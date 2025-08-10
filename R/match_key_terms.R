#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df dataframe
#' @param name_col character, the anme of the column that tas the text t0 be matched
#' @param lut a dataframe, like key_terms()
#' @return OUTPUT_DESCRIPTIONdtaframe
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  df <- fetch_all_agenda_items(decisionBodyId = 2984)
#' match_key_terms(df, "agendaItemTitle", key_terms())
#'  }
#' }
#' @rdname match_key_terms
#' @export
#'
match_key_terms <- function(df, name_col = "agendaItemTitle", lut = key_terms) {
  # Ensure the column exists
  if (!name_col %in% names(df)) {
    stop("The specified name column does not exist in the dataframe.")
  }

  # Create an empty list to store matches
  results <- lapply(seq_len(nrow(df)), function(i) {
    text <- df[[name_col]][i]
    matches <- lapply(seq_len(nrow(lut)), function(j) {
      key <- lut$key_terms[j]
      sector <- lut$sector[j]
      if (grepl(key, text, ignore.case = TRUE)) {
        list(
          key_term = key,
          sector = sector,
          match = regmatches(text, regexpr(key, text, ignore.case = TRUE))
        )
      } else {
        NULL
      }
    })
    matches <- Filter(Negate(is.null), matches)
    if (length(matches) == 0) {
      list(key_term = NA, sector = NA, match = NA)
    } else {
      # Return all matches as separate rows
      lapply(matches, function(m) {
        c(df[i, , drop = FALSE], m)
      })
    }
  })

  # Flatten the list and bind into a dataframe
  do.call(rbind, unlist(results, recursive = FALSE))
}
