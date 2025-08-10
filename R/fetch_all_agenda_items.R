#' @title fetch agenda item
#' @description from meeting, get the dataframe $sections$agendeItems[[1]]
#' @param meetingId integer meetingId
#' @return data frame
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{pluck}}
#' @rdname fetch_agenda_item
#' @export
#' @importFrom purrr pluck
fetch_agenda_item <- function(meetingId) {
  fetch_individual_meeting(meetingId) |>
    purrr::pluck("sections", "agendaItems", 1)
}

#' @title fetch all agenda_items
#' @description fetch all agendaItems for a decisionBody
#' @param decisionBodyId integer, decisonBodyId
#' @return data frame
#' @details gets all the meeitngIds for a decsionBody,
#' then all the agendaItems,
#' converts the condiderStartTime from millisonds into a POSIX date
#' @examples
#' \dontrun{
#' if(interactive()){
#'  agenda_items_29784 <- fetch_all_agenda_items(decisionBodyId = 2984)
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{keep}}, \code{\link[purrr]{list_c}}
#'  \code{\link[lubridate]{as_date}}
#' @rdname fetch_all_agenda_items
#' @export
#' @importFrom dplyr pull mutate
#' @importFrom purrr map compact list_rbind
#' @importFrom lubridate as_datetime
fetch_all_agenda_items <- function(decisionBodyId) {
  fetch_multiple_meeting(decisionBodyId = decisionBodyId) |>
    dplyr::pull(.data$meetingId) |>
    purrr::map(fetch_agenda_item) |>
    purrr::compact() |>
    purrr::list_rbind() |>
    dplyr::mutate(
      considerStartTime = lubridate::as_datetime(
        .data$considerStartTime / 1000
      ),
      url = paste0(
        "https://secure.toronto.ca/council/agenda-item.do?item=",
        .data$nativeTermYear,
        ".",
        .data$referenceNumber
      )
    )
}
