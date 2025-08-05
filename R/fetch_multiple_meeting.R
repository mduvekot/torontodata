#' @title fetch_multiple_meeting
#' @description FUNCTION_DESCRIPTION
#' @param decisionBodyId integer, Default: 2566
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  meetings_2566 <- fetch_multiple_meeting(decisionBodyId = 2566)
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname fetch_multiple_meeting
#' @export
#' @importFrom jsonlite fromJSON
#'
fetch_multiple_meeting <- function(decisionBodyId = 2566) {
  # https://secure.toronto.ca/council/api/multiple/meeting.json?decisionBodyId=2566
  meeting_list <- jsonlite::fromJSON(
    paste0(
      "https://secure.toronto.ca/council/api/multiple/meeting.json?decisionBodyId=",
      decisionBodyId
    )
  )
  meeting_list$Records
}
