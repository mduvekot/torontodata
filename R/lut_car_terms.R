#' @title car terms
#' @description returns a lookup table for car-related terms

#' @return tibble with two columns: sector and key_terms
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  car_terms()
#'  }
#' }
#' @seealso
#'  \code{\link[snakecase]{caseconverter}}
#'  \code{\link[tibble]{tribble}}
#'  \code{\link[tidyr]{unnest_longer}}
#'  \code{\link[dplyr]{arrange}}
#' @rdname car_terms
#' @export
#' @importFrom snakecase to_title_case to_sentence_case
#' @importFrom tibble tribble
#' @importFrom tidyr unnest_longer
#' @importFrom dplyr arrange

car_terms <- function() {
  all_cases <- function(x) {
    paste0(
      unique(c(
        x,
        tolower(x),
        toupper(x),
        snakecase::to_title_case(x),
        snakecase::to_sentence_case(x)
      ))
    )
  }

  tibble::tribble(
    ~sectors,
    ~key_terms,
    "Crime",
    c(
      "car theft",
      "car thieves",
      "carjacking",
      "joyriding"
    ),
    "Parking",
    c(
      all_cases("parking"),
      all_cases("car park"),
      all_cases("garage"),
      all_cases("truck"),
      all_cases("vehicle")
    ),
    "Speeding",
    c(
      "Automated speed cameras",
      "Traffic Calming",
      "Automated Speed Enforcement Infractions",
      all_cases("speed\\w*"),
      all_cases("street racing"),
      all_cases("street race"),
      all_cases("speed camera"),
      "Stunt Driving",
      "Speed Humps",
      "Automated Speed Enforcement",
      all_cases("crash")
    ),
    "Pollution",
    c(
      all_cases("gas\\w*"),
      all_cases("diesel\\w*"),
      all_cases("LPG"),
      "Vehicle Noise",
      all_cases("pump")
    ),
    "Congestion",
    c(
      all_cases("Gardiner"),
      all_cases("congest"),
      all_cases("traffic jam")
    ),
    "Transportation terms",
    c(
      all_cases("bike lanes"),
      all_cases("pilons"),
      all_cases("sidewalk\\w*"),
      "Expressways",
      "Pedestrianization",
      "Laneway",
      all_cases("lane"),
      all_cases("street"),
      all_cases("road"),
      all_cases("bike lanes"),
      all_cases("traffic"),
      all_cases("highway\\w*"),
      all_cases("crossing"),
      all_cases("intersection"),
      all_cases("zebra")
    ),
    "Safety",
    c(
      "aggressive driving",
      "Road Safety",
      "Vision Zero Road Safety Initiatives",
      "community safety zone",
      "Community Safety Zone"
    ),
    "Other",
    c(
      "U-Turn",
      "Towing",
      "Demerit Points",
      "Blocking Snow Routes",
      "Red Light Camera",
      "Road Classification Criteria",
      "Bus",
      "Streetcar",
      all_cases("Road Rehabilitation"),
      "Turn Prohibitions",
      "Laneway Reconstruction",
      "Road Resurfacing",
      "Dash Camera",
      "Pavement Markings",
      "on-street signage",
      all_cases("automotive"),
      all_cases("motorist")
    )
  ) |>
    tidyr::unnest_longer(.data[["key_terms"]]) |>
    dplyr::arrange(.by = .data[["sectors"]], .by_group = TRUE, key_terms)
}
