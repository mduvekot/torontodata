#' @title key terms
#' @description creates a tibble to use as a look u table

#' @return tibble
#' @details based on climatefast's "Key terms list-2.pdf"
#' @examples
#' \dontrun{
#' if(interactive()){
#'  View(climate_terms())
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tribble}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[tidyr]{unnest_longer}}
#' @rdname climate_terms
#' @export
#' @importFrom tibble tribble
#' @importFrom dplyr mutate arrange
#' @importFrom purrr map
#' @importFrom tidyr unnest_longer
#' @importFrom snakecase to_title_case
#'
climate_terms <- function() {
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
    "Climate change-related terms",
    c(
      all_cases("climate"),
      all_cases("cooling centres"),
      all_cases("emission"),
      all_cases("environment"),
      all_cases("flood"),
      all_cases("fossil"),
      all_cases("fuel"),
      all_cases("gas"),
      all_cases("global warming"),
      all_cases("greenhouse"),
      all_cases("mitig\\w*"),
      all_cases("Net zero"),
      all_cases("pollut\\w*"),
      all_cases("smog"),
      all_cases("sustainab\\w*"),
      "TransformTO"
    ),
    "Energy terms",
    c(
      all_cases("Energy"),
      all_cases("retrofit"),
      all_cases("pump"),
      all_cases("wind"),
      all_cases("power"),
      all_cases("solar"),
      all_cases("hydro"),
      all_cases("electri\\w*"),
      all_cases("Air condition\\w*")
    ),
    "Biodiversity and Indigenous",
    c(
      all_cases("Biodiversity"),
      all_cases("conservation"),
      all_cases("green"),
      all_cases("Indig\\w*"),
      all_cases("pollen"),
      all_cases("Pollin\\w*"),
      all_cases("pollu\\w*"),
      all_cases("tree")
    ),
    "Transportation terms",
    c(
      all_cases("bike lanes"),
      all_cases("bus"),
      all_cases("busing"),
      all_cases("congestion"),
      all_cases("cycling"),
      "GO",
      "Metrolinx",
      "Pearson",
      all_cases("pedestrian"),
      "Scarborough",
      all_cases("streetcars"),
      all_cases("subways"),
      "Transit",
      "TTC",
      "UP"
    ),
    "Other",
    c(
      all_cases("Air"),
      all_cases("Carbon"),
      all_cases("compost"),
      all_cases("Conservation"),
      all_cases("democracy"),
      all_cases("Ecosystem"),
      all_cases("food"),
      all_cases("green jobs"),
      all_cases("housing"),
      all_cases("Local governance"),
      all_cases("local"),
      "Ontario Place",
      all_cases("polling"),
      all_cases("pollins"),
      all_cases("Ravine"),
      all_cases("refugee"),
      all_cases("supportive homes"),
      all_cases("tariffs"),
      all_cases("Warming centres"),
      all_cases("Cooling centres"),
      all_cases("Waste"),
      "Waterfront"
    )
  ) |>
    # dplyr::mutate(
    #   key_terms = key_terms |> purrr::map(tolower)
    # ) |>
    tidyr::unnest_longer(.data[["key_terms"]]) |>
    dplyr::arrange(.by = .data[["sectors"]], .by_group = TRUE, key_terms)
}
