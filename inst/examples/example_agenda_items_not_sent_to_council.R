library(torontodata)

decisionBodyId <- fetch_multiple_decisionbody_list() |>
  dplyr::filter(
    #decisionBodyName == "Infrastructure and Environment Committee"
    decisionBodyName == "Board of Health"
  ) |>
  dplyr::pull(decisionBodyId)

objname <- paste0("agendas_", decisionBodyId)
assign(objname, categorize_agendas(decisionBodyId))


get(objname)$categorized

agendas_council <- get(objname)$categorized |>
  dplyr::rowwise() |>
  dplyr::mutate(
    council = council_date(
      nativeTermYear = nativeTermYear,
      referenceNumber = referenceNumber
    )
  )

write.csv(
  file = paste0("~/Desktop/agendas_", decisionBodyId, "_council.csv"),
  agendas_council
)


# open all the urls of agandaItems that have a climate-related key term in their title, but  have not (yet) been considered by council.
agendas_council |>
  dplyr::filter(is.na(council)) |>
  dplyr::mutate(
    url = paste0(
      "https://secure.toronto.ca/council/agenda-item.do?item=",
      nativeTermYear,
      ".",
      referenceNumber
    )
  ) |>
  dplyr::pull(url) |>
  purrr::map(browseURL)
