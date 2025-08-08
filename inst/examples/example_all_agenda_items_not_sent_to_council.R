library(torontodata)
library(tidyverse)

decisionBodies <- fetch_multiple_decisionbody_list()
decisionBodyIds <- fetch_multiple_decisionbody_list() |>
  dplyr::pull(decisionBodyId)
str(decisionBodyIds)

do_something <- function(decisionBodyId) {
  local_decisionBodyId <- decisionBodyId
  print(paste(which(decisionBodyIds == decisionBodyId), decisionBodyId))
  decisionBodyName = decisionBodies |>
    filter(decisionBodyId == local_decisionBodyId) |>
    pull(decisionBodyName) |>
    str_replace_all(" ", "_") |>
    tolower()

  # decisionBodyId <- 2466
  objname <- paste0("agendas_", decisionBodyId)
  assign(objname, categorize_agendas(decisionBodyId))

  get(objname)

  agendas_council <- get(objname) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      decisionBodyId = decisionBodyId,
      council = council_date(
        nativeTermYear = nativeTermYear,
        referenceNumber = referenceNumber
      )
    )

  print(head(agendas_council))

  agendas_council <- agendas_council |>
    # filter(!is.na(key_term)) |>
    # head() |>
    mutate(
      pattern = str_replace_all(key_term, "(\\w+).*", "\\\\b\\1\\\\w*\\\\b"),
      matches = str_extract_all(
        agendaItemTitle,
        regex(pattern, ignore_case = TRUE)
      )
    ) |>
    tidyr::unnest_longer(matches)

  write.csv(
    file = paste0(
      "~/Desktop/council/agendas_",
      decisionBodyId,
      "_",
      decisionBodyName,
      ".csv"
    ),
    agendas_council
  )
}

do_something(2748)

map(decisionBodyIds[1:76], do_something)
