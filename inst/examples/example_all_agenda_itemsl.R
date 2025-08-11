library(torontodata)
library(tidyverse)

decisionBodies <- fetch_multiple_decisionbody_list()
decisionBodyIds <- fetch_multiple_decisionbody_list() |>
  dplyr::pull(decisionBodyId)
# str(decisionBodyIds)

make_csv <- function(decisionBodyId) {
  #d decisionBodyId <- 2748
  local_decisionBodyId <- decisionBodyId
  print(paste(which(decisionBodyIds == decisionBodyId), decisionBodyId))
  decisionBodyName = decisionBodies |>
    filter(decisionBodyId == local_decisionBodyId) |>
    pull(decisionBodyName) |>
    str_replace_all(" ", "_") |>
    tolower()

  print(decisionBodyName)
  objname <- paste0("agendas_", decisionBodyId)
  assign(
    objname,
    # categorize_agendas(decisionBodyId)
    fetch_all_agenda_items(decisionBodyId)
  )

  get(objname)
  print(objname)

  tmp <- match_key_terms_tidy(get(objname))
  head(tmp)
  write.csv(
    file = paste0(
      "~/Desktop/test/agendas_",
      decisionBodyId,
      "_",
      decisionBodyName,
      ".csv"
    ),
    tmp
  )
}

make_csv(2587)


map(decisionBodyIds[16:76], make_csv)
