library(torontodata)
library(tidyverse)

decisionBodies <- fetch_multiple_decisionbody_list()
decisionBodyIds <- fetch_multiple_decisionbody_list() |>dplyr::pull(decisionBodyId)
str(decisionBodyIds)

do_something <- function(decisionBodyId) { 
  local_decisionBodyId <- decisionBodyId
  print(paste(which(decisionBodyIds == decisionBodyId), decisionBodyId))
  decisionBodyName = decisionBodies |> 
    filter(decisionBodyId == local_decisionBodyId) |> 
    pull(decisionBodyName) |>
    str_replace_all(" ", "_") |> 
    tolower()

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
  file = paste0("~/Desktop/agendas_", decisionBodyId, "_",  decisionBodyName, ".csv"),
  agendas_council
)

}

do_something(2566)

map(decisionBodyIds[47:76], do_something)
