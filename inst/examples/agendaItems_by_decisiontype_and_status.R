# Sort agendaItems by Decision Type (considerTypeCd) and Status (ItemStatusCd)

library(tidyverse)
# library(torontodata)

# set a decisonBodyId
# fetch_multiple_decisionbody_list() |> View()
decisionBodyId <- 2587 # Toronto Atmospheric Fund

# utility function that returns a clean agendaItem
fetch_agenda_item <- function(meetingId) {
  fetch_individual_meeting(meetingId) |>
    pluck("sections", "agendaItems", 1)
  # compact() # do we really need this?
}


agenda_items_2587 <- fetch_multiple_meeting(decisionBodyId = 2587) |>
  pull(meetingId) |>
  map(fetch_agenda_item) |>
  compact() |>
  list_rbind() |>
  mutate(
    considerStartTime = lubridate::as_datetime(considerStartTime / 1000),
    url = paste0(
      "https://secure.toronto.ca/council/agenda-item.do?item=",
      nativeTermYear,
      ".",
      referenceNumber
    )
  )

# to see which items were adopted this year
agenda_items_2587 |>
  filter(
    nativeTermYear == year(now()),
    considerTypeCd == "ACTION",
    itemStatusCd == "ADOPTED"
  ) |>
  pull(url) |>
  map(browseURL)
