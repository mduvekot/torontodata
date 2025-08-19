library(tidyverse)
library(torontodata)
# example of an AgendaItem that has recorded votes
browseURL("https://secure.toronto.ca/council/agenda-item.do?item=2025.IE22.1")
# https://secure.toronto.ca/council/agenda-item.do?item=2025.EX23.2
# https://secure.toronto.ca/council/agenda-item.do?item=2025.EX25.1
# https://secure.toronto.ca/council/agenda-item.do?item=2023.EC3.5

# if the file exists, and the object agenda_itens does not alreay exists
# load it with load(file = here::here("inst/extdata/agenda_items.RData"))

if (
  file.exists(here::here("inst/extdata/agenda_items.RData")) &&
    !exists("agenda_itens")
) {
  load(file = here::here("inst/extdata/agenda_items.RData"))
}

# find all agendItems for 2025

# first find all the decisionBodyIds
decisionBodyIds <- fetch_multiple_decisionbody_list()

fetcher <- fetcher <- make_fetch_meetings()

meetings <- map(decisionBodyIds$decisionBodyId[1:76], fetcher)

meetings <- map(meetings, function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  NULL
})


meetings <- list_rbind(meetings)
meetingIds <- meetings$meetingId

fetch_individual_meeting_agenda_item_titles(24988)

agenda_fetcher <- make_agenda_item_title_fetcher()

agenda_items <- map(meetingIds, agenda_fetcher) |>
  lapply(function(x) {
    if (is.data.frame(x)) {
      return(x)
    }
    NULL
  }) |>
  list_rbind() |>
  mutate(
    url = paste0(
      "https://secure.toronto.ca/council/agenda-item.do?item=",
      nativeTermYear,
      ".",
      referenceNumber
    )
  )

# show ten random samples
map(sample(agenda_items$url, 10), browseURL)


save(agenda_items, file = here::here("inst/extdata/agenda_items.RData"))

agenda_items_backup <- agenda_items
rm(agenda_items)


load(file = here::here("inst/extdata/agenda_items.RData"))
