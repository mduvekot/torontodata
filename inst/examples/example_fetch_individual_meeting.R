meeting_data <- fetch_individual_meeting(meetingId = 25849)
agendaItemTitles <- meeting_data$sections$agendaItems |>
  purrr::map(~ dplyr::pull(.x, agendaItemTitle)) |>
  purrr::flatten_chr()
agendaItemTitles
