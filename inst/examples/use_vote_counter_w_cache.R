vote_counter(2025, "EX23.2")
vote_counter(2025, "EX23.2")
vote_counter(2025, "IE21.10")
vote_counter(2025, "IE21.4")
vote_counter(2025, "PH20.7")
vote_counter(2025, "EX22.5")
vote_counter(2025, "MM28.37")
vote_counter(2025, "EX21.2")
vote_counter(2025, "EX21.4")
vote_counter(2025, "MM26.7")
vote_counter(2025, "EX20.13")
vote_counter(2025, "PH18.6")
vote_counter(2024, "MM24.42")
vote_counter(2024, "IE18.5")
vote_counter(2024, "PH16.1")
vote_counter(2024, "EX17.1")
vote_counter(2024, "MM22.20")
vote_counter(2024, "EX17.3")
vote_counter(2024, "IE16.8")
vote_counter(2024, "MM20.24")
vote_counter(2024, "IE14.4")
vote_counter(2024, "MM19.9")
vote_counter(2024, "IE14.9")
vote_counter(2024, "IE14.4")
vote_counter(2024, "MM17.9")
vote_counter(2024, "EX13.5")
vote_counter(2024, "EX11.8")
vote_counter(2024, "IE10.6")
vote_counter(2024, "IE10.3")
vote_counter(2023, "IE9.5")
vote_counter(2023, "EC6.6")
vote_counter(2023, "MM7.25")
vote_counter(2023, "MM6.13")
vote_counter(2023, "EX4.10")
vote_counter(2023, "MM6.17")
vote_counter(2023, "IE3.3")
vote_counter(2023, "MM2.14")
vote_counter(2022, "GL32.1")

df <- tibble::tribble(
  ~nativeTermYear,
  ~referenceNumber,
  2025,
  "EX23.2",
  2025,
  "EX23.2",
  2025,
  "IE21.10",
  2025,
  "IE21.4",
  2025,
  "PH20.7",
  2025,
  "EX22.5",
  2025,
  "MM28.37",
  2025,
  "EX21.2",
  2025,
  "EX21.4",
  2025,
  "MM26.7",
  2025,
  "EX20.13",
  2025,
  "PH18.6",
  2024,
  "MM24.42",
  2024,
  "IE18.5",
  2024,
  "PH16.1",
  2024,
  "EX17.1",
  2024,
  "MM22.20",
  2024,
  "EX17.3",
  2024,
  "IE16.8",
  2024,
  "MM20.24",
  2024,
  "IE14.4",
  2024,
  "MM19.9",
  2024,
  "IE14.9",
  2024,
  "IE14.4",
  2024,
  "MM17.9",
  2024,
  "EX13.5",
  2024,
  "EX11.8",
  2024,
  "IE10.6",
  2024,
  "IE10.3",
  2023,
  "IE9.5",
  2023,
  "EC6.6",
  2023,
  "MM7.25",
  2023,
  "MM6.13",
  2023,
  "EX4.10",
  2023,
  "MM6.17",
  2023,
  "IE3.3",
  2023,
  "MM2.14",
  2022,
  "GL32.1",
  # not in cf
  2025,
  "EX25.31",
  2025,
  "HL26.5",
  2025,
  "MM31.21",
  2010,
  "RM52.1"
)

df |> purrr::pmap(vote_counter) |> purrr::list_rbind()

###############################

# this is pretty slow
all_votes_list <- agenda_items |>
  dplyr::select(nativeTermYear, referenceNumber) |>
  purrr::pmap(safe_vote_counter, .progress = TRUE)

save(all_votes_list, file = here::here("inst/extdata/all_votes_list.RData"))

# all_votes_list <- foo
all_votes_results <- purrr::map(all_votes_list, "result")
all_votes_results_ne <- purrr::keep(
  all_votes_results,
  ~ is.data.frame(.) && nrow(.) > 0
)
all_agenda_items_with_votes <- final_df <- dplyr::bind_rows(
  all_votes_results_ne
)


## all the votes on items that did not get a vote in council that might be  climate related
all_agenda_items_with_votes |>
  dplyr::summarise(
    .by = c(referenceNumber, nativeTermYear),
    sections = list(unique(section))
  ) |>
  tidyr::unnest_wider(sections, names_sep = "_") |>
  dplyr::filter(
    sections_1 != "council",
    # nativeTermYear == 2025
  ) |>
  dplyr::select(c(nativeTermYear, referenceNumber)) |>
  dplyr::left_join(agenda_items, by = c("referenceNumber", "nativeTermYear")) |>
  torontodata::categorize_df(terms_tbl = torontodata::climate_terms()) |>
  # make sure we don't catch any that are climate issues after all
  dplyr::filter(!is.na(match)) |>
  dplyr::pull(url) |>
  cat(sep = "\n")
purrr::map(browseURL)


# dplyr::left_join(all_agenda_items_with_votes, by = c("referenceNumber", "nativeTermYear"))

#  dplyr::mutate(url =  paste0("https://secure.toronto.ca/council/agenda-item.do?item=", nativeTermYear, ".", referenceNumber))

# who has most carried motions
stringr::str_extract(
  "Majority Required - RM32.7 - Perks - motion 1",
  pattern = ".* - (.*) - .*",
  group = 1
)

# I should rename note to Vote Description per the TO OpenData
all_agenda_items_with_votes |>
  dplyr::filter(nativeTermYear == 2025) |>
  dplyr::select(note, result) |>
  unique()

dplyr::mutate(
  whose = stringr::str_extract(note, pattern = ".* - (.*) - .*", group = 1)
) |>

  dplyr::distinct() |>
  dplyr::summarize(.by = c(whose, result), n = dplyr::n()) |>
  tidyr::drop_na() |>
  dplyr::arrange(desc(n)) |>
  tidyr::pivot_wider(names_from = result, values_from = n) |>
  # replace NAs with 0s
  dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0))) |>
  dplyr::mutate(success_rate = Carried / (Lost + Carried)) |>
  dplyr::arrange(desc(success_rate)) |>
  View()


# how many agenda items have votes

all_agenda_items_with_votes |>
  dplyr::select(c(nativeTermYear, referenceNumber, section)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    section = dplyr::case_when(
      section == "unknown" ~ "committee",
      .default = section
    )
  ) |>
  print(n = Inf) |>
  dplyr::summarise(.by = section, n = dplyr::n())
