vote_counter3 <- function(nativeTermYear = 2025, referenceNumber = "MM28.37") {
  url <- paste0(
    "https://secure.toronto.ca/council/agenda-item.do?item=",
    nativeTermYear,
    ".",
    referenceNumber
  )

  print(url)
  # browseURL(url)
  html <- rvest::read_html(url)

  # Define your parser
  table2df <- function(table, section) {
    # to do: an empty list of absentees when Absent: 0 adds entry to the list of councillors
    # also happens when no concillor vots no, as in 2025 EX21.2
    table |>
      dplyr::select(-1) |>
      dplyr::rename(votes = 1) |>
      dplyr::mutate(
        result = stringr::str_replace_all(colnames(table)[1], "Result: ", ""),
        note = colnames(table)[2],
        vote_type = dplyr::case_when(
          stringr::str_detect(votes, "voted Yes") ~ "yes",
          stringr::str_detect(votes, "voted No") ~ "no",
          stringr::str_detect(votes, "were absent") ~ "absent"
        ),
        votes = stringr::str_remove(votes, "Members that .* are\\s*")
      ) |>
      tidyr::separate_rows(votes, sep = ",\\s*") |>
      dplyr::rename(councillor = votes) |>
      dplyr::mutate(
        nativeTermYear = nativeTermYear,
        referenceNumber = referenceNumber,
        section = section
      )
  }

  # Get all collapsible sections
  cards <- rvest::html_elements(html, "#agenda-item-accordion .card")
  # cards
  # cat(as.character(cards[1]))
  # str(cards)
  # Parse each section
  parsed_sections <- purrr::imap(cards, function(card, index) {
    heading <- rvest::html_element(card, ".card-header button") |>
      rvest::html_text(trim = TRUE)

    section_type <- dplyr::case_when(
      stringr::str_detect(heading, "City Council") ~ "council",
      stringr::str_detect(heading, "Committee") ~ "committee",
      TRUE ~ "unknown"
    )
    # this also finds tables that are notvote tables
    # tables <- rvest::html_elements(card, "table")
    # find only tables after <h4 class="vote-table-header">
    tables <- rvest::html_elements(card, "h4.vote-table-header + table")

    if (length(tables) == 0) {
      message("No vote tables found for ", referenceNumber)
      return(NULL) # or return NULL
    }

    purrr::imap(
      tables,
      ~ table2df(
        rvest::html_table(.x, fill = TRUE) |> tibble::as_tibble(),
        section = section_type
      ) |>
        dplyr::mutate(
          # section_id = paste0("section_", index, "_table_", .y)
          section_id = index,
          idx = .y
        )
    )
  })

  # Flatten and return
  # purrr::list_rbind(purrr::flatten(parsed_sections))  |>
  #   dplyr::select(councillor, vote_type, result, nativeTermYear, referenceNumber, section, idx, note)
  #
  #
  flattened <- purrr::flatten(parsed_sections)

  if (length(flattened) == 0) {
    message("No vote data to return for ", referenceNumber)
    return(tibble::tibble()) # or return NULL if preferred
  }

  purrr::list_rbind(flattened) |>
    dplyr::select(
      councillor,
      vote_type,
      result,
      nativeTermYear,
      referenceNumber,
      section,
      idx,
      note
    )
}


vote_counter3(2025, "EX23.2") |> print(n = Inf)
vote_counter3(2025, "IE21.10") |> print(n = Inf)
vote_counter3(2025, "IE21.4") |> print(n = Inf)
vote_counter3(2025, "PH20.7") |> print(n = Inf)
vote_counter3(2025, "EX22.5") |> print(n = Inf)
vote_counter3(2025, "MM28.37") |> print(n = Inf)
vote_counter3(2025, "EX21.2") |> print(n = Inf)
vote_counter3(2025, "EX21.4") |> print(n = Inf)
vote_counter3(2025, "MM26.7") |> print(n = Inf)
vote_counter3(2025, "EX20.13") |> print(n = Inf)
vote_counter3(2025, "PH18.6") |> print(n = Inf)

vote_counter3(2024, "MM24.42") |> print(n = Inf)
vote_counter3(2024, "IE18.5") |> print(n = Inf)
vote_counter3(2024, "PH16.1") |> print(n = Inf)
vote_counter3(2024, "EX17.1") |> print(n = Inf)
vote_counter3(2024, "MM22.10") |> print(n = Inf)
vote_counter3(2024, "EX17.3") |> print(n = Inf)
vote_counter3(2024, "IE16.8") |> print(n = Inf)
vote_counter3(2024, "MM20.24") |> print(n = Inf)

vote_counter3(2024, "IE14.4") |> print(n = Inf)

vote_counter3(2024, "MM19.9") |> print(n = Inf)
vote_counter3(2024, "IE14.9") |> print(n = Inf)
vote_counter3(2024, "IE14.3") |> print(n = Inf)
vote_counter3(2024, "MM17.9") |> print(n = Inf)
vote_counter3(2024, "EX13.5") |> print(n = Inf)
vote_counter3(2024, "EX11.8") |> print(n = Inf)
vote_counter3(2024, "IE10.6") |> print(n = Inf)
vote_counter3(2024, "IE10.3") |> print(n = Inf)

vote_counter3(2023, "IE9.5") |> print(n = Inf)
vote_counter3(2023, "EC6.6") |> print(n = Inf)
vote_counter3(2023, "MM7.25") |> print(n = Inf)
vote_counter3(2023, "MM6.13") |> print(n = Inf)
vote_counter3(2023, "EX4.10") |> print(n = Inf)
vote_counter3(2023, "MM6.17") |> print(n = Inf)
vote_counter3(2023, "IE3.3") |> print(n = Inf)
vote_counter3(2023, "MM2.14") |> print(n = Inf)

vote_counter3(2022, "GL32.1") |> print(n = Inf)


################################################################################
library(dplyr)
library(tidyr)
library(purrr)

df <- tibble::tribble(
  ~nativeTermYear,
  ~referenceNumber,
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
  "MM22.10",
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
  "IE14.3",
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
  "GL32.1"
)

vote_list <- df |>
  purrr::pmap(vote_counter3)

vote_data <- purrr::list_rbind(vote_list)


filtered_votes <- vote_data |>
  dplyr::filter(vote_type %in% c("yes", "no")) |>
  dplyr::select(councillor, vote_type, referenceNumber)


str(filtered_votes)
###############################################################################
###############################################################################
co_vote_pairs <- filtered_votes %>%
  group_by(referenceNumber, vote_type) %>%
  filter(n() >= 2) %>%
  reframe(pairs = combn(councillor, 2, simplify = FALSE)) %>%
  #unnest(pairs) %>%
  mutate(
    councillor1 = map_chr(pairs, 1),
    councillor2 = map_chr(pairs, 2)
  ) %>%
  select(councillor1, councillor2) |>
  summarize(.by = everything(), n = n()) |>
  mutate(
    councillor1 = factor(
      councillor1,
      levels = c(
        "Rachel Chernos Lin",
        "Michael Thompson",
        "Parthi Kandavel",
        "Paul Ainslie",
        "Brad Bradford",
        "Alejandra Bravo",
        "Jon Burnside",
        "Shelley Carroll",
        "Lily Cheng",
        "Olivia Chow",
        "Mike Colle",
        "Vincent Crisanti",
        "Paula Fletcher",
        "Stephen Holyday",
        "Ausma Malik",
        "Nick Mantas",
        "Josh Matlow",
        "Jennifer McKelvie",
        "Chris Moise",
        "Amber Morley",
        "Jamaal Myers",
        "Frances Nunziata",
        "James Pasternak",
        "Gord Perks",
        "Anthony Perruzza",
        "Dianne Saxe"
      )
    )
  ) |>
  mutate(
    councillor2 = factor(
      councillor2,
      levels = c(
        "Paul Ainslie",
        "Brad Bradford",
        "Alejandra Bravo",
        "Jon Burnside",
        "Shelley Carroll",
        "Lily Cheng",
        "Olivia Chow",
        "Mike Colle",
        "Vincent Crisanti",
        "Paula Fletcher",
        "Stephen Holyday",
        "Ausma Malik",
        "Nick Mantas",
        "Josh Matlow",
        "Jennifer McKelvie",
        "Chris Moise",
        "Amber Morley",
        "Jamaal Myers",
        "James Pasternak",
        "Gord Perks",
        "Anthony Perruzza",
        "Dianne Saxe",
        "Michael Thompson",
        "Parthi Kandavel",
        "Rachel Chernos Lin",
        "Frances Nunziata"
      )
    )
  ) |>
  drop_na()


# co_vote_pairs$councillor1 |>  unique()
co_vote_pairs |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(aes(x = councillor1, y = councillor2, fill = n)) +
  ggplot2::geom_text(
    aes(x = councillor1, y = councillor2, label = n),
    color = "#ffffff"
  ) +
  ggplot2::scale_fill_viridis_c() +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.text.x.bottom = element_text(angle = -90, hjust = 0),
    axis.text.y.left = element_text(angle = 0, hjust = 1),
    panel.background = element_rect(fill = "#000")
  )


# Sémiologie Graphique,
#
library(tidyr)
library(dplyr)
library(tibble)

# Convert to wide format
vote_matrix <- co_vote_pairs |>
  pivot_wider(names_from = councillor2, values_from = n, values_fill = 0) |>
  column_to_rownames("councillor1")

vote_matrix
# Compute distance (or similarity)
dist_matrix <- dist(vote_matrix, method = "euclidean") # or "manhattan", etc.

dist_matrix

hc <- hclust(dist_matrix)
hc
ordered_levels <- rownames(vote_matrix)[hc$order]
ordered_levels

co_vote_pairs <- co_vote_pairs |>
  mutate(
    councillor1 = factor(councillor1, levels = ordered_levels),
    councillor2 = factor(councillor2, levels = ordered_levels)
  )


################################################################################
################################################################################
################################################################################
# Bertinm, but with co-vote rates

library(tidyverse)

co_vote_counts <- filtered_votes %>%
  group_by(referenceNumber, vote_type) %>%
  filter(n() >= 2) %>%
  reframe(pairs = combn(councillor, 2, simplify = FALSE)) %>%
  mutate(
    councillor1 = map_chr(pairs, 1),
    councillor2 = map_chr(pairs, 2)
  ) %>%
  count(councillor1, councillor2, name = "same_vote_count")


joint_vote_counts <- filtered_votes %>%
  group_by(referenceNumber) %>%
  filter(n() >= 2) %>%
  reframe(pairs = combn(councillor, 2, simplify = FALSE)) %>%
  mutate(
    councillor1 = map_chr(pairs, 1),
    councillor2 = map_chr(pairs, 2)
  ) %>%
  count(councillor1, councillor2, name = "joint_vote_count")

co_vote_rates <- full_join(
  co_vote_counts,
  joint_vote_counts,
  by = c("councillor1", "councillor2")
) %>%
  mutate(
    same_vote_count = replace_na(same_vote_count, 0),
    joint_vote_count = replace_na(joint_vote_count, 0),
    co_vote_rate = same_vote_count / joint_vote_count
  ) %>%
  filter(joint_vote_count > 0)

co_vote_rates_clean <- co_vote_rates |>
  filter(
    councillor1 != "",
    councillor2 != "",
    !is.na(councillor1),
    !is.na(councillor2)
  ) |>
  group_by(councillor1, councillor2) |>
  summarise(co_vote_rate = mean(co_vote_rate, na.rm = TRUE), .groups = "drop")

co_vote_matrix <- co_vote_rates_clean |>
  pivot_wider(
    names_from = councillor2,
    values_from = co_vote_rate,
    values_fill = 0
  ) |>
  distinct(councillor1, .keep_all = TRUE) |>
  column_to_rownames("councillor1") |>
  as.matrix()

# Compute clustering order
row_order <- hclust(dist(co_vote_matrix))$order
col_order <- hclust(dist(t(co_vote_matrix)))$order

row_names <- rownames(co_vote_matrix)[row_order]
col_names <- colnames(co_vote_matrix)[col_order]


# Reorder matrix
ordered_matrix <- co_vote_matrix[row_order, col_order]

plot_data <- ordered_matrix |>
  as.data.frame() |>
  rownames_to_column("councillor1") |>
  pivot_longer(
    -councillor1,
    names_to = "councillor2",
    values_to = "co_vote_rate"
  )

plot_data <- plot_data |>
  mutate(
    councillor1 = factor(councillor1, levels = row_names),
    councillor2 = factor(councillor2, levels = col_names)
  )


plot_data <- plot_data |>
  filter(
    councillor1 %in% get_councillors()$Councillor,
    councillor2 %in% get_councillors()$Councillor
  )

ggplot(plot_data) +
  aes(x = councillor1, y = councillor2, fill = co_vote_rate) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.x.bottom = element_text(angle = -90, hjust = 0))
