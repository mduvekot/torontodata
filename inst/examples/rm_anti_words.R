library(tidyverse)
library(constructive)
library(dplyr)
library(ggplot2)
library(stringr)
# library(torontodata)

source(here::here("inst/examples/anti_words.R"))
folder_path <- "~/Desktop/council"
csv_files <- list.files(
  path = folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

df <- csv_files |>
  lapply(
    read.csv,
    stringsAsFactors = FALSE,
    colClasses = c(
      "character",
      "integer",
      "character",
      "character",
      "character",
      "character",
      "integer",
      "character"
    )
  ) |>
  dplyr::bind_rows()

pattern_ <- paste0("\\b(", paste(anti_words, collapse = "|"), ")\\b")
pattern_

df |>
  dplyr::rowwise() |>
  dplyr::filter(
    !stringr::str_detect(
      agendaItemTitle,
      stringr::regex(pattern = pattern_, ignore_case = TRUE)
    )
  ) |> 
  mutate (titles = str_split(agendaItemTitle, pattern = " - ")) |> 
  unnest_longer(titles) |> 
    summarise(.by = titles, n = n()) |> 
    arrange(desc(n)) |> 
  filter(n > 3) |> View() 
  print(n = Inf)
