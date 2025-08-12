library(constructive)
library(dplyr)
library(ggplot2)
library(stringr)
library(torontodata)
library(readr)
library(tidyverse)

folder_path <- "~/Desktop/to_council_commitees_2025-08-11"

csv_files <- list.files(
  path = folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

df <- csv_files |>
  lapply(
    readr::read_csv,
    col_types = readr::cols(
      ...1 = col_integer(),
      nativeTermYear = col_integer(),
      considerStartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      considerTypeCd = col_character(),
      itemStatusCd = col_character(),
      meetingId = col_integer(),
      publishTypeCd = col_character(),
      wards = col_character(),
      agendaId = col_integer(),
      inCamera = col_character(),
      agendaItemId = col_integer(),
      currentInd = col_character(),
      nativeItemStatusCd = col_character(),
      referenceNumber = col_character(),
      keyItemInd = col_character(),
      heldByFirstName = col_character(),
      heldByLastName = col_character(),
      statutory = col_character(),
      urgent = col_character(),
      heldByMemberId = col_character(),
      confidentialReason = col_character(),
      annotation = col_character(),
      url = col_character(),
      sectors = col_character(),
      key_terms = col_character(),
      key_term = col_character(),
      match = col_character()
    )
  ) |>
  bind_rows()

colnames(df)

# df <- rename(df, "row_number" = "...1")

# we don't need the first row
# those are row numbers that are not really paort of the dataset anyway
df <- df |> select(-c(`...1`))

df <- df |>
  mutate(
    # decisionBodyCode = str_sub(referenceNumber, 1,2),
    decisionBodyCode = str_extract(referenceNumber, "[A-Z]+(?=\\d)")
  )

lut <- data_decisionbody_id_code_name()
df <- left_join(df, lut)

# redo with tighter matching
df <- df |>
  select(-c(sectors, key_terms, key_term, match))
df <- df |> match_key_terms_tidy()

committee_labels <- setNames(
  data_decisionbody_id_code_name()$decisionBodyName |> str_wrap(40),
  data_decisionbody_id_code_name()$decisionBodyCode
)
str(committee_labels)


df <- df |> mutate(has_match = !is.na(match))

df_council = NULL
# costly, do not run automatically
# rm(df_council)
df_council <- df |>
  filter(has_match == TRUE) |>
  #  slice_sample(n = 10) |>
  rowwise() |>
  mutate(council = council_date(nativeTermYear, referenceNumber))


# save df_council

save(df, file = here::here("inst/extdata/df.RData"))
save(df_council, file = here::here("inst/extdata/df_council.RData"))

# restore df_coucil
load(file = here::here("inst/extdata/df.RData"), .GlobalEnv)
load(file = here::here("inst/extdata/df_council.RData"), .GlobalEnv)


rm(df_all)
rm(x)
rm(y)
x <- df |> distinct() |> filter(!is.na(agendaItemId))
y <- df_council |>
  select(agendaItemId, council) |>
  distinct() |>
  filter(!is.na(council))


df_all <- left_join(x, y, by = join_by(agendaItemId))

#save(df_all, file = here::here("inst/extdata/df_all.RDS"))
save(df_all, file = here::here("inst/extdata/df_all.RData"))

# load(here::here("inst/extdata/df_all.RData"))
###############################################################################

# save the data to an excel file
library(xlsx)

library(openxlsx2)
wb <- wb_workbook() %>% wb_add_worksheet() %>% wb_add_data(x = df_all)
wb_save(wb, file = "~/Desktop/climatefast.xlsx")


###############################################################################
df_all |>
  filter(!is.na(sectors)) |>
  summarize(.by = everything(), n = n()) |>
  ggplot() +
  labs(
    title = "Number of Agenda Items per committee and sector",
    subtitle = "and whether they were or will be considered by City Council",
    caption = " source: hhttps://www.toronto.ca/city-government/council/council-committee-meetings/"
  ) +
  aes(
    x = n,
    y = sectors,
    fill = sectors,
    alpha = is.na(council)
  ) +
  geom_col(orientation = "y") +
  scale_fill_manual(
    values = c(
      "Transportation terms" = "#db251c",
      "Biodiversity and Indigenous" = "#ed7c26",
      "Other" = "#165788",
      "Energy terms" = "#005d55",
      "Climate change-related terms" = "forestgreen"
    ),
    na.value = "#00ffff"
  ) +
  scale_alpha_manual(values = c(.5, 1), na.value = 0) +
  facet_wrap(
    ~decisionBodyCode,
    labeller = as_labeller(committee_labels)
  ) +
  theme_void() +
  guides(alpha = guide_legend("considered by council")) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(18, 18, 18, 18, "pt"),
    text = element_text(family = "AtkinsonHyperlegibleNext-Regular", size = 10),
    plot.subtitle = element_text(margin = margin(2, 0, 48, 0, "pt")),
    strip.text = element_text(
      family = "AtkinsonHyperlegibleNext-Regular",
      size = 10,
      hjust = 0,
      vjust = 1
    ),
    strip.placement = "inside",
    axis.text.y.left = element_text(
      family = "AtkinsonHyperlegibleNext-Light",
      size = 10,
      hjust = 1
    ),
    legend.position = "none",
    # legend.position = c(0, 1.1),
    legend.justification = c(0, 1),
    legend.title = element_text(hjust = 0.5),
    legend.text = element_text(hjust = 0),
    legend.box.just = "left",
    legend.direction = "horizontal",
    legend.box.margin = margin(t = 48, r = 0, b = 72, l = 0, unit = "pt")
  )

###############################################################################
# per the chart abovem the highedt propostryion of climate ossues that did not
# get to council is in IE and EX.
# can we see wehy?
# Note that where match == NA we didn't check (because it's expensive) if those
# went to council, so that number just tells us how mnany other agendaItems the
# committtee discussed

df_all |>
  filter(decisionBodyCode == "IE") |>
  mutate(has_council_date = !is.na(council)) |>
  summarise(.by = c(match, has_council_date), n = n()) |>
  pivot_wider(
    id_cols = match,
    names_from = has_council_date,
    values_from = n
  ) |>
  mutate(
    `TRUE` = replace_na(`TRUE`, 0),
    `FALSE` = replace_na(`FALSE`, 0),
    pct = `TRUE` / (`TRUE` + `FALSE`)
  ) |>
  arrange(desc(pct)) |>
  mutate(pct_ = scales::label_percent()(pct)) |>
  print(n = Inf)
# here we saw that of the agendaItems that matched "Climate", only 3 out of 7
# went to council. What were they? Did thay ALSO do poorly in other commitees?

df_all |>
  filter(match == "Climate") |>
  select(decisionBodyName, council, itemStatusCd, agendaItemTitle, url)

# It looks like the agendaItems that didn't get sent to council were reports

df |>
  filter(!is.na(match)) |>
  summarize(.by = c(key_terms, sectors, decisionBodyCode), n = n()) |>
  arrange(desc(n)) |>
  filter(n > 5) |>
  tidyr::complete(decisionBodyCode, key_terms) |>
  ggplot() +
  aes(
    x = decisionBodyCode,
    y = key_terms,
    fill = n
  ) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "#000000")


# word frequency
library(tidyr)
library(stringr)
library(tidytext)
df |>
  select(agendaItemTitle) |>
  unnest_tokens(word, agendaItemTitle) %>% # Split into words
  count(word, sort = TRUE) |>
  select(word) |>
  head(10000) |>
  tail(1001) |>
  constructive::construct(opts_list(constructor = c("list")))


# match all_wprds
library(dplyr)
library(stringr)

# Create regex pattern from all_words
pattern <- paste0("\\b(", paste(all_words, collapse = "|"), ")\\b")

library(dplyr)
library(stringr)

# open all the agendaItems that match a pattern but not a key_term in a browswer
pattern_ <- paste0("\\b(", paste(all_words, collapse = "|"), ")\\b")
df %>%
  rowwise() |>
  filter(str_detect(
    agendaItemTitle,
    regex(pattern = pattern_, ignore_case = TRUE)
  )) |>
  #select(matches, key_term, agendaItemTitle) |>
  filter(is.na(matches)) |>
  mutate(
    url = paste0(
      "https://secure.toronto.ca/council/agenda-item.do?item=",
      nativeTermYear,
      ".",
      referenceNumber
    )
  ) |>
  pull(url) |>
  purrr::map(browseURL)
