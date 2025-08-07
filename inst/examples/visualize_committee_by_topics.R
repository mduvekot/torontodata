# Load necessary library


library(constructive)
library(dplyr)
library(ggplot2)

# Set the path to your folder
folder_path <- "~/Desktop/toronto_council_committees/"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSV files
df <- csv_files %>%
  lapply(read.csv, stringsAsFactors = FALSE, 
    colClasses = c("character","integer","character","character","character","character","character")) %>%
  bind_rows()

# referenceNumber = "RG5.3"
# commmittee = str_sub(referenceNumber, 1,2)
df <- df |> mutate(
  commmittee = str_sub(referenceNumber, 1,2)
)

df |>  
  summarize(.by = everything(), n = n() ) |> 
ggplot()+
  aes(
    x = n, 
    y = sector)+
  geom_col(orientation = "y")+
  facet_wrap(~commmittee)


db_list <- fetch_multiple_decisionbody_list() |> 
  select(decisionBodyId, decisionBodyName)

constructive::construct(db_list, opts_tbl_df("tibble"))

commitee_codes <- tibble(
  ~decisionBodyId, ~decisonBodyCode,
  2462, "RM", 
  2463, "ST",
  2464, "EY",
  2465, "NY",
  2466, "TE",
  2467, "SC",
  2468, "EX",
  2469, "BA",
  2482, "CT",
  2483, "RA",
  2484, "SX",
  2488, "RY",
  2489, "ZB",
  2490, "PA",
  2491, "IB", 
  2502, "property standards to",
  2503, "RG",
  2505, "property standards et",
  2509, "property standards ny",
  2510, "EP", 
  2511, "PB",
  2512, NA,
  2522, NA,
  2542, "GG",
  2562, NA,
  2563, "EC", 
  2564, "HL",
  2565, "PH",
  2566, NA,
  2582, "AU",
  2583, "CA",
  2585, "NC",
  2586, "DI",
  2587, "TA",
  2588, "members",
  2602,  "debentures",
  2622, "NA",
  2623, NA,
  2624, NA,
  2625, NA,
  2626, "XV",
  2627, NA,
  2628, NA,
  2629, NA,
  2630, NA,
  2631, NA,
  2642, NA,
  2643, NA,
  2644,NA,
  2645, NA,
  2646, "BX",
  2647, NA,
  2663, "PR",
  2683, "TM",
  2703, NA,
  2704, "EA",
  2724, NA,
  2725, NA,
  2744, NA,
  2745, "FB",
  2746, "MA",
  2747, NA,
  2748, "FA",
  2749, "AA",
  2750, "HS",
  2765, "TS",
  2784, "SE", 
  2804, NA,
  2844, NA,
  2864, NA,
  2904, NA,
  2924, "LR",
  2944, "TTC",
  2945, NA,
  2964, "TTA",
  2984, "TTS"
)




fetch_decisionBodyCode <- function(decisionBodyId) {
  meetings <- fetch_multiple_meeting(decisionBodyId)
  
  if (length(meetings) == 0 || nrow(meetings) == 0) {
    return(tibble::tibble(decisionBodyId = decisionBodyId, decisionBodyCode = NA_character_))
  }
  
  meetings |>  
    head(1) |>   
    select(decisionBodyId, meetingReference) |> 
    mutate(
      decisionBodyCode = stringr::str_sub(meetingReference, 6, 7)
    ) |> 
    select(decisionBodyId, decisionBodyCode)
}
fetch_decisionBodyCode(2512)

db_list$decisionBodyId[30]
fetch_multiple_meeting(2512)

df_decisionBodyCodes <- purrr::map(db_list$decisionBodyId, fetch_decisionBodyCode) |>  list_rbind()

constructive::construct(df_decisionBodyCodes, opts_tbl_df("tribble"))

left_join(db_list, df_decisionBodyCodes)
df_decisionBodyList <- left_join(fetch_multiple_decisionbody_list(), df_decisionBodyCodes)
