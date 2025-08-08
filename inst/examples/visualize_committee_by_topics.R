library(constructive)
library(dplyr)
library(ggplot2)
library(stringr)
library(torontodata)

folder_path <- "~/Desktop/council/"
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

df <- csv_files %>%
  lapply(read.csv, stringsAsFactors = FALSE, 
    colClasses = c("character","integer","character","character","character","character", "integer", "character")) %>%
  bind_rows()

df <- df |> 
  mutate(
    pattern =  str_replace_all(key_term, "(\\w+).*", "\\\\b\\1\\\\w*\\\\b"),
    matches = str_extract_all(agendaItemTitle, regex(pattern, ignore_case = TRUE))
) |> 
  tidyr::unnest_longer(matches)



df <- df |> mutate(
  # decisionBodyCode = str_sub(referenceNumber, 1,2),
  decisionBodyCode = str_extract(referenceNumber, "[A-Z]+(?=\\d)"),
  has_date = !is.na(council)
)

data_decisonbody_id_code_name

committee_labels <- setNames(
  data_decisonbody_id_code_name()$decisionBodyName |> str_wrap(40), 
  data_decisonbody_id_code_name()$decisionBodyCode)
str(committee_labels)


df$sector |> unique() 
df |>  
  filter(!is.na(sector)) |> 
  summarize(.by = everything(), n = n() ) |> 
ggplot()+
labs(
  title = "Number of Agenda Items per committee and sector",
  subtitle = "and whether they were or will be considered by City Council",
  caption = " source: hhttps://www.toronto.ca/city-government/council/council-committee-meetings/"
)+
  aes(
    x = n, 
    y = sector,
    fill= sector,
  alpha = has_date
)+
  geom_col(orientation = "y")+
  scale_fill_manual(values = c(
    "Transportation terms" = "#db251c",
    "Biodiversity and Indigenous"= "#ed7c26" ,
    "Other" = "#165788",
    "Energy terms" = "#005d55",
    "Climate change-related terms" = "forestgreen"
), na.value = "#00ffff")+
  scale_alpha_manual(values = c(.5, 1), na.value = 0)+
  facet_wrap(
    ~decisionBodyCode, 
    labeller = as_labeller(committee_labels)
    )+
      theme_void()+
      guides(alpha = guide_legend("considered by council"))+
  coord_cartesian(clip = "off") +   
  theme(
        plot.margin= margin(18, 18, 18, 18, "pt"),
        text = element_text(family = "AtkinsonHyperlegibleNext-Regular", size = 10),
        plot.subtitle = element_text(margin = margin(2, 0, 48, 0, "pt")), 
        strip.text = element_text(family = "AtkinsonHyperlegibleNext-Regular", size = 10, hjust = 0, vjust = 1),
        strip.placement = "inside",    
        axis.text.y.left = element_text(family = "AtkinsonHyperlegibleNext-Light", size = 10, hjust = 1),
        legend.position = "none",
        # legend.position = c(0, 1.1),              
        legend.justification = c(0, 1),         
        legend.title = element_text(hjust = 0.5), 
        legend.text = element_text(hjust = 0), 
        legend.box.just = "left",                
        legend.direction = "horizontal",
        legend.box.margin = margin(t = 48, r = 0, b = 72, l = 0, unit = "pt")
    )



df
