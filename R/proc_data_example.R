# Process raw data into processed data

rm(list = ls())

library(tidyverse)
library(readxl)

d1 <- readxl::read_excel("data/raw/Example questionnaire.xlsx",
                         skip = 5)

#--process into tidy data
d2 <-
  d1 %>%
  tidyr::fill(title) |>
  dplyr::mutate(weight = as.numeric(weight),
                rating_numeric = rating_1to5) |>
  dplyr::select(-rating_1to5) |> 
  dplyr::mutate(rating_numeric = as.numeric(rating_numeric))

#--change to confidence_text
d3 <- 
  d2 |> 
  dplyr::mutate(confidence_text = dplyr::case_when(
    confidence == "vh" ~ "Very high",
    confidence == "h" ~ "High",
    confidence == "m" ~ "Medium",
    confidence == "l" ~ "Low"
  )) |> 
  select(-confidence)


#--change 'crop losses' to 'crop value'
#--change 'User health and safety' to 'Human health and safety'
d4 <- 
  d3 |> 
  mutate(across(everything(), ~ str_replace_all(.x, "Crop losses", "Crop value"))) |> 
  mutate(across(everything(), ~ str_replace_all(.x, "User health and safety", "Human health and safety")))
  

data_example <- 
  d4 |> 
  mutate(weight = as.numeric(weight),
         rating_numeric = as.numeric(rating_numeric))

data_example |> 
  saveRDS("data/processed/data_example.RDS")
