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
  dplyr::select(-rating_1to5)

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


data_example <- d3

data_example |> 
  saveRDS("data/processed/data_example.RDS")
