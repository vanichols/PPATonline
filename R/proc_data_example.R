# Process raw data into processed data

rm(list = ls())

library(tidyverse)
library(readxl)

d1 <- readxl::read_excel("data/raw/Example questionnaire.xlsx",
                         skip = 5)

#--process into tidy data
d2 <-
  d1 %>%
  tidyr::fill(title, pesticide_load) |>
  dplyr::mutate(pesticide_load = as.numeric(pesticide_load),
                weight = as.numeric(weight)) |>
  dplyr::select(-notes, -pesticide_load)

data_example <- d2

data_example |> 
  saveRDS("data/processed/data_example.RDS")
