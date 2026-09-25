# Process Adrian's raw beta parameters

rm(list = ls())

library(tidyverse)
library(readxl)

d1 <- 
  readxl::read_excel("data/raw/Betas params for Gina.xlsx",
                         skip = 1) |> 
  janitor::clean_names()

d2 <- 
  d1 |> 
  pivot_longer(l:vh)

d3 <- 
  d2 |> 
  mutate(rating = case_when(
    cat_code == 1 ~ 1,
    cat_code == 2 ~ 2,
    cat_code == 3 ~ 3,
    cat_code == 4 ~ 4,
    cat_code == 5 ~ 5,
    TRUE ~ 9999
  ))

d4 <- 
  d3 |> 
  select(rating_numeric = rating, confidence = name, param, value) |> 
  arrange(confidence, param, rating_numeric)

#--assign character values to ratings - NOT
d5 <- d4
  # d4 |>
  # dplyr::mutate(rating = dplyr::case_when(
  #   rating_numeric == 5 ~ "very high value",
  #   rating_numeric == 4 ~ "high value",
  #   rating_numeric == 3 ~ "neutral value",
  #   rating_numeric == 2 ~ "low value",
  #   rating_numeric == 1 ~ "very low value",
  #   TRUE ~ "uhoh"
  # )) 

#--assign full text to confidence_text
d6 <-
  d5 |>
  dplyr::mutate(confidence_text = dplyr::case_when(
    confidence == "vh" ~ "Very high",
    confidence == "h" ~ "High",
    confidence == "m" ~ "Medium",
    confidence == "l" ~ "Low"
  ))

#--assign numeric values to confidence_text
d7 <-
  d6 |>
  dplyr::mutate(confidence_numeric = dplyr::case_when(
    confidence == "vh" ~ 4,
    confidence == "h" ~ 3,
    confidence == "m" ~ 2,
    confidence == "l" ~ 1
  ))

d8 <- 
  d7 |> 
  pivot_wider(names_from = param, values_from = value) |> 
  janitor::clean_names()

data_adrianbetas <- d8

data_adrianbetas |> 
  write_rds("data/processed/data_adrianbetas.RDS")


data_adrianbetas |> 
  ggplot(aes(confidence_numeric, beta)) +
  geom_point(aes(color = as.factor(rating_numeric)))

