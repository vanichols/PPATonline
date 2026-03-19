# Process raw data into processed data

rm(list = ls())

library(tidyverse)
library(readxl)

d1 <- readxl::read_excel("data/raw/beta-distribution-cheat-sheet-impact.xlsx",
                         skip = 5)

#--process into tidy data
#--the values listed are for impact, not value
#--create a mirror image
d2 <-
  d1 %>%
  dplyr::select(-tot) %>%
  tidyr::fill(confidence) %>%
  janitor::clean_names() %>%
  tidyr::pivot_longer(x1:x5) %>%
  dplyr::mutate(name = readr::parse_number(name),
                value_bin = dplyr::case_when(
                  name == 1 ~ 5,
                  name == 2 ~ 4,
                  name == 3 ~ 3,
                  name == 4 ~ 2,
                  name == 5 ~ 1,
                  TRUE ~ 9999
                )) %>%
  dplyr::rename(score = value) %>%
  dplyr::select(-name) %>%
  dplyr::mutate_if(is.character, stringr::str_to_lower)


#--check it
d2 %>%
  dplyr::mutate(ratingF = forcats::fct_inorder(rating),
                confidenceF = forcats::fct_inorder(confidence)) %>%
  ggplot2::ggplot(ggplot2::aes(value_bin, score)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(ratingF ~confidenceF)

#--rename the ratings
d3 <-
  d2 |>
  dplyr::mutate(rating = dplyr::case_when(
    rating == "very low impact" ~ "very high value",
    rating == "low impact" ~ "high value",
    rating == "medium impact" ~ "neutral value",
    rating == "high impact" ~ "low value",
    rating == "very high impact" ~ "very low value"
  ))

#--check it
d3 %>%
  dplyr::mutate(ratingF = forcats::fct_inorder(rating),
                confidenceF = forcats::fct_inorder(confidence)) %>%
  ggplot2::ggplot(ggplot2::aes(value_bin, score)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(confidenceF~ratingF , labeller = ggplot2::label_wrap_gen(5))

#--assign numeric values to ratings
d4 <-
  d3 |>
  dplyr::mutate(rating_numeric = dplyr::case_when(
    rating == "very high value" ~ 5,
    rating == "high value" ~ 4,
    rating == "neutral value" ~ 3,
    rating == "low value" ~ 2,
    rating == "very low value" ~ 1
  )) 

#--assign full text to confidence_text
d5 <-
  d4 |>
  dplyr::mutate(confidence_text = dplyr::case_when(
    confidence == "vh" ~ "Very high",
    confidence == "h" ~ "High",
    confidence == "m" ~ "Medium",
    confidence == "l" ~ "Low"
  ))


data_betas <- 
  d5 |> 
  select(rating_numeric, confidence_text, value_bin, score)

data_betas |> 
  write_rds("data/processed/data_betas.RDS")

#--make a nice figure for the PDF
d5 %>%
  dplyr::mutate(confidence_text2 = dplyr::case_when(
    confidence == "vh" ~ "Very high confidence",
    confidence == "h" ~ "High confidence",
    confidence == "m" ~ "Medium confidence",
    confidence == "l" ~ "Low confidence"
  ),
  rating = str_to_sentence(rating)) |> 
  dplyr::mutate(ratingF = forcats::fct_inorder(rating),
                confidenceF = forcats::fct_inorder(confidence_text2)) %>%
  ggplot2::ggplot(ggplot2::aes(value_bin, score)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(confidenceF~ratingF , labeller = ggplot2::label_wrap_gen(5))
