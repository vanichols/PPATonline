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
  ggplot(aes(value_bin, score)) +
  geom_col() +
  facet_grid(ratingF ~confidenceF)

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
  ggplot(aes(value_bin, score)) +
  geom_col() +
  facet_grid(confidenceF~ratingF , labeller = label_wrap_gen(5))

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


# figure ------------------------------------------------------------------


#--make a nice figure for the PDF

value_colors <- c(
  "1" = "#270B55",
  "2" = "#65156E",
  "3" = "#9F2963",
  "4" = "#D44842",
  "5" = "#F9C126"
)

plot_data <- 
  d5 %>%
  mutate(rating2 = case_when(
    rating == "very low value" ~ "1 - Not acceptable",
    rating == "low value" ~ "2 - Likely to dissuade from use",
    rating == "neutral value" ~ "3 - Will be a consideration in decision to use", 
    rating == "high value" ~ "4- Acceptable",
    rating == "very high value" ~ "5 - Highly acceptable or improved")
    ) |> 
  dplyr::mutate(confidence_text2 = dplyr::case_when(
    confidence == "vh" ~ "Very high confidence",
    confidence == "h" ~ "High confidence",
    confidence == "m" ~ "Medium confidence",
    confidence == "l" ~ "Low confidence"
  ),
  rating = str_to_sentence(rating)) |> 
  dplyr::mutate(ratingF = fct_inorder(rating2),
                ratingF = fct_rev(ratingF),
                confidenceF = factor(confidence_text2, levels = c("Low confidence",
                                                                            "Medium confidence",
                                                                            "High confidence",
                                                                            "Very high confidence")),
                confidenceF = fct_rev(confidenceF),
                value_binF = fct_inorder(as.character(value_bin)),
                value_binF = fct_rev(value_binF)) 


plot_data %>%
  ggplot(aes(value_binF, score)) +
  geom_col(aes(fill = value_binF), color = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = value_colors) +
  labs(x = "Value bin (higher value indicates more value)",
       y = "Density of value (1-100)") +
  facet_grid(confidenceF~ratingF , labeller = label_wrap_gen(15)) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    # legend.position = "top",
    # legend.justification = "center",
    legend.position = c(0.05, 0.95),  # x=5% from left, y=95% from bottom
    legend.justification = c("left", "top"), # anchor legend box
    legend.box = "horizontal",
    legend.key = element_blank(),
    legend.box.margin = margin(),
    legend.margin = margin(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    legend.location = "plot",
    #--get rid of minor gridlines
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = "black", fill = "white", linewidth = 2),
    #--ratings text
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text = element_text(color = "gray"),
    strip.text.x = element_text(size = rel(1.2)),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("www/rating-confidence-pairs.pdf", width = 8, height = 6)
ggsave("www/rating-confidence-pairs.png", width = 8, height = 6)
