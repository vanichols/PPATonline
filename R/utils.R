#' Create three plots at once
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A plot
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @import ggridges
#' @import patchwork
#' @export

# for practice, comment out
# data_betas <- read_rds("data/processed/data_betas.RDS")
# data_betas_interpolated <- read_rds("data/processed/data_betas_interpolated.RDS")
# data_example <- read_rds("data/processed/data_example.RDS")

fxn_Make_Plots <- function(data = data_example, betas = data_betas) {
  
  metric_names <- data_example$metric[1:6]
  
  clr1 <- "#ffd74a"
  clr2 <- "#3faf4a"
  
    #--get names of approaches
  strategy_name <-
    data |>
    dplyr::pull(title) |>
    unique()
  
  data1 <- 
    data |>
    #--make metric into a factor
    dplyr::mutate(
      metricF = factor(metric, levels = (metric_names))) 
  
  
  plot_data1 <-
    data1 |>
    dplyr::filter(title == strategy_name[1]) |>
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence_text"),
                     relationship = "many-to-many") |>
    #--make some things for the figure
    dplyr::arrange(metricF) |>
    dplyr::mutate(score = as.integer(score)) |>
    dplyr::select(title, metricF, 
                  #weight, 
                  value_bin, score)
  
  plot_data2 <-
    data1 |>
    dplyr::filter(title == strategy_name[2]) |>
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence_text"),
                     relationship = "many-to-many") |>
    #--make some things for the figure
    dplyr::arrange(metricF) |>
    dplyr::mutate(score = as.integer(score)) |>
    dplyr::select(title, metricF, 
                  #weight, 
                  value_bin, score)
  
  plot_data3 <-
    data1 |>
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence_text"),
                     relationship = "many-to-many") |>
    dplyr::select(title, metricF, value_bin, score) |> 
    mutate(title = fct_inorder(title))
  
  
  #--package 1
  plot1 <-
    plot_data1 |>
    tidyr::uncount(score) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = value_bin,
                   fill = title),
      bw = 0.5,
      show.legend = F
    ) +
    ggplot2::geom_col(data = plot_data1,
                      ggplot2::aes(value_bin, score/100),
                      alpha = 0.5) +
    ggplot2::facet_wrap(~metricF, labeller = label_wrap_gen(width = 20)) +
    ggplot2::scale_fill_manual(values = clr1) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      limits = c(0, 6),
      labels = c("Unacceptable",
                 "Disuaded",
                 "Is a consideration",
                 "Acceptable",
                 "Highly acceptable")
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, .2, .4, .6, .8, 1),
      limits = c(0, 1),
      labels = scales::label_percent(),
    ) +
    ggplot2::labs(
      title = paste(strategy_name[1]),
      y = NULL,
      fill = NULL,
      x = NULL
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = element_blank(),
      legend.position = "top",
      legend.justification = "center",
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
      #--ratings text
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  #--package 2
  plot2 <-
    plot_data2 |>
    tidyr::uncount(score) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = value_bin,
                   fill = title),
      bw = 0.5,
      show.legend = F
    ) +
    ggplot2::geom_col(data = plot_data2,
                      ggplot2::aes(value_bin, score/100),
                      alpha = 0.5) +
    ggplot2::facet_wrap(~metricF, labeller = ggplot2::label_wrap_gen(width = 20)) +
    ggplot2::scale_fill_manual(values = clr2) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      limits = c(0, 6),
      labels = c("Unacceptable",
                 "Disuaded",
                 "Is a consideration",
                 "Acceptable",
                 "Highly acceptable")
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, .2, .4, .6, .8, 1),
      limits = c(0, 1),
      labels = scales::label_percent(),
    ) +
    ggplot2::labs(
      title = paste(strategy_name[2]),
      y = NULL,
      fill = NULL,
      x = NULL
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = element_blank(),
      legend.position = "top",
      legend.justification = "center",
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
      #--ratings text
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  #--overlapping alternative
  plot3a <- 
    plot_data3 |>
    dplyr::mutate(score = as.integer(score)) |>
    tidyr::uncount(score) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = value_bin,
                   fill = title),
      bw = 0.5,
      alpha = 0.8,
      #show.legend = F
    ) +
    ggplot2::facet_wrap(~metricF, labeller = ggplot2::label_wrap_gen(width = 20)) +
    ggplot2::scale_fill_manual(
      values = c(clr1, clr2),
      #values = c("#fdbe85", "#08519c"),
      guide = guide_legend(ncol = 1)
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      limits = c(0, 6),
      labels = c("Unacceptable",
                 "Disuaded",
                 "Is a consideration",
                 "Acceptable",
                 "Highly acceptable")
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, .2, .4, .6, .8, 1),
      limits = c(0, 1),
      labels = scales::label_percent(),
    ) +
    ggplot2::labs(
      title = "Comparison",
      y = NULL,
      fill = NULL,
      x = NULL
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
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
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
    plot3a
    
  (plot1|plot3a|plot2)
  
}


fxn_Calc_Overall_Utility_New <- function(data) {
  
  #--get a weighted average of the rating and confidence
  #--first add a confidence_numeric column
  d1 <-
    data |>
    dplyr::mutate(confidence_numeric = dplyr::case_when(
      confidence_text == "Very high" ~ 4,
      confidence_text == "High" ~ 3,
      confidence_text == "Medium" ~ 2,
      confidence_text == "Low" ~ 1
    ))
  
  #--take the weighted average, round to the tenth digit place, or zero to assign conf text
  d2 <-
    d1 |> 
    group_by(title) |> 
    summarise(utility = round(weighted.mean(rating_numeric, weight), 1), 
              confidence_numeric = round(weighted.mean(confidence_numeric, weight), 1),
              conf_round = round(confidence_numeric, 0))
  
  #--change the conf_numeric to a categorical value
  d3 <- 
    d2 |>
    dplyr::mutate(
      conf = dplyr::case_when(
        conf_round == 4 ~ "Very high",
        conf_round == 3 ~ "High",
        conf_round == 2 ~"Medium",
        conf_round == 1 ~ "Low"
      ))
  
  d.res <-
    d3 |> 
    mutate(metric = "Weighted combo") |> 
    select(title, metric, everything()) |> 
    select(-conf_round)
  
  return(d.res)
  
} 


#' Calcuate the overall utility of the strategies with user-defined weighting
#'
#' @param data_utility 
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A figure
#' @export


# data_example_utility_new <- read_rds("data/processed/data_example_utility_new.RDS")
# data_betas_interpolated <- read_rds("data/processed/data_betas_interpolated.RDS")


fxn_Make_Overall_Utility_Fig_New <- function(data_utility,
                                 betas) {
  
  betas2 <- 
    betas |> 
    mutate(rating_numeric = round(rating_numeric, 1),
           confidence_numeric = round(confidence_numeric, 1))
  
  # betas2 |> 
  #   filter(rating_numeric == 3.8) 
  # 
  
  clr1 <- "#ffd74a"
  clr2 <- "#3faf4a"
  
  plot_data <- 
    data_utility |>
    mutate(rating_numeric = utility) |> 
    dplyr::left_join(betas2,
                     by = c("rating_numeric", "confidence_numeric"),
                     relationship = "many-to-many") |>
    #--make some things for the figure
    dplyr::mutate(score = as.integer(score_new))
  
  
  plot_data |> 
    tidyr::uncount(score) |>
    mutate(title = fct_inorder(title)) |> 
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = value_bin,
                   fill = title),
      bw = 0.5,
      alpha = 0.8,
      #show.legend = F
    ) +
    ggplot2::scale_fill_manual(
      values = c(clr1, clr2),
      #values = c("#fdbe85", "#08519c"),
      guide = guide_legend(ncol = 1)
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      limits = c(1, 5),
      labels = c("Unacceptable",
                 "Disuaded",
                 "Is a consideration",
                 "Acceptable",
                 "Highly acceptable")
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, .2, .4, .6, .8, 1),
      limits = c(0, 1),
      labels = scales::label_percent(),
    ) +
    
    ggplot2::labs(
      title = "Weighted combination of metrics",
      y = NULL,
      fill = NULL,
      x = NULL
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
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
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  
  
}






fxn_create_valuebox_plot <- function(value, subtitle) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.6, label = value, 
             size = 20, fontface = "bold", color = "#ffd74a") +
    annotate("text", x = 0.5, y = 0.4, label = subtitle, 
             size = 5, color = "#333333") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
}

fxn_create_valuebox_plot2 <- function(value, subtitle) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.6, label = value, 
             size = 20, fontface = "bold", color = "#3faf4a") +
    annotate("text", x = 0.5, y = 0.4, label = subtitle, 
             size = 5, color = "#333333") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
}
