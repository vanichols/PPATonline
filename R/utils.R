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


fxn_Make_Plots <- function(data = data_example,
                                        betas = data_betas) {
  
  metric_names <- data_example$metric[1:6]
  
  clr1 <- "#ffd74a"
  clr2 <- "#3faf4a"
  
  # metric_colors <- c(
  #   "#c2e699",
  #   "#fd8d3c",
  #   "#f768a1",
  #   "#fdbe85",
  #   "#7a0177",
  #   "#6baed6"
  # )
  # 
  # names(metric_colors) <- metric_names
  
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

#' Calcuate the overall utility of the strategies with user-defined weighting
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A dataframe
#' @import stringr
#' @export


fxn_Calc_Overall_Utility <- function(data = data_example,
                                 betas = data_betas,
                                 nsim = 10000) {
  
  #--make sure data has things that are numeric
  data <- 
    data |> 
    mutate(rating_numeric = as.numeric(rating_numeric),
           weight = as.numeric(weight))
  
  #-- vector of strategies
  v.strat <-
    data |>
    dplyr::pull(title) |>
    unique()
  
  #-- vector of metrics
  v.met <-
    data |>
    dplyr::pull(metric) |>
    unique()
  
  #--data with confidence intervals
  data_conf <-
    data |>
    #--join with confidence bins
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence_text"),
                     relationship =
                       "many-to-many") |>
    dplyr::select(title, metric, weight, score, value_bin)
  
  
  value_bin_options <-
    data_conf %>%
    dplyr::select(value_bin) %>%
    dplyr::distinct()
  
  #--first scenario-------------------------
  data_1 <-
    data_conf %>%
    dplyr::filter(title == v.strat[1])
  
  bayes.value.vector1 <- NULL
  
  for (k in 1:length(v.met)) {
    tmp.impact <- v.met[k]
    
    tmp.df <-
      data_1 %>%
      dplyr::select(metric, value_bin, score) %>%
      dplyr::filter(metric == tmp.impact)
    
    tmp.wt <-
      data_1 %>%
      dplyr::select(weight, metric) %>%
      dplyr::filter(metric == tmp.impact) %>%
      dplyr::distinct() %>%
      dplyr::pull(weight)
    
    tmp.samp <-
      sample(
        x = tmp.df$value_bin,
        prob = tmp.df$score,
        size = nsim * tmp.wt,
        replace = TRUE
      )
    
    bayes.value.vector1 <- c(bayes.value.vector1, tmp.samp)
    
    k <- k + 1
    
  }
  
  datares_1 <-
    value_bin_options %>%
    dplyr::left_join(
      tibble::tibble(value_bin = bayes.value.vector1) %>%
        dplyr::group_by(value_bin) %>%
        dplyr::summarise(score = dplyr::n() / nsim),
      by = "value_bin") %>%
    dplyr::mutate(
      score = ifelse(is.na(score), 0, score),
      metric  = "Weighted combo",
      title = v.strat[1]
    )
  
  #--second scenario-------------------------
  data_2 <-
    data_conf %>%
    dplyr::filter(title == v.strat[2])
  
  bayes.value.vector2 <- NULL
  
  for (j in 1:length(v.met)) {
    tmp.impact <- v.met[j]
    
    tmp.df <-
      data_2 %>%
      dplyr::select(metric, value_bin, score) %>%
      dplyr::filter(metric == tmp.impact)
    
    tmp.wt <-
      data_2 %>%
      dplyr::select(weight, metric) %>%
      dplyr::filter(metric == tmp.impact) %>%
      dplyr::distinct() %>%
      dplyr::pull(weight)
    
    tmp.samp <-
      sample(
        x = tmp.df$value_bin,
        prob = tmp.df$score,
        size = nsim * tmp.wt,
        replace = TRUE
      )
    
    bayes.value.vector2 <- c(bayes.value.vector2, tmp.samp)
    
    j <- j + 1
    
  }
  
  datares_2 <-
    value_bin_options %>%
    dplyr::left_join(
      tibble::tibble(value_bin = bayes.value.vector2) %>%
        dplyr::group_by(value_bin) %>%
        dplyr::summarise(score = dplyr::n() / nsim),
      by = c("value_bin")
    ) %>%
    dplyr::mutate(
      score = ifelse(is.na(score), 0, score),
      metric  = "Weighted combo",
      title = v.strat[2]
    )
  
  
  datares <-
    dplyr::bind_rows(datares_1, datares_2) |>
    dplyr::select(title, metric, value_bin, score)
  
  
  #--calculate utility
  suppressMessages(
    data_util <-
      datares |>
      dplyr::group_by(title, metric) |>
      dplyr::summarise(utility = weighted.mean(value_bin, w = score))
  )
  
  #--calculate standard deviation of distribution
  suppressMessages(
    data_sd <-
      datares |>
      dplyr::select(title, metric, score, value_bin) |>
      dplyr::left_join(data_util |>
                         dplyr::select(title, metric, utility)) |>
      dplyr::mutate(term = (value_bin - utility)^2 * score) |>
      dplyr::group_by(title, metric) |>
      dplyr::summarise(mysd = sum(term)^0.5)
  )
  
  #--what is the maximum sd possible? I think it is 20
  #--if they were split
  #sqrt((3 - 5)^2 * 50 + (3 - 1)^2 * 50)
  
  #--check if the values make sense, they do I guess
  # datares |>
  #   dplyr::left_join(data_util) |>
  #   dplyr::left_join(data_sd) |>
  #   ggplot(aes(value_bin, score)) +
  #   geom_col() +
  #   geom_label(aes(3, 100, label = utility)) +
  #   geom_label(aes(3, 80, label = round(mysd, 2))) +
  #   facet_grid(title ~ metric)
  
  suppressMessages(
    final_data <-
      data_util |>
      dplyr::left_join(data_sd) |>
      dplyr::mutate(conf = dplyr::case_when(
        (mysd < 5) ~ "Very high",
        ((mysd >= 5) & (mysd < 10)) ~ "High",
        ((mysd >= 10) & (mysd < 15)) ~ "Medium",
        (mysd >= 15) ~ "Low",
        TRUE ~ "XXX"
      ))
  )
  
  return(final_data)
  
  
}

#' Calcuate the overall utility of the strategies with user-defined weighting
#'
#' @param data_utility The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A figure
#' @export


fxn_Make_Overall_Utility_Fig <- function(data_utility = data_example_utility,
                                 betas = data_betas) {
  
  clr1 <- "#ffd74a"
  clr2 <- "#3faf4a"
  
  plot_data <- 
    data_utility |>
    mutate(rating_numeric = round(utility, 0)) |>
    rename(confidence_text = conf) |> 
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence_text"),
                     relationship = "many-to-many") |>
    #--make some things for the figure
    dplyr::mutate(score = as.integer(score))
  
  
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
