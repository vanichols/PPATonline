#' Create a ridge plot of the six metrics for a given strategy
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


fxn_Make_Paired_Ridge_Plots <- function(data = data_example,
                                        betas = data_betas) {
  
  metric_colors <- c(
    "Crop value" =  "#c2e699",
    "Costs" = "#ffffcc",
    "Time/management\ncomplexity" = "#fa9fb5",
    "Immediate usability" = "#fdbe85",
    "Environmental impact" = "#c51b8a",
    "User health and safety" = "#6baed6"
  )
  
  metric_names <-
    data |>
    dplyr::pull(metric) |>
    unique()
  
  metric_names_nice <-
    c(
      "Crop value",
      "Costs",
      "Time/management\ncomplexity",
      "Immediate usability",
      "Environmental impact",
      "User health and safety"
    )
  
  #--get names of approaches
  strategy_name <-
    data |>
    dplyr::pull(title) |>
    unique()
  
  
  plot_data1 <-
    data |>
    dplyr::filter(title == strategy_name[1]) |>
    #--make metric into a factor
    dplyr::mutate(
      metric2 = dplyr::case_when(
        metric == metric_names[1] ~ metric_names_nice[1],
        metric == metric_names[2] ~ metric_names_nice[2],
        metric == metric_names[3] ~ metric_names_nice[3],
        metric == metric_names[4] ~ metric_names_nice[4],
        metric == metric_names[5] ~ metric_names_nice[5],
        metric == metric_names[6] ~ metric_names_nice[6]),
      metricF = factor(metric2, levels = (metric_names_nice))) |>
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence_text"),
                     relationship = "many-to-many") |>
    #--make some things for the figure
    dplyr::arrange(metricF) |>
    dplyr::mutate(
      #metric_label = paste0(metricF, " (", round(weight, 2), "%)"),
      metric_label = paste0(metricF),
      metricF = as.factor(metric_label),
      metricF = forcats::fct_inorder(metricF)) |>
    dplyr::mutate(score = as.integer(score)) |>
    dplyr::select(title, metricF, 
                  #weight, 
                  value_bin, score)
  
  plot_data2 <-
    data |>
    dplyr::filter(title == strategy_name[2]) |>
    #--make metric into a factor
    dplyr::mutate(
      metric2 = dplyr::case_when(
        metric == metric_names[1] ~ metric_names_nice[1],
        metric == metric_names[2] ~ metric_names_nice[2],
        metric == metric_names[3] ~ metric_names_nice[3],
        metric == metric_names[4] ~ metric_names_nice[4],
        metric == metric_names[5] ~ metric_names_nice[5],
        metric == metric_names[6] ~ metric_names_nice[6]),
      metricF = factor(metric2, levels = (metric_names_nice))) |>
    #--join with confidence bins - FIXED: added 'by' argument
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence_text"),
                     relationship = "many-to-many") |>
    #--make some things for the figure
    dplyr::arrange(metricF) |>
    dplyr::mutate(
      # metric_label = paste0(metricF, " (", round(weight, 2), "%)"),
      metric_label = paste0(metricF),
      metricF = as.factor(metric_label),
      metricF = forcats::fct_inorder(metricF)) |>
    dplyr::mutate(score = as.integer(score)) |>
    dplyr::select(title, metricF, 
                  #weight, 
                  value_bin, score)
  
  
  #--ridge plots
  
  plot1 <-
    plot_data1 |>
    tidyr::uncount(score) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = value_bin,
                   fill = metricF),
      bw = 0.5,
      show.legend = F
    ) +
    ggplot2::geom_col(data = plot_data1,
                      ggplot2::aes(value_bin, score/100),
                      alpha = 0.5) +
    ggplot2::facet_wrap(~metricF, labeller = label_wrap_gen(width = 20)) +
    ggplot2::scale_fill_manual(values = unname(metric_colors)) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
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
  
  plot2 <-
    plot_data2 |>
    tidyr::uncount(score) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = value_bin,
                   fill = metricF),
      bw = 0.5,
      show.legend = F
    ) +
    ggplot2::geom_col(data = plot_data2,
                      ggplot2::aes(value_bin, score/100),
                      alpha = 0.5) +
    ggplot2::facet_wrap(~metricF, labeller = ggplot2::label_wrap_gen(width = 20)) +
    ggplot2::scale_fill_manual(values = unname(metric_colors)) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
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
  
  patchwork::wrap_plots(plot1, plot2, ncol = 2)
  
}


#' Create overlapping ridge plots of the six metrics, comparing each strategy
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A plot
#' @import ggplot2
#' @import stringr
#' @import ggridges
#' @import dplyr
#' @import tidyr
#' @import patchwork
#' @export


fxn_Make_Overlapping_Ridge_Plots <- function(data = data_example, betas = data_betas) {
  
  metric_colors6 <- c(
    "Crop value" =  "#c2e699",
    "Costs" = "#fd8d3c",
    "Time/management complexity" = "#f768a1",
    "Immediate usability" = "#fdbe85",
    "Environmental impact" = "#7a0177",
    "User health and safety" = "#6baed6"
  )
  
  metric_names6 <-
    c(
      "Crop value",
      "Costs",
      "Time/management\ncomplexity",
      "Immediate usability",
      "Environmental impact",
      "User health and safety"
    )
  
  
  #--get names of approaches
  strategy_names <-
    data |>
    dplyr::select(title) |>
    dplyr::distinct()
  
  
  plot_data <-
    data |>
    dplyr::mutate(
      metric = c(metric_names6, metric_names6),
      metricF = factor(metric, levels = rev(metric_names6))
    ) |>
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence"),
                     relationship = "many-to-many") |>
    dplyr::select(title, metricF, value_bin, score)
  
  #--ridge plot
  
  p1 <- 
    plot_data |>
    dplyr::mutate(score = as.integer(score)) |>
    tidyr::uncount(score) |>
    ggplot2::ggplot(ggplot2::aes(x = value_bin, y = metricF)) +
    ggridges::geom_density_ridges2(
      ggplot2::aes(fill = title),
      alpha = 0.6,
      bandwidth = 0.5,
      scale = 0.9 #--height of distributions
    ) +
    ggplot2::scale_fill_manual(
      values = c("gray10", "#ffffcc"),
      #values = c("#fdbe85", "#08519c"),
      guide = guide_legend(ncol = 2)
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      labels = c(
        "Unacceptable",
        "Disuaded",
        "Is a consideration",
        "Acceptable",
        "Highly acceptable"
      )
    ) +
    ggplot2::labs(
      title = "Performance",
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
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  (plot_spacer()|p1|plot_spacer()) +
    plot_layout(widths = c(1, 0.5, 1))
}


#' Calcuate the overall utility of the strategies with user-defined weighting
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A dataframe
#' @import stringr
#' @export


calc_overall_utility <- function(data = data_example,
                                 betas = data_betas,
                                 nsim = 10000) {
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
    dplyr::rename(rating_numeric = rating_1to5) |>
    #--join with confidence bins
    dplyr::left_join(betas,
                     by = c("rating_numeric", "confidence"),
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
        (mysd < 5) ~ "vh",
        ((mysd >= 5) & (mysd < 10)) ~ "h",
        ((mysd >= 10) & (mysd < 15)) ~ "m",
        (mysd >= 15) ~ "l",
        TRUE ~ "XXX"
      ))
  )
  
  return(final_data)
  
  
}


