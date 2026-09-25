#--use the beta parameters to make an overall distribution

#--for trouble shooting
#data_example <- read_rds("data/processed/data_example.RDS")

fxn_Calc_Overall_Utility_New <- function(data = data_example, betas = data_adrianbetas) {
  
  #--just do a weighted combo of alphas and betas?
  d.util <-
    data_example |>
    dplyr::mutate(confidence_numeric = dplyr::case_when(
      confidence_text == "Very high" ~ 4,
      confidence_text == "High" ~ 3,
      confidence_text == "Medium" ~ 2,
      confidence_text == "Low" ~ 1
    )) |> 
    group_by(title) |> 
    summarise(utility = weighted.mean(rating_numeric, weight), 
              conf_numeric = weighted.mean(confidence_numeric, weight),
              conf_round = round(conf_numeric, 0)) |> 
    mutate(
      conf = case_when(
        conf_round == 1 ~ "Low",
        conf_round == 2 ~ "Medium",
        conf_round == 3 ~ "High",
        conf_round == 4 ~ "Very high"
      )
    ) 
  
  d.betaparams <-
    data_example |>
    left_join(data_adrianbetas) |> 
    group_by(title) |>
    summarise(w.alpha = weighted.mean(alpha, weight),
              w.beta = weighted.mean(beta, weight))
  
  d.res <-
    d.betaparams |> 
    left_join(d.util) |> 
    mutate(metric = "Weighted combo") |> 
    select(title, metric, everything())
  
  return(d.res)
  
} 

#---still working on this interpolation method

fxn_Calc_Overall_Utility_New_Interp <- function(data = data_example, betas = data_adrianbetas) {
  
  #--get alpha and beta values using interpolation
  #--get a weighted rating, a weighted numeric confidence
  #--use a bilinear interpolation to get alpha and beta values
  
  d.util <-
    data_example |>
    dplyr::mutate(confidence_numeric = dplyr::case_when(
      confidence_text == "Very high" ~ 4,
      confidence_text == "High" ~ 3,
      confidence_text == "Medium" ~ 2,
      confidence_text == "Low" ~ 1
    )) |> 
    group_by(title) |> 
    summarise(utility = weighted.mean(rating_numeric, weight), 
              conf_numeric = weighted.mean(confidence_numeric, weight),
              conf_round = round(conf_numeric, 0)) |> 
    mutate(
      conf = case_when(
        conf_round == 1 ~ "Low",
        conf_round == 2 ~ "Medium",
        conf_round == 3 ~ "High",
        conf_round == 4 ~ "Very high"
      )
    ) 
  
  estimate_alpha_beta <- function(rating, confidence, lookup_df) {
    
    interp_one <- function(zcol) {
      
      x1 <- floor(rating)
      x2 <- ceiling(rating)
      y1 <- floor(confidence)
      y2 <- ceiling(confidence)
      
      if (x1 == x2) x2 <- min(x1 + 1, max(lookup_df$rating_numeric))
      if (y1 == y2) y2 <- min(y1 + 1, max(lookup_df$confidence_numeric))
      
      z11 <- lookup_df[lookup_df$rating_numeric == x1 &
                         lookup_df$confidence_numeric == y1, zcol]
      
      z21 <- lookup_df[lookup_df$rating_numeric == x2 &
                         lookup_df$confidence_numeric == y1, zcol]
      
      z12 <- lookup_df[lookup_df$rating_numeric == x1 &
                         lookup_df$confidence_numeric == y2, zcol]
      
      z22 <- lookup_df[lookup_df$rating_numeric == x2 &
                         lookup_df$confidence_numeric == y2, zcol]
      
      tx <- (rating - x1) / (x2 - x1)
      ty <- (confidence - y1) / (y2 - y1)
      
      (1 - tx) * (1 - ty) * z11 +
        tx * (1 - ty) * z21 +
        (1 - tx) * ty * z12 +
        tx * ty * z22
    }
    
    data.frame(
      alpha = interp_one("alpha"),
      beta = interp_one("beta")
    )
  }
  
  
  
  d.betaparams <-
    data_example |>
    left_join(data_adrianbetas) |> 
    group_by(title) |>
    summarise(w.alpha = weighted.mean(alpha, weight),
              w.beta = weighted.mean(beta, weight))
  
  d.res <-
    d.betaparams |> 
    left_join(d.util) |> 
    mutate(metric = "Weighted combo") |> 
    select(title, metric, everything())
  
  return(d.res)
  
} 


fxn_Make_Overall_Utility_Fig_New <- function(data_utility = data_example_utility_new) {
  
  
  # Function to create beta distribution dataframe given an alpha and a beta
  minifxn_GetBetaDF <- function(alpha, beta) {
    # Validate parameters
    if (!is.numeric(alpha) || !is.numeric(beta) || alpha <= 0 || beta <= 0) {
      stop("Both alpha and beta must be positive numeric values.")
    }
    
    # Create a sequence of x values between 0 and 1
    x <- seq(0, 1, length.out = 500)
    
    # Calculate the Beta probability density function
    y <- dbeta(x, shape1 = alpha, shape2 = beta)
    
    # Create a data frame for plotting
    df <- 
      data.frame(x = x, y = y) |> 
      as_tibble() |> 
      mutate(
        y_max = max(y),
        y_norm = y / y_max)
    
    return(df)
  }
  
  data_utility1 <- data_utility[1,]
  
  plot_data1 <- 
    minifxn_GetBetaDF(data_utility1$w.alpha, data_utility1$w.beta) |> 
    mutate(title = data_utility1$title)
  
  data_utility2 <- data_utility[2,]
  plot_data2 <- 
    minifxn_GetBetaDF(data_utility2$w.alpha, data_utility2$w.beta) |> 
    mutate(title = data_utility2$title)
  
  plot_data <- 
    plot_data1 |> 
    bind_rows(plot_data2)
  
  clr1 <- "#ffd74a"
  clr2 <- "#3faf4a"
  
  
  plot_data |> 
    mutate(title = fct_inorder(title)) |> 
    ggplot(group = title) +
    geom_area(
      position = "identity",
      aes(x = x,
          y = y_norm,
          fill = title),
      #bw = 0.5,
      alpha = 0.8,
      #show.legend = F
    ) +
    geom_line(linewidth = 1, aes(color = title)) +
    scale_fill_manual(
      values = c(clr1, clr2),
      #values = c("#fdbe85", "#08519c"),
      guide = guide_legend(ncol = 1)
    ) +
    scale_x_continuous(
      breaks = c(0, .25, .5, .75, 1),
      limits = c(0, 1),
      labels = c("Unacceptable",
                 "Disuaded",
                 "Is a consideration",
                 "Acceptable",
                 "Highly acceptable")
    ) +
    scale_y_continuous(
      breaks = c(0, .2, .4, .6, .8, 1),
      limits = c(0, 1),
      labels = scales::label_percent(),
    ) +
    
    labs(
      title = "Weighted combination of metrics",
      y = NULL,
      fill = NULL,
      x = NULL
    ) +
    # Theme
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
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
}

