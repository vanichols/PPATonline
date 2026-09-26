# Process Adrian's raw beta parameters into more nuanced distributions across value bins

rm(list = ls())

library(tidyverse)
library(readxl)


# 1. parameter data prep --------------------------------------------------

d1 <-
  readxl::read_excel("data/raw/Betas params for Gina.xlsx", skip = 1) |>
  janitor::clean_names()

d2 <-
  d1 |>
  pivot_longer(l:vh)

#--reverse, since we are looking at value, not 'badness'
d3 <-
  d2 |>
  mutate(
    rating_numeric = case_when(
      cat_code == 1 ~ 1,
      cat_code == 2 ~ 2,
      cat_code == 3 ~ 3,
      cat_code == 4 ~ 4,
      cat_code == 5 ~ 5,
      TRUE ~ 9999
    )
  )


#--turn confidence ratings into numeric values
d4 <-
  d3 |>
  dplyr::mutate(
    confidence_numeric = dplyr::case_when(name == "vh" ~ 4, name == "h" ~ 3, name == "m" ~ 2, name == "l" ~ 1)
  )

#--simplify dataset
d5 <-
  d4 |>
  select(rating_numeric, confidence_numeric, param, value)

#--pivot wider
d6 <-
  d5 |>
  pivot_wider(names_from = param, values_from = value) |>
  janitor::clean_names()

data_betaparams <- d6

#--for ai
data_betaparams |>
  write_csv("data/raw/Adrians beta params for AI to make function.csv")



# 2. function -------------------------------------------------------------
# interpolate_beta_params.R
#
# Provides get_beta_params(rating_numeric, confidence_numeric), which bilinearly
# interpolates alpha and beta from the grid in
# Adrians_beta_params_for_AI_to_make_function.csv.
#
# The CSV defines a regular grid: rating_numeric = 1..5, confidence_numeric = 1..4.
# Values outside the grid range are clamped to the nearest edge (no extrapolation).
#
# rating_numeric and confidence_numeric can each be a single value OR a vector.
# Vectors are recycled/paired element-by-element (like most R math), so to get
# every combination of two sequences, build them with expand.grid() first (see
# bottom of this file) rather than passing the raw sequences directly.

build_beta_param_interpolator <- function(data) {
  params <- data
  
  rating_levels <- sort(unique(params$rating_numeric))
  confidence_levels <- sort(unique(params$confidence_numeric))
  nR <- length(rating_levels)
  nC <- length(confidence_levels)
  
  # Build lookup matrices: rows = rating_levels, cols = confidence_levels
  alpha_mat <- matrix(NA_real_, nrow = nR, ncol = nC)
  beta_mat  <- matrix(NA_real_, nrow = nR, ncol = nC)
  for (i in seq_len(nR)) {
    for (j in seq_len(nC)) {
      row <- params[params$rating_numeric == rating_levels[i] &
                      params$confidence_numeric == confidence_levels[j], ]
      if (nrow(row) != 1) {
        stop(
          sprintf(
            "No unique grid point for rating=%s, confidence=%s",
            rating_levels[i],
            confidence_levels[j]
          )
        )
      }
      alpha_mat[i, j] <- row$alpha
      beta_mat[i, j]  <- row$beta
    }
  }
  
  # Vectorized helper: for a vector of query values, return the bracketing
  # grid index pairs (lo_idx, hi_idx) and the interpolation weight in [0, 1].
  get_bracket_info <- function(values, levels) {
    n <- length(levels)
    clamped <- pmin(pmax(values, levels[1]), levels[n])
    # findInterval gives, for each value, the index i such that
    # levels[i] <= value (with levels[n] itself mapping to i = n)
    lo_idx <- findInterval(clamped, levels, rightmost.closed = TRUE)
    lo_idx <- pmin(pmax(lo_idx, 1), n - 1)
    hi_idx <- lo_idx + 1
    lo_val <- levels[lo_idx]
    hi_val <- levels[hi_idx]
    weight <- ifelse(hi_val == lo_val, 0, (clamped - lo_val) / (hi_val - lo_val))
    list(lo_idx = lo_idx,
         hi_idx = hi_idx,
         weight = weight)
  }
  
  function(rating_numeric, confidence_numeric) {
    n <- max(length(rating_numeric), length(confidence_numeric))
    rating_numeric <- rep_len(rating_numeric, n)
    confidence_numeric <- rep_len(confidence_numeric, n)
    
    r_info <- get_bracket_info(rating_numeric, rating_levels)
    c_info <- get_bracket_info(confidence_numeric, confidence_levels)
    
    interp_col <- function(mat) {
      v_lo_lo <- mat[cbind(r_info$lo_idx, c_info$lo_idx)]
      v_hi_lo <- mat[cbind(r_info$hi_idx, c_info$lo_idx)]
      v_lo_hi <- mat[cbind(r_info$lo_idx, c_info$hi_idx)]
      v_hi_hi <- mat[cbind(r_info$hi_idx, c_info$hi_idx)]
      
      v_at_c_lo <- v_lo_lo + (v_hi_lo - v_lo_lo) * r_info$weight
      v_at_c_hi <- v_lo_hi + (v_hi_hi - v_lo_hi) * r_info$weight
      
      v_at_c_lo + (v_at_c_hi - v_at_c_lo) * c_info$weight
    }
    
    data.frame(
      rating_numeric = rating_numeric,
      confidence_numeric = confidence_numeric,
      alpha = interp_col(alpha_mat),
      beta = interp_col(beta_mat)
    )
  }
}

# For each row's Beta(alpha, beta) distribution, compute the probability mass
# falling in each of n_bins equally spaced bins over [0, 1], and append one
# column per bin (named by the bin's [lo, hi) range) to the data frame.
# Fully vectorized: pbeta() is called once per bin edge across all rows at once.
add_beta_bin_probabilities <- function(results, n_bins = 5) {
  edges <- seq(0, 1, length.out = n_bins + 1)
  
  # cdf_mat: one column per edge, one row per record in `results`
  cdf_mat <- sapply(edges, function(e)
    pbeta(e, results$alpha, results$beta))
  
  bin_mass <- cdf_mat[, -1, drop = FALSE] - cdf_mat[, -(n_bins + 1), drop = FALSE]
  colnames(bin_mass) <- sprintf("valuebin_%d", seq_len(n_bins))
  
  cbind(results, as.data.frame(bin_mass))
}


# ---- Usage ----
get_beta_params <- build_beta_param_interpolator(data_betaparams)
#
# Single point:
#   get_beta_params(2.5, 3.2)
#
# All combinations of two sequences (fully vectorized, no explicit loop):
grid <- expand.grid(
  rating_numeric = seq(1, 5, by = 0.1),
  confidence_numeric = seq(1, 4, by = 0.1)
)
results <- get_beta_params(grid$rating_numeric, grid$confidence_numeric)
results <- add_beta_bin_probabilities(results, n_bins = 5)

d_res <-
  results |>
  as_tibble()


# 4. compare to adrians original value bins -------------------------------

# CONCLUSION, THEY ARE GREAT

d_orig <-
  read_rds("data/processed/data_betas.RDS") |> 
  dplyr::mutate(
    confidence_numeric = dplyr::case_when(confidence_text == "Very high" ~ 4, 
                                          confidence_text == "High" ~ 3, 
                                          confidence_text == "Medium" ~ 2, 
                                          confidence_text == "Low" ~ 1)
  )


d_res2 <- 
  d_res |> 
  pivot_longer(valuebin_1:valuebin_5) |> 
  mutate(value_bin = parse_number(name),
         score_new = value * 100) |> 
  select(-name, -value)

d_orig |> 
  left_join(d_res2)


# 5. write new interpolated dataframe -------------------------------------

data_betas_interpolated <- d_res2

data_betas_interpolated |> 
  write_rds("data/processed/data_betas_interpolated.RDS")
