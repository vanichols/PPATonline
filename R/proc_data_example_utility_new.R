# # Process raw data into processed data
# 
# rm(list = ls())
# 
# library(tidyverse)
# 
# source("R/utils.R")
# 
# data_example <- read_rds("data/processed/data_example.RDS")
# 
# data_example_utility_new <- fxn_Calc_Overall_Utility_New(data = data_example)
# 
# data_example_utility_new |> 
#   write_rds("data/processed/data_example_utility_new.RDS")
