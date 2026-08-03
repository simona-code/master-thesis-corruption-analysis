###############################################################################
# Project: Master's Thesis - Business Attitudes Toward Corruption
# Script: 01_Data Import & Data Cleaning
# Author: Simona Grigore
###############################################################################

# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(here)

# 1. Data Import --------------------------------------------------------------
raw_data <- haven::read_sav(here::here("ZA7984_v1-0-0.sav"))
data_df <- as.data.frame(raw_data)

# 2. Variable Selection & Translation -----------------------------------------
cleaned_data <- data_df %>%
  dplyr::select(
    country = isocntry,
    sector = nace_a,
    company_size = d2,
    company_age = d3r,
    turnover_trend = d4,
    turnover_value = d5,
    q7_1, q7_2, q7_3, q7_4, q7_5, q7_6, q7_7, q7_8
  ) %>%
  # Convert haven numerical variables to labeled factors
  dplyr::mutate(across(everything(), haven::as_factor))

# 3. Data Cleaning ------------------------------------------------------------
invalid_responses <- c("Don't know/No Answer", "DK/NA", "Not applicable")

cleaned_data <- cleaned_data %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  # Recode invalid survey responses to structural NA
  dplyr::mutate(across(everything(), ~ dplyr::if_else(. %in% invalid_responses, NA_character_, .))) %>%
  # Listwise deletion (remove companies with at least one NA)
  tidyr::drop_na() %>%
  # Convert back to factors and drop unused levels
  dplyr::mutate(across(everything(), as.factor)) 

  cleaned_data <- base::droplevels(cleaned_data)

# Save cleaned data for subsequent steps
saveRDS(cleaned_data, here::here("data", "cleaned_business_data.rds"))
cat("Data cleaning complete. Output saved to data/cleaned_business_data.rds\n")