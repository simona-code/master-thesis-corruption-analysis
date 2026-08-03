###############################################################################
# Script: 05_Binary Logistic Regression on Target Cluster
# Author: Simona Grigore
###############################################################################

library(dplyr)
library(forcats)
library(here)

datelca <- readRDS(here::here("data", "lca_assigned_data.rds"))

# 1. Define Macro-Regions ----------------------------------------------------
eastern_med_cluster <- c("RO", "HU", "SK", "PL", "BG", "GR", "HR", "MT", "CY", "PT", "IT", "ES", "SI", "CZ", "LV")

datelca <- datelca %>%
  dplyr::mutate(macro_region = dplyr::if_else(country %in% eastern_med_cluster, "Cluster_Est_Mediteranean", "Cluster_Vest_Nord"))

# Save macro-region cross-table metrics
table_macro <- table(datelca$macro_region, datelca$class)
write.csv(round(prop.table(table_macro, 1) * 100, 2), here::here("results", "macro_region_class_distribution.csv"))

# 2. Filter and Prepare Targeted Sub-population -------------------------------
eastern_cluster_data <- datelca %>% 
  dplyr::filter(country %in% eastern_med_cluster) %>%
  dplyr::mutate(
    is_class_3 = dplyr::if_else(class == 3, 1, 0), # Binary outcome for Logistic Model
    sector_collapsed = fct_collapse(sector,
                                    "Industrie și Inginerie" = c("Energie, minerit, petrol, gaze și chimice", "Inginerie, electronică și auto")
    )
  )

# 3. Model Estimation --------------------------------------------------------
logistic_model <- glm(
  is_class_3 ~ sector_collapsed + company_size + turnover_trend, 
  data = eastern_cluster_data, 
  family = binomial
)

# Export Model Summary Tables
model_summary <- as.data.frame(summary(logistic_model)$coefficients)
model_summary$Odds_Ratio <- round(exp(coef(logistic_model)), 3)

write.csv(model_summary, here::here("results", "logistic_regression_class3_results.csv"))
cat("Logistic regression complete. Results saved in results/\n")