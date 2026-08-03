###############################################################################
# Script: 04_Correspondence Analysis (CA) & Non-Parametric Tests
# Author: Simona Grigore
###############################################################################

library(dplyr)
library(forcats)
library(ca)
library(FactoMineR)
library(factoextra)
library(rcompanion)
library(here)

datelca <- readRDS(here::here("data", "lca_assigned_data.rds"))

# Translate Sector Labels to Romanian (or English if preferred) for Visual Plots
datelca <- datelca %>%
  dplyr::mutate(sector = fct_recode(sector,
                                    "Servicii financiare, bancare și investiții" = "Financial services, banking and investment",
                                    "Sănătate și farma" = "Healthcare and pharmaceutical",
                                    "Inginerie, electronică și auto" = "Engineering and electronics,motor vehicles",
                                    "Energie, minerit, petrol, gaze și chimice" = "Energy, mining, oil and gas, chemicals",
                                    "Construcții și imobiliare" = "Construction and building",
                                    "Telecomunicații și tehnologia informației" = "Telecommunications and Information technologies"
  ))

# Helper Function to automate Table Generation, Chi-Square, Cramer's V, and Biplots
run_ca_pipeline <- function(data, variable_name, output_prefix) {
  contingency_table <- table(data$class, data[[variable_name]])
  
  # Tests
  chi_test <- chisq.test(contingency_table)
  cramer_val <- rcompanion::cramerV(contingency_table)
  
  # Save Test Results
  test_results <- data.frame(
    Variable = variable_name,
    Chi_Square_Stat = chi_test$statistic,
    p_value = chi_test$p.value,
    Cramers_V = cramer_val
  )
  write.csv(test_results, here::here("results", paste0(output_prefix, "_statistical_tests.csv")), row.names = FALSE)
  
  # Fit Model & Save Plots
  ca_model <- ca::ca(contingency_table)
  biplot <- factoextra::fviz_ca_biplot(ca_model, repel = TRUE, title = paste("Correspondence Analysis: Latent Class vs", variable_name))
  ggplot2::ggsave(here::here("figures", paste0(output_prefix, "_ca_biplot.png")), biplot, width = 8, height = 6)
}

# Run Correspondence Analysis for Demographics
run_ca_pipeline(datelca, "country", "country")
run_ca_pipeline(datelca, "company_size", "size")
run_ca_pipeline(datelca, "company_age", "age")
run_ca_pipeline(datelca, "sector", "sector")

# Export updated dataset with clean labels
saveRDS(datelca, here::here("data", "lca_assigned_data.rds"))