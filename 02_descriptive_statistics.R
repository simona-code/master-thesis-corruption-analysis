###############################################################################
# Script: 02_Descriptive Statistics & Baseline Visualization
# Author: Simona Grigore
###############################################################################

library(dplyr)
library(ggplot2)
library(scales)
library(here)

# Load cleaned data
data_firme <- readRDS(here::here("data", "cleaned_business_data.rds"))

# 1. Refactor and Reorder Factor Levels for Presentation ----------------------
data_firme$company_size <- factor(
  data_firme$company_size, 
  levels = c("1 to 9 employees", "10 to 49 employees", "50 to 249 employees", "250 or more employees")
)

data_firme$company_age <- factor(
  data_firme$company_age, 
  levels = c("Less than 1 year", "1-5", "6-10", "11 years or more")
)

# Save re-ordered factor dataset
saveRDS(data_firme, here::here("data", "cleaned_business_data.rds"))

# 2. Generate and Save Descriptive Visualizations -----------------------------

# Plot 1: Company Size Structure by Country
p1 <- ggplot(data_firme, aes(x = country, fill = company_size)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    title = "Company Size Structure by Country",
    x = "Country", y = "Proportion (100%)", fill = "Company Size"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")

ggplot2::ggsave(here::here("figures", "companies_by_size_country.png"), p1, width = 10, height = 7)

# Plot 2: Company Age Structure by Country
p2 <- ggplot(data_firme, aes(x = country, fill = company_age)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    title = "Company Age Structure by Country",
    x = "Country", y = "Proportion (100%)", fill = "Company Age"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

ggplot2::ggsave(here::here("figures", "companies_by_age_country.png"), p2, width = 10, height = 7)

# Plot 3: Turnover Trend by Country
p3 <- ggplot(data_firme, aes(x = country, fill = turnover_trend)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    title = "Turnover Development Trend by Country",
    x = "Country", y = "Proportion (100%)", fill = "Turnover Trend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")

ggplot2::ggsave(here::here("figures", "turnover_trend_country.png"), p3, width = 10, height = 7)

# Plot 4: Sector Distribution by Country
p4 <- ggplot(data_firme, aes(x = country, fill = sector)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    title = "Sector Distribution by Country",
    x = "Country", y = "Proportion (100%)", fill = "Industry Sector"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2))

ggplot2::ggsave(here::here("figures", "sector_distribution_country.png"), p4, width = 10, height = 7)