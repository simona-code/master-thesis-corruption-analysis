###############################################################################
# Script: 06_Attitudinal Profiling & Final Visualizations (Lollipop)
# Author: Simona Grigore
###############################################################################

library(dplyr)
library(ggplot2)
library(here)

# Load Models and Regional Data
model_lca_3 <- readRDS(here::here("data", "selected_lca_model.rds"))

intrebari <- paste0("q7_", 1:8)

# Compute condition probabilities dynamically for Class 3
prob_agree_class3 <- sapply(intrebari, function(q) {
  return(model_lca_3$probs[[q]][3, "Agree"])
})

# Construct Profile DataFrame
profile_class_3 <- data.frame(
  Question = intrebari,
  Agree_Percentage = round(prob_agree_class3 * 100, 2),
  Description = c(
    "Q7.1: Close links between politics & business lead to corruption",
    "Q7.2: Favoritism/bribery is the easiest way to access public services",
    "Q7.3: Political party financing is transparent and closely monitored",
    "Q7.4: The only way to succeed in business is through political connections",
    "Q7.5: Favoritism and corruption undermine business competition",
    "Q7.6: Anti-corruption measures are applied impartially without hidden motives",
    "Q7.7: Petty corruption is appropriately sanctioned",
    "Q7.8: High-level corruption/bribery is appropriately sanctioned"
  )
)

write.csv(profile_class_3, here::here("results", "latent_class_3_profile.csv"), row.names = FALSE)

# Generate Elegant Portfolio Lollipop Plot ------------------------------------
lollipop_plot <- ggplot(profile_class_3, aes(x = reorder(Description, Agree_Percentage), y = Agree_Percentage)) +
  geom_segment(aes(xend = Description, yend = 0), color = "#bdc3c7", lwd = 1.2) +
  geom_point(color = "#2c3e50", size = 4.5) +
  coord_flip() +  
  theme_minimal() +
  labs(
    title = "Attitudinal Profile of Latent Class 3",
    subtitle = "Conditional agreement probabilities (%) regarding corruption items (Flash Eurobarometer 524)",
    x = "Perceptions of Corruption & Business Environment (Q7 Items)",
    y = "Probability of Agreement (%)"
  ) +
  geom_text(aes(label = paste0(Agree_Percentage, "%")), hjust = -0.3, size = 3.5, fontface = "bold") +
  ylim(0, 115)

# Save Plot
ggplot2::ggsave(here::here("figures", "lollipop_profile_class3.png"), lollipop_plot, width = 11, height = 7)
cat("Lollipop plot successfully exported to figures/\n")
