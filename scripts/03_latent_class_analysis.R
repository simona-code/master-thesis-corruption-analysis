###############################################################################
# Script: 03_Latent Class Analysis (LCA)
# Author: Simona Grigore
###############################################################################

library(dplyr)
library(poLCA)
library(here)

data_firme <- readRDS(here::here("data", "cleaned_business_data.rds"))
datelca <- data_firme 

# 1. Optimized Data Prep for LCA (Replaced repetitive chunks with a clean loop)
q_variables <- paste0("q7_", 1:8)

for (q in q_variables) {
  datelca[[q]] <- factor(
    datelca[[q]],
    levels = c("Tend to agree", "Tend to disagree", "Totally agree", "Totally disagree"),
    labels = c("Agree", "Disagree", "Agree", "Disagree")
  )
}

# Representativeness Check Before and After NA Omission
datelca_before <- datelca
datelca <- na.omit(datelca)

# Save distribution matrices to assess consistency
write.csv(round(prop.table(table(datelca_before$country)), 3), here::here("results", "country_dist_before_na.csv"))
write.csv(round(prop.table(table(datelca$country)), 3), here::here("results", "country_dist_after_na.csv"))

# 2. Run Latent Class Models -------------------------------------------------
set.seed(50)
formula_lca <- as.formula(cbind(q7_1, q7_2, q7_3, q7_4, q7_5, q7_6, q7_7, q7_8) ~ 1)

model_lca_2 <- poLCA(formula_lca, data = datelca, nclass = 2, maxiter = 5000, nrep = 10, verbose = FALSE)
model_lca_3 <- poLCA(formula_lca, data = datelca, nclass = 3, maxiter = 5000, nrep = 10, verbose = FALSE)
model_lca_4 <- poLCA(formula_lca, data = datelca, nclass = 4, maxiter = 5000, nrep = 10, verbose = FALSE)

# Export Model Fit Statistics
fit_stats <- data.frame(
  Model = c("2 Classes", "3 Classes", "4 Classes"),
  AIC = c(model_lca_2$aic, model_lca_3$aic, model_lca_4$aic),
  BIC = c(model_lca_2$bic, model_lca_3$bic, model_lca_4$bic)
)
write.csv(fit_stats, here::here("results", "lca_fit_statistics.csv"), row.names = FALSE)

# Assign Predicted Latent Classes back to Data
datelca$class <- model_lca_3$predclass

# Save LCA workspace for visualization and regression tasks
saveRDS(datelca, here::here("data", "lca_assigned_data.rds"))
saveRDS(model_lca_3, here::here("data", "selected_lca_model.rds"))