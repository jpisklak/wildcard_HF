# setwd('../..')
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")

# Note: Pre-registration called for 3 separate one-way ANOVAs (not a 2x3 mixed)

# One-Way ANOVA on High Value selection
#-------------------------------------------------------------------------------
# Subset High Value
hv <- risky_res %>%
  filter(block == 7 & risky_choice == "High")

# Set Contrasts
E1_v_NE <- c(1, 0, 0)
E2_v_NE <- c(0, 1, 0)
contrasts(hv$condition) <- cbind(E1_v_NE, E2_v_NE)

# Model
hv_mod <- gls(cp ~ condition, data = hv, method = "ML")

# Main Effect
df_resid <- summary(hv_mod)$dims$N - summary(hv_mod)$dims$p
anova_hv <- anova(hv_mod)
anova_hv <- add_row(anova_hv,
  numDF = df_resid,
  `F-value` = NULL,
  `p-value` = NULL
)
row.names(anova_hv)[3] <- "Residuals"

# Nagelkerke (Cragg and Uhler) Pseudo R-squared
anova_hv$pseudo_R2 <- c(
  NA,
  nagelkerke(hv_mod)$Pseudo.R.squared.for.model.vs.null[3],
  NA
)

# Planned Contrasts Results
pc_hv <- as.data.frame(summary(hv_mod)$tTable)
pc_hv$sig <- ifelse(pc_hv$`p-value` < .05, TRUE, FALSE)
pc_hv$DF <- df_resid
pc_hv$r_effect <- sqrt((pc_hv$`t-value`^2) / (pc_hv$`t-value`^2 + pc_hv$DF))

# Post-hoc comparisons for heteroscedastic means
# Assumes Normality
ph_hv <- lincon(cp ~ condition,
  data = hv,
  tr = 0,
  method = "hochberg"
)$comp[, 3:6]
rownames(ph_hv) <- c(
  paste(levels(hv$condition)[1], levels(hv$condition)[2], sep = "-"),
  paste(levels(hv$condition)[1], levels(hv$condition)[3], sep = "-"),
  paste(levels(hv$condition)[2], levels(hv$condition)[3], sep = "-")
)

