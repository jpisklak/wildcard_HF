# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/choice-trials
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")
#-------------------------------------------------------------------------------

# Note: Pre-registration called for 3 separate one-way ANOVAs (not a 2x3 mixed)

# One-Way ANOVA on Low Value selection
#-------------------------------------------------------------------------------
# Subset Low Value
lv <- risky_res %>%
  filter(block == 7 & risky_choice == "Low")

# Set Contrasts
E1_v_NE <- c(1, 0, 0)
E2_v_NE <- c(0, 1, 0)
contrasts(lv$condition) <- cbind(E1_v_NE, E2_v_NE)

# Model
lv_mod <- gls(cp ~ condition, data = lv, method = "ML")

# Main Effect
df_resid <- summary(lv_mod)$dims$N - summary(lv_mod)$dims$p
anova_lv <- anova(lv_mod)
anova_lv <- add_row(anova_lv,
  numDF = df_resid,
  `F-value` = NULL,
  `p-value` = NULL
)
row.names(anova_lv)[3] <- "Residuals"

# Nagelkerke (Cragg and Uhler) Pseudo R-squared
anova_lv$pseudo_R2 <- c(
  NA,
  nagelkerke(lv_mod)$Pseudo.R.squared.for.model.vs.null[3],
  NA
)

# Planned Contrasts Results
pc_lv <- as.data.frame(summary(lv_mod)$tTable)
pc_lv$sig <- ifelse(pc_lv$`p-value` < .05, TRUE, FALSE)
pc_lv$DF <- df_resid
pc_lv$r_effect <- sqrt((pc_lv$`t-value`^2) / (pc_lv$`t-value`^2 + pc_lv$DF))

# Post-hoc comparisons for heteroscedastic means
# Assumes Normality
ph_lv <- lincon(cp ~ condition,
  data = lv,
  tr = 0,
  method = "hochberg"
)$comp[, 3:6]
rownames(ph_lv) <- c(
  paste(levels(lv$condition)[1], levels(lv$condition)[2], sep = "-"),
  paste(levels(lv$condition)[1], levels(lv$condition)[3], sep = "-"),
  paste(levels(lv$condition)[2], levels(lv$condition)[3], sep = "-")
)
