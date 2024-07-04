# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/choice-trials
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")
#-------------------------------------------------------------------------------

# One-Way ANOVA on Differences (i.e., EO effect)
#-------------------------------------------------------------------------------

# diffs_b7

# Set Contrasts
E1_v_NE <- c(0, 1, 0)
E2_v_NE <- c(0, 0, 1)
contrasts(diffs_b7$condition) <- cbind(E1_v_NE, E2_v_NE)

# Model
diff_mod <- gls(diff ~ condition, data = diffs_b7, method = "ML")
summary(diff_mod)

# Main Effect
df_resid <- summary(diff_mod)$dims$N - summary(diff_mod)$dims$p
anova_diff <- anova(diff_mod)
anova_diff <- add_row(anova_diff,
  numDF = df_resid,
  `F-value` = NULL,
  `p-value` = NULL
)
row.names(anova_diff)[3] <- "Residuals"

# Nagelkerke (Cragg and Uhler) Pseudo R-squared
anova_diff$pseudo_R2 <- c(
  NA,
  nagelkerke(diff_mod)$Pseudo.R.squared.for.model.vs.null[3],
  NA
)

# Planned Contrasts Results
pc_diff <- as.data.frame(summary(diff_mod)$tTable)
pc_diff$sig <- ifelse(pc_diff$`p-value` < .05, TRUE, FALSE)
pc_diff$DF <- df_resid
pc_diff$r_effect <- sqrt((pc_diff$`t-value`^2) /
  (pc_diff$`t-value`^2 + pc_diff$DF))

# Post-hoc comparisons for heteroscedastic means
# Assumes Normality
ph_diff <- lincon(diff ~ condition,
  data = diffs_b7,
  tr = 0,
  method = "hochberg"
)$comp[, 3:6]
rownames(ph_diff) <- c(
  paste(levels(diffs_b7$condition)[1], levels(diffs_b7$condition)[2], sep = "-"),
  paste(levels(diffs_b7$condition)[1], levels(diffs_b7$condition)[3], sep = "-"),
  paste(levels(diffs_b7$condition)[2], levels(diffs_b7$condition)[3], sep = "-")
)
