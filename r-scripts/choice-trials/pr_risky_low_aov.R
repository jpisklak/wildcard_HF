# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/choice-trials
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")
#-------------------------------------------------------------------------------

# One-Way ANOVA on Low Value selection
#-------------------------------------------------------------------------------
# Subset Low Value
lv <- risky_res %>%
  filter(block == 7 & risky_choice == "Low")

# Set Contrasts
NE_v_E1 <- c(1, 0, 0)
E2_v_E1 <- c(0, 0, 1)
contrasts(lv$condition) <- cbind(NE_v_E1, E2_v_E1)

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

# Adjust p-value for one-sided test
pc_lv$`p-value` <- pc_lv$`p-value` / 2

pc_lv$sig <- ifelse(pc_lv$`p-value` < .05, TRUE, FALSE)
pc_lv$DF <- df_resid
pc_lv$r_effect <- sqrt((pc_lv$`t-value`^2) / (pc_lv$`t-value`^2 + pc_lv$DF))

# Test of equality between E2 and NE
E2_v_NE <- filter(lv, condition %in% c("Extreme Last", "No Extreme")) %>% 
  droplevels()
welch_E2_v_NE <- t.test(cp ~ condition, data = E2_v_NE, var.equal = FALSE)
d_E2_v_NE <- cohen.d(cp ~ condition, data = E2_v_NE)