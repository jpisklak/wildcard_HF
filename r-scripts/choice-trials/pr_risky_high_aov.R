# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/choice-trials
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")
#-------------------------------------------------------------------------------

# One-Way ANOVA on High Value selection
#-------------------------------------------------------------------------------
# Subset High Value
hv <- risky_res %>%
  filter(block == 7 & risky_choice == "High")

# Set Contrasts
NE_v_E1 <- c(1, 0, 0)
E2_v_E1 <- c(0, 0, 1)
contrasts(hv$condition) <- cbind(NE_v_E1, E2_v_E1)

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

# Test of equality between E2 and NE
NE_v_E2 <- hv %>% 
  filter(condition %in% c("Extreme Last", "No Extreme")) %>% 
  droplevels()

welch_NE_v_E2 <- t.test(cp ~ condition, data = NE_v_E2, var.equal = FALSE)
d_NE_v_E2 <- cohen.d(cp ~ condition, data = NE_v_E2)
