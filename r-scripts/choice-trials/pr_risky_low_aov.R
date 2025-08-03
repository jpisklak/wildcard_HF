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
lv_null_mod <- gls(cp ~ 1, data = lv, method = "ML")
lv_mod <- gls(cp ~ condition, data = lv, method = "ML")

# L-Ratio Table-----------------------------------------------------------------
anova_lv_lrat <- as.data.frame(anova(lv_null_mod, lv_mod))[, -c(1)]

# Inverse Bayes factor
delta_BIC <- anova_lv_lrat$BIC[2] - anova_lv_lrat$BIC[1]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
anova_lv_lrat$BF_10 <- c(NA, BF10)

df_resid <- summary(lv_mod)$dims$N - summary(lv_mod)$dims$p

# Planned Contrasts Results
pc_lv <- as.data.frame(summary(lv_mod)$tTable)
pc_lv$sig <- ifelse(pc_lv$`p-value` < .05, TRUE, FALSE)
pc_lv$DF <- df_resid
pc_lv$r_effect <- sqrt((pc_lv$`t-value`^2) / (pc_lv$`t-value`^2 + pc_lv$DF))

# Test of equality between E2 and NE
NE_v_E2 <- filter(lv, condition %in% c("Extreme Last", "No Extreme")) %>% 
  droplevels()

welch_NE_v_E2 <- t.test(cp ~ condition, data = NE_v_E2, var.equal = FALSE)
d_NE_v_E2 <- cohen.d(cp ~ condition, data = NE_v_E2)