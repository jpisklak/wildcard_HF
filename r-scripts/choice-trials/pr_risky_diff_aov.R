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
NE_v_E1 <- c(1, 0, 0)
E2_v_E1 <- c(0, 0, 1)
contrasts(diffs_b7$condition) <- cbind(NE_v_E1, E2_v_E1)

# Models
null_mod <- gls(diff ~ 1, data = diffs_b7, method = "ML")
summary(null_mod)

diff_mod <- gls(diff ~ condition, data = diffs_b7, method = "ML")
summary(diff_mod)

# L-ratio anova table-----------------------------------------------------------
anova_lrat <- as.data.frame(anova(null_mod, diff_mod))[, -c(1)]

# Inverse Bayes factor
delta_BIC <- anova_lrat$BIC[2] - anova_lrat$BIC[1]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
anova_lrat$BF_10 <- c(NA, BF10)

df_resid <- summary(diff_mod)$dims$N - summary(diff_mod)$dims$p

# Planned Contrasts Results-----------------------------------------------------
pc_diff <- as.data.frame(summary(diff_mod)$tTable)

# Adjust p-value for one-sided test as per pre-registration
pc_diff$`p-value` <- pc_diff$`p-value` / 2

pc_diff$sig <- ifelse(pc_diff$`p-value` < .05, TRUE, FALSE)
pc_diff$DF <- df_resid
pc_diff$r_effect <- sqrt((pc_diff$`t-value`^2) /
  (pc_diff$`t-value`^2 + pc_diff$DF))

# Test of equality between E2 and NE
NE_v_E2 <- diffs_b7 %>% 
  filter(condition %in% c("Extreme Last", "No Extreme")) %>% 
  droplevels()

welch_NE_v_E2 <- t.test(diff ~ condition, data = NE_v_E2, var.equal = FALSE)
d_NE_v_E2 <- cohen.d(diff ~ condition, data = NE_v_E2)


