# Run the following 4 lines to execute this script independently
 # setwd('../..') # assumes working dir is ./r-scripts/choice-trials
 source("r-scripts/prelim_code.R")
 source("r-scripts/subj_stats.R")
 source("r-scripts/choice-trials/risk_pref_filter.R")
#-------------------------------------------------------------------------------

#risky_res_b7
# levels(risky_res_b7$condition)
# levels(risky_res_b7$risky_choice)

# Factor ID
risky_res_b7$ID <- factor(risky_res_b7$ID)

# Re-order factor levels
risky_res_b7$risky_choice <- factor(risky_res_b7$risky_choice,
  levels = c("High", "Low")
)

# Set Contrasts
NE_v_E1 <- c(1, 0, 0)
E2_v_E1 <- c(0, 0, 1)
contrasts(risky_res_b7$condition) <- cbind(NE_v_E1, E2_v_E1)

# Choice Value Comparisons
` high_vs_low ` <- c(1, -1)
contrasts(risky_res_b7$risky_choice) <- cbind(` high_vs_low `)

# Multilevel model
base_mod <- lme(cp ~ 1,
  random = ~ 1 | ID / risky_choice,
  method = "ML", data = risky_res_b7
)

#Main effects
cond_mod <- update(base_mod, .~. + condition)
value_mod <- update(cond_mod, .~. + risky_choice)

# Interaction
cond_val <- update(value_mod, .~. + condition:risky_choice)

# Main Effects
risky_main <- anova(base_mod, cond_mod, value_mod, cond_val)

# Inverse Bayes Factors
# Inverse Bayes factor
delta_BIC <- risky_main$BIC[2:4] - risky_main$BIC[1:3]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
risky_main$BF_10 <- c(NA, BF10)
risky_main

# Planned Comparisons
round(summary(cond_val)$tTable, 4)
pc <- as.data.frame(summary(cond_val)$tTable)

  # Adjust for one-sided test
pc$`p-value`[c(2:3, 5:6)] <- pc$`p-value`[c(2:3, 5:6)] / 2
pc$sig <- ifelse(pc$`p-value` < .05, TRUE, FALSE)

pc$r_effect <- sqrt((pc$`t-value`^2) /
                      (pc$`t-value`^2 + pc$DF))

round(pc, 4)
