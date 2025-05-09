# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/freq-judge
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/freq-judge/freq_judge_filter.R")
#-------------------------------------------------------------------------------

# 2 x 3 mixed ANOVA
#-------------------------------------------------------------------------------
# Note: data is qualitative, thus the validity of this method is questionable.
# E.g., One person's 20 may be another persons 60.

fj_long <- filter(
  fj_long,
  FJ_outcome %in% c("+20", "+80")
)

# Contrasts
NE_v_E1 <- c(1, 0, 0)
E2_v_E1 <- c(0, 0, 1)
contrasts(fj_long$condition) <- cbind(NE_v_E1, E2_v_E1)

H_v_L <- c(1, -1)
contrasts(fj_long$FJ_context) <- H_v_L

fj_long$ID <- factor(fj_long$ID)

# Models
base <- lme(FJ_resp ~ 1,
  random = ~ 1 | ID / FJ_context,
  method = "ML",
  data = fj_long
)

cond_mod <- lme(FJ_resp ~ condition,
  random = ~ 1 | ID / FJ_context,
  method = "ML",
  data = fj_long
)

HL_mod <- lme(FJ_resp ~ condition + FJ_context,
  random = ~ 1 | ID / FJ_context,
  method = "ML",
  data = fj_long
)

int_mod <- lme(FJ_resp ~ condition + FJ_context + condition:FJ_context,
  random = ~ 1 | ID / FJ_context,
  method = "ML",
  data = fj_long
)

# Residuals
#plot(int_mod)

# Main effects and interactions
aov_2x3 <- data.frame(anova(base, cond_mod, HL_mod, int_mod))
aov_2x3 <- aov_2x3[, 2:ncol(aov_2x3)]

# Inverse Bayes Factor
delta_BIC <- aov_2x3$BIC[2:nrow(aov_2x3)] - aov_2x3$BIC[1:(nrow(aov_2x3) - 1)]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
aov_2x3$BF_10 <- c(NA, BF10)

# Planned comparisons
pc <- as.data.frame(summary(int_mod)$tTable)

# Adjust p-value for one-sided test as per pre-registration 
  # (interactions and intercept not adjusted)
pc$`p-value`[2:3] <- pc$`p-value`[2:3] / 2

pc$r_effect <- sqrt((pc$`t-value`^2) / (pc$`t-value`^2 + pc$DF))

# Test of equality between E2 and NE
NE_v_E2 <- fj_long %>% 
  filter(condition %in% c("Extreme Last", "No Extreme")) %>% 
  droplevels()

welch_NE_v_E2 <- t.test(FJ_resp ~ condition, data = NE_v_E2, var.equal = FALSE)
d_NE_v_E2 <- cohen.d(FJ_resp ~ condition, data = NE_v_E2)



