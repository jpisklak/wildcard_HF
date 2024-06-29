# Run the following 4 lines to execute this script independently
# setwd('../..')
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/freq-judge/freq_judge_filter.R")

# 2 x 3 mixed ANOVA
#-------------------------------------------------------------------------------
# Note: data is qualitative, thus the validity of this method is questionable.
# E.g., One person's 20 may be another persons 60.

fj_long <- filter(
  fj_long,
  FJ_outcome %in% c("+20", "+80")
)

# Contrasts
E1_v_NE <- c(1, 0, 0)
E2_v_NE <- c(0, 1, 0)
contrasts(fj_long$condition) <- cbind(E1_v_NE, E2_v_NE)

H_v_L <- c(-1, 1)
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

# Nagelkerke (Cragg and Uhler) Pseudo R-squared
# cond_R2 <- nagelkerke(cond_mod, null = base)$Pseudo.R.squared.for.model.vs.null[3]
# HL_R2 <- nagelkerke(HL_mod, null = cond_mod)$Pseudo.R.squared.for.model.vs.null[3]
# int_R2 <- nagelkerke(int_mod, null = HL_mod)$Pseudo.R.squared.for.model.vs.null[3]
# 
# aov_2x3$pseudo_R2 <- c(NA, cond_R2, HL_R2, int_R2)

# Inverse Bayes Factor
delta_BIC <- aov_2x3$BIC[2:nrow(aov_2x3)] - aov_2x3$BIC[1:(nrow(aov_2x3) - 1)]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
aov_2x3$BF_10 <- c(NA, BF10)

# Planned comparisons
pc <- as.data.frame(summary(int_mod)$tTable)
pc$r_effect <- sqrt((pc$`t-value`^2) / (pc$`t-value`^2 + pc$DF))





