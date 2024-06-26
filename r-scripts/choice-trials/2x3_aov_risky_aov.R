# Run the following 4 lines to execute this script independently
# setwd('../..')
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")

# Note: Pre-registration called for 3 separate one-way ANOVAs (not a 2x3 mixed)

# 2 x 3 Mixed ANOVA using LME
#-------------------------------------------------------------------------------

# Factor Data
risky_res_b7$ID <- factor(risky_res_b7$ID)

#Set Contrasts
E1_v_NE <- c(1, 0, 0)
E2_v_NE <- c(0, 1, 0)
contrasts(risky_res_b7$condition) <- cbind(E1_v_NE, E2_v_NE)

H_v_L <- c(-1, 1)
contrasts(risky_res_b7$risky_choice) <- H_v_L

base <- lme(cp ~ 1, 
            random = ~ 1 | ID / risky_choice,
            method = 'ML',
            data = risky_res_b7)

cond_mod <- lme(cp ~ condition,
                random = ~ 1 | ID / risky_choice,
                method = 'ML',
                data = risky_res_b7)

HL_mod <- lme(cp ~ condition + risky_choice,
              random = ~ 1 | ID / risky_choice,
              method = 'ML',
              data = risky_res_b7)

int_mod <- lme(cp ~ condition + risky_choice + condition:risky_choice, 
               random = ~ 1 | ID / risky_choice,
               method = 'ML',
               data = risky_res_b7)

summary(int_mod)
# plot(int_mod)

# Bootstrapped LME Check
  # library(lmeresampler)
  # lme_boot <- bootstrap(int_mod, .f = fixef, type = "parametric", B = 5000)
  # lme_boot
  # confint(lme_boot, type = 'norm')
  # plot(lme_boot)


# Main effects and interactions
aov_2x3 <- data.frame(anova(base, cond_mod, HL_mod, int_mod))
aov_2x3 <- aov_2x3[, 2:ncol(aov_2x3)]

# Nagelkerke (Cragg and Uhler) Pseudo R-squared
cond_R2 <- nagelkerke(cond_mod, null = base)$Pseudo.R.squared.for.model.vs.null[3]
HL_R2 <- nagelkerke(HL_mod, null = cond_mod)$Pseudo.R.squared.for.model.vs.null[3]
int_R2 <- nagelkerke(int_mod, null = HL_mod)$Pseudo.R.squared.for.model.vs.null[3]

aov_2x3$pseudo_R2 <- c(NA, cond_R2, HL_R2, int_R2)

# Inverse Bayes Factor
delta_BIC <- aov_2x3$BIC[2:nrow(aov_2x3)] - aov_2x3$BIC[1:(nrow(aov_2x3) - 1)]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
aov_2x3$BF_10 <- c(NA, BF10)

# Planned comparisons
pc <- as.data.frame(summary(int_mod)$tTable)
pc$r_effect <- sqrt((pc$`t-value`^2) / (pc$`t-value`^2 + pc$DF))










