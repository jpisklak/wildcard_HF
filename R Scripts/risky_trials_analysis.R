#setwd('..') #Running this R script alone requires being in the main dir.
source('R Scripts/risky_trials_plots.R')

#Note: Pre-registration called for 3 separate one-way ANOVAs (not a 2x3 mixed)

#One-Way ANOVA on Differences (i.e., EO effect)
#-------------------------------------------------------------------------------

diffs <- filter(diffs, block == 7)

#Set Contrasts
E1_v_E2NE <- c(2, -1, -1)
E2_v_NE <- c(0, 1, -1)
contrasts(diffs$condition) <- cbind(E1_v_E2NE, E2_v_NE)

#Model
diff_mod <- gls(diff ~ condition, data = diffs, method = 'ML')

#Main Effect
df_resid <- summary(diff_mod)$dims$N - summary(diff_mod)$dims$p
anova_diff <- anova(diff_mod)
anova_diff <- add_row(anova_diff,
                      numDF = df_resid,
                      `F-value` = NULL,
                      `p-value` = NULL)
row.names(anova_diff)[3] <- "Residuals"

#Nagelkerke (Cragg and Uhler) Pseudo R-squared
anova_diff$pseudo_R2 <- c(NA,
                          nagelkerke(diff_mod)$Pseudo.R.squared.for.model.vs.null[3],
                          NA)

#Planned Contrasts Results
pc_diff <- as.data.frame(summary(diff_mod)$tTable)
pc_diff$sig <- ifelse(pc_diff$`p-value` < .05, TRUE, FALSE)
pc_diff$DF <- df_resid
pc_diff$r_effect <- sqrt((pc_diff$`t-value`^2) / (pc_diff$`t-value`^2 + pc_diff$DF))

#Post-hoc comparisons for heteroscedastic means
#Assumes Normality
ph_diff <- lincon(diff ~ condition, 
                  data = diffs, 
                  tr = 0, 
                  method = 'hochberg')$comp[, 3:6]
rownames(ph_diff) <- c(
  paste(levels(diffs$condition)[1], levels(diffs$condition)[2], sep = '-'),
  paste(levels(diffs$condition)[1], levels(diffs$condition)[3], sep = '-'),
  paste(levels(diffs$condition)[2], levels(diffs$condition)[3], sep = '-')
)


#One-Way ANOVA on High Value selection
#-------------------------------------------------------------------------------
#Subset High Value
hv <- subject_res %>%
  filter(block == 7 & risky_choice == 'High')

#Set Contrasts
E1_v_E2NE <- c(2, -1, -1)
E2_v_NE <- c(0, 1, -1)
contrasts(hv$condition) <- cbind(E1_v_E2NE, E2_v_NE)

#Model
hv_mod <- gls(cp ~ condition, data = hv, method = 'ML')

#Main Effect
df_resid <- summary(hv_mod)$dims$N - summary(hv_mod)$dims$p
anova_hv <- anova(hv_mod)
anova_hv <- add_row(anova_hv,
                    numDF = df_resid,
                    `F-value` = NULL,
                    `p-value` = NULL)
row.names(anova_hv)[3] <- "Residuals"

#Nagelkerke (Cragg and Uhler) Pseudo R-squared
anova_hv$pseudo_R2 <- c(NA,
                        nagelkerke(hv_mod)$Pseudo.R.squared.for.model.vs.null[3],
                        NA)

#Planned Contrasts Results
pc_hv <- as.data.frame(summary(hv_mod)$tTable)
pc_hv$sig <- ifelse(pc_hv$`p-value` < .05, TRUE, FALSE)
pc_hv$DF <- df_resid
pc_hv$r_effect <- sqrt((pc_hv$`t-value`^2) / (pc_hv$`t-value`^2 + pc_hv$DF))

#Post-hoc comparisons for heteroscedastic means
#Assumes Normality
ph_hv <- lincon(cp ~ condition, 
                data = hv, 
                tr = 0, 
                method = 'hochberg')$comp[, 3:6]
rownames(ph_hv) <- c(
  paste(levels(hv$condition)[1], levels(hv$condition)[2], sep = '-'),
  paste(levels(hv$condition)[1], levels(hv$condition)[3], sep = '-'),
  paste(levels(hv$condition)[2], levels(hv$condition)[3], sep = '-')
)


#One-Way ANOVA on Low Value selection
#-------------------------------------------------------------------------------
# Subset Low Value
lv <- subject_res %>%
  filter(block == 7 & risky_choice == 'Low')

#Set Contrasts
E1_v_E2NE <- c(2, -1, -1)
E2_v_NE <- c(0, 1, -1)
contrasts(lv$condition) <- cbind(E1_v_E2NE, E2_v_NE)

#Model
lv_mod <- gls(cp ~ condition, data = lv, method = 'ML')

#Main Effect
df_resid <- summary(lv_mod)$dims$N - summary(lv_mod)$dims$p
anova_lv <- anova(lv_mod)
anova_lv <- add_row(anova_lv,
                    numDF = df_resid,
                    `F-value` = NULL,
                    `p-value` = NULL)
row.names(anova_lv)[3] <- "Residuals"

#Nagelkerke (Cragg and Uhler) Pseudo R-squared
anova_lv$pseudo_R2 <- c(NA,
                        nagelkerke(lv_mod)$Pseudo.R.squared.for.model.vs.null[3],
                        NA)

#Planned Contrasts Results
pc_lv <- as.data.frame(summary(lv_mod)$tTable)
pc_lv$sig <- ifelse(pc_lv$`p-value` < .05, TRUE, FALSE)
pc_lv$DF <- df_resid
pc_lv$r_effect <- sqrt((pc_lv$`t-value`^2) / (pc_lv$`t-value`^2 + pc_lv$DF))

#Post-hoc comparisons for heteroscedastic means
  #Assumes Normality
ph_lv <- lincon(cp ~ condition, 
                data = lv, 
                tr = 0, 
                method = 'hochberg')$comp[, 3:6]
rownames(ph_lv) <- c(
  paste(levels(lv$condition)[1], levels(lv$condition)[2], sep = '-'),
  paste(levels(lv$condition)[1], levels(lv$condition)[3], sep = '-'),
  paste(levels(lv$condition)[2], levels(lv$condition)[3], sep = '-')
)


#2 x 3 Mixed ANOVA using LME
#-------------------------------------------------------------------------------
subject_res <- filter(subject_res, block == 7)

#Factor Data
subject_res$ID <- factor(subject_res$ID)

#Set Contrasts
E1_v_E2NE <- c(2, -1, -1)
E2_v_NE <- c(0, 1, -1)
contrasts(subject_res$condition) <- cbind(E1_v_E2NE, E2_v_NE)

H_v_L <- c(-1, 1)
contrasts(subject_res$risky_choice) <- H_v_L

base <- lme(cp ~ 1, 
            random = ~1|ID,
            method = 'ML',
            data = subject_res)

cond_mod <- lme(cp ~ condition,
                random = ~1|ID,
                method = 'ML',
                data = subject_res)

HL_mod <- lme(cp ~ condition + risky_choice,
              random = ~1|ID,
              method = 'ML',
              data = subject_res)

int_mod <- lme(cp ~ condition + risky_choice + condition:risky_choice, 
               random = ~1|ID,
               method = 'ML',
               data = subject_res)

summary(int_mod)
#plot(int_mod)

# Bootstrapped LME Check
  # library(lmeresampler)
  # lme_boot <- bootstrap(int_mod, .f = fixef, type = "parametric", B = 5000)
  # lme_boot
  # confint(lme_boot, type = 'norm')
  # plot(lme_boot)


#Main effects and interactions
aov_2x3 <- anova(base, cond_mod, HL_mod, int_mod)
aov_2x3$`p ≤ 0.05` <- aov_2x3$`p-value` <= 0.05

#Nagelkerke (Cragg and Uhler) Pseudo R-squared
cond_R2 <- nagelkerke(cond_mod, null = base)$Pseudo.R.squared.for.model.vs.null[3]
HL_R2 <- nagelkerke(HL_mod, null = cond_mod)$Pseudo.R.squared.for.model.vs.null[3]
int_R2 <- nagelkerke(int_mod, null = HL_mod)$Pseudo.R.squared.for.model.vs.null[3]

aov_2x3$pseudo_R2 <- c(NA, cond_R2, HL_R2, int_R2)

#Inverse Bayes Factor
delta_BIC <- aov_2x3$BIC[2:4] - aov_2x3$BIC[1:3]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
aov_2x3$BF_10 <- c(NA, BF10)

#Planned comparisons
pc <- as.data.frame(summary(int_mod)$tTable)
pc$r_effect <- sqrt((pc$`t-value`^2) / (pc$`t-value`^2 + pc$DF))










