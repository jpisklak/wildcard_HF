#setwd('..') #Running this R script alone requires being in the main dir.
source('R Scripts/freq_judge_plots.R')

#2 x 3 mixed ANOVA
#-------------------------------------------------------------------------------
#Note: data is qualitative, thus the validity of this method is questionable.
#E.g., One person's 20 may be another persons 60.

fj_long <- filter(fj_long,
                  FJ_outcome %in% c('+20', '+80'))

#Contrasts
E1_v_E2NE <- c(2, -1, -1)
E2_v_NE <- c(0, 1, -1)
contrasts(fj_long$condition) <- cbind(E1_v_E2NE, E2_v_NE)

H_v_L <- c(-1, 1)
contrasts(fj_long$FJ_context) <- H_v_L

fj_long$ID <- factor(fj_long$ID)

#Models
base <- lme(FJ_resp ~ 1, 
            random = ~1|ID,
            method = 'ML',
            data = fj_long)

cond_mod <- lme(FJ_resp ~ condition,
                random = ~1|ID,
                method = 'ML',
                data = fj_long)

HL_mod <- lme(FJ_resp ~ condition + FJ_context,
              random = ~1|ID,
              method = 'ML',
              data = fj_long)

int_mod <- lme(FJ_resp ~ condition + FJ_context + condition:FJ_context, 
               random = ~1|ID,
               method = 'ML',
               data = fj_long)

#Residuals
#plot(int_mod)

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
Inv_BF <- round(BF10, 5)

aov_2x3$BF_10 <- c(NA, Inv_BF)

#Planned comparisons
pc <- as.data.frame(round(summary(int_mod)$tTable, 4))
pc$r_effect <- sqrt((pc$`t-value`^2) / (pc$`t-value`^2 + pc$DF))

    
    
    