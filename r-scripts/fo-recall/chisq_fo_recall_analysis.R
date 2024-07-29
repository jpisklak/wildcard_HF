# Run the following 5 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/fo-recall
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/fo-recall/fo_recall_filter.R")
# source("r-scripts/fo-recall/pr_fo_recall_analysis.R")
#-------------------------------------------------------------------------------

# 2 X 3 Pearson’s Chi-squared Test
#-------------------------------------------------------------------------------

fo_high <- fo %>% filter(FO_context == "High")
fo_high_tab <- xtabs(~ condition + FO_eval, data = fo_high)

fo_low <- fo %>% filter(FO_context == "Low")
fo_low_tab <- xtabs(~ condition + FO_eval, data = fo_low)

fo_high_test <- chisq.test(fo_high_tab)
fo_high_eff <- cramerV(fo_high_tab, digits = 3)

fo_low_test <- chisq.test(fo_low_tab)
fo_low_eff <- cramerV(fo_low_tab, digits = 3)

# Post Hoc Analysis
# Standardized Residuals
std_res_high <- chisq.test(fo_high_tab)$stdres
std_res_low <- chisq.test(fo_low_tab)$stdres

# P-values
p_high <- pnorm(abs(std_res_high), lower.tail = FALSE) * 2
p_low <- pnorm(abs(std_res_low), lower.tail = FALSE) * 2


# https://rcompanion.org/handbook/H_04.html
# https://www.statology.org/interpret-cramers-v/
