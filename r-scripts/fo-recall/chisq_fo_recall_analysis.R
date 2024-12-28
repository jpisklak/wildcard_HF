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

# Post Hoc Analysis 1 (preferred)-----------------------------------------------
# Standardized Residuals
std_res_high <- chisq.test(fo_high_tab)$stdres
std_res_low <- chisq.test(fo_low_tab)$stdres

# P-values
p_high <- pnorm(abs(std_res_high), lower.tail = FALSE) * 2
p_low <- pnorm(abs(std_res_low), lower.tail = FALSE) * 2

# Post Hoc Analysis 2-----------------------------------------------------------

# Pairwise Fisher-Exact tests - High Value

# No extreme vs Extreme 1st
h_NO_v_E1 <- fo %>% 
  filter(FO_context == "High" & condition != "Extreme Last") %>%
  droplevels()
h_NO_v_E1 <- xtabs(~ condition + FO_eval, data = h_NO_v_E1)
h_NO_v_E1 <- fisher.test(h_NO_v_E1)

# No extreme vs Extreme Last
h_NO_v_EL <- fo %>% 
  filter(FO_context == "High" & condition != "Extreme 1st") %>%
  droplevels()
h_NO_v_EL <- xtabs(~ condition + FO_eval, data = h_NO_v_EL)
h_NO_v_EL <- fisher.test(h_NO_v_EL)

# Extreme 1st vs Extreme Last
h_E1_v_EL <- fo %>% 
  filter(FO_context == "High" & condition != "No Extreme") %>%
  droplevels()
h_E1_v_EL <- xtabs(~ condition + FO_eval, data = h_E1_v_EL)
h_E1_v_EL <- fisher.test(h_E1_v_EL)


# Pairwise Fisher-Exact tests - Low Value

# No extreme vs Extreme 1st
l_NO_v_E1 <- fo %>% 
  filter(FO_context == "Low" & condition != "Extreme Last") %>%
  droplevels()
l_NO_v_E1 <- xtabs(~ condition + FO_eval, data = l_NO_v_E1)
l_NO_v_E1 <- fisher.test(l_NO_v_E1)

# No extreme vs Extreme Last
l_NO_v_EL <- fo %>% 
  filter(FO_context == "Low" & condition != "Extreme 1st") %>%
  droplevels()
l_NO_v_EL <- xtabs(~ condition + FO_eval, data = l_NO_v_EL)
l_NO_v_EL <- fisher.test(l_NO_v_EL)

# Extreme 1st vs Extreme Last
l_E1_v_EL <- fo %>% 
  filter(FO_context == "Low" & condition != "No Extreme") %>%
  droplevels()
l_E1_v_EL <- xtabs(~ condition + FO_eval, data = l_E1_v_EL)
l_E1_v_EL <- fisher.test(l_E1_v_EL)


# Dataframe of Fisher tests

fish_tests <- tibble(
  value = c(rep("High", 3), rep("Low", 3)),
  comparison = rep(c(
    "No Extreme : Extreme 1st",
    "No Extreme : Extreme Last",
    "Extreme 1st : Extreme Last"
  ), 2),
  odds_ratio = c(
    h_NO_v_E1$estimate, h_NO_v_EL$estimate, h_E1_v_EL$estimate,
    l_NO_v_E1$estimate, l_NO_v_EL$estimate, l_E1_v_EL$estimate
  ),
  p_value = c(
    h_NO_v_E1$p.value, h_NO_v_EL$p.value, h_E1_v_EL$p.value,
    l_NO_v_E1$p.value, l_NO_v_EL$p.value, l_E1_v_EL$p.value
  ),
  CI_low = c(
    h_NO_v_E1$conf.int[1], h_NO_v_EL$conf.int[1], h_E1_v_EL$conf.int[1],
    l_NO_v_E1$conf.int[1], l_NO_v_EL$conf.int[1], l_E1_v_EL$conf.int[1]
  ),
  CI_upper = c(
    h_NO_v_E1$conf.int[2], h_NO_v_EL$conf.int[2], h_E1_v_EL$conf.int[2],
    l_NO_v_E1$conf.int[2], l_NO_v_EL$conf.int[2], l_E1_v_EL$conf.int[2]
  ),
)

fish_tests$p_value_adj <- p.adjust(fish_tests$p_value, method = "holm")


# https://rcompanion.org/handbook/H_04.html
# https://www.statology.org/interpret-cramers-v/
