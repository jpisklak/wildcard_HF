# Run the following 4 lines to execute this script independently
# setwd('..')
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/fo-recall/fo_recall_filter.R")

# Create category column for results
fo <- fo %>%
  mutate(FO_eval = case_when(
    FO_stim == "LR" & FO_resp == 20 ~ "Yes",
    FO_stim == "HR" & FO_resp == 80 ~ "Yes",
    FO_stim == "LR" & FO_resp != 20 ~ "No",
    FO_stim == "HR" & FO_resp != 80 ~ "No",
  ))

# McNemar Test
#-------------------------------------------------------------------------------

# Seperate Groups
g1 <- fo %>%
  filter(condition == "Extreme 1st")
g1_tab <- xtabs(~ FO_context + FO_eval,
  data = g1
)

g2 <- fo %>%
  filter(condition == "Extreme Last")
g2_tab <- xtabs(~ FO_context + FO_eval,
  data = g2
)

g3 <- fo %>%
  filter(condition == "No Extreme")
g3_tab <- xtabs(~ FO_context + FO_eval,
  data = g3
)

# McNemar Test W/ Odds Ratio and Cohen's g effect size
g1_test <- mcnemar.test(g1_tab, y = NULL, correct = FALSE)
g1_effect <- cohenG(g1_tab, ci = TRUE, type = "norm")

g2_test <- mcnemar.test(g2_tab, y = NULL, correct = FALSE)
g2_effect <- cohenG(g2_tab, ci = TRUE, type = "norm")

g3_test <- mcnemar.test(g3_tab, y = NULL, correct = FALSE)
g3_effect <- cohenG(g3_tab, ci = TRUE, type = "norm")

interp <- data.frame(
  Small = c("0.05 : 0.15", "1.22 : 1.86", "0.820 : 0.538"),
  Medium = c("0.15 : 0.25", "1.86 : 3.00", "0.538 : 0.333"),
  Large = c("≥ 0.25", "≥ 3.00", "≤ 0.333")
)

rownames(interp) <- c("Cohen's g", "Odds Ratio > 1", "Odds Ratio < 1")


# Capture Results
#------------------------------------------------------------------------------- 

fo_stats <- data.frame(
  Condition = c("Extreme 1st", "Extreme Last", "No Extreme"),
  Statistic = c(
    g1_test$statistic,
    g2_test$statistic,
    g3_test$statistic
  ),
  df = c(
    g1_test$parameter,
    g2_test$parameter,
    g3_test$parameter
  ),
  `p-value` = c(
    g1_test$p.value,
    g2_test$p.value,
    g3_test$p.value
  ),
  OR = c(
    g1_effect$Global.statistics[1, 3],
    g2_effect$Global.statistics[1, 3],
    g3_effect$Global.statistics[1, 3]
  ),
  Cohen_g = c(
    g1_effect$Global.statistics[3, 3],
    g2_effect$Global.statistics[3, 3],
    g3_effect$Global.statistics[3, 3]
  ),
  lower_g_ci = c(
    g1_effect$Global.statistics[3, 4],
    g2_effect$Global.statistics[3, 4],
    g3_effect$Global.statistics[3, 4]
  ),
  upper_g_ci = c(
    g1_effect$Global.statistics[3, 5],
    g2_effect$Global.statistics[3, 5],
    g3_effect$Global.statistics[3, 5]
  )
)