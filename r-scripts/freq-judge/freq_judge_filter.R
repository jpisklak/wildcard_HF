# Run the following 3 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/freq-judge
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
#-------------------------------------------------------------------------------

fj <- filter(
  data, phase == "Freq_judge",
  !(ID %in% exclude$ID), # Remove catch trial exclusions
  !(ID %in% fj_comp$ID)
) %>%
  select(
    -(PROLIFIC_PID:employment),
    -(block:money_tot),
    -(FO_stim:FO_resp)
  )

# Rename condition levels
fj$condition <- factor(fj$condition, levels = c(2, 0, 1))
levels(fj$condition) <- c(
  "No Extreme",
  "Extreme 1st",
  "Extreme Last"
)

# Create context column
fj <- fj %>%
  mutate(FJ_context = case_when(
    FJ_stim %in% c("LF", "LR") ~ "Low",
    FJ_stim %in% c("HF", "HR") ~ "High",
    FJ_stim %in% c("WC1", "WC2") ~ "WC"
  ))

# Exclude wildcard and fixed outcomes
fj <- filter(fj, FJ_stim %in% c("LR", "HR"))

# Create outcome column to easily categorize results
fj_wide <- fj %>%
  select(
    ID,
    condition,
    FJ_stim,
    FJ1_resp,
    FJ2_resp,
    FJ_context
  )

fj_long <- pivot_longer(fj_wide, c(FJ1_resp, FJ2_resp),
  names_to = "FJ_order",
  values_to = "FJ_resp"
)


fj_long <- fj_long %>%
  mutate(FJ_outcome = case_when(
    FJ_stim == "LR" & FJ_order == "FJ1_resp" ~ "+20",
    FJ_stim == "LR" & FJ_order == "FJ2_resp" ~ "+40",
    FJ_stim == "HR" & FJ_order == "FJ1_resp" ~ "+60",
    FJ_stim == "HR" & FJ_order == "FJ2_resp" ~ "+80"
  ))


# Reorder context levels
fj_long$FJ_context <- factor(fj_long$FJ_context,
  levels = c(
    "High",
    "Low"
  )
)


