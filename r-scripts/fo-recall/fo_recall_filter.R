# Run the following 3 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/fo-recall
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
#-------------------------------------------------------------------------------

fo <- filter(
  data, phase == "FO_recall" &
    !(ID %in% exclude$ID) # Remove catch trial exclusions
) %>%
  select(
    -(PROLIFIC_PID:employment),
    -(block:money_tot),
    -(FJ_stim:FJ2_resp)
  )

# Rename condition levels
fo$condition <- factor(fo$condition)
levels(fo$condition) <- c(
  "Extreme 1st",
  "Extreme Last",
  "No Extreme"
)

# Create context column
fo <- fo %>%
  mutate(FO_context = case_when(
    FO_stim %in% c("LF", "LR") ~ "Low",
    FO_stim %in% c("HF", "HR") ~ "High",
    FO_stim %in% c("WC1", "WC2") ~ "WC"
  ))

# Exclude fixed and wildcard outcomes (so they don't contaminate 'other')'
fo <- filter(fo, FO_stim %in% c("LR", "HR"))

# Create category column for results
fo <- fo %>%
  mutate(FO_cat = case_when(
    FO_context == "Low" & FO_resp == 20 ~ "+20",
    FO_context == "Low" & FO_resp == 40 ~ "+40",
    FO_context == "High" & FO_resp == 60 ~ "+60",
    FO_context == "High" & FO_resp == 80 ~ "+80",
    FO_context == "Low" & FO_resp != c(20, 40) ~ "Other",
    FO_context == "High" & FO_resp != c(60, 80) ~ "Other"
  ))

# Proportion results
props <- fo %>%
  group_by(FO_context, condition) %>%
  count(FO_cat)


totals <- props %>%
  group_by(FO_context, condition) %>%
  summarise(
    total = sum(n)
  )

props <- merge(props, totals,
  by = c("FO_context", "condition"),
  all.x = TRUE
)

props$prop <- props$n / props$total


# Reorder context levels
props$FO_context <- factor(props$FO_context,
  levels = c(
    "Low",
    "High"
  )
)

