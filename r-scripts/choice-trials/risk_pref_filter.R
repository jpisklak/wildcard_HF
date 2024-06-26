# setwd('../..') #Running this R script alone requires being in the main dir.
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")

risky <- filter(
  data, trial_type == "decisionTrl" &
    trial_tot == 300 &
    # block == 7 &
    !(ID %in% exclude$ID) # Remove catch trial exclusions
) %>%
  select(
    -(PROLIFIC_PID:employment),
    -(money_tot:FJ2_resp)
  )

# Risky choice selection
risky <- risky %>%
  mutate(risky_choice = case_when(
    choice == "LF_LR" | choice == "LR_LF" ~ "Low",
    choice == "HF_HR" | choice == "HR_HF" ~ "High"
  )) %>%
  drop_na(risky_choice)

# Door chosen
risky <- risky %>%
  mutate(risky_resp = case_when(
    response == "LR" | response == "HR" ~ 1,
    response == "LF" | response == "HF" ~ 0
  ))

# Subject Data
risky_res <- risky %>%
  group_by(ID, condition, block, risky_choice) %>%
  summarise(
    cp = mean(risky_resp)
  )

# Factor and rename conditions
risky_res$block <- factor(risky_res$block)

risky_res$condition <- factor(risky_res$condition)
levels(risky_res$condition) <- c(
  "Extreme 1st",
  "Extreme Last",
  "No Extreme"
)

risky_res$risky_choice <- factor(risky_res$risky_choice,
  levels = c("Low", "High")
)

# Diff Scores
diffs <- risky_res %>%
  group_by(ID, condition, block) %>%
  summarise(
    diff = cp[risky_choice == "High"] - cp[risky_choice == "Low"]
  )

# Block 7 Only
risky_res_b7 <- filter(risky_res, block == 7)
diffs_b7 <- filter(diffs, block == 7)

