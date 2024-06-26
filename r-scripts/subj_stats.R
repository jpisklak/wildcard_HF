# Run the following 2 lines to execute this script independently
# setwd('..')
# source("r-scripts/prelim_code.R")

# Load data
data <- read_csv("data/wc_full_data.csv",
  col_types = cols(
    .default = "?",
    money_tot = "c"
  )
)

# Collect catch trials
ctch <- data %>%
  filter(trial_type == "ctch" & trial_tot == 300) %>%
  select(
    -PROLIFIC_PID,
    -STUDY_ID,
    -SESSION_ID,
    -(money_tot:FJ2_resp)
  )
# names(ctch)

# Correct Choice
ctch <- ctch %>%
  mutate(corr_resp = case_when(
    response == "HF" | response == "HR" ~ 1,
    response == "LF" | response == "LR" ~ 0
  ))

# Catch Res
ctch_res <- ctch %>%
  group_by(condition, ID, block) %>%
  summarise(cp = mean(corr_resp))

# Catch Exclusions
exclude <- filter(ctch_res, block == 7 & cp < 0.6)
ctch_res <- filter(ctch_res, !(ID %in% exclude$ID))
ctch_res

# Get relevant info
demo_info <- filter(data, trial_tot == 300 &
  block == 1 &
  trial == 0) %>%
  select(ID, condition, (exp_dur:employment))

# Make age numeric and rename condition
demo_info$age <- as.numeric(demo_info$age)

demo_info$condition <- factor(demo_info$condition)
levels(demo_info$condition) <- c(
  "Extreme 1st",
  "Extreme 2nd",
  "No Extreme"
)

# Stats pre catch exclusion
pre_catch <- demo_info %>%
  group_by(condition, sex) %>%
  summarise(
    n = length(ID),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE),
    dur_minutes_mean = mean(exp_dur / 60, na.rm = TRUE),
    dur_minutes_median = median(exp_dur / 60, na.rm = TRUE)
  )

N <- sum(pre_catch$n)

# Stats post catch exclusion
demo_info_f <- filter(demo_info, !(ID %in% exclude$ID))

post_catch <- demo_info_f %>%
  group_by(condition, sex) %>%
  summarise(
    n = length(ID),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE),
    dur_minutes_mean = mean(exp_dur / 60, na.rm = TRUE),
    dur_minutes_median = median(exp_dur / 60, na.rm = TRUE)
  )

catch_n <- nrow(exclude)

# Condition totals
cond_tot <- demo_info_f %>%
  group_by(condition) %>%
  summarise(
    n = length(ID),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE),
    dur_minutes_mean = mean(exp_dur / 60, na.rm = TRUE),
    dur_minutes_median = median(exp_dur / 60, na.rm = TRUE)
  )

# Completion of memory Phases
  # Some participants had program crash during the memory phase
  # Need to be removed from respective memory analysis
mem <- data %>%
  filter(phase %in% c("FO_recall", "Freq_judge"))

mem$ones <- rep(1, nrow(mem))

fo_comp <- mem %>%
  filter(phase == "FO_recall") %>%
  group_by(ID, condition) %>%
  summarise(fo_trial_tot = sum(ones)) %>%
  filter(fo_trial_tot < 6)

fj_comp <- mem %>%
  filter(phase == "Freq_judge") %>%
  group_by(ID, condition) %>%
  summarise(fj_trial_tot = sum(ones)) %>%
  filter(fj_trial_tot < 6)




