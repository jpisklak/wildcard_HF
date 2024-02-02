#setwd('..') #Running this code in isolation requires being in the Analysis dir.
source('R Scripts/catch_trials.R') #Loads data and exclusions

#Get relevant info
demo_info <- filter(data, trial_tot == 300 &
                    block == 1 &
                    trial == 0) %>%
  select(ID, condition, (exp_dur : employment))

#Make age numeric and rename condition
demo_info$age <- as.numeric(demo_info$age)

demo_info$condition <- factor(demo_info$condition)
levels(demo_info$condition) <- c('Extreme 1st',
                                 'Extreme 2nd',
                                 'No Extreme')

#Stats pre catch exclusion
pre_catch <- demo_info %>% 
  group_by(condition, sex) %>% 
  summarise(
    n = length(ID),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE),
    dur_minutes_mean = mean(exp_dur/60, na.rm = TRUE),
    dur_minutes_median = median(exp_dur/60, na.rm = TRUE)
    )

N <- sum(pre_catch$n)


#Stats post catch exclusion
demo_info_f <- filter(demo_info, !(ID %in% exclude$ID))

post_catch <- demo_info_f %>% 
  group_by(condition, sex) %>% 
  summarise(
    n = length(ID),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE),
    dur_minutes_mean = mean(exp_dur/60, na.rm = TRUE),
    dur_minutes_median = median(exp_dur/60, na.rm = TRUE)
  )


catch_n <- nrow(exclude)


#Completion of memory Phases
# Some participants had program crash during the memory phase
# Need to be removed from respective memory analysis
mem <- data %>%
  filter(phase %in% c('FO_recall', 'Freq_judge'))

mem$ones <- rep(1, nrow(mem))

fo_comp <- mem %>%
  filter(phase == 'FO_recall') %>%
  group_by(ID, condition) %>%
  summarise(fo_trial_tot = sum(ones))%>%
  filter(fo_trial_tot < 6)

fo_comp

fj_comp <- mem %>%
  filter(phase == 'Freq_judge') %>%
  group_by(ID, condition) %>%
  summarise(fj_trial_tot = sum(ones)) %>%
  filter(fj_trial_tot < 6)

fj_comp




