library(tidyverse)
setwd('..')
data <- read_csv('Data/wc_full_data.csv',
                 col_types = cols(.default = '?', 
                                  money_tot = 'c'))

#Assess completion time
  #Note: Prolific allows rejection only if < 3 SDs below the mean
durs <- filter(data, trial == 1,
               block == 1, 
               trial_tot == 300, 
               !is.na(exp_dur)) %>%
  select(ID, exp_dur)

durs$z <- (durs$exp_dur - mean(durs$exp_dur))/sd(durs$exp_dur)

data <- merge(data, durs[, c(1, 3)], by = "ID",
              all.x = TRUE) 
data <- data[order(data$row), ]


#Who gets paid
#----------------------------------------------
date <- '2023-08-24 00:00:00'

moolah <- filter(data, money_tot != 'NA',
                   date_time >= date,
                   complete_code != 'NOCODE',
                   trial_tot == 300,
                   z > -3
                   ) %>% 
  select(row,
         date_time,
         PROLIFIC_PID,
         STUDY_ID,
         complete_code,
         nRow,
         trial_tot,
         exp_dur,
         z,
         ID,
         money_tot)

View(moolah)

#Approve and Pay
txt_approve <- paste(moolah$PROLIFIC_PID, collapse = ', ')
capture.output(txt_approve, file = "Misc Scripts/bulk_approval.txt")

#Bonus
txt_bonus <- select(moolah, PROLIFIC_PID, money_tot)
write.csv(txt_bonus, 'Misc Scripts/bulk_bonus.txt', 
          row.names = FALSE,
          quote = FALSE)

#Manual cata check
#--------------------------------------------
subject <- '56ca0400b30699000bd9d94f'
check <- filter(data, PROLIFIC_PID == subject)
View(check)





