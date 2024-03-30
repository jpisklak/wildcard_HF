# setwd('..') #Running this R script alone requires being in the main dir
source("r-scripts/prelim_code.R")

# Store data file names
wd <- getwd()

path <- paste(wd, "/data/pavlovia/", sep = "")
files <- list.files(path = path, full.names = TRUE, pattern = "*.csv")

# Merge data files
data <- files %>%
  map_df(~ suppressMessages(read_csv(.,
    # col_types = cols(.default = "c"),
    col_types = cols(.default = "?", First.Outcome.Reported = "c"),
    name_repair = "universal",
    show_col_types = FALSE,
    id = "file_path"
  )))

# Fix Column
data$First.Outcome.Reported <- as.numeric(data$First.Outcome.Reported)

# Convert date column to calendar time
data$date <- as.POSIXct(data$date,
  format = "%Y-%m-%d_%Hh%M.%S",
  tz = "Europe/London"
)

# Sort data frame by date
data <- data[order(data$date), ]

# Absolute order
data$abs_order <- 1:nrow(data)


#Better Columns (for easier analysis)
#-------------------------------------------------------------------------------
# Simple ID column
data <- transform(data,
  ID = as.numeric(factor(PROLIFIC_PID,
    levels = unique(data$PROLIFIC_PID)
  ))
)
# Unique ID
# Note: some incomplete files contain the same PROLIFIC_PID and SESSION_ID
# as completed files.
data$ID_unique <- paste(data$ID, data$date, sep = "_")

# Data size
data$ones <- rep(1, nrow(data))

df_sizes <- data %>%
  group_by(date, ID_unique) %>%
  summarise(nRow = sum(ones))

data <- merge(data, df_sizes[, 2:3], by = "ID_unique", all.x = TRUE)
data <- data[order(data$abs_order), ]

# Condition column
conds <- data %>% drop_na(Condition)
data$Condition_2 <- rep(conds$Condition, conds$nRow)

#WildCard
  #Note: Wildcard info was spread across 4 different columns depending 
  # on condition.
  #This truncates it into two columns:
  #WC1 = Extreme wildcard
  #WC2 = Non-extreme wildcard
  #WC1 and WC2 have no meaning for condition = 2 (i.e., randomly chosen)

data <- data %>%
  mutate(WC1 = case_when(
    Condition_2 == 0 ~ Extreme.Wild.Card,
    Condition_2 == 1 ~ Extreme.Wild.Card,
    Condition_2 == 2 ~ NonExtreme.Wild.Card.1
  ))

data <- data %>%
  mutate(WC2 = case_when(
    Condition_2 == 0 ~ NonExtreme.Wild.Card,
    Condition_2 == 1 ~ NonExtreme.Wild.Card,
    Condition_2 == 2 ~ data$NonExtreme.Wild.Card.2
  ))

conds <- data %>% drop_na(Condition) 

# Door columns
data$LF <- rep(conds$Low.fixed.image..Door1., conds$nRow)
data$LR <- rep(conds$Low.risky.image..Door2., conds$nRow)
data$HF <- rep(conds$High.fixed.image..Door3., conds$nRow)
data$HR <- rep(conds$High.risky.image..Door4., conds$nRow)
data$WC1 <- rep(conds$WC1, conds$nRow) # replacing earlier WC1
data$WC2 <- rep(conds$WC2, conds$nRow) # replacing earlier WC2

# Left alternative column
data <- data %>%
  mutate(left_alt = case_when(
    LF == Left.Option.Image ~ "LF",
    LR == Left.Option.Image ~ "LR",
    HF == Left.Option.Image ~ "HF",
    HR == Left.Option.Image ~ "HR",
    WC1 == Left.Option.Image ~ "WC1",
    WC2 == Left.Option.Image ~ "WC2"
  ))

# Right alternative column
data <- data %>%
  mutate(right_alt = case_when(
    LF == Right.Option.Image ~ "LF",
    LR == Right.Option.Image ~ "LR",
    HF == Right.Option.Image ~ "HF",
    HR == Right.Option.Image ~ "HR",
    WC1 == Right.Option.Image ~ "WC1",
    WC2 == Right.Option.Image ~ "WC2"
  ))

# Choice column
data$choice <- paste(data$left_alt, data$right_alt, sep = "_")

# Response column
data <- data %>%
  mutate(response = case_when(
    Chosen.Door == "Door1" ~ "LF",
    Chosen.Door == "Door2" ~ "LR",
    Chosen.Door == "Door3" ~ "HF",
    Chosen.Door == "Door4" ~ "HR",
    Chosen.Door == "WildCard_NonEx" ~ "WC",
    Chosen.Door == "WildCard_Ex" ~ "WC"
  ))

#Number of trials complete
trials <- data %>% 
  drop_na(trial.ran) %>%
  group_by(ID_unique) %>%
  summarise(
    trial_tot = sum(trial.ran)
  )

data <- merge(data, trials, by = "ID_unique", all.x = TRUE)
data <- data[order(data$abs_order), ]

# Phase column
data <- data %>%
  mutate(
    phase = case_when(
      Block >= 0 ~ "choice",
      is.na(Shown.Door..First.Outcome.) == FALSE ~ "FO_recall",
      is.na(Shown.Door) == FALSE ~ "Freq_judge"
    )
  )

# First outcome info
data <- data %>%
  mutate(FO_stim = case_when(
    Shown.Door..First.Outcome. == "Door1" ~ "LF",
    Shown.Door..First.Outcome. == "Door2" ~ "LR",
    Shown.Door..First.Outcome. == "Door3" ~ "HF",
    Shown.Door..First.Outcome. == "Door4" ~ "HR",
    Shown.Door..First.Outcome. == "WC_nonExtreme" ~ "WC2",
    Shown.Door..First.Outcome. == "WC_extreme" ~ "WC1"
  ))

# Frequency judgement info
data <- data %>%
  mutate(FJ_stim = case_when(
    Shown.Door == "Door1" ~ "LF",
    Shown.Door == "Door2" ~ "LR",
    Shown.Door == "Door3" ~ "HF",
    Shown.Door == "Door4" ~ "HR",
    Shown.Door == "WC_nonExtreme" ~ "WC2",
    Shown.Door == "WC_extreme" ~ "WC1"
  ))

# Remove percent signs
data$FQJ.for.first.outcome <- as.numeric(gsub(
  "[\\%,]", "",
  data$FQJ.for.first.outcome
))
data$FQJ.for.second.outcome <- as.numeric(gsub(
  "[\\%,]", "",
  data$FQJ.for.second.outcome
))


# Add demographic information
#-------------------------------------------------------------------------------
# Store data file names
demog_files <- paste(wd,
  "/data/prolific/",
  list.files(path = "data/prolific", pattern = "*.csv"),
  sep = ""
)

# Merge data files
demog <-
  demog_files %>%
  map_df(~ suppressMessages(read_csv(.,
    name_repair = "universal",
    col_types = cols(.default = "?", Age = "c"),
    show_col_types = FALSE
  )))

names(demog)[names(demog) == "Participant.id"] <- "PROLIFIC_PID"

data <- merge(data, demog, by = "PROLIFIC_PID", all.x = TRUE)
data <- data[order(data$abs_order), ]

# Remove files revoking consent
cr_files <- data %>%
  filter(Age == "CONSENT_REVOKED")

data <- filter(data, Age != "CONSENT_REVOKED")

cr_files <- unique(cr_files$file_path)

if (length(cr_files) > 0) {
  file.remove(cr_files)
}


#Data file
#------------------------------------------------------------------------------
#Get useful columns
data <- data %>%
  select(
    abs_order,
    date,
    ID,
    PROLIFIC_PID,
    STUDY_ID,
    SESSION_ID,
    Completion.code,
    Time.taken,
    Age,
    Sex,
    Language,
    Fluent.languages,
    Ethnicity.simplified,
    Country.of.birth,
    Country.of.residence,
    Nationality,
    Student.status,
    Employment.status,
    nRow,
    Condition_2,
    phase,
    Block,
    trial.thisRepN,
    trial.ran,
    trial_tot,
    left_alt,
    right_alt,
    Trial.Type,
    
    # Low.fixed.image..Door1.,
    # LF,
    # Low.risky.image..Door2.,
    # LR,
    # High.fixed.image..Door3.,
    # HF,
    # High.risky.image..Door4.,
    # HR,
    # Extreme.Wild.Card,
    # NonExtreme.Wild.Card,
    # NonExtreme.Wild.Card.1,
    # NonExtreme.Wild.Card.2,
    # WC1,
    # WC2,
    # Left.Option.Image,
    # Right.Option.Image,
    
    choice,
    response,
    mouse.time,
    Feedback,
    Cumulative.Reward,
    Money.conversion,
    FO_stim,
    First.Outcome.Reported,
    #Shown.Door..First.Outcome.,
    FJ_stim,
    FQJ.for.first.outcome,
    FQJ.for.second.outcome,
    #Shown.Door,
    #Displayed.Image
    )

#rename columns
lookup <- c(
  row = 'abs_order',
  date_time = 'date',
  condition = 'Condition_2',
  block = 'Block',
  trial = 'trial.thisRepN',
  trial_ran = 'trial.ran',
  trial_type = 'Trial.Type',
  resp_time = 'mouse.time',
  feedback = 'Feedback',
  cuml_reward = 'Cumulative.Reward',
  money_tot = 'Money.conversion',
  FO_resp = 'First.Outcome.Reported',
  FJ1_resp = 'FQJ.for.first.outcome',
  FJ2_resp = 'FQJ.for.second.outcome',
  
  age = 'Age',
  sex = 'Sex',
  language = 'Language',
  fluent_languages = 'Fluent.languages',
  ethnicity = 'Ethnicity.simplified',
  country_born = 'Country.of.birth',
  country_resid = 'Country.of.residence',
  nationality = 'Nationality',
  student = 'Student.status',
  employment = 'Employment.status',
  complete_code = 'Completion.code',
  exp_dur = 'Time.taken'
)

data <- rename(data, all_of(lookup))
data <- data[order(data$row), ]

#Cleaned Data
# Note: Contains aborted/incomplete results
write_csv(data, 'Data/wc_full_data.csv')

