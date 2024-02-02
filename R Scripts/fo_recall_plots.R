#setwd('..') #Running this R script alone requires being in the main dir.
source('R Scripts/catch_trials.R') #Loads data and exclusions

fo <- filter(data, phase == 'FO_recall' &
               !(ID %in% exclude$ID) #Remove catch trial exclusions
             ) %>% 
  select(-PROLIFIC_PID,
         -STUDY_ID, 
         -SESSION_ID,
         -(block:money_tot),
         -(FJ_stim:FJ2_resp)
)

#Rename condition levels
fo$condition <- factor(fo$condition)
levels(fo$condition) <- c('Extreme 1st',
                          'Extreme Last',
                          'No Extreme')

#Create context column
fo <- fo %>%
  mutate(FO_context = case_when(FO_stim %in% c('LF', 'LR') ~ 'Low',
                                FO_stim %in% c('HF', 'HR') ~ 'High',
                                FO_stim %in% c('WC1', 'WC2') ~ 'WC'
                                ))

#Exclude fixed and wildcard outcomes (so they don't contaminate 'other')'
fo <- filter(fo, FO_stim %in% c('LR', 'HR'))

#Create category column for results
fo <- fo %>%
  mutate(FO_cat = case_when(FO_context == 'Low' & FO_resp == 20 ~ '+20',
                            FO_context == 'Low' & FO_resp == 40 ~ '+40',
                            FO_context == 'High' & FO_resp == 60 ~ '+60',
                            FO_context == 'High' & FO_resp == 80 ~ '+80',
                            FO_context == 'Low' & FO_resp != c(20, 40) ~ 'Other',
                            FO_context == 'High' & FO_resp != c(60, 80) ~ 'Other'
  ))

#Proportion results
props <- fo %>%
  group_by(FO_context, condition) %>%
  count(FO_cat)


totals <- props %>%
  group_by(FO_context, condition) %>%
  summarise(
    total = sum(n)
  )

props <- merge(props, totals, by = c("FO_context", 'condition'),
              all.x = TRUE)

props$prop <- props$n / props$total


#Reorder context levels
props$FO_context <- factor(props$FO_context, 
                           levels = c('Low',
                                      'High'))

#Plot
plt_fo_prop <- ggplot(props, aes(x = FO_cat, y = prop, group = condition)) +
  geom_bar(stat = "identity",
           aes(fill = condition),
           colour = 'black',
           linewidth = 1,
           position = "dodge") +
  
  facet_wrap(~ FO_context, scales = 'free_x') +
  
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  
  xlab('Outcome') + ylab('p(Reported)') +
  labs(fill = 'Condition') +
  
  theme_custom() +
  theme(legend.position = 'right',
        panel.spacing.x = unit(4, "lines"))

#Save Plot  
ggsave('Plots/plt_fo_prop.png', 
       plot = plt_fo_prop,
       units = 'in', width = 16, height = 9, 
       dpi = 500)

ggsave('Plots/plt_fo_prop.svg', 
       plot = plt_fo_prop,
       units = 'in', width = 16, height = 9)











