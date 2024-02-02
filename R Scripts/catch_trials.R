#setwd('..') #Running this R script alone requires being in the main dir.
source('R Scripts/prelim_code.R')

data <- read_csv('Data/wc_full_data.csv',
                 col_types = cols(.default = '?', 
                                  money_tot = 'c'))

ctch <- filter(data, trial_type == 'ctch' & trial_tot == 300)
#names(ctch)

ctch <- ctch %>% 
  select(-PROLIFIC_PID, 
         -STUDY_ID, 
         -SESSION_ID,
         -(money_tot:FJ2_resp))

#Correct Choice
ctch <- ctch %>%
  mutate(corr_resp = case_when(response == 'HF' | response == 'HR' ~ 1,
                               response == 'LF' | response == 'LR' ~ 0))

#Subject Data
subject_res <- ctch %>%
  group_by(condition, ID, block) %>%
  summarise(
    cp = mean(corr_resp)
  )

#Catch Exclusions
exclude <- filter(subject_res, block == 7 & cp < 0.6)
subject_res <- filter(subject_res, !(ID %in% exclude$ID))
exclude

#Factor and rename conditions
subject_res$condition <- factor(subject_res$condition)
levels(subject_res$condition) <- c('Extreme 1st',
                                   'Extreme Last',
                                   'No Extreme')

#Plot
dodge <- position_dodge(.3)

plt_catch <- ggplot(subject_res, aes(x = block, y = cp)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(stat = "summary", fun = mean,
            aes(group = condition, 
                colour = condition),
            linewidth = 2,
            position = dodge) +
  
  geom_errorbar(stat = "summary",
                fun.data = "mean_cl_normal",
                fun.args = list(conf.int = .95),
                width = 0.2,
                aes(group = condition),
                colour = 'black',
                linewidth = 1,
                position = dodge) +
  
  geom_point(stat = "summary", fun = mean,
             aes(fill = condition,
                 shape = condition),
             size = 7,
             stroke = 2,
             position = dodge) +
  
  scale_shape_manual(values = 21:23) +
  scale_colour_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +

  scale_x_continuous(breaks = seq(1, 7, 1)) +
  
  #coord_cartesian(ylim = c(0, 1)) +
  xlab('Block') + ylab('p(High Value)') +
  labs(colour = 'Condition',
       shape = 'Condition',
       fill = 'Condition'
       ) +
  
  theme_custom() +
  theme(legend.position = c(0.7, 0.3))

#Save Plot  
ggsave('Plots/catch_results.png',
       plot = plt_catch,
       units = 'in', width = 16, height = 9, 
       dpi = 500)

ggsave('Plots/catch_results.svg',
       plot = plt_catch,
       units = 'in', width = 16, height = 9)
    
    
