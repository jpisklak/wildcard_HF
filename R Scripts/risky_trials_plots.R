#setwd('..') #Running this R script alone requires being in the main dir.
source('R Scripts/catch_trials.R') #Loads data and exclusions

risky <- filter(data, trial_type == 'decisionTrl' & 
                  trial_tot == 300 & 
                  #block == 7 &
                  !(ID %in% exclude$ID) #Remove catch trial exclusions
                ) %>%
  select(-PROLIFIC_PID,
         -STUDY_ID,
         -SESSION_ID,
         -(money_tot:FJ2_resp)
         )

#Risky choice selection
risky <- risky %>%
  mutate(risky_choice = case_when(choice == 'LF_LR' | choice == 'LR_LF' ~ 'Low',
                                  choice == 'HF_HR' | choice == 'HR_HF' ~ 'High')) %>% 
  drop_na(risky_choice)

#Door chosen
risky <- risky %>%
  mutate(risky_resp = case_when(response == 'LR' | response == 'HR' ~ 1,
                               response == 'LF' | response == 'HF' ~ 0))

#Subject Data
subject_res <- risky %>%
  group_by(ID, condition, block, risky_choice) %>%
  summarise(
    cp = mean(risky_resp)
  )

#Factor and rename conditions
subject_res$block <- factor(subject_res$block)

subject_res$condition <- factor(subject_res$condition)
levels(subject_res$condition) <- c('Extreme 1st',
                                   'Extreme Last',
                                   'No Extreme')

subject_res$risky_choice <- factor(subject_res$risky_choice,
                                   levels = c('Low', 'High'))

# Diff Scores
diffs <- subject_res %>%
  group_by(ID, condition, block) %>%
  summarise(
    diff = cp[risky_choice == 'High'] - cp[risky_choice == 'Low']
  )


#Block 7 Only
subject_res_b7 <- filter(subject_res, block == 7)
diffs_b7 <- filter(diffs, block == 7)

# Conventional EO Plot
#-------------------------------------------------------------------------------
plt_risky_std <- ggplot(subject_res_b7, aes(x = risky_choice, y = cp)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_bar(stat = "summary", fun = mean,
           aes(fill = risky_choice),
           colour = 'black',
           linewidth = 1) +
  
  geom_errorbar(stat = "summary",
                fun.data = "mean_cl_normal",
                #fun.data = "mean_cl_boot",
                fun.args = list(conf.int = .95),
                #fun.args = list(conf.int = .95, B = 5000),
                width = 0.2,
                aes(group = condition),
                colour = 'black',
                linewidth = 1) +
  
  facet_wrap(~condition) +
  
  scale_fill_manual(values = c('#ff6961', '#77dd77')) +

  #coord_cartesian(ylim = c(0, 1)) +
  xlab('') + ylab('p(Risky)') +
  
  theme_custom()

#Save Plot
ggsave('Plots/plt_risky_std.png',
       plot = plt_risky_std,
       units = 'in', width = 16, height = 9, 
       dpi = 500)

ggsave('Plots/plt_risky_std.svg',
       plot = plt_risky_std,
       units = 'in', width = 16, height = 9)


# Conventional EO Plot by Block
#-------------------------------------------------------------------------------
dodge = 0.25
plt_risky_std_blks <- ggplot(subject_res, aes(x = block, y = cp,
                                              group = condition,
                                              shape = condition,
                                              fill = risky_choice)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(stat = "summary", fun = mean,
            aes(colour = risky_choice),
            linewidth = 1,
            position = position_dodge(width = dodge)) +
  
  geom_errorbar(stat = "summary",
                fun.data = "mean_cl_normal",
                #fun.data = "mean_cl_boot",
                fun.args = list(conf.int = .95),
                #fun.args = list(conf.int = .95, B = 5000),
                width = 0.2,
                #aes(group = condition),
                colour = 'black',
                alpha = 0.25,
                linewidth = 1,
                position = position_dodge(width = dodge)) +
  
  geom_point(stat = "summary", fun = mean,
             stroke = 2,
             size = 5,
             position = position_dodge(width = dodge)) +

  facet_wrap(~ risky_choice) +
  
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, linewidth = 1) +
  
  scale_colour_manual(values = c('#ff6961', '#77dd77'), guide="none") +
  scale_fill_manual(values = c('#ff6961', '#77dd77'), guide="none") +
  scale_shape_manual(values = 21:23) +
  
  xlab('Block') + ylab('p(Risky)') +
  labs(shape = 'Condition') +
  
  theme_custom() +
  theme(legend.position = c(0.75, 0.30))


#Save Plot
ggsave('Plots/plt_risky_std_blks.png',
       plot = plt_risky_std_blks,
       units = 'in', width = 16, height = 9, 
       dpi = 500)

ggsave('Plots/plt_risky_std_blks.svg',
       plot = plt_risky_std_blks,
       units = 'in', width = 16, height = 9)


#EO Diff Plot
#-------------------------------------------------------------------------------
plt_risky_diff <- ggplot(diffs_b7, aes(x = condition, y = diff)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_bar(stat = "summary", fun = mean,
           aes(fill = condition),
           colour = 'black',
           linewidth = 1) +
  
  geom_errorbar(stat = "summary",
                fun.data = "mean_cl_normal",
                #fun.data = "mean_cl_boot",
                fun.args = list(conf.int = .95),
                #fun.args = list(conf.int = .95, B = 5000),
                width = 0.2,
                aes(group = condition),
                colour = 'black',
                linewidth = 1) +

  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  
  xlab('') + ylab('p(High Risk - Low Risk)') +
  
  theme_custom()

#Save Plot
ggsave('Plots/plt_risky_diff.png',
       plot = plt_risky_diff,
       units = 'in', width = 10, height = 9, 
       dpi = 500)

ggsave('Plots/plt_risky_diff.svg',
       plot = plt_risky_diff,
       units = 'in', width = 10, height = 9)


#EO Diff Plot by Block
#-------------------------------------------------------------------------------
dodge = 0.25
plt_risky_diff_blks <- ggplot(diffs, aes(x = block, y = diff, 
                                         group = condition,
                                         shape = condition,
                                         fill = condition)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_line(stat = "summary", fun = mean,
           aes(colour = condition),
           linewidth = 1,
           position = position_dodge(width = dodge)) +
  
  geom_errorbar(stat = "summary",
                fun.data = "mean_cl_normal",
                #fun.data = "mean_cl_boot",
                fun.args = list(conf.int = .95),
                #fun.args = list(conf.int = .95, B = 5000),
                width = 0.2,
                #aes(group = condition),
                colour = 'black',
                alpha = 0.25,
                linewidth = 1,
                position = position_dodge(width = dodge)) +
  
  geom_point(stat = "summary", fun = mean,
            stroke = 2,
            size = 5,
            position = position_dodge(width = dodge)) +
  
  scale_colour_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_shape_manual(values = 21:23) +
  
  xlab('Block') + ylab('p(High - Low)') +
  labs(shape = 'Condition',
       fill = 'Condition',
       colour = 'Condition') +
  
  theme_custom() +
  theme(legend.position = "right")


#Save Plot
ggsave('Plots/plt_risky_diff_blks.png',
       plot = plt_risky_diff_blks,
       units = 'in', width = 16, height = 9, 
       dpi = 500)

ggsave('Plots/plt_risky_diff_blks.svg',
       plot = plt_risky_diff_blks,
       units = 'in', width = 16, height = 9)



#Plot Combo
#-------------------------------------------------------------------------------
library(patchwork)

plts_risky <- plt_risky_std / plt_risky_diff + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 32, face = 'bold'))

ggsave('Plots/risky_results_full.png',
       plot = plts_risky,
       units = 'in', width = 16, height = 20, 
       dpi = 500)

ggsave('Plots/risky_results_full.svg',
       plot = plts_risky,
       units = 'in', width = 16, height = 20)


# EO Violin Plot
#-------------------------------------------------------------------------------
plt_risky_violin <- ggplot(subject_res_b7, aes(x = risky_choice, y = cp)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_violin(aes(fill = risky_choice),
              colour = 'black',
              linewidth = 0.5) +
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange",
               color = "black",
               size = 1,
               linewidth = 1) +
  
  stat_summary(fun = median, 
               geom = "crossbar",
               colour = 'black',
               width = 0.5
               ) +
  
  geom_jitter(aes(shape = condition), 
              size = 2.5, 
              width = 0.1,
              height = 0,
              alpha = 0.2
              ) +

  facet_wrap(~condition) +
  
  scale_fill_manual(values = c('#ff6961', '#77dd77')) +
  
  #coord_cartesian(ylim = c(0, 1)) +
  xlab('') + ylab('p(Risky)') +
  labs(shape = 'Condition') +
  
  theme_custom()

#Save Plot
ggsave('Plots/plt_risky_violin.png',
       plot = plt_risky_violin,
       units = 'in', width = 16, height = 9, 
       dpi = 500)

ggsave('Plots/plt_risky_violin.svg',
       plot = plt_risky_violin,
       units = 'in', width = 16, height = 9)













