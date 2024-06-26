# setwd('../..') #Running this R script alone requires being in the main dir
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")

# Factor and rename conditions
ctch_res$condition <- factor(ctch_res$condition)
levels(ctch_res$condition) <- c(
  "Extreme 1st",
  "Extreme Last",
  "No Extreme"
)

#Plot
dodge <- position_dodge(.3)

plt_catch <- ggplot(ctch_res, aes(x = block, y = cp)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(
    stat = "summary", fun = mean,
    aes(group = condition, colour = condition),
    linewidth = 2,
    position = dodge
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    fun.args = list(conf.int = .95),
    width = 0.2,
    aes(group = condition),
    colour = "black",
    linewidth = 1,
    position = dodge
  ) +
  geom_point(
    stat = "summary", fun = mean,
    aes(fill = condition, shape = condition),
    size = 7,
    stroke = 2,
    position = dodge
  ) +
  scale_shape_manual(values = 21:23) +
  scale_colour_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_x_continuous(breaks = seq(1, 7, 1)) +

  # coord_cartesian(ylim = c(0, 1)) +
  xlab("Block") +
  ylab("p(High Value)") +
  labs(
    colour = "Condition",
    shape = "Condition",
    fill = "Condition"
  ) +
  guides(shape = guide_legend(position = 'inside')) +
  theme_custom() +
  theme(legend.position.inside = c(0.7, 0.3))

#Save Plot  
ggsave('plots/choice-trials/catch_results.png',
       plot = plt_catch,
       units = 'in', width = 16, height = 9, 
       dpi = 500)

ggsave('plots/choice-trials/catch_results.svg',
       plot = plt_catch,
       units = 'in', width = 16, height = 9)
    
    
