# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/freq-judge
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/freq-judge/freq_judge_filter.R")
#-------------------------------------------------------------------------------

# Plot (Means and 95% CI)
#-------------------------------------------------------------------------------
dodge <- position_dodge(.9)

fj_long_rename <- fj_long
levels(fj_long_rename$condition) <- c("No Extreme", "Extreme First", "Extreme Last")

# New Colour levels
brewer.pal(n = 8, name = "Dark2")

fj_long_rename$colour_col <- paste(fj_long_rename$condition, fj_long_rename$FJ_outcome,
                                 sep = "_")
fj_long_rename$colour_col <- factor(fj_long_rename$colour_col)
levels(fj_long_rename$colour_col)

col_palette <- c("#D95F02", "white", "white", "#D95F02",
                 "#7570B3", "white", "white", "#7570B3",
                 "#1B9E77", "white", "white", "#1B9E77")

# Plot
plt_fj_means <- ggplot(fj_long_rename, aes(
  x = FJ_outcome, y = FJ_resp,
  fill = colour_col, group = condition
)) +
  #geom_hline(yintercept = 50, linetype = 3) +
  geom_bar(
    stat = "summary", fun = mean,
    colour = "black",
    linewidth = 1,
    position = dodge
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    fun.args = list(conf.int = .95),
    width = 0.2,
    colour = "black",
    linewidth = 1,
    position = dodge
  ) +
  scale_fill_manual(values = col_palette) +
  #facet_wrap(~FJ_context, scales = "free_x") +
  facet_grid2(FJ_context ~ condition, scales = 'free_x', independent = 'x') +
  coord_cartesian(ylim = c(0, 100)) +
  xlab("") +
  ylab("Judged Percentage") +
  labs(fill = "Condition") +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 28),
    strip.text.x = element_text(size = 26),
    strip.text.y = element_text(
      size = 26,
      margin = unit(c(0, 0, 0, 5), "mm")),
    legend.position = "none",
    panel.spacing.x = unit(4, "lines"),
    panel.spacing.y = unit(2, "lines")
  )

# Save Plot
ggsave("plots/freq-judge/plt_fj_means.png",
  plot = plt_fj_means,
  units = "in", width = 11, height = 8,
  dpi = 500
)

ggsave("plots/freq-judge/plt_fj_means.svg",
  plot = plt_fj_means,
  units = "in", width = 11, height = 8
)

# Acommpanying table
fj_bar_tab <- fj_long %>%
  group_by(FJ_context, FJ_outcome, condition) %>%
  summarise(
    n = length(FJ_resp),
    mean = mean(FJ_resp),
    sd = sd(FJ_resp),
    median = median(FJ_resp),
    IQR = IQR(FJ_resp)
  )

lookup <- c(context = "FJ_context", outcome = "FJ_outcome")
fj_bar_tab <- rename(fj_bar_tab, all_of(lookup))
