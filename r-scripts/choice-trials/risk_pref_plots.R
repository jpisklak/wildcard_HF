# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/choice-trials
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")

# Conventional EO Plot by Block
#------------------------------------------------------------------------------
dodge <- 0.25

plt_risky_blk <- ggplot(risky_res, aes(
  x = block, y = cp,
  group = risky_choice,
  shape = risky_choice,
  fill = risky_choice
)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(
    stat = "summary", fun = mean,
    aes(colour = risky_choice),
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    # fun.data = "mean_cl_boot",
    fun.args = list(conf.int = .95),
    # fun.args = list(conf.int = .95, B = 5000),
    width = 0.2,
    # aes(group = condition),
    colour = "black",
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_point(
    stat = "summary", fun = mean,
    stroke = 1.5,
    size = 5,
    position = position_dodge(width = dodge)
  ) +
  facet_wrap(~ condition) +
  annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, linewidth = 1) +
  scale_colour_manual(values = c("#ff6961", "#77dd77")) +
  scale_fill_manual(values = c("#ff6961", "#77dd77")) +
  scale_shape_manual(values = 21:23) +
  coord_cartesian(ylim = c(0, 0.8)) +
  xlab("Block") +
  ylab("p(Risky)") +
  labs(shape = "Choice Value:",
       fill = "Choice Value:",
       colour = "Choice Value:") +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 28),
    strip.text = element_text(size = 30),
    legend.position = 'bottom'
    )


# Save Plot
ggsave("plots/choice-trials/plt_risky_blk.png",
  plot = plt_risky_blk,
  units = "in", width = 11, height = 7,
  dpi = 500
)

ggsave("plots/choice-trials/plt_risky_blk.svg",
  plot = plt_risky_blk,
  units = "in", width = 11, height = 7
)



# Block 7 Difference Scores
#------------------------------------------------------------------------------

plt_risky_diff <- ggplot(diffs_b7, aes(x = condition, y = diff)) +
  geom_bar(stat = "summary", fun = mean,
           aes(fill = condition),
           colour = "black",
           linewidth = 1
           ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    fun.args = list(conf.int = .95),
    # fun.args = list(conf.int = .95, B = 5000),
    width = 0.2,
    # aes(group = condition),
    colour = "black",
    linewidth = 1
  ) +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  xlab("Condition") +
  ylab("p(Risky: High - Low)") +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 22),
    legend.position = 'none'
  )

# Save Plot
ggsave("plots/choice-trials/plt_risky_diff.png",
       plot = plt_risky_diff,
       units = "in", width = 8, height = 7,
       dpi = 500
)

ggsave("plots/choice-trials/plt_risky_diff.svg",
       plot = plt_risky_diff,
       units = "in", width = 9, height = 7
)
