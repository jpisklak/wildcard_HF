# setwd('../..')
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/choice-trials/risk_pref_filter.R")


# Conventional EO Plot by Block
#------------------------------------------------------------------------------
dodge <- 0.25

plt_risky_std_blks <- ggplot(risky_res, aes(
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
    alpha = 0.25,
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_point(
    stat = "summary", fun = mean,
    stroke = 2,
    size = 5,
    position = position_dodge(width = dodge)
  ) +
  facet_wrap(~ condition) +
  annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, linewidth = 1) +
  scale_colour_manual(values = c("#ff6961", "#77dd77")) +
  scale_fill_manual(values = c("#ff6961", "#77dd77")) +
  scale_shape_manual(values = 21:23) +
  xlab("Block") +
  ylab("p(Risky)") +
  labs(shape = "Choice Value:",
       fill = "Choice Value:",
       colour = "Choice Value:") +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 20),
    legend.position = 'bottom'
    )


# Save Plot
ggsave("plots/choice-trials/plt_risky_blks.png",
  plot = plt_risky_std_blks,
  units = "in", width = 16, height = 9,
  dpi = 500
)

ggsave("plots/choice-trials/plt_risky_blks.svg",
  plot = plt_risky_std_blks,
  units = "in", width = 16, height = 9
)


