# Run the following 4 lines to execute this script independently
# setwd('../..')
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/fo-recall/fo_recall_filter.R")

# Plot
plt_fo_prop <- ggplot(props, aes(x = FO_cat, y = prop, group = condition)) +
  geom_bar(
    stat = "identity",
    aes(fill = condition),
    colour = "black",
    linewidth = 1,
    position = "dodge"
  ) +
  facet_wrap(~FO_context, scales = "free_x") +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  xlab("Outcome") +
  ylab("p(Reported)") +
  labs(fill = "Condition") +
  theme_custom() +
  theme(
    legend.position = "right",
    panel.spacing.x = unit(4, "lines")
  )

# Save Plot
ggsave("plots/fo-recall/plt_fo_prop.png",
  plot = plt_fo_prop,
  units = "in", width = 16, height = 9,
  dpi = 500
)

ggsave("plots/fo-recall/plt_fo_prop.svg",
  plot = plt_fo_prop,
  units = "in", width = 16, height = 9
)
