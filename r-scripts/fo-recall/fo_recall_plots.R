# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/fo-recall
# source("r-scripts/prelim_code.R")
# source("r-scripts/subj_stats.R")
# source("r-scripts/fo-recall/fo_recall_filter.R")
#-------------------------------------------------------------------------------

props$FO_context <- factor(props$FO_context, levels = c("High", "Low"))

props_rename <- props
levels(props_rename$condition) <- 
  c("No Extreme", "Extreme First", "Extreme Last")


# Plot
plt_fo_prop <- ggplot(props_rename, aes(x = FO_cat, y = prop, group = condition)) +
  #geom_hline(yintercept = 0.5, linetype = 3) +
  geom_bar(
    stat = "identity",
    aes(fill = condition),
    colour = "black",
    linewidth = 1,
    position = "dodge"
  ) +
  #facet_wrap(FO_context ~ condition, scales = 'free_x') +
  facet_grid2(FO_context ~ condition, scales = 'free_x', independent = 'x') +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  xlab("Outcome") +
  ylab("p(Reported)") +
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
ggsave("plots/fo-recall/plt_fo_prop.png",
  plot = plt_fo_prop,
  units = "in", width = 11, height = 8,
  dpi = 500
)

ggsave("plots/fo-recall/plt_fo_prop.svg",
  plot = plt_fo_prop,
  units = "in", width = 11, height = 8
)
