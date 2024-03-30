# setwd('..') #Running this R script alone requires being in the main dir.
source("r-scripts/subj_stats.R") # Loads data and exclusions

fj <- filter(
  data, phase == "Freq_judge",
  !(ID %in% exclude$ID), # Remove catch trial exclusions
  !(ID %in% fj_comp$ID)
) %>%
  select(
    -PROLIFIC_PID,
    -STUDY_ID,
    -SESSION_ID,
    -(block:money_tot),
    -(FO_stim:FO_resp)
  )

# Rename condition levels
fj$condition <- factor(fj$condition)
levels(fj$condition) <- c(
  "Extreme 1st",
  "Extreme Last",
  "No Extreme"
)

# Create context column
fj <- fj %>%
  mutate(FJ_context = case_when(
    FJ_stim %in% c("LF", "LR") ~ "Low",
    FJ_stim %in% c("HF", "HR") ~ "High",
    FJ_stim %in% c("WC1", "WC2") ~ "WC"
  ))

# Exclude wildcard and fixed outcomes
fj <- filter(fj, FJ_stim %in% c("LR", "HR"))

# Create outcome column to easily categorize results
fj_wide <- fj %>%
  select(
    ID,
    condition,
    FJ_stim,
    FJ1_resp,
    FJ2_resp,
    FJ_context
  )

fj_long <- pivot_longer(fj_wide, c(FJ1_resp, FJ2_resp),
  names_to = "FJ_order",
  values_to = "FJ_resp"
)


fj_long <- fj_long %>%
  mutate(FJ_outcome = case_when(
    FJ_stim == "LR" & FJ_order == "FJ1_resp" ~ "+20",
    FJ_stim == "LR" & FJ_order == "FJ2_resp" ~ "+40",
    FJ_stim == "HR" & FJ_order == "FJ1_resp" ~ "+60",
    FJ_stim == "HR" & FJ_order == "FJ2_resp" ~ "+80"
  ))


# Reorder context levels
fj_long$FJ_context <- factor(fj_long$FJ_context,
  levels = c(
    "Low",
    "High"
  )
)


# Plot (Means and 95% CI)
#-------------------------------------------------------------------------------
# Note data is qualitative and thus probably shouldn't be plotted this way.
dodge <- position_dodge(.9)

plt_fj_means <- ggplot(fj_long, aes(
  x = FJ_outcome, y = FJ_resp,
  fill = condition, group = condition
)) +
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
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  facet_wrap(~FJ_context, scales = "free_x") +
  coord_cartesian(ylim = c(0, 100)) +
  xlab("") +
  ylab("Judged Percentage") +
  labs(fill = "Condition") +
  theme_custom() +
  theme(
    legend.position = "right",
    panel.spacing.x = unit(3, "lines")
  )

# Save Plot
ggsave("Plots/plt_fj_means.png",
  plot = plt_fj_means,
  units = "in", width = 16, height = 9,
  dpi = 500
)

ggsave("Plots/plt_fj_means.svg",
  plot = plt_fj_means,
  units = "in", width = 16, height = 9
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

# EO Violin Plot
#-------------------------------------------------------------------------------
dodge <- position_dodge(.6)

plt_fj_violin <- ggplot(fj_long, aes(x = FJ_outcome, y = FJ_resp)) +
  geom_hline(yintercept = 50, linetype = 3) +
  geom_violin(aes(fill = condition),
    colour = "black",
    linewidth = 0.5,
    position = dodge
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "pointrange",
    aes(group = condition),
    color = "black",
    size = 1.5,
    linewidth = 1,
    position = dodge
  ) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    aes(group = condition),
    colour = "black",
    width = .5,
    position = dodge
  ) +
  geom_point(aes(shape = condition),
    size = 2.5,
    alpha = 0.2,
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = .6
    )
  ) +
  facet_wrap(~FJ_context, scales = "free_x") +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +

  # coord_cartesian(ylim = c(0, 1)) +
  xlab("") +
  ylab("Judged Percentage") +
  labs(
    fill = "Condition:",
    shape = "Condition:"
  ) +
  theme_custom() +
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(colour = "white")
  )

# Save Plot
ggsave("Plots/plt_fj_violin.png",
  plot = plt_fj_violin,
  units = "in", width = 16, height = 9,
  dpi = 500
)

ggsave("Plots/plt_fj_violin.svg",
  plot = plt_fj_violin,
  units = "in", width = 16, height = 9
)
