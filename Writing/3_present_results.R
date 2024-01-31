## -----------------------------------------------------------------------------
sum_results_whole <- readRDS(here::here("Shared/Results/sum_results_whole.rds"))

comp_results <-
  readRDS(here::here("Shared/Results/comp_results.rds")) %>%
  data.table()

sum_results_gam <- readRDS(here::here("Shared/Results/sum_results_gam.rds"))


## -----------------------------------------------------------------------------
theme_fig <-
  theme_bw() +
  theme(
    axis.title.x =
      element_text(
        size = 7, angle = 0, hjust = .5, vjust = -0.3, family = "Helvetica"
      ),
    axis.title.y =
      element_text(
        size = 7, angle = 90, hjust = .5, vjust = .9, family = "Helvetica"
      ),
    axis.text.x =
      element_text(
        size = 7, angle = 0, hjust = .5, vjust = 1.5, family = "Helvetica"
      ),
    axis.text.y =
      element_text(
        size = 7, angle = 0, hjust = 1, vjust = 0, family = "Helvetica"
      ),
    axis.ticks =
      element_line(
        linewidth = 0.3, linetype = "solid"
      ),
    axis.ticks.length = unit(.15, "cm"),
    #--- legend ---#
    legend.text =
      element_text(
        size = 7, angle = 0, hjust = 0, vjust = 0.5, family = "Helvetica"
      ),
    legend.title =
      element_text(
        size = 7, angle = 0, hjust = 0, vjust = 0, family = "Helvetica"
      ),
    legend.key.size = unit(0.5, "cm"),
    #--- strip (for faceting) ---#
    strip.text = element_text(size = 7, family = "Helvetica"),
    #--- plot title ---#
    plot.title = element_text(family = "Helvetica", face = "bold", size = 7),
    #--- margin ---#
    # plot.margin = margin(0, 0, 0, 0, "cm"),
    #--- panel ---#
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA)
  )


## -----------------------------------------------------------------------------
true_rank <-
  sum_results_whole[, .(N = sum(eonr_selected_true)), by = method] %>%
  .[, type := "True"]


model_selection_ranks <-
  readRDS(here::here("Shared/Results/selection_ranks.rds")) %>%
  .[, .(N_LE = sum(eonr_selected_gam), N_Y = sum(yield_selected)), by = method] %>%
  melt(id.var = "method", value.name = "N") %>%
  .[, type := case_when(
    variable == "N_LE" ~ "Local EONR",
    variable == "N_Y" ~ "Yield"
  )] %>%
  .[, variable := NULL]

all_ranks <- rbind(true_rank, model_selection_ranks)

g_ranking <-
  ggplot(all_ranks) +
  geom_bar(
    aes(y = N, x = method, fill = type),
    stat = "identity",
    position = "dodge"
  ) +
  scale_fill_viridis_d() +
  # Add labels and titles
  labs(x = "Model", y = "Count", fill = "Selection Type") +
  theme_fig +
  theme(
    legend.position = "bottom"
  )

# ggsave(
#   "GitControlled/Writing/Figures/g_ranking.png",
#   g_ranking,
#   width = 6,
#   height = 3.5,
#   dpi = 600
# )


## -----------------------------------------------------------------------------
accuracy_table <-
  comp_results %>%
  .[, .(num_folds, num_repeats, comp_summary)] %>%
  unnest() %>%
  data.table() %>%
  # .[eonr_selected_true == 1, ] %>%
  .[, .(
    num_selected_true = sum(eonr_selected_true),
    #--- number of times eonr-based selection correctly chose the best model by model ---#
    num_selected_leonr_correct = sum(eonr_selected_true == 1 & eonr_selected_gam),
    num_selected_leonr = sum(eonr_selected_gam),
    #--- number of times yield-based selection correctly chose the best model by model ---#
    num_selected_yield_correct = sum(eonr_selected_true == 1 & yield_selected),
    num_selected_yield = sum(yield_selected)
  ),
  by = .(method, num_folds, num_repeats)
  ]

main_accuracy_data <-
  accuracy_table[num_folds == 10 & num_repeats == 10, ] %>%
  .[, leonr_based := paste0(num_selected_leonr_correct, " (", num_selected_leonr, ")")] %>%
  .[, yield_based := paste0(num_selected_yield_correct, " (", num_selected_yield, ")")] %>%
  .[, .(method, num_selected_true, leonr_based, yield_based)]

main_accuracy_table <-
  flextable(main_accuracy_data) %>%
  autofit() %>%
  align(j = 2:4, align = "center")


## -----------------------------------------------------------------------------
profit_loss_data <-
  comp_results[, .(num_folds, num_repeats, loss_data)] %>%
  unnest() %>%
  data.table() %>%
  .[, .(sim, num_folds, num_repeats, e_pi_loss, y_pi_loss)] %>%
  melt(id.var = c("sim", "num_folds", "num_repeats")) %>%
  .[, selection := case_when(
    variable == "e_pi_loss" ~ "LEONR-based Selection",
    variable == "y_pi_loss" ~ "Yield-based Selection"
  )]


## -----------------------------------------------------------------------------
g_pi_loss <-
  ggplot(profit_loss_data[num_folds == 5 & num_repeats == 5, ]) +
  geom_histogram(
    aes(x = value),
    color = "blue",
    fill = "white"
  ) +
  facet_wrap(selection ~ ., ncol = 1) +
  ylab("Count") +
  xlab("Profit Loss ($/acre)") +
  theme_bw()


## -----------------------------------------------------------------------------

profit_loss_data[, .(mean = mean(value), sd = sd(value)), by = .(num_folds, num_repeats, selection)] %>%
  .[selection == "LEONR-based Selection", ] %>%
  .[, mean_sd := paste0(round(mean, digits = 2), "(", round(sd, digits = 2), ")")] %>%
  .[, .(num_folds, num_repeats, mean_sd)] %>%
  dcast(num_repeats ~ num_folds, value.var = "mean_sd")

# ggplot(profit_loss_data[selection == "LEONR-based Selection", ]) +
#   geom_boxplot(aes(y = value, x = factor(num_folds), fill = factor(num_repeats)))


## -----------------------------------------------------------------------------
gam_leonr_fit_all <-
  ggplot(sum_results_gam) +
  geom_point(
    aes(x = opt_N, y = opt_N_gam),
    size = 0.5
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  xlim(50, 300) +
  ylim(50, 300) +
  facet_grid(
    rows = vars(`Number of Folds` = num_folds),
    cols = vars(`Number of Repeats` = num_repeats),
    labeller = labeller(
      `Number of Folds` = label_both,
      `Number of Repeats` = label_both
    )
  ) +
  xlab("True Local EONR") +
  ylab("GAM-Estimated Local EONR") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )


## -----------------------------------------------------------------------------
gam_leonr_fit_main <-
  ggplot(sum_results_gam[num_folds == 5 & num_repeats == 5, ]) +
  geom_point(
    aes(x = opt_N, y = opt_N_gam),
    size = 0.5
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  xlim(50, 300) +
  ylim(50, 300) +
  xlab("True Local EONR") +
  ylab("GAM-Estimated Local EONR") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )


## -----------------------------------------------------------------------------
#--- ranking based on LEONR ---#
perf_ranking_e <-
  comp_results[, .(num_folds, num_repeats, selection_perf_rank_LE)] %>%
  unnest(selection_perf_rank_LE) %>%
  data.table()

#--- ranking based on Yield ---#
perf_ranking_y <-
  comp_results[, .(num_folds, num_repeats, selection_perf_rank_Y)] %>%
  unnest(selection_perf_rank_Y) %>%
  data.table()

# Add a new column "Type" to distinguish between 'e' and 'y'
perf_ranking_e$Selection_Type <- "Local EONR Selection"
perf_ranking_y$Selection_Type <- "Yield Selection"

# Merge the two data frames into one
perf_ranking_combined <- rbind(perf_ranking_e, perf_ranking_y)

# Plot the combined data using dodge position and refined colors
g_sensitivity_fold_repeat <-
  ggplot(perf_ranking_combined) +
  geom_bar(aes(y = N, x = eonr_rank_true, fill = Selection_Type), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Local EONR Selection" = "#333333", "Yield Selection" = "#999999")) + # Set colors
  facet_grid(
    rows = vars(`Number of Folds` = num_folds), cols = vars(`Number of Repeats` = num_repeats),
    labeller = labeller(`Number of Folds` = label_both, `Number of Repeats` = label_both)
  ) +
  labs(
    x = "True EONR Rank",
    y = "Count",
    fill = "Selection Type"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") + # Move the legend to the bottom
  theme(strip.text = element_text(size = 6.5)) +
  theme(
    legend.position = "bottom",
    # plot.title = element_text(size = 10, face = "bold"),  # Title appearance
    # plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(size = 10), # X-axis label appearance
    axis.title.y = element_text(size = 10), # Y-axis label appearance
    legend.title = element_text(size = 11), # Legend title appearance
    legend.text = element_text(size = 10) # Legend text appearance
  ) +
  theme(axis.title = element_text(size = 12, family = "Times"))


## -----------------------------------------------------------------------------
sum_results_gam <- readRDS(here::here("Shared/Results/Mona_results/sum_results_gam.rds"))
main_sum_results <- readRDS(here::here("Shared/Results/Mona_results/main_sum_results.rds"))

# combined results other version
combined_results_other_v <-
  sum_results_gam[main_sum_results, on = c("sim", "split_id")] %>%
  .[, opt_N_dif_select := opt_N_hat - opt_N_gam]


# Group the data by "sim" and "method" columns, and calculate the average of "opt_N_hat" and "opt_N"
averaged_df <- combined_results_other_v %>%
  group_by(sim, method) %>%
  summarize(
    avg_opt_N_hat = mean(opt_N_hat),
    avg_opt_N_gam = mean(opt_N_gam)
  )



# Create separate graphs by method using facet_wrap
ggplot(averaged_df, aes(x = avg_opt_N_gam, y = avg_opt_N_hat)) +
  geom_point(colour = "black", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(100, 180)) +
  scale_y_continuous(limits = c(100, 180)) +
  labs(x = "Average GAM EONR", y = "Average Local EONR") +
  facet_wrap(~method, ncol = 2) +
  theme(strip.text = element_text(size = 6.5)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    # plot.title = element_text(size = 10, face = "bold"),  # Title appearance
    # plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(size = 10), # X-axis label appearance
    axis.title.y = element_text(size = 10), # Y-axis label appearance
    legend.title = element_text(size = 11), # Legend title appearance
    legend.text = element_text(size = 10) # Legend text appearance
  ) +
  theme(axis.title = element_text(size = 12, family = "Times"))

