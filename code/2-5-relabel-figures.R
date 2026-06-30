# Regenerate the three figures whose unit labels were corrected during
# review round 01: g_pi_loss and g_pi_loss_nosp ($/acre -> $/ha) and
# g_gam_leonr (lb/acre -> kg/ha, and axis mapping aligned with labels:
# x = GAM-estimated, y = true). Mirrors the chunks in
# code/3-2-present-results.qmd.

suppressPackageStartupMessages({
  library(data.table); library(tidyverse); library(ggplot2)
})

pl_from <- function(path) {
  cr <- as.data.table(readRDS(here::here(path)))
  d <- rbindlist(lapply(seq_len(nrow(cr)), function(i) {
    x <- as.data.table(cr$loss_data[[i]])
    x[, `:=`(num_folds = cr$num_folds[i], num_repeats = cr$num_repeats[i])]
    x
  }))
  d <- melt(d[, .(sim, num_folds, num_repeats, e_pi_loss, y_pi_loss)],
            id.var = c("sim", "num_folds", "num_repeats"))
  d[, selection := fifelse(variable == "e_pi_loss",
                           "LEONR-based Selection", "Yield-based Selection")]
  d[]
}

mk_loss_plot <- function(d) {
  ggplot(d) +
    geom_histogram(aes(x = value,
                       y = ifelse(after_stat(count) > 0, after_stat(count), NA)),
                   color = "blue", fill = "white") +
    facet_wrap(selection ~ ., ncol = 1) +
    ylab("Number of iterations") +
    xlab("Profit loss relative to the true best model ($/acre)") +
    theme_bw()
}

# main case (10 folds, 5 repeats)
pld <- pl_from("results/main/comp_results.rds")
ggsave(here::here("writing/figures-tables/g_pi_loss.png"),
       mk_loss_plot(pld[num_folds == 10 & num_repeats == 5]),
       dpi = 300, width = 6, height = 3.5)

# no-spatial-correlation case (single case in the file)
pld_nosp <- pl_from("results/no-sp/comp_results.rds")
ggsave(here::here("writing/figures-tables/g_pi_loss_nosp.png"),
       mk_loss_plot(pld_nosp),
       dpi = 300, width = 6, height = 3.5)

# GAM vs true local EONR (main case): x = GAM-estimated, y = true
srg <- as.data.table(readRDS(here::here("results/main/sum_results_gam.rds")))
g_gam <- ggplot(srg[num_folds == 10 & num_repeats == 5]) +
  geom_point(aes(x = opt_N_gam, y = opt_N), size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  xlim(50, 300) + ylim(50, 300) +
  ylab("True optimal Local EONR (kg/ha)") +
  xlab("GAM-estimated optimal Local EONR (kg/ha)") +
  theme_bw() + theme(legend.position = "bottom")
ggsave(here::here("writing/figures-tables/g_gam_leonr.png"),
       g_gam, width = 5, height = 3.5, dpi = 300)

cat("regenerated g_pi_loss, g_pi_loss_nosp, g_gam_leonr\n")
