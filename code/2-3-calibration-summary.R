# ------------------------------------------------------------------
# Compute calibration summary statistics for Section 2.1 of the
# manuscript (true-data-generation realism figures), and save a small
# summary object the manuscript reads inline. This avoids hard-coding
# calibration numbers in the prose and keeps them reproducible.
#
# Source data: per-field simulated analysis data in
#   data/main/individual-fields/sim_data_*.rds
# (subplot-level: yield, N_tgt, and quadratic-plateau coefficients
#  b0, b1, b2, Nk).
#
# Output: writing/figures-tables/calibration_summary.rds
# ------------------------------------------------------------------

library(data.table)

#--- prices, identical to code/2-1-summarize-results-main.qmd ---#
pCorn <- 6.25 / 25.4    # $/kg  (= $6.25/bushel)
pN <- 1 / 0.453592      # $/kg  (= $1.00/lb)

field_dir <- here::here("data/main/individual-fields")
files <- list.files(field_dir, pattern = "^sim_data_.*\\.rds$", full.names = TRUE)

collect <- vector("list", length(files))

for (i in seq_along(files)) {
  d <- readRDS(files[i])$data[[1]]
  setDT(d)

  #--- subplot-level true EONR under the quadratic-plateau response ---#
  # unconstrained profit-max N = (w/p - b1) / (2 b2), clipped to [0, Nk]
  d[, opt_N_cell := pmin(pmax((pN / pCorn - b1) / (2 * b2), 0), Nk)]

  trial_rates <- sort(unique(d[!is.na(N_tgt), N_tgt]))

  collect[[i]] <- data.table(
    sim = i,
    n_rates = length(trial_rates),
    rate_rank = seq_along(trial_rates),
    trial_rate = trial_rates,
    mean_yield_sim = d[, mean(yield, na.rm = TRUE)],
    eonr_cell_min = d[, min(opt_N_cell, na.rm = TRUE)],
    eonr_cell_max = d[, max(opt_N_cell, na.rm = TRUE)],
    eonr_sim_mean = d[, mean(opt_N_cell, na.rm = TRUE)]
  )
}

all <- rbindlist(collect)

#--- average trial rate by rank across simulations ---#
avg_trial_rates <- all[, .(avg_rate = mean(trial_rate)), by = rate_rank][order(rate_rank)]

calibration_summary <- list(
  n_sims               = length(files),
  n_trial_rates        = all[, as.integer(round(mean(n_rates)))],
  avg_trial_rates      = round(avg_trial_rates$avg_rate, 0),
  mean_yield           = all[, round(mean(mean_yield_sim), 0)],            # kg/ha, across all sims
  eonr_cell_min        = all[, round(min(eonr_cell_min), 0)],              # min cell-level true EONR
  eonr_cell_max        = all[, round(max(eonr_cell_max), 0)],              # max cell-level true EONR
  eonr_simmean_min     = all[, round(min(eonr_sim_mean), 0)],             # min of per-sim field-mean EONR
  eonr_simmean_max     = all[, round(max(eonr_sim_mean), 0)]              # max of per-sim field-mean EONR
)

print(calibration_summary)

saveRDS(
  calibration_summary,
  here::here("writing/figures-tables/calibration_summary.rds")
)
