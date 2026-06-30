# ------------------------------------------------------------------
# Analyses requested during the internal review round 01:
#   (1) Monte Carlo inference on the profit-loss comparison
#       (paired per-iteration LEONR vs yield), plus MC standard
#       errors on the selection success rates.
#   (2) Uniform-vs-variable-rate baseline: profit gain of the TRUE
#       site-specific EONR over the best single uniform N rate, so the
#       model-selection losses can be read as a share of the
#       variable-rate premium.
#
# Output: writing/figures-tables/review_stats.rds  (small list object
#         the manuscript reads inline; no re-simulation involved).
# ------------------------------------------------------------------

library(data.table)

#--- prices, identical to code/2-1-summarize-results-main.qmd ---#
pCorn <- 6.25 / 25.4    # $/kg
pN <- 1 / 0.453592      # $/kg

gen_yield_QP <- function(b0, b1, b2, Nk, N) {
  (N < Nk) * (b0 + b1 * N + b2 * N^2) + (N >= Nk) * (b0 + b1 * Nk + b2 * Nk^2)
}

## ============================================================
## (1) Inference on the profit-loss comparison (main case 10f/5r)
## ============================================================
cr <- as.data.table(readRDS(here::here("results/main/comp_results.rds")))
ld <- as.data.table(cr[num_folds == 10 & num_repeats == 5, loss_data][[1]])
n <- nrow(ld)

se <- function(x) sd(x) / sqrt(length(x))
d <- ld$y_pi_loss - ld$e_pi_loss          # extra loss of yield-based vs LEONR, per iteration
tval <- mean(d) / se(d)

inference <- list(
  n_sims        = n,
  leonr_mean    = mean(ld$e_pi_loss), leonr_se = se(ld$e_pi_loss), leonr_med = median(ld$e_pi_loss),
  yield_mean    = mean(ld$y_pi_loss), yield_se = se(ld$y_pi_loss), yield_med = median(ld$y_pi_loss),
  diff_mean     = mean(d),
  diff_se       = se(d),
  diff_t        = tval,
  diff_ci_lo    = mean(d) - 1.96 * se(d),
  diff_ci_hi    = mean(d) + 1.96 * se(d),
  share_leonr_beats = mean(ld$e_pi_loss <= ld$y_pi_loss)   # fraction of fields LEONR is no worse
)

#--- MC standard errors on the success rates (main case) ---#
at <- as.data.table(readRDS(here::here("writing/figures-tables/selection_accuracy_table.rds")))
m  <- at[num_folds == 10 & num_repeats == 5]
p_leonr <- sum(m$num_selected_leonr_correct) / n
p_yield <- sum(m$num_selected_yield_correct) / n
success <- list(
  leonr_rate = p_leonr, leonr_se = sqrt(p_leonr * (1 - p_leonr) / n),
  yield_rate = p_yield, yield_se = sqrt(p_yield * (1 - p_yield) / n)
)

## ============================================================
## (2) Uniform-vs-variable-rate baseline (true profit gain of VR)
## ============================================================
files <- list.files(here::here("data/main/individual-fields"),
                    pattern = "^sim_data_.*\\.rds$", full.names = TRUE)
Ngrid <- 0:300

per_field <- vector("list", length(files))
for (i in seq_along(files)) {
  d_i <- as.data.table(readRDS(files[i])$data[[1]])

  #--- true site-specific optimum ---#
  d_i[, optN := pmin(Nk, pmax(0, (pN / pCorn - b1) / (2 * b2)))]
  pi_si <- d_i[, mean(pCorn * gen_yield_QP(b0, b1, b2, Nk, optN) - pN * optN)]

  #--- best single uniform rate over the field ---#
  pi_by_N <- vapply(Ngrid, function(NN) {
    d_i[, mean(pCorn * gen_yield_QP(b0, b1, b2, Nk, NN) - pN * NN)]
  }, numeric(1))
  pi_uni  <- max(pi_by_N)
  N_uni   <- Ngrid[which.max(pi_by_N)]

  per_field[[i]] <- data.table(sim = i, pi_si = pi_si, pi_uni = pi_uni,
                               N_uni = N_uni, vr_premium = pi_si - pi_uni)
}
pf <- rbindlist(per_field)

baseline <- list(
  vr_premium_mean = mean(pf$vr_premium),
  vr_premium_se   = se(pf$vr_premium),
  pi_si_mean      = mean(pf$pi_si),
  pi_uni_mean     = mean(pf$pi_uni),
  N_uni_mean      = mean(pf$N_uni)
)

review_stats <- list(inference = inference, success = success, baseline = baseline)
print(review_stats)
saveRDS(review_stats, here::here("writing/figures-tables/review_stats.rds"))
