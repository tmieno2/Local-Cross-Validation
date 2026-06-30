# Author-side claim verification — Round 01

- **Manuscript version:** commit 4cccd0f ("Abstract added"); repo HEAD is at 4cccd0f at time of check
- **Verifier file:** reviewers/verifier.md (v1, 2026-06-26)
- **Access mode:** author-side (reads code + results; independent of reviewer access flag)
- **Round:** 01
- **Date:** 2026-06-26

## Headline count

**Matches: 19 | Does-not-match: 11 | Cannot-verify: 3 | Stale (data-version mismatch): 5**

The selection-count claims (Figure 4, Table 2, Table 3, and the inline `r ...` values) all reproduce exactly from the current `.rds` files. The problems cluster in two places: (1) the **fold-repeat success-rate paragraph** (Section 3.3) is wrong throughout — it cites percentages that do not match the data and describes a "7 folds" case that does not exist; and (2) the **calibration figures** (Section 2.1: six trial rates, mean yield 11,416, EONR range 113-273, error 1,370) are stale — they describe an older data-generation setup, while the current `data/main/` produces five trial rates, mean yield ~13,458, etc. The repeated claim that yield-based selection "selected BRF for all the 500 simulation rounds" is also contradicted by the data (495, not 500).

---

## Claim-by-claim table

### A. Field geometry and design (Section 2.1)

| # | Claim (location) | Source checked | Verdict |
|---|---|---|---|
| A1 | Field 432m × 864m ≈ 37.3 ha (Method, ~ln 110) | `432*864/10000 = 37.32` | **matches** |
| A2 | 288 N plots (ln 149) | `raw_sim_data$field_sf[[1]]`, unique `plot_id` = 288 | **matches** |
| A3 | 1440 yield subplots (ln 149) | analysis data `nrow` = 1440, unique `aunit_id` = 1440 | **matches** |
| A4 | 8640 effective cells (ln 149) | 1440 subplots × 6 cells = 8640; raw field has 10368 cells incl. transition/buffer | **matches** (8640 = effective) |
| A5 | Each N plot = 5 effective yield subplots (ln 144) | 288 plots × 5 = 1440 subplots | **matches** |
| A6 | 500 Monte Carlo iterations (ln 85, 192) | 500 files in `data/main/individual-fields/`; all `.rds` have 9 fold-repeat combos × 500 sims | **matches** |
| A7 | Spherical variogram, range 600m, nugget 0 (ln 197) | `0-data-generation.qmd`: `sp_range=600, gstat_model="Sph"` | **matches** |
| A8 | "20 kg/ha subtracted from Q(0), 20 added to Q(100)" → rates {Q(0)−20,...,Q(100)+20} (ln 218) | `gen_trial_design(..., Nk_minus = 40, Nk_plus = 20)` in `0-data-generation.qmd` ln 53 | **does-not-match**: code subtracts **40** from the low end (`Nk_minus = 40`), not 20. (Plus side = 20 matches.) |
| A9 | Six trial N rates per experiment (ln 214, "Six trial N rates") | analysis data: every one of 500 sims has exactly **5** unique `N_tgt` values; figure code (`g_N`) uses a 6-color scale = transition + 5 rates | **does-not-match**: data has **5** trial rates, not 6 |
| A10 | Average trial rates ≈ {80, 130, 154, 184, 219, 270} kg/ha (ln 225) | per-rank means across 500 sims = **{74.8, 114.6, 154.4, 194.2, 234.0}** (5 values) | **does-not-match / stale**: only 5 rates exist; the magnitudes also differ (esp. the top rate 234 vs 270) |
| A11 | 6×6 Latin square; 8 (=4×2) blocks; 36 (=6×6) plots/block (ln 227-230) | `assign_trial_design("Latin Square Fixed 5")`; 288 plots / 8 blocks = 36 plots/block | **partially matches**: 288/8 = 36 plots/block is consistent, but design is "Fixed 5" (5 rates), inconsistent with the stated 6×6 structure (would need 6 rates). Flag with A9. |

### B. Calibration figures (Section 2.1, ln 203-206)

| # | Claim | Source checked | Verdict |
|---|---|---|---|
| B1 | True EONR ranged 113-273 kg/ha across simulations | per-cell true EONR (`opt_N`, clipped to [0,Nk]) over all sims: range **82.3 to 209.9**; per-sim mean-field EONR range **124 to 164** | **does-not-match / stale**: neither cell-level (82-210) nor field-mean (124-164) matches 113-273 |
| B2 | Average simulated yield = 11,416 kg/ha | mean observed yield across all 500 sims (subplot level) = **13,458**; per-sim means span 12,517-14,495 (11,416 is below the minimum) | **does-not-match / stale** |
| B3 | Mean of yield error terms ε = 1,370 kg/ha | `ε` not stored as a named column in the saved analysis data; cannot recompute without re-running `pasim::gen_analysis_data`. Generation uses `pasim` internals not in this repo. | **cannot-verify** (object/parameter not exposed in saved data or in `code/`) |

*Note on B1-B2:* The combination of stale calibration numbers + 6-vs-5 trial rates strongly indicates Section 2.1's descriptive numbers were written against an earlier data-generation configuration and were not refreshed when the current `data/main/` was regenerated (data files dated Aug 2025).

### C. Prices and EONR formulas (Section 2.2)

| # | Claim | Source checked | Verdict |
|---|---|---|---|
| C1 | Crop price p / P_C and N price w / P_N used in profit max | `1-1-main-simulation.qmd` ln 36-37, `2-1-summarize-results-main.qmd` ln 13-14: `pCorn = 6.25/25.4 ≈ 0.246 $/kg`, `pN = 1/0.453592 ≈ 2.205 $/kg` | **matches** that prices exist and are used. **cannot-verify against text**: the manuscript never states numerical price values, so the reader cannot check them. (Contrary to the brief's hypothetical, prices ARE defined in code.) |
| C2 | Closed-form LM/SE EONR = (P_N/P_C − λ1 − Xλ2)/2(λ3+Xλ4) (ln 406) | matches code form `opt_N = (pN/pCorn − b1)/(2*b2)` for the true-EONR analogue | **matches** (functional form) |

### D. Main results — Figure 4 / Table 2 (Section 3.1)

All values below recomputed from `selection_accuracy_table.rds` filtered to `num_folds==10 & num_repeats==5`, and cross-checked against the rendered flextable in `main_selection_accuracy_table.rds`.

| # | Claim (location) | Inline var | Computed | Verdict |
|---|---|---|---|---|
| D1 | BRF was actually best in only 61 rounds (ln 700) | `num_brf_true` | 61 | **matches** |
| D2 | SE best most often at 412 (ln 702) | `num_se_true` | 412 | **matches** |
| D3 | "selected SE as often as it was selected as the actual best" (ln 703) | LEONR-selected SE = 381 vs SE-true = 412 | 381 vs 412 | **does-not-match (overstated)**: 381 ≠ 412; "as often as" is an approximation, off by 31 |
| D4 | LEONR approach selected SE 381 times (ln 721) | `num_se_by_leonr` | 381 | **matches** |
| D5 | Among 412 SE-best, LEONR correctly selected SE 317 times (ln 724) | `num_se_by_leonr_correct` | 317 | **matches** |
| D6 | LEONR selected LM 93 times (ln 726) | `num_lm_by_lenor` | 93 | **matches** |
| D7 | LM choice correct only 8 times (ln 726) | `num_lm_by_lenor_correct` | 8 | **matches** |
| D8 | LEONR correctly selected best model 327 times total (ln 727) | `num_all_by_lenor_correct` = sum(leonr_correct) = 8+317+0+1+1 | 327 | **matches** |
| D9 | Yield-based selected BRF for all 500 rounds (ln 699, 728) | `num_selected_yield`: BRF=**495**, LM=5 | 495 | **does-not-match**: BRF selected 495 times, not 500 (LM selected 5 times). Stated twice. |
| D10 | Yield-based choice correct only 60 times (ln 729) | `num_brf_by_yield_correct` | 60 | **matches** |
| D11 | Table 2 body (LM 25 / 8(93) / 0(5); SE 412 / 317(381) / 0(0); RF 0/0(0)/0(0); BRF 61 / 1(6) / 60(495); CF 2 / 1(20) / 0(0)) | `main_selection_accuracy_table.rds` body | identical | **matches** |

### E. No-spatial-correlation — Table 3 (Section 3.4)

Recomputed from `selection_accuracy_table_no_sp.rds` and cross-checked vs `selection_accuracy_table_no_sp_ft.rds`.

| # | Claim (location) | Computed | Verdict |
|---|---|---|---|
| E1 | SE best only 270 times (no-sp) (ln 862) | `se_best` = 270 | **matches** |
| E2 | LM best 87 times (no-sp) (ln 863) | `lm_best` = 87 | **matches** |
| E3 | SE best 412 / LM best 25 (high-sp comparison) (ln 863-864) | `num_se_true`=412, `num_lm_true`=25 | **matches** |
| E4 | Table 3 body (LM 87 / 31(183) / 0(0); SE 270 / 123(230) / 0(0); RF 0/0(0)/0(0); BRF 104 / 1(12) / 104(500); CF 39 / 4(75) / 0(0)) | `selection_accuracy_table_no_sp_ft.rds` body | identical | **matches** |
| E5 | (implicit) no-sp yield-based "selected BRF for all rounds" pattern | BRF `num_selected_yield` = 500 (no-sp) | **matches** (in the no-sp case it really is 500) |

### F. Fold-repeat sensitivity (Section 3.3, ln 825-839)

Success rate = count where the selected model has `eonr_rank_true == 1`, divided by 500, recomputed two independent ways (from `selection_perf_rank_LE`/`_Y` in `results/main/comp_results.rds`, and as `sum(num_selected_*_correct)/500` from `selection_accuracy_table.rds`); both agree.

| # | Claim | Computed | Verdict |
|---|---|---|---|
| F1 | LEONR, 5 folds 1 repeat ≈ 57.2% | 301/500 = **60.2%** | **does-not-match** |
| F2 | LEONR, "5 repeats" → 58.4% (reads as 5 folds 5 repeats) | 5f5r = 338/500 = **67.6%** | **does-not-match** |
| F3 | LEONR, 7 folds 5 repeats → 62.4% | **no 7-fold case exists** in the data (folds are only 5, 10, 20). 62.4% does coincide with the **20 folds 5 repeats** value (312/500). | **does-not-match**: the "7 folds" case is fabricated/mislabeled |
| F4 | LEONR, 10 folds 5 repeats → 65.8% | 327/500 = **65.4%** | **does-not-match** (close, 65.4 vs 65.8) |
| F5 | LEONR, 5 folds 10 repeats → 59.4% | 323/500 = **64.6%** | **does-not-match** |
| F6 | LEONR, 10 folds 10 repeats → 65.2% | 341/500 = **68.2%** | **does-not-match** |
| F7 | "consistently demonstrated an increasing trend as folds and repeats increased" | computed LEONR pct by combo: 5f={60.2,67.6,64.6}, 10f={59.6,65.4,68.2}, 20f={59.0,62.4,68.0} — not monotone in folds; within a fold count it rises then varies | **does-not-match** (the monotone-increase narrative is not supported; e.g., 5f gives the single highest at low fold count, 67.6%) |
| F8 | Yield-based success rate "between 14.2 and 14.4 percent" across all combos | computed yield rank-1 pct range = **12.0% to 13.0%** (counts 60-65 / 500) | **does-not-match**: actual range is 12.0-13.0%, not 14.2-14.4% |
| F9 | Yield-based "largely unaffected by fold/repeat" (qualitative) | range 12.0-13.0%, indeed nearly flat | **matches** (qualitative direction correct, magnitude wrong per F8) |

### G. GAM local-EONR accuracy (Section 3.2, Figure 6)

| # | Claim | Source | Verdict |
|---|---|---|---|
| G1 | Figure plots true LEONR vs GAM-estimated LEONR for all folds of 500 sims; most points near 1-to-1 line; two clusters over/under-estimate (ln 782-785) | `results/main/sum_results_gam.rds`, main case n=12,500 points; cor(true, gam)=**0.668**, RMSE=**23.3**, mean true 143.7 vs mean GAM 151.2 | **matches** (consistent with the figure-script `g_gam_leonr`: points cluster around the dashed 1-to-1 line; the qualitative description is supported) |
| G2 | Figure 6 is the **main case** (10 folds, 5 repeats) | `3-2-present-results.qmd` ln 246 filters `num_folds==10 & num_repeats==5` for `g_gam_leonr.png` | **matches** |

### H. Other figures

| # | Claim | Source | Verdict |
|---|---|---|---|
| H1 | Figure 5 (g_pi_loss) shows main-case profit loss, LEONR vs yield | code filters main case (10f,5r); LEONR loss << yield loss by construction | **matches** (consistent with code; histogram not numerically re-summarized) |
| H2 | Figure 8 (g_pi_loss_nosp) shows no-sp profit loss | no-sp `comp_results.rds` has only 1 case (10f,5r), so unfiltered code is correct | **matches** |
| H3 | Figure 3 (g_split_example) shows 5 folds | generated from `sim_results_num_repeats_1_num_folds_5` | **matches** |

---

## Most urgent mismatches (fix before submission)

1. **Section 3.3 fold-repeat percentages are wrong throughout (F1-F8).** None of the six cited LEONR percentages match the data, the "7 folds, 5 repeats → 62.4%" case **does not exist** (only 5/10/20 folds were run; 62.4% actually belongs to 20 folds, 5 repeats), the "increasing trend" narrative is not supported, and the yield-based range is **12.0-13.0%**, not the stated 14.2-14.4%. This entire paragraph must be regenerated from `selection_accuracy_table.rds` / `comp_results.rds`. Correct LEONR values: 5f1r 60.2, 5f5r 67.6, 5f10r 64.6, 10f1r 59.6, 10f5r 65.4, 10f10r 68.2, 20f1r 59.0, 20f5r 62.4, 20f10r 68.0.

2. **"Yield-based selected BRF for all the 500 rounds" is false in the main case (D9).** The data show BRF=495, LM=5. Stated twice (ln 699 and ln 728) and contradicted by the paper's own Table 2 cell "60 (495)". Either soften to "nearly all (495)" or reconcile. (It IS exactly 500 in the no-sp case, E5.)

3. **Calibration figures in Section 2.1 are stale (A9, A10, B1, B2).** The current data have **5** trial rates (avg ≈ {75,115,154,194,234}), mean yield ≈ **13,458 kg/ha**, and true EONR spanning ≈ 82-210 (cells) / 124-164 (field means) — none matching the manuscript's "six rates {80,130,154,184,219,270}", "11,416 kg/ha", or "113-273". The error-term mean (1,370) could not be recomputed from saved data (B3, cannot-verify). These read as left over from a prior data version.

4. **Trial-rate construction text vs code (A8).** Manuscript says 20 kg/ha subtracted from the low rate; code uses `Nk_minus = 40`.

5. **"selected SE as often as it was selected as the actual best" overstates (D3).** 381 vs 412 (off by 31); reword to "almost as often" or give both numbers.

*All other quantitative claims (geometry, the entire main Table 2 and no-sp Table 3, the inline `r num_*` values, and the GAM-accuracy description) reproduce exactly from the saved `.rds` objects.*
