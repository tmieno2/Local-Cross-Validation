# Manuscript revision summary — internal review round 1

**Date:** 2026-06-30
**Manuscript:** "A New Approach to Model Selection Based on Local Economically Optimal Input Application Rates" (target journal: AJAE)
**What to open first:** `writing/manuscript.html` (the freshly rendered paper). A clean text version is at `writing/manuscript.md`.

> **Note (merge):** your pushed changes have been merged in, your bibliography update (which added the three references that were previously missing) and your updated `g_Nrate_yield.png` and `g_field_map.png` figures. The manuscript now renders with **no broken citations**, and the experimental-design figure you see is your own latest version.

---

## 1. What happened, in plain terms

We ran a simulated pre-submission peer review of the paper. Three "referees" (all economists, matched to AJAE), an independent **claim-verifier** that re-checks every number against the code and saved results, and an "editor" who synthesized everything. Their full write-ups live in `writing/reviews/`:

- `01-econ-production-ag-2026-06-26.md` — production / precision-ag economist
- `01-econ-econometrics-ml-2026-06-26.md` — econometrics / machine-learning referee
- `01-econ-applied-micro-2026-06-26.md` — general applied microeconomist
- `01-verify-claims-2026-06-26.md` — the number-by-number fact check (most important)
- `01-editor-2026-06-26.md` — the synthesis

We then revised the manuscript to address the findings. This document explains **what changed and why**, and gives you a **checklist** (Section 5) of what to confirm. Your main job is to read `writing/manuscript.html` and sanity-check that the corrected numbers and the new wording match what you know is true.

> Important: the numbers in the paper are now generated automatically from the saved results files (`.rds`), not typed by hand. So if a number looks wrong, it usually means the saved result is being read or computed in a way we should double-check, not that someone mistyped it.

---

## 2. The big picture: the paper had stale numbers

The single most important finding was that several numbers in the text were **left over from an older version of the simulated data** and no longer matched the current results. The verifier caught these. They are now all recomputed from the saved `.rds` files. The corrected numbers are listed in Section 5 so you can confirm them.

---

## 3. What changed and why

### A. Corrected factual numbers (highest priority — these were wrong)

| What | Was | Now | Why |
|---|---|---|---|
| Trial N rates | "six rates" | **five rates** | The simulation uses a `Latin Square Fixed 5` design (5 rates). Confirmed directly from the data. |
| Trial-rate values | {80,130,154,184,219,270} | **{75,115,154,194,234} kg/ha** | Recomputed from the field data. |
| Mean simulated yield | 11,416 kg/ha | **13,458 kg/ha** | Recomputed. |
| True EONR range | 113–273 kg/ha | **82–210 kg/ha** (cell level) | Recomputed. |
| Yield-based selection picked BRF | "all 500 rounds" | **495 of 500** (LM the other 5) | The data show 495, not 500. |
| Fold-repeat success rates (LEONR) | 57.2–65.8%, and a "7-fold" case | **59.0–68.2%** across folds of 5/10/20 and repeats of 1/5/10 | The old paragraph cited a "7 folds" case **that does not exist** and percentages that did not match. Fully rewritten. |
| Yield-based success rate | "14.2–14.4%" | **12.0–13.0%** | Recomputed. |
| Field design description | "6×6 Latin square, 8 blocks of 36" | **5×5 Latin square, tiled across 288 plots (12 × 24)** | Verified from the data: each of the 5 rates appears once per row and column of a 5×5 block, tiled across the field. |
| Trial-rate construction | "20 kg/ha subtracted from the minimum" | **40 kg/ha below the minimum, 20 above the maximum** | Matches the code (`Nk_minus = 40, Nk_plus = 20`). |
| Error-term statistic | "mean of error terms = 1,370 kg/ha" | **removed** | A mean-zero error cannot have a mean of 1,370; the value could not be reproduced from the saved data. |

### B. Added the statistics referees asked for

All three referees noted there was **no measure of uncertainty** on the headline comparison. Added (computed from the 500 simulations):

- A **paired comparison** of profit loss per field: LEONR-based mean **$1.76/acre** (median $0) vs. yield-based **$11.97/acre** (median $10.84); mean difference **$10.21/acre** (95% CI $9.30–$11.12; paired *t* ≈ 22). LEONR is no worse than yield in **85.8%** of fields.
- **Monte Carlo standard errors** on the success rates (65.4% ± 2.1% vs. 12.0% ± 1.5%).
- A **variable-rate baseline**: the true site-specific EONR earns about **$26.3/acre** more than the best single uniform rate, so yield-based selection forfeits ~46% of that premium while LEONR forfeits only ~7%. (This answers the referee question "is the gain economically meaningful?")

These come from new scripts: `code/2-4-review-analyses.R` (saves `writing/figures-tables/review_stats.rds`) and `code/2-3-calibration-summary.R` (saves `calibration_summary.rds`).

### C. Clarified the method (no change to what was done, only how it is explained)

- **The proxy logic.** A referee thought matching a fold-average EONR to a single "local uniform EONR" was a flaw. We added a paragraph (in *Model selection based on local EONR*) explaining why it is a **deliberate, correct choice**: the true site-specific EONR is unobservable, so we cannot validate against it directly; validating one model's site-specific estimate against another's would just reward similarity to the benchmark; spatial clustering plus positive spatial correlation makes EONR nearly uniform within a small fold, so a local uniform EONR is both faithful and reliably estimable; and GAM is a neutral benchmark outside the candidate set, so matching it signals that a model truly captures how soil shifts EONR.
- **Causal forest description.** The text described CF as picking the best among the 5 experimental rates. **The code actually fits a smooth curve through the treatment effects and picks a continuous EONR.** We corrected the description to match the code (and fixed a sign error in the profit-differential equation). *Please confirm this matches your understanding of the CF code.*
- **Tuning.** Added a sentence: RF/BRF use `grf` with 1,000 trees and automatic cross-validation tuning; the tuning does not use the yield or EONR loss, so it does not favor either selection approach.

### D. Reframed the contribution (novelty)

Two referees said the core idea (select models by a decision-relevant loss, not predictive accuracy) is a known general principle presented as new. We added an introduction paragraph that positions the contribution as **operationalizing this idea when the decision target (EONR) is unobservable**, and cites the general literature: Kleinberg et al. (2015), Elmachtoub & Grigas (2022), Bertsimas & Kallus (2020). These three references were added to `PA.bib`.

### E. Cleanups

- **Abstract** moved into the document header (it was an empty field before).
- **Notation** made consistent: the covariates are written as `X_j` everywhere (the text previously sometimes wrote "(N, α, β)"). Confirmed against the code that all models use the 12 covariates in `X`.
- **Units:** profit/loss kept in **$/acre** (AJAE accepts imperial); EONR kept in **kg/ha** throughout, including the GAM figure (which previously said lb/acre and is now kg/ha to match the text). The GAM scatter (Fig. 6) also had its axes mislabeled relative to the data; that is fixed (x = GAM-estimated, y = true).
- **Equation fixes:** Eq. 6 (the η decomposition had a stray "+"), and the yield-RMSE formula now averages over folds.
- **Limitations paragraph** added to the Conclusion (single simulation design, no real-field validation, etc.).
- **Typos** fixed throughout (e.g., "calcurate", "liner model", "seleted", "substracted", "multia-arm", doubled "the the").

---

## 4. Files that changed

- `writing/manuscript.qmd` — the paper (edited)
- `writing/manuscript.html`, `writing/manuscript.md` — re-rendered output
- `writing/PA.bib` — three new citations added; three citation-key typos fixed
- `code/3-2-present-results.qmd` — figure labels/units; GAM axis fix
- `writing/figures-tables/` — `g_pi_loss.png`, `g_pi_loss_nosp.png`, `g_gam_leonr.png` regenerated; new `calibration_summary.rds`, `review_stats.rds`
- New scripts: `code/2-3-calibration-summary.R`, `code/2-4-review-analyses.R`, `code/2-5-relabel-figures.R`

Nothing has been committed to git yet.

---

## 5. What YOU need to check (go through `writing/manuscript.html` top to bottom)

Please confirm each of these. Where it says "confirm number," check it against what you know or against the code/results.

1. **Abstract** appears at the top and reads correctly.
2. **Introduction** — the new paragraph about "prediction policy / decision-focused learning" and the three new citations (Kleinberg, Elmachtoub, Bertsimas). Are you comfortable with this framing and these references? Are there other citations you'd prefer?
3. **Data generation (§2.1):**
   - "Five trial N rates," average ≈ {75, 115, 154, 194, 234} kg/ha — confirm.
   - Mean yield ≈ 13,458 kg/ha; true EONR range 82–210 kg/ha — confirm these are right for the current data.
   - The **5×5 Latin square** description — confirm this is how the trial design actually works.
   - Corn price **$6.25/bushel**, N price **$1.00/lb** — confirm these are the prices used.
4. **Causal forest (§2.2):** the description now says CF fits a smooth curve and produces a **continuous** EONR. **Confirm this matches the code** (`find_opt_vra_CF_smooth`).
5. **Model selection based on local EONR (§2.3):** read the new "Three considerations motivate this design" paragraph. Does it correctly explain why the local-uniform-EONR proxy is the right target? This is the heart of the method, so it matters that the explanation is right.
6. **Results (§3):**
   - "Yield-based selection picked BRF in **495** of 500" — confirm.
   - The new **inference paragraph** after the profit-loss figure (mean losses, paired difference $10.21/acre, *t* ≈ 22, 85.8%). Confirm the wording reads correctly.
   - The **variable-rate premium** sentence ($26.3/acre; forfeits ~46% vs ~7%). Confirm it reads correctly.
   - **Fold-repeat section:** percentages 59–68% (LEONR) vs 12–13% (yield); the "7 folds" case is gone. Confirm.
   - **Figure 6 (GAM vs true EONR):** look at the axes — x should be "GAM-estimated," y "true," both in kg/ha. Does the cloud look right?
   - The accuracy **Tables 2 and 3**: in the HTML these should render. Confirm the numbers in the tables look sensible.
7. **Conclusion:** read the new **limitations paragraph** — is it accurate and not over-conceding anything you disagree with?
8. **References:** all citations now resolve, there are **no** broken/placeholder references in the HTML. The three that were previously missing (`dietrich1996fast`, `wood1994simulation`, `zhao2021causal`) arrived in your bibliography update and have been merged in. Still worth a quick scan of the reference list at the end of the HTML to confirm the entries look correct.

---

## 6. Open items still needing a decision (not yet done)

- **a. The three missing citations — resolved** (they came in with your bibliography update and are merged; nothing further needed unless the entries themselves need correcting).
- **b. "True best model" definition.** The selection-frequency results (Fig. 4 / Table 2) define the "best" model as the one with the lowest **EONR RMSE**, while the profit-loss figure (Fig. 5) measures loss relative to the **most profitable** model. These are two slightly different baselines. We added a sentence explaining both, but you may want to pick one consistent definition. Flag for Taro.
- **c. Price-ratio robustness.** A referee asked whether the conclusion holds across different corn:N price ratios. This needs re-running the simulation, so it is noted as a possible robustness check rather than done.

If anything in the checklist looks off, note the section and the specific number/sentence, and we can trace it back to the code or the saved results.
