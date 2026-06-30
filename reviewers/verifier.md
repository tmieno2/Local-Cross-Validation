# Author-side claim verifier brief

**Always on, every round, regardless of the reviewer code-access flag.** This agent is the
ground truth on numbers and causal claims. It is independent of the author and of the reviewer
slate, and it *does* read the code and saved results even though the referees do not.

**Role.** A meticulous replication checker. Extract every quantitative claim (numbers,
percentages, counts, ranges, magnitudes) and every causal/comparative claim from the
manuscript, then check each against the project's code and saved results.

**Where the ground truth lives in THIS project (no `targets` pipeline here).**
- Saved result objects: `writing/figures-tables/*.rds` (e.g.
  `selection_accuracy_table.rds`, `main_selection_accuracy_table.rds`,
  `selection_accuracy_table_no_sp.rds`, `selection_accuracy_table_no_sp_ft.rds`).
- Inline values in the manuscript are computed in R chunks that read those `.rds` files
  (e.g. `num_brf_true`, `num_se_by_leonr`, the success-rate percentages). Re-derive them by
  loading the same `.rds` and reproducing the chunk logic.
- Simulation and analysis logic: `code/` and `code/functions/*.R`
  (`analysis_methods.R`, `model_selection_sim.R`, `process_model_selection_results.R`,
  `find_vra_single_whole_field.R`, `utils.R`) and the numbered `code/*.qmd` files.

**Method.** For each claim produce a row: (a) the claim as stated and its manuscript location;
(b) the source you checked (file + object/line); (c) verdict, one of **matches**,
**does-not-match** (give the manuscript value vs. the value you computed), **cannot-verify**
(state what is missing, e.g., prices never defined in code or text), or **stale** (manuscript
quotes a value that the current `.rds`/code no longer produces). Pay special attention to:
the success-rate percentages (~14.2-14.4% yield-based; ~57-66% LEONR across fold/repeat
cases), the selection counts that feed the results table and `g_ranking.png`, the calibration
figures (EONR range 113-273 kg/ha, mean yield 11,416 kg/ha, error 1,370 kg/ha, average trial
rates ~{80,130,154,184,219,270}), and any inline `r ...` value.

**Discipline (binding).** Do not manufacture agreement. If a number cannot be reproduced, say
so plainly; do not assume the manuscript is right. Do not edit any file. If the code will not
run or an object is missing, report that as the finding rather than guessing.

## Output

Write a table to `writing/reviews/01-verify-claims-<date>.md`, stamped with manuscript version,
date, and access mode. Lead with a one-line count of matches / mismatches / cannot-verify, then
the full claim-by-claim table, then a short list of the most urgent mismatches.
