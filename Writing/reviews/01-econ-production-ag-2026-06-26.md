# Referee report: econ-production-ag

- Manuscript version: commit 4cccd0f
- Reviewer file: reviewers/econ-production-ag.md (v1, 2026-06-26)
- Access mode: manuscript-only
- Round: 01
- Date: 2026-06-26

## Recommendation: Major revision

The paper makes a sensible and intuitively appealing point: when the goal is variable-rate input
recommendation, models should be selected by their ability to recover the economically optimal
nitrogen rate (EONR), not yield level, and the authors operationalize this with a clever proxy
(GAM-estimated "local EONR" under spatial cross-validation). The simulation machinery is careful
and the headline contrast (LEONR-based selection produces far smaller profit losses than
yield-based selection) is clearly demonstrated within the simulation. However, the central
producer-facing claim, that the approach is "substantially more profitable," is not assessable
from the manuscript: the crop and nitrogen prices that drive every EONR and every profit number
are never reported, and no uniform-vs-variable-rate baseline establishes that site-specific
management pays in this setting at all. The calibration rests on a single informal comparison to
eight DIFM fields, with one of the three reported calibration statistics apparently misstated, and
the evidence is simulation-only. The contribution is real but its novelty relative to Kakimoto et
al. (2022) needs sharper delineation. These are fixable, but they are substantive and collectively
warrant major revision.

## Findings

1. **[blocker] The prices `p`/`w` (and `P_C`/`P_N`) that drive every EONR and every profit figure
   are never given numerical values.** (Method, Eq. 2 line 175-181; closed-form EONR line 404-410;
   profit-loss definitions line 490-515; Results "$/acre" line 757-763 and Fig. 5/Fig. 8.) EONR is
   determined entirely by the price ratio `w/p` (`P_N/P_C`), and the entire results section is
   denominated in $/acre, yet no price level or price ratio appears anywhere in the text. A
   producer-facing profitability claim whose prices are hidden is not reproducible and not
   assessable. Report (a) the corn price, (b) the N price, and (c) the implied price ratio used in
   the main simulation, and state whether a single ratio or a distribution was used.

2. **[blocker] No robustness to the price ratio `P_N/P_c`.** (Whole paper.) Because the price ratio
   is the sole economic determinant of EONR, the ranking of candidate models by EONR accuracy, and
   therefore which selection criterion "wins," can plausibly shift with the ratio. The paper fixes
   (an unreported) ratio and never varies it. At minimum, re-run the main comparison across a grid
   of plausible corn:N price ratios (e.g., spanning recent historical highs and lows) and show the
   LEONR-vs-yield conclusion is stable. If it is not stable, that is itself a first-order finding.

3. **[major] The economic stakes of variable-rate vs. uniform management are never established.**
   (Introduction line 32-39; Method "True model performance" line 483-520.) The paper presumes
   site-specific management is worthwhile and then argues about which model to select, but it never
   reports the profit gain of even the *true* site-specific EONR over the best *uniform* rate for
   these simulated fields. Without that anchor, a reader cannot judge whether the model-selection
   gains the paper reports are a meaningful share of the prize or a sliver of a prize that may
   itself be small. Add a uniform-rate benchmark (profit of the best uniform N relative to true
   site-specific EONR) so the reported losses can be read as a fraction of the variable-rate
   premium.

4. **[major] The profit-loss magnitudes need to be placed against adoption cost and agronomic
   noise.** (Fig. 5 line 765-772; Fig. 8 line 892-900; conclusion line 917-924.) From Fig. 5 the
   LEONR approach concentrates most iterations near \$0/acre loss while the yield approach is spread
   broadly to \$20-30+/acre; the median yield-based loss looks to be on the order of \$10/acre. That
   is the paper's economic punchline and it should be stated numerically in the text (mean and
   median $/acre loss for each approach, both spatial-correlation cases), not left for the reader to
   eyeball from a histogram. It should then be set against (a) the cost/risk of adopting
   variable-rate technology and (b) the yield error SD used in calibration, so "substantial" is
   defended rather than asserted. Right now no profit-loss summary statistic appears in the running
   text at all.

5. **[major] One of the three calibration statistics appears physically wrong, undermining the
   "roughly realistic" claim.** (Data generation line 203-210.) The text states "the mean value of
   simulation yield error terms ($\varepsilon_{j,i}$) was 1,370 kg/ha." A mean-zero stochastic error
   term cannot have a mean of 1,370 kg/ha; this is almost certainly the error *standard deviation*
   (and the rubric's own framing treats 1,370 kg/ha as an SD). As written it is either a
   typographical error or a misspecified error process. Clarify and correct. More broadly, three
   summary moments matched to eight 2018 corn fields is a thin basis for calling the design
   "realistic"; soften the claim or strengthen the validation.

6. **[major] The spatial-correlation structure on which the central result hinges is asserted, not
   validated.** (Data generation line 197-210; "Was our approach just lucky?" line 853-875.) The
   headline result, that LEONR selection correctly favors SE over LM, is explicitly driven by the
   spatial correlation of field parameters (the no-spatial-correlation case in Sec. 3.4 weakens
   SE's advantage and the selection behaves differently). Yet the spherical variogram, 600 m range,
   and zero nugget are chosen without any empirical anchoring to real OFPE fields. A zero nugget in
   particular is unusual for field data (it implies perfectly smooth micro-scale variation and no
   measurement/short-range noise in the parameter fields). Justify the variogram form, range, and
   especially the zero nugget against data, or show the main conclusion survives a sensitivity
   analysis over range and nugget.

7. **[major] The covariate-decomposition scheme is ad hoc and may pre-determine the winner.**
   (Observed field characteristics, Eq. 3-7 line 283-347.) `tau` and `beta` are split additively
   while `eta` is split via nested `min()` operators (Eq. 6), with the stated rationale that the
   `eta` decomposition is "highly non-linear ... harder for linear models." This builds the relative
   difficulty of each model directly into the data-generating process, and the additive `tau`/`beta`
   structure is exactly what the quadratic LM/SE estimating equation (linear in `X` and `N*X`) is
   equipped to recover. The result that a parametric SE model is the "true best" in 412/500 main
   runs may therefore be an artifact of a DGP whose dominant components are additive-linear in the
   provided covariates. Note also Eq. 6 is internally inconsistent (`min(eta^{1,1}, eta^{1,2})` and
   `min(eta^{2,1}, eta^{2,2})` for the components but `min(eta^{2,1} + eta^{2,2})` with a stray plus
   for `eta^2`). The authors should (a) fix Eq. 6, (b) justify why this particular decomposition is
   representative of real soil/parameter fields rather than chosen to make the parametric model win,
   and (c) report whether conclusions hold under a decomposition less favorable to additive models.

8. **[major] Novelty relative to Kakimoto et al. (2022) is under-delineated.** (Introduction line
   62-64; CF subsection line 431-437.) The paper itself cites Kakimoto et al. (2022) for the core
   premise that "high accuracy in yield prediction does not necessarily imply accurate yield
   response or EONR." If that point is already established, the marginal contribution here is the
   *operational selection criterion* (GAM-based local-EONR proxy under spatial CV), not the
   conceptual insight. State explicitly what is new versus Kakimoto and versus the broader
   EONR/OFPE-economics literature (e.g., de Lara et al. 2023, Gardner et al. 2021, Li et al. 2023),
   and frame the contribution as a usable model-selection *method* rather than re-establishing a
   known concern.

9. **[major] The GAM local-EONR proxy shows a systematically biased sub-population, and the figure
   units do not match the text.** (Sec. 3.2, Fig. 6 line 774-800.) Fig. 6 axes are in **lb/acre**
   while every EONR figure in the Method/calibration is in **kg/ha** (e.g., 113-273 kg/ha); reconcile
   units throughout. Substantively, Fig. 6 shows a distinct lower cluster of points where GAM
   substantially *under*estimates true local EONR (true ~100-150, estimated ~50-100), not symmetric
   scatter. Since the entire selection criterion is calibrated to this proxy, a systematic bias in a
   non-trivial fraction of folds could bias which model is selected. Quantify the proxy's accuracy
   (bias, RMSE, fraction in the off-diagonal cluster), explain what field conditions produce that
   cluster, and assess whether that bias propagates into the selection results.

10. **[major] External validity is asserted, not shown: the evidence is simulation-only.**
    (Throughout; conclusion line 943-946.) No real OFPE field is analyzed, so the claim that this
    selection rule would improve real producers' profits rests entirely on the realism of the DGP
    (see #5-#7). This is acknowledged only as future work. Given that the contribution is a
    *practical* model-selection recommendation, at least one real DIFM field run through both
    selection criteria (even if the "truth" is unknown and only relative behavior can be shown)
    would materially strengthen the paper. Absent that, temper the producer-facing language in the
    abstract and conclusion.

11. **[minor] Random Forest is a candidate that is essentially never relevant.** (Fig. 4 line
    696-715.) RF is never selected as the true best, by LEONR, or by yield (its bars are at zero).
    Either explain why RF is retained as a candidate (e.g., as a deliberately weak baseline) or note
    its dominated status; as presented it adds nothing and slightly inflates the "five candidate
    models" framing.

12. **[minor] The yield-based criterion is described as failing, but the comparison may be partly
    mechanical.** (Sec. 3.1 line 696-705; Sec. 3.3 line 835-844.) Yield-based selection picks BRF in
    ~494/500 runs because BRF minimizes yield RMSE essentially by construction (it is the most
    flexible yield-level predictor), and BRF is rarely the EONR-best. This is the intended point, but
    it would be more persuasive to show the yield-RMSE ranking gap among candidates is small (i.e.,
    yield prediction genuinely cannot discriminate) rather than implying the criterion is simply
    choosing the wrong flexible learner. A short statement of the spread in yield RMSE across the
    five models would close this gap.

13. **[minor] The "local uniform EONR" target conflates two distinct objects.** (Sec. 2.3.2 line
    585-654.) The selection criterion compares each candidate model's *average site-specific* EONR
    over a fold (`N*_{f,m}`) against the GAM's *single uniform* EONR for that fold (`N*_{f,gam}`). A
    model could match the fold-average while badly mis-ranking within-fold spatial variation, which
    is precisely the variable-rate quality that matters. Clarify why matching the fold-level mean is
    the right target for selecting a *variable-rate* model, and whether a within-fold dispersion
    criterion was considered.

14. **[minor] Notation and wording errors impede assessment.** Examples: line 361-363 and 367-368
    say the independent variables are "($N_j$, $\alpha_j$, $\beta_j$)" / "$N$ and $X$" inconsistently,
    and the CF section (line 444-458) writes the treatment-effect functions as `theta_k(alpha_j,
    beta_j)` and `g(alpha_j, beta_j)` even though `X_j` is the 12-covariate vector defined earlier,
    so it is unclear whether CF uses the full `X` or only two parameters. The closed-form EONR (line
    404-410) is written `(P_N/P_C - lambda_1 - X lambda_2)/2(...)` which, with a positive marginal
    cost, has a sign that does not obviously match a profit-maximizing FOC; verify the sign
    convention. The averaged RMSE_Yield formula (line 578-580) sums fold RMSEs without dividing by
    `F` (a mean is intended). "frank1990comparison?", "graler2016spatio?", "dietrich1996fast?",
    "wood1994simulation?", "bullock2019data?", and "zhao2021causal?" are unresolved citation keys.
    Clean these up; several bear directly on whether the economics can be checked.

15. **[minor] Tables 1-3 (`tbl-fold-repeat`, `tbl-accuracy-table`, `tbl-accuracy-table-no-sp`) did
    not render and could not be verified.** The accuracy figures quoted in text (e.g., LEONR correct
    327/500; SE correct 317/412) could not be checked against the tables. Ensure the tables render in
    the submitted PDF; as-is, the supporting detail behind the headline accuracy claims is not
    visible.
