# Reviewer code-access policy

- **Target journal:** American Journal of Agricultural Economics (AJAE), AAEA / Wiley.
- **Policy as found:** AJAE follows the AAEA Data and Code Availability Policy, which
  requires authors to deposit a replication package (data + code) **at acceptance**, verified
  by the AEA Data Editor / AAEA prior to publication. During the refereeing stage, reviewers
  are normally sent the manuscript only; code and data are not routinely shared with referees
  while the paper is under review.
- **Resulting access level:** **manuscript-only.**
- **Confirmed by:** Taro Mieno, 2026-06-26.

## What this means for the round

- The reviewer agents see only the rendered manuscript plus their own rubric. They do **not**
  read `code/` or any cached results. A number a reviewer cannot verify from the text is to be
  raised as an *under-specification* finding, which is itself informative: it means the paper
  does not stand on its own.
- The **author-side claim verifier runs regardless of this flag.** Manuscript-only access for
  referees never turns off the independent claim check against `code/` and the saved results
  (`writing/figures-tables/*.rds`).
