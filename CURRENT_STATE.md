# Current project state

Updated 17 October 2026.

## Implemented and locally validated

- Deterministic study compilation with the reflowR strict composition, fixed workflow stages, declared research design and domain colours.
- An initial synthetic, independent-observation, continuous-outcome association recipe. Unsupported designs and inconsistent data stop before report acceptance.
- A separate synthetic paired mean-change recipe with explicit pair independence, ID matching, fixed after-minus-before direction and complete-pair checks.
- Separate analysis/rendering receipts with current input, dependency and output identity checks.
- A separate app flow for fixed association and paired synthetic examples, with explicit preview, compilation, analysis, rendering and declaration download. Stale previews, changed fixtures and repeated write steps are rejected.
- Study-owned Git repositories with only generated report templates staged, no automatic commits, and rejection of ambiguous report-source status.
- Read-only evidence search and citation resolution with exact source locators, restriction checks and externally pinned registry identities.
- Local model setup, owned-runtime controls and assistance validation. Setup success does not qualify a model for research advice.

The integrated app archive passed `R CMD check --no-manual` without errors, warnings or notes: 7,481 assertions passed, with 15 explicit skips. Independent app execution, stale-input and English/German display checks passed. An isolated browser completed the paired workflow, rendered all 12 reports and downloaded a valid declaration. The archive predates the later test-only Windows path normalization; that correction passed its separate 11-assertion Linux check and independent review. All app source files match the reviewed archive. A subsequent diagnostic-axis label correction passed isolated before/after execution for both recipes: all numerical output files remained byte-identical, and both rendered images were inspected. The correction does not change the statistical computation.

The paired-workflow and report-provenance extension previously passed 97 independent focused checks. The supporting reflowR archive passed 2,593 assertions with two skips, and its latest six-job hosted matrix passed. Earlier Windows portability fixes passed the complete five-job hosted cttiR matrix. The later paired-workflow and documentation checkpoints passed all Linux and macOS jobs but exposed a Windows path-spelling assertion failure. The test-only correction subsequently passed all five hosted jobs, including Windows. The new app and diagnostic-label checkpoints failed hosted checks. Windows lacked a build dependency required by the study lock; report rendering also failed on Linux. CI provisioning now explicitly includes cpp11, and dependency failures name missing packages. Cross-platform verification and the separate rendering diagnosis remain pending.

## Model and evaluation work

The Qwen shared training run completed all 134 optimizer updates and matched 148-case baseline and final evaluations. Both checkpoints, the exact training schedule, cleanup and restored-service accounting passed independent technical review. The original Ministral comparison also completed all 134 updates and both 148-case evaluations. Its completion receipt records cleanup and restoration; independent terminal review and consolidated accounting remain pending. Blinded semantic review and selected-artifact serving evaluation remain outstanding. No trained candidate has been selected, deployed or scientifically qualified.

Source-domain readiness review identifies training exposure and unresolved independent-family coverage. Indexed references are not automatically independent evaluation sources. Broad statistical, programming, legal or institutional ethics coverage is not established by package tests or the current development comparison.

## Publication boundaries

Reviewed public code checkpoints are pushed to main after substantial completed work. Private reference collections, training data, model weights, operational receipts and detailed continuation logs remain excluded from Git. Upstream documentation retains its original notices and licenses. Publication of package code does not approve a model release, real-study execution or scientific conclusions.
