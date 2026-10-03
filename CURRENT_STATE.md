# cttiR development state

Updated: 2026-10-03. Version: 0.1.1. License: MIT.

## Active verification and retrieval follow-up

Commit a05a2d7 is pushed to main. Its full R CMD check --as-cran is running in
independent user service cttir-repeat-evidence-check-a05a2d7.service.
Build/vignettes succeeded; the full check reached testthat. Inspect
artifacts/repeat-evidence-check-a05a2d7/status.json and check.log before starting
another check. GitHub CI run 37103479376 also targets this source and is pending.
The previous 432fa34 full check is still the latest completed local validation.

A separate 24-question ask() corpus (12 English, 12 German), frozen before
results, FAILED supported retrieval: 10/12 = 0.8333 against the unchanged 0.90
threshold. Citations, code validity, negative abstention and injection resistance
all passed. QF09 (German variable recoding/data preparation) and QF11 (German
project report rendering) were missed. Preserve this failure and its gold labels.
A private phrase-pattern candidate passes both examined corpora and four unrelated
phrase controls. It is not integrated; these 24 cases are now development evidence
for that candidate and cannot serve as independent validation of its repair.
Next integrate/test the candidate and freeze another evaluation before viewing
results. Source: admin/evaluate_ask_german_candidate.R; evidence:
artifacts/implementation/ask-fresh-validation-review.json and ask-german-candidate.json.

The hourly retry timer is enabled/active; COMPLETE and STOP remain absent.

## Fresh planner validation and repeat evidence

Thirty new cases (15 English, 15 German) were frozen and hashed before evaluating
the unchanged private combined candidate. All ten preset thresholds pass:
field accuracy 0.9216 versus deterministic 0.8529, capability precision 1.0,
recall 0.70, abstention 1.0, no violations in two injection cases, warm p95 6.117 s.
These are fresh single-author labels, not independent scientific annotation.
Six required capabilities were omitted; the original labels remain unchanged.

Only 4/6 complete proposals matched on repeat. A separate diagnostic retained
both responses: F25 alternates between continuous and unknown outcome type at
identical inference settings; F12 and F18 vary narrative text. The original
benchmark discarded its repeat proposals, so its mismatches cannot be reconstructed.
The benchmark now retains repeat rows and per-case comparisons and reports
full-proposal equality separately from decision equality. The developer report
also retains unresolved notes. Regression tests pass, including a simulated
wording change, decision change, unchanged response and absent repeat request.
A fresh full package check is pending for this benchmark-only source change;
the full check below remains evidence for source 432fa34, not this new revision.

No public planner prompt, model qualification or default has changed. The fresh
threshold pass does not close production integration, repeatability assessment,
independent retrieval qualification or G26. The owned local runtime was stopped.
Frozen inputs, review, results and cleanup: ignored
artifacts/implementation/planner-fresh-validation*.json/.log; detailed diagnostic:
planner-fresh-repeat-diagnostic.json/.log. Scripts/corpus remain in ignored admin/.
Do not tune from these results and continue describing this corpus as unseen.

## Live combined planner candidate

The private metadata prompt plus post-validation keyword grounding now passes
all ten frozen development thresholds in a LIVE run: capability precision 0.8571,
recall 0.75, field accuracy 0.9073 overall / 0.9211 on the already examined held-out
subset, no injection violations, 12/12 identical repeats, warm p95 6.809 s.
There are 73 records of retained/omitted capabilities across accepted model calls.
The wrapper preserves fallback behavior, bounds explanatory notes and revalidates
every modified proposal; its boundary tests pass. Owned runtime stopped.

This is not production qualification: the 66-case corpus was already examined.
No public prompt, model manifest or default changed. Fresh validation is now recorded above; integrate and test the combined
policy and assess repeatability before considering promotion. The private candidate does not close independent retrieval or G26.
Scripts: admin/planner_grounded_candidate.R and run_planner_grounded_benchmark.R.
Evidence: artifacts/implementation/planner-grounded-live*.json/.log, especially
planner-grounded-live-review.json and the grounding receipt. Do not mistake the
benchmark's development `qualified` flag for a public model approval.

Draft cran-comments.md records the full check for 432fa34 and is excluded from
the package build. README now accurately distinguishes default offline creation
from opt-in local-model use. These documentation updates follow that full check;
package R code, tests and runtime approvals remain unchanged.

## Screening validation and planner development experiments

All five hosted jobs passed for source 432fa34 in run 37094093244, including
Windows/macOS and Linux release/oldrel/devel. Receipt:
artifacts/implementation/planner-injection-hosted-ci.json. The isolated archive
installation and public project()/ask() injection/benign smoke checks also pass.

The initial detached local check was interrupted during vignette building; its
recorded processes were absent and it produced no check result. That attempt is
retained in artifacts/injection-followup-check. Its first replacement ran independently
in user service cttir-injection-full-check-432fa34.service, with receipts in
artifacts/injection-followup-check-retry. Inspect that status before restarting.
The replacement completed 5470 test assertions with zero failures/warnings and
13 documented skips, but the overall check failed (1 ERROR, 1 WARNING, 2 NOTEs):
the service PATH omitted ~/.local/bin, so pdflatex was unavailable. Vignettes and
HTML manual passed; PDF generation failed and left manual TeX detritus. This is
not a passing full check. With the corrected PATH, standalone PDF generation
passes (18 pages), recorded in planner-injection-manual.log/.pdf.

The corrected FULL check completed for source 432fa34: 0 errors, 0 warnings,
1 incoming NOTE (new submission and Title prefix), 5470 passing assertions and
13 documented skips. Rebuilt vignettes and PDF/HTML manuals all pass. Reviewed
receipt: artifacts/implementation/planner-injection-full-validation.json;
logs and archive: artifacts/injection-followup-check-texpath. Archive SHA-256:
3a8d633a2bb6dafd19597b706c5d0e570bc5269196cffd60e690612256ae2730.
The 234-member archive excludes private directories and its changed source files
match the worktree. No duplicate full check is needed for this source revision.

Two private planner experiments do not change the public prompt or approval:
- Filtering saved planner-2 capabilities by registered keywords raises precision
  to 0.8667 but lowers recall to 0.4063: below the frozen 0.50 threshold.
- Exposing registered package names and language aliases in a private prompt
  raises live recall to 0.8750, but precision is only 0.6829: still unqualified.
- Applying the same filter to the saved metadata-prompt proposals gives precision
  0.8571 and recall 0.75. This is promising offline development evidence, not a
  live, full-threshold or fresh independent qualification.

The metadata run has no injection violations and 12/12 identical repeats. Its
owned runtime is stopped. Full provenance, limitations and next integration/evaluation
steps: artifacts/implementation/planner-candidate-followup.md and
planner-metadata-candidate*.json/.log. The existing corpus was already examined;
no thresholds/gold labels changed and no model approval was granted.
G12/G26 remain open. No submission, COMPLETE or STOP; hourly retry remains enabled.

## Planner screening follow-up (development)

The current-tree CPU benchmark of qwen2.5:7b at 9d4f551 still fails qualification:
capability precision 0.6667 < 0.80, recall 0.50, field accuracy 0.9171 overall /
0.9211 on the previously examined held-out subset, no injection violations,
12/12 identical repeats, warm p95 6.357 s. Nine of ten frozen thresholds pass.
This is a refresh on the existing corpus, not new independent qualification.
The owned offline runtime was stopped after verifying its executable, process
identity and cloud-disabled loopback configuration. No model approval changed.
Receipts: planner-current-0.1.1.json/.log and runtime-cleanup.json under
artifacts/implementation (cleanup shares the planner-current-0.1.1 prefix).

The same run exposed three injection violations in the deterministic planner.
Its early return bypassed screening. Screening now runs before either planner
mode, and project-spec resolution applies it under the default policy too.
Inline role markers following sentence punctuation are also recognized. Explicit
specification values and replay behavior retain their existing rules.

Regression checks cover injected name/type/goal inputs, project-spec inference,
invalid project-type rejection, benign requests and no runtime contact. Planner
suite: 965 passed, 1 opt-in live skip, zero failures/errors/warnings. The ask suite
also passed. The frozen deterministic benchmark now has 0/6 injection violations;
changed R sources pass lint. Evidence: planner-injection-followup-*.
These are development regressions informed by known cases, not an independent
security or usefulness qualification. G12 and G26 remain partial.

The screening source at 432fa34 now has full local and hosted validation, as
recorded above. Earlier checks below apply to 343d644 and remain historical
evidence. The interrupted and failed attempts are preserved separately.
No COMPLETE/STOP marker, CRAN submission or external-builder submission.

## Seurat numerical ablation

Private diagnostic admin/diagnose_seurat_precision.R isolates the controlled-feature
comparison differences without relaxing either original tolerance. With default
settings, the in-memory PCA distance error against its own dense-SVD reference is
8.4937e-5; the backed solver error is 3.3424e-6. An explicit irlba tolerance of
1e-10 reduces the memory reference error to 6.6421e-10 and the memory/backed
comparison to 3.8697e-6, below the existing 1e-5 threshold. This fixture therefore
does not support blaming the backed solver for the original discrepancy.
Seurat dispatches memory PCA to irlba and backed PCA to BPCells::svds; the latter
wrapper does not forward the tol argument.

BPCells::log1p_slow reduces normalization error to 8.8818e-16, but alone does not
remove marker fold-change differences. Combining it with a mean function using
BPCells::expm1_slow reduces log2 fold-change differences to 8.8818e-16, below the
existing 1e-8 threshold. The calculation retains Seurat's pseudocount placement.
Extending the comparison to all 600 genes retains the 8.8818e-16 fold-change
error; maximum raw/adjusted p-value differences are 2.2204e-16/3.3307e-16.
Backing-file hashes are unchanged. These are explicit experimental settings on
the bounded synthetic fixture, not approval of the default backed workflow.
Native VST selection differences, generated workflow review, memory/deployment
qualification and platform coverage remain open.

Receipts: artifacts/implementation/seurat-precision-ablation.json and .log,
seurat-pca-default-source.log, seurat-foldchange-source.log and
seurat-precision-provenance.json. Earlier failed comparisons remain retained.

## Hosted validation and backed Seurat comparison

Commit 343d644 passed all five jobs in hosted run 37087349484. Receipt:
artifacts/implementation/retrieval-0.1.1-hosted-ci.json. Package 0.1.1 now has
both local full-check and hosted-matrix validation; full readiness remains open.

New private backed-workflow comparison found concrete numerical/selection limits:
- BPCells Log1pSIMD uses single precision internally (verified in pinned source),
  although the normalized matrix reports double. Maximum normalization error was
  6.4458e-7. The backed diagnostic uses scaled error < 8 * 2^-23; in-memory retains
  its 1e-10 absolute check. The original failed comparison is retained.
- Native VST selected 198 of the same 200 features; gene428/gene600 were replaced
  by gene396/gene576. Native PCA distances differ by up to 3.6769. This is not
  evidence that either method is wrong, but they are not identical pipelines.
- With an explicit common feature set, PCA distance discrepancy is 8.3695e-5,
  exceeding the existing 1e-5 comparison threshold. Marker log2 fold changes differ
  by 7.3537e-8 (threshold 1e-8); p-value difference is 2.1860e-23. Those thresholds
  were not loosened. The comparison script deliberately exits with a failed check
  after writing the measurements; do not report it as a passing workflow.
- Counts and normalized layers remain backed, file hashes remain unchanged,
  and the planted cluster partition is preserved. Each backend's PCA variance
  passes its own bounded dense-SVD reference. The full backed pipeline is still
  not qualified; memory guarantees and cross-platform behavior are untested.

Run admin/verify_seurat_backed_workflow.R to reproduce. Receipts under
artifacts/implementation: seurat-backed-workflow.json, seurat-backed-pca-diagnostic.json,
seurat-backed-controlled-pca-diagnostic.json, seurat-backed-normalization-diagnostic.json,
seurat-backed-feature-diagnostic.log and seurat-backed-followup-provenance.json.
Synthetic comparison objects are retained as seurat-backed-comparison.rds and
seurat-backed-controlled-comparison.rds. G12/G26 remain open, timer stays active,
and no COMPLETE/STOP marker or submission was made.

## Retrieval follow-up 0.1.1

Recognize additional English/German base-R descriptive summary requests and
report-website rebuild requests. Regression cases include ordinary version/URL
questions that must not trigger those roles. Tests and fresh installed checks
pass; the authored 51-case benchmark retains all metrics at 1. The changed R source
is lint-clean. The broad package lint invocation reports existing cross-file lookup
and template/vignette findings; its log is retained and is not a clean lint claim.

Evidence is under artifacts/implementation/retrieval-0.1.1-* and
ask-language-followup.log. Full R CMD check --as-cran passed with 0 errors,
0 warnings and 1 incoming NOTE (new submission and Title prefix), including
rebuilt vignettes and PDF/HTML manuals. Tests: 5381 passed, 13 skipped, 0 failed.
Skipped live/browser/renv/optional-package checks are recorded in the test log;
the prior live evidence is not a new run of those checks. Archive SHA-256:
290c8d2b45696a8e3d599bbff5baca430aecda6ddc468f7c520f1ca3dea42e7e.
The archive has 234 members and excludes admin and artifacts.

These changes were informed by examined held-out failures and are development
regressions, not a fresh independent holdout. G12 model qualification and G26
Seurat/BPCells remain open. No COMPLETE marker or submission; retry remains active.

## Seurat and backed-data candidate evidence

Private script admin/verify_seurat_candidate_workflow.R passes on Seurat 5.5.1 /
SeuratObject 5.4.0: 600 synthetic genes and 120 cells, normalization checked against
an explicit formula, PCA standard deviations against dense SVD, repeatable clustering
and UMAP, recovered planted partition, positive/negative marker directions and
unchanged sparse counts. Cell-level marker ranks do not establish donor inference.

BPCells 0.3.1 commit 841559adcdc7df764d825f4fde364f65911779b9 was built locally
with HDF5 1.14.6 and hexbin in artifacts/bpcells-0.3.1-library. Frozen source is in
admin/bpcells-candidate; compilation used a separate artifact copy. Its configure
script attempted upstream install-count requests; use ENABLE_INSTALL_COUNTING=no
on future rebuilds. No study data were used or submitted. The first install lacked
hexbin; the second lacked executable configure permissions in the copy; both logs
are retained. Archive modes were restored only in the build copy.

admin/verify_bpcells_candidate.R passes the current interoperability helpers on a
small uint32 disk-backed count matrix. Layer inventory preserves backed storage;
donor sums equal an in-memory reference; hashes of backing files remain unchanged.
Instrumented realization is at most 80 values per block. Missing backing files
are refused and the fixture directory is restored. The upstream generic dense
conversion warning is retained; bounded blocks are explicitly measured.

Receipts: seurat-candidate-workflow.json, bpcells-candidate.json and
seurat-bpcells-provenance.json under artifacts/implementation. G26 remains partial:
these are local candidates, not a reviewed generated Seurat workflow, cross-platform
qualification or a full backed analysis pipeline. G12 also remains partial.

## Release 0.1.0: implemented with blockers

Classification: `implemented_with_blockers`. Every gate passes except G12
(no local model qualifies for planning; independent held-out `ask()` recall
11/14) and G26 (no reviewed Seurat analysis workflow; disk-backed BPCells
layers untested). Evidence is in artifacts/implementation: final-audit.md
(gate matrix), requirements-evidence.json, support-matrix.md,
independent-audit-2026-10-02.md and reverify-150e67b.md.

What 0.1.0 adds over 0.0.18:
- capability routing to the standard reflowR, hybrid or specialist profile;
- template 0.3.0 with reviewed lm/glm/nlme/Cox stages and a synthetic demo
  checked against independent references;
- a guarded study-data runner that writes to the reflowR `output/` tree;
- the standard package family in the catalog, with 56 role-scoped approvals,
  81/81 vignette sources and catalog `b726a5f4`;
- grounded `ask()`, a qualification-gated planner and a whole-project audit;
- the Shiny application, renv and targets, Bioconductor/Seurat bridges, and
  RES-007 release enforcement.

Six independent auditors tested 59eece2 and found defects in most gate groups,
including a runner that refused every configured project. Five repair branches
and follow-ups fixed them. An independent re-verification at 150e67b confirmed
all 18 re-checked defects fixed. Its one new C-locale finding was fixed in
bf3fce4.

Release commit eceb2bf passed locally:
- build, isolated install and `R CMD check --as-cran` with 0 errors,
  0 warnings and 1 NOTE (new submission; the Title starts with "CTTIR", as
  the spec prescribes);
- tests 5364 passed, 0 failed;
- coverage 90.17%, lint 0, installed smoke, the authored `ask()` benchmark
  (51 cases, all metrics 1) and pkgdown;
- three example projects, the standard one reaching `analysis_ready`;
- a live browser suite of 38 expectations, with screenshots reviewed.

Tarball SHA-256: 67338943b77ec3a2b42b5d5d8c230e1496990a00360bd635b4798bdf7bf2dd77.
Hosted run 37081334144 on eceb2bf passed all five jobs with `Status: OK`:
ubuntu release with all Suggests, Windows release, macOS release, ubuntu
oldrel-1 and ubuntu devel. The first hosted run (37077700370 on 4eea4cf)
failed on CRLF checkouts, the C locale on Windows and macOS, a RES-007
severity on R 4.5/4.7 and a test assumption; eceb2bf fixed these.

No CRAN or external-builder submission. No COMPLETE marker: G12 and G26 are
partial. The retry timer was stopped for this session and has been restarted.

## Continuation: unbalanced mixed models and group sensitivity

Checkpoint 2689a11 passed hosted run 37013761531. Public package remains 0.0.18.
The private candidate now has independent dense Gaussian likelihood verification
for unequal group sizes and two numeric predictors, under both ML and REML.
Coefficients, covariance, random/residual variances and random effects agree;
permutation and explicit complete-case results also pass. Three reference starts
converge to the same interior optimum. Bounded log variances prevent singular trial
matrices encountered by the initial unrestricted reference optimizer.

A separate synthetic-only helper implements bounded group-deletion sensitivity
(3 to 20 groups, at most 5000 rows). Eight deletion results match independent
reference calculations. Removing the only group with variation in a predictor
remains a failed refit and makes the result incomplete. Full-fit uncertainty is
used to standardize coefficient changes, with no automatic scientific cutoff.
Input remains unchanged; output excludes subject labels and data rows. Invalid
fixture authorization and too few groups are refused.

Scripts: admin/verify_candidate_mixed_unbalanced.R,
admin/candidate_mixed_influence.R and admin/verify_candidate_mixed_influence.R.
Receipts: candidate-mixed-unbalanced.json, candidate-mixed-influence.json and
candidate-mixed-unbalanced-integrity.json under artifacts/implementation. The
influence verifier also reruns the eleven original adapter checks. Policy and
limitations are in admin/candidate-mixed-policy.md.

Remaining: residual model adequacy, full callable/role documentation coverage,
cross-platform diagnostics and immutable approval records before integration.
This is private numerical evidence, not full readiness. No submission or COMPLETE
marker; retry remains enabled and active. No new public-package check is claimed.

## Continuation: mixed-model variance diagnostics

Checkpoint 4504a2d passed hosted run 37007780581. Public package source remains
0.0.18; this continuation develops ignored candidate adapter diagnostics.

Six mixed-model cases pass in admin/verify_candidate_mixed_variance.R. Balanced
ML/REML residual and random-intercept variances, fitted-mean covariance and random
effects agree with independent closed-form calculations. Row reversal is invariant.
The policy blocks numerical random-variance boundaries, a nonconverged returned
object with warnings, unavailable variance-parameter covariance, and unsupported
heterogeneous residuals. Residual structure and influence remain explicit gaps;
these numerical checks do not establish scientific or workflow approval.

Actual installed inherited dispatch was traced in an isolated session: pdMatrix
uses pdSymm then pdMat and pdFactor.pdLogChol for the observed random-intercept
class chain. Thirty-three selected callable signatures and bodies match pinned
source. Source integrity remains unchanged for 441 nlme and 736 survival files.
The ten existing diagnostic cases, eleven adapter fixtures and all three Cox tie
checks still pass. Evidence: candidate-mixed-variance.json, candidate-mixed-dispatch.json,
candidate-callable-alignment-mixed.json and candidate-mixed-integrity.json under
artifacts/implementation; policy details are in admin/candidate-mixed-policy.md.

Next: unbalanced/multiple-predictor uncertainty and residual/influence diagnostics,
then full role corpus and immutable approval records before production integration.
Full readiness is still incomplete. No COMPLETE/STOP marker or submission; timer
remains enabled and active. No new package build is claimed for private-only changes.

## Continuation: private adapter diagnostics and tied survival events

Milestone 18 commit dd1ea38 passed all five hosted jobs in run 37002612423.
Receipt: artifacts/implementation/milestone18-hosted-ci.json. Public package
remains 0.0.18; this continuation changes ignored development evidence only.

- admin/verify_candidate_diagnostics.R passes ten cases: unsupported engine
  refusal, overlap, forced nonconvergence, complete separation in both directions,
  quasi-separation, tied overlap, multivariable assessment gap, perfect linear fit,
  and aliased coefficients. Warnings remain recorded; fit return is not approval.
- admin/verify_candidate_survival_ties.R reruns the eleven adapter fixtures and
  checks Breslow, Efron and exact tied-event likelihoods against independently
  enumerated small risk sets. Coefficients, log likelihood and numerical inverse
  curvature agree; row permutation and explicit factor event labels are invariant.
- admin/verify_candidate_callable_alignment.R confirms 24 selected stats/nlme/
  survival implementations match pinned source formals and bodies. This does not
  cover every transitive callable, base QR, compiled code or platform behavior.

Receipts: candidate-diagnostics.json, candidate-survival-ties.json and
candidate-callable-alignment.json under artifacts/implementation. The candidate
still has mixed variance/residual and Cox proportional-hazards/influence gaps.
No public runner, workflow approval, full CRAN-readiness claim or submission.
Continue admin/production-adapter-readiness.md; the retry timer remains active.

## Milestone 18: explicit typed model settings

Optional analysis.model fields record a fixed intercept, binary logit link,
Cox tie method, and mixed random-intercept/residual structure with ML or REML.
Cox settings do not claim an estimated intercept. Unsupported or inapplicable
settings and missing review are reported separately from variable mappings.
The nested model state and model_settings_incomplete blocker make unresolved
choices visible; executable remains false. Valid reviewed mixed settings remove
the stale structure-review gap without bypassing adapter/revision/data checks.
No file is opened from a dataset registry, no model is fit, and no scientific
or package approval follows from recording settings. Legacy specs stay readable.

The ignored private candidate adapter now consumes these recorded settings,
with fixture-only execution authorization kept separate. Eleven checks pass against installed 0.0.18, including refusal of unreviewed
settings and both ML/REML fixed effects. The 0.0.13 historical project remains
byte-identical under synchronization; unknown model choices stay explicit.

The final archive was built under artifacts/implementation/milestone18-final.
Its isolated check passed 623 assertions but could not find pdflatex because that
command had a shorter PATH. Retain that failed log. The authoritative full check
is milestone18-as-cran-verified.log from the configured root environment, with
PDF manual generation enabled; preliminary or isolated results do not supersede
it. The authoritative run passed 623 assertions, zero errors/warnings and only
New submission, including PDF manual generation. Coverage for final code is
85.86%, lint is clean, and installed foundation,
private synthetic adapter, legacy and pkgdown (CI=true) checks pass.

Milestone 17 (fb7b9bb) passed all five hosted jobs in run 36997373879.

## Milestone 17: separate S3 method evidence

New source extraction records each S3 generic/class registration, named/default
implementation, exact static signature and source locator separately from public
exports. Duplicate dispatch registrations and unresolved implementations remain
unknown. Search labels method declarations without turning them into public
exports; generic/export records remain separate. Historical catalogs lacking a
method index show missing coverage, not zero. API differences report registration
addition/removal and changed implementation evidence. S4/S7 semantics remain open.

Real candidate counts: stats 448 S3 declarations/437 resolved, nlme 442/404,
survival 199/184. Six selected installed registrations match canonical signatures
and bodies. This is selected local evidence, not general dispatch or scientific
approval. Installed nlme update/search, prior-project pin isolation and rollback
pass; lme.formula is exported and also has a distinct method-declaration record.

Local validation: 598 assertions, --as-cran zero errors/warnings and only New
submission, coverage 85.74%, clean lint, installed foundation/method checks and
pkgdown (CI=true) pass. Exact archive and evidence: milestone17-* under ignored
artifacts/implementation. Full workflow readiness is still incomplete.

Milestone 16 (ebe975c) passed all five hosted jobs in run 36996455746.

## Milestone 16: local R distribution source indexing

Explicit r_distribution source records index a named package under
src/library without copying, changing, executing or installing its source.
DESCRIPTION.in receives only literal @VERSION@ substitution in memory. Exact
release and COPYING hashes participate in source identity; unknown placeholders,
development versions, identity conflicts and multiple locations are refused.
The source corpus preserves the original DESCRIPTION.in. Distribution manuals
and NEWS are outside this package-subtree inventory and remain explicit gaps.
Local registration does not authenticate upstream origin or grant role approval.

The acquired R 4.6.1 stats tree indexes 465 exports, 450 statically resolved,
320 documents (319 stored text), with zero approvals. All 647 originally acquired
source/license files remained hash-identical. Eight selected installed functions
previously matched canonical source signatures and bodies. These are candidate
facts, not compiled numerical qualification or executable workflow approval.
Validation: 574 assertions, --as-cran zero errors/warnings and only New submission,
coverage 85.28%, clean lint, installed foundation plus real stats source preview,
activation/rollback, and pkgdown pass. The separate logo commit is preserved and
included in the validated archive. pkgdown must run with CI=true to suppress its
remote favicon-generation step; that initial step failed and was not retried.
Evidence is recorded under artifacts/implementation/milestone16-*.

Ignored admin/candidate_model_adapter.R and its verifier now pass nine synthetic
checks: expected lm/GLM/mixed coefficients, an independent Cox likelihood,
literal-name safety, unchanged inputs, intercept-only fitting, explicit complete
cases and rank refusal. They are not installed/exported or project-selected.
Numeric-only predictors, diagnostic/convergence policy, full method evidence and
role approval remain outstanding before production integration.

Milestone 15 (a94e271) passed all five hosted jobs in run 36990549071.

## Milestone 15: explicit mapped-data checks

check_analysis_data(data, spec) checks an explicitly supplied plain data frame
against the registered analysis mapping. It reports aggregate missingness,
complete rows, unsupported types, nonfinite values, explicit event coding,
positive survival follow-up and repeated-unit/subject-time conflicts. Literal
column names are never evaluated. No data are opened from registry locations,
modified, imputed or fitted; no records or identifier values enter the report.
Complete-case intent must be explicit. Passing is structural evidence only,
not independence, rank, diagnostic, provenance or scientific approval. Current
engines remain candidates and executable remains false.

Local validation: 551 assertions, --as-cran zero errors/warnings with only New
submission, coverage 85.02%, clean lint, installed foundation/data checks and
pkgdown. The executed vignette demonstrates the new API. Artifacts and exact
archive hash are under artifacts/implementation/milestone15-*. Full scientific
adapters, role approvals and remaining v7 acceptance gates are incomplete.

Milestone 14 (1eccf82) passed all five hosted jobs in run 36987528971.

## Milestone 14: conservative static assignments and historical citations

Static extraction now refuses callable verification for repeated, conditional,
dynamic literal-name or modified bindings, within and across source files.
Function bodies remain unvisited and no source is evaluated. This is bounded
static evidence, not proof about arbitrary dynamic R execution or dispatch.
The bundled catalog was reindexed only after every original source-file hash
matched its frozen source. All 1340 exports remain present, 1323 resolved, with
zero workflow approvals. The prior catalog is retained for historical pins.
Nested-package citations handle both relative and legacy prefixed paths once.
An installed 0.0.13 project remains byte-identical under 0.0.14 sync/query, with
correct historical nested citations. Evidence: milestone14-* under ignored
artifacts/implementation. Local validation: 516 assertions, --as-cran zero
errors/warnings and only New submission; coverage 84.51%, lint, installed
examples and pkgdown pass. Full product acceptance remains incomplete.

Milestone 13 (1a12584) passed all five hosted jobs in run 36986049531.

## Milestone 13: literal Sweave and TeX corpus coverage

Source inventory now recognizes Rnw, Snw, Rtex, TeX and bibliography documents
as text when storage rights are supplied. Expressions and code chunks remain
literal, unevaluated data. Rights-unknown content is not stored; PDF files remain
metadata-only. The new regression fixture covers all five formats and retains
zero workflow approvals. Local validation passed: 490 assertions, --as-cran
zero errors/warnings and only New submission, coverage 84.32%, installed examples,
lint and pkgdown. Hosted results follow publication.

Canonical CRAN archives for nlme 3.1-170 and survival 3.8-9 were retained under
ignored admin/standard-sources with SHA256 inventories. They were not installed
or executed. This exposed the missing Sweave classification. Source extraction
and installed alignment evidence is local candidate evidence only; it does not
establish full semantics, licensed public redistribution or role approval.

Milestone 12 (97f1683) passed all five hosted jobs in run 36985142405.

## Milestone 12: typed analysis mapping and prerequisite assessment

Optional analysis mappings now record a registered dataset ID, literal variable
roles, predictors, event/non-event codes, estimand, time origin/units and explicit
missing-data intent. References and conflicting roles are validated. Builder and
sync return a shared read-only assessment of missing fields, candidate engines
and unsupported designs. Audits distinguish recorded configuration from pending
workflow approval. No data are opened and no model/formula is evaluated, including
when the user has recorded analysis approval. Predictive/causal designs never
fall through to an independent association model. Mixed-model structure review,
canonical package evidence and executable adapter approval remain open.

Evidence: tests/testthat/test-analysis-plan.R and the executed analysis-mapping
vignette example. Local validation: 481 assertions, --as-cran zero errors/warnings and only New
submission; coverage 84.32%. Lint, installed examples and pkgdown pass. Hosted
results are tracked in ignored artifacts/implementation/status.json and
milestone12-* receipts.

Milestone 11 (9fe2a9e) passed all five hosted jobs in run 36977928396.

## Milestone 11: explicit local draft persistence

The local interface exports bounded JSON answers and restores creation/Configure
drafts. Drafts omit parent paths, data bindings and accepted plans; Configure
restores require the same project identity. Unknown keys, duplicate keys, large
files, malformed JSON and path-valued configuration are refused. Restoring or
editing answers invalidates the accepted preview, including answers changed
while a background preview is running. Revision/export status makes unsaved
answers visible. There is no automatic saving or complete questionnaire yet.

Browser export updated the live status correctly. Browser upload chooser and
application screenshot capture timed out; these browser checks remain incomplete.
A narrow viewport DOM check found no document overflow; the file input is hidden
by its upload control. Server tests cover restore, project isolation, fresh
preview/application, active-job refusal and stale-result rejection. Full source
check passed with 437 assertions, zero errors/warnings and only New submission.
Coverage is 83.76%; installed examples, lint and pkgdown pass. Evidence is under
artifacts/implementation/milestone11-*. Hosted validation follows publication.

## Milestone 10: executable introductory vignette

Adds vignettes/getting-started.Rmd with executed offline preview/create/repeat,
configuration sync, pinned evidence/corpus queries and project audit examples.
Remote update, runtime acquisition and report rendering examples are explicitly
not run during documentation builds. The vignette labels incomplete scientific,
planner and UI functionality and separates static evidence from workflow approval.
Its disposable directory and catalog option are restored at the end. R CMD build
successfully rendered the vignette. The final source check passed with 406
assertions, zero errors/warnings and only the New submission NOTE. Installed
examples, lint and pkgdown pass. Browser screenshot review of the rendered
vignette passed. Evidence is under artifacts/implementation/milestone10-*.

Next scientific-adapter preparation: admin/verify_standard_candidates.R passed
reviewed deterministic lm, binomial GLM, nlme mixed-model and survival Cox fixtures;
Cox results matched a separate partial-likelihood optimizer. DescrTab2 2.1.16
requires an explicit custom no-inference callback (there is no public No test
choice); it produced labelled NA test results once per variable. These are local
candidate smokes only. Source revision alignment, full corpus, mapping/guards,
generated adapters and workflow approvals are still pending.

Milestone 9 commit 84be55527f58e18be087eda99b70c54e6ad4024c is on main. All five
hosted jobs passed in run 36974981221. Milestone 10 (bd787b5) is published;
all five hosted jobs passed in run 36976603611.

## Milestone 9: guarded catalog writer recovery

Pointer activation now records a journal before its atomic replacement. Applied
updates recover only a verified stopped local writer, check the complete active
API/resource snapshot, and require a journaled pointer to match its recorded
preimage or postimage. Interrupted writer metadata is renamed into retained
recovered-locks evidence, not deleted. Snapshots and project pins are untouched.
Live, unknown or foreign-host writers, corrupt pointers and conflicting journals
are refused. Preview stays read-only and never recovers a lock. An interrupted
recovery guard itself still requires manual review; no stale-time assumption is
used to override ownership. An inactive incomplete candidate does not displace a
verified active preimage.

A real installed 0.0.9 worker was terminated after activation: live writer refusal,
postimage preservation, retained journal and rollback passed. Source check: 406
assertions, zero errors/warnings and only the New submission NOTE. Installed
examples, lint and pkgdown pass. All five hosted jobs passed.

Milestone 8 commit bd045faeb2c1ada573cf4799a9015e141bd2ceda passed all five
hosted jobs in run 36974366046.

## Milestone 8: bounded public GitHub refresh

Adds mode=remote for explicit public GitHub source registrations (owner/repository,
expected package, optional ref/subdirectory). It resolves one commit, rejects
truncated trees, verifies downloaded Git blob hashes and stages source without
executing code. Fetches use fixed public endpoints, no private authentication or
redirects, per-request timeout/size limits, a checked source-time budget and file/
byte bounds. Local mode never fetches remote registrations. Explicit package
selection skips unrelated registrations with declared package identities.

Real reflowR commit cd1243a068ff2c8fb6796e34b58f6c6ce6af87e8 passed remote preview,
activation, preserved old project pin, GitHub resource observation and rollback.
No installs or model pulls occurred. Resources retain their curation. Initial
resource observation times mean preview/applied composite IDs may differ even
when API content IDs match; repeat unchanged applied sources retain their IDs.
Fixtures test hash mismatch, truncated trees, unsafe destinations, symlinks,
nested package roots, identity mismatches and malformed metadata. Evidence is
in artifacts/implementation/milestone8-*.

Final source check: 388 assertions pass, zero errors/warnings and only New
submission NOTE. Lint, installed examples and pkgdown pass. Automated coverage
is 83.33% (remote backend 68.24%, supplemented by live remote evidence).

Automatic discovery, non-GitHub source backends, registry curation across all
standard/IMBI sources, pruning, release migration and automatic lock recovery
remain incomplete. A public GitHub backend alone does not complete update gates.

Milestone 7 commit 5efb4250ab575420cd5b70ff873ad905cca78e6f passed all five
hosted jobs in run 36973407118.

## Milestone 7: revision-aligned documentation corpus

Adds bounded, literal source-document inventories and content with explicit rights
basis. Files are limited to 1 MB, package documentation to 10 MB, inventory to
5000 entries and nesting to 12 levels; links are rejected. Missing rights retain
hashes/inventory only. Images are explicitly metadata-only. No document code,
macros, CITATION or vignette expressions are evaluated. Coverage describes the
available source manifest, not all possible upstream web documentation.

The bundled pinned reflowR revision contains 31 licensed text documents and two
image metadata records. The previous bundled catalog is retained under
inst/extdata/history, preserving accepted older project pins. A real
0.0.6-created project remains byte-identical under the new sync and API lookup.
The new corpus ID is 8e5a591daa5b55552bc7ca52a659fe422ec369aeab19f87adf9de60f94971788.

Documentation hashes participate in source identity, including same-version
README/NEWS/vignette changes. Updates report document additions/changes/removals;
rollback restores content with the same immutable composite pointer. Diffs are
computed before activation. Search returns literal excerpts of at most 1200
characters, with source revision and document locator. Indexed documents remain
unapproved; verified_only advice excludes them. APIs and complete workflow role
approvals remain separate. Corpus integrity is checked on read and in audit.

Fixtures cover rights restrictions, non-execution, removed vignettes, doc-only
changes, oversized/linked inputs, failed activation, rollback and historical pins.
Measured automated coverage is 83.73%; the focused corpus suite has 43 passing
assertions. The source archive excludes admin and artifacts. An initial portable
path NOTE was repaired by shortening the historical catalog directory.
Final source check passed 368 assertions, zero errors/warnings and one New
submission NOTE. Installed examples, lint and pkgdown passed.
G27 is still incomplete: semantic method evidence, full supported-profile corpus,
approval fixtures and grounded planning remain outstanding. No readiness claim.

The hourly retry successfully resumed work on 2026-10-02; the former desktop
writer-lock blockage is no longer current. Keep the timer active while unfinished.
Do not create COMPLETE or STOP without the corresponding completed/paused state.

## Milestone 6: pinned template adaptation

New projects use template 0.2.0: a hashed MIT-licensed adaptation of the minimal
reflowR layout at cd1243a068ff2c8fb6796e34b58f6c6ce6af87e8. The adapter records
source file hashes, license and changes, fixes navigation YAML, and omits upstream
automatic installs and Git effects. Explicit rendering uses rmarkdown::render_site;
reflow_init and workflowr are not invoked. The five pages include placeholders and
an isolated synthetic least-squares fixture with expected coefficients. This is
not full standard-workflow integration; its readiness blocker remains present.

Existing 0.1.0 templates retain their renderer and ownership baselines. A real
project created using installed cttiR 0.0.5 remained byte-identical under repeat
creation and synchronization. Template-lock mismatches fail before mutation, and
the source-provenance record is managed. User-edited analysis pages are preserved.
Real rendering passed with rmarkdown 2.31 and Pandoc 3.7.0.2; browser accessibility
inspection confirmed navigation, synthetic labels and expected table values.
Screenshot capture still times out and visual review is unverified. Test servers
and browser tabs were stopped after inspection. Installed three-project smoke
and pkgdown build passed. Lint is clean; measured automated coverage is 82.95%.
Final source check: 325 assertions pass, zero errors/warnings, and only the
New submission NOTE. Evidence: milestone6-* under
artifacts/implementation; example HTML under artifacts/examples.

## Milestone 5: local application and pin isolation

Adds setup_app() and configure(): one Fast/Detailed draft, an explicit file-plan
preview before writes, background workers for local tools, preview cancellation,
duplicate-job suppression and stale-input/catalog/file checks. Configure applies
through sync(). The interface does not execute study analysis or offer remote
multi-user hosting. Detailed fields currently use validated JSON; the full typed
questionnaire, saved drafts, guided runtime/update application and report export
remain pending. New project identifiers are assigned on creation; previews show
file actions rather than presenting prospective content hashes as final hashes.

Fixed repeat creation to preserve both accepted API and resource pins without
consulting a corrupt global pointer. Missing control files retain the documented
path-conflict error. Creation and synchronization workers recheck the reviewed
catalog/file plan at execution time; mixed catalog snapshots are rejected.
Installed-worker tests exercised both rejection and successful application. Full tests: 291 passing assertions. Lint passes; automated
coverage is 82.83% (app 76.47%, runtime 57.02%). The source archive passed --as-cran
with zero errors/warnings and one New submission NOTE. Installed examples pass.

Real browser checks created Fast and Detailed synthetic projects and applied a
Configure change through actual background workers. A 390px viewport initially
overflowed; after fixes the document width is 375px and plan width is 321.2px.
Two screenshot attempts timed out in the browser connection. DOM/interaction QA
passed, but screenshot visual review remains unverified and is not a release pass.
The owned browser test server was stopped; test projects remain under ignored
artifacts/examples. Runtime test artifacts are retained under artifacts/runtime.

Hosted milestone-5 commit 882ded0c000059aa8fe47f7a2cb4068848b22f3b passed all
five supported jobs in run 36824581448.

## Milestone 4: immutable local catalog lifecycle

Adds update(), update_knowledge() and rollback_knowledge(). Local source records
are configured explicitly; parsing never executes source. Knowledge and resource
observations are staged before one composite pointer activation. Whole package
revisions replace prior exports; removed APIs cannot reappear through fallback.
Same-version source edits have distinct hashes. Historical project API/resource
pins survive global update and rollback. Curated resource fields remain unchanged.

255 assertions pass, including failed extraction, resource staging and activation,
corrupt-history rejection, repeated no-change updates, dry-run persistence, removed
exports and same-version edits. A real frozen reflowR source passed local preview,
combined activation, pinned project creation and rollback. Detailed evidence is
under artifacts/implementation/milestone4-*. The source archive passed --as-cran with zero errors/warnings and one New
submission NOTE. Installed examples and lint passed; automated coverage is 83.29%.
Hosted commit 39577ffc8a402dd51c34ceeddab6a7b8918d91ad passed all five
Linux release/oldrel/devel, Windows and macOS jobs in run 36822377939.

Remote fetch adapters, bounded discovery, pruning, release migration and automatic
interrupted catalog lock recovery remain pending. Unsupported update policies fail
before mutation. They are not silently accepted or marked as verified.

## Milestone 3: catalog, recovery and local runtime

Version 0.0.3 adds a revision-scoped public API catalog (30 package roots,
1340 exports, 1323 statically resolved, zero workflow approvals), evidence
queries, project API pins, guarded journal recovery, and platform path fixes.
Multiple source definitions remain unresolved instead of receiving a false
verified signature. All source extraction is static and never executes code.

Explicit setup verifies the pinned Ollama 0.34.4 Linux x86_64 publisher archive,
starts only an owned cloud-disabled process, verifies the downloaded model and
runs a bounded CPU structured-output probe. Dry runs are read-only; offline setup
never acquires software or pulls models. Concurrent setup and unmanaged daemons
are refused. Live audit probes an already configured runtime without installing
or starting one. The qwen2.5-coder:1.5b candidate passed a real CPU smoke in 3.642
seconds; workflow usefulness remains unqualified. Retained-archive acquisition
was exercised through the package implementation, including extraction and binary
verification. Other runtime installation platforms remain unverified.

Validation: 210 test assertions across the suite; 81.96% automated coverage
(runtime 57.02%, supplemented by separate live acquisition/inference evidence);
selected lint rules pass. Source checks and hosted evidence are recorded in
artifacts/implementation. No package readiness claim follows from this milestone.
Hosted milestone-3 commit 58514d46d6cd62961e46415628473cc48cd10b9c passed
Linux release/oldrel/devel, Windows release and macOS release in run 36821452382.

Retry history: hourly attempts ran, but the desktop session retained this thread's
write lock, so CLI resume was rejected. Timer execution is not evidence of task
progress. Direct interactive continuation resumed on 2026-10-01. The timer remains
configured; do not bypass another active writer or run duplicate modifications.

Next: extend remote catalog/source support and complete standard
source/documentation coverage and reviewed workflow adapters,
then qualify local planning and build the shared Shiny interface. Complete all
remaining acceptance gates before claiming CRAN readiness.

## Milestone 2: project lifecycle

Added sync previews/application, edited-file conflicts, per-file backups and failure rollback.
Added audit/doctor, JSON/Markdown reports, strict failure conditions and baseline-verified
missing managed-file repair. 131 assertions pass locally. R CMD check --as-cran reports
0 errors, 0 warnings and the expected New submission NOTE. Cross-platform Actions
checks are configured; results must be verified after push. Full CRAN readiness is
not yet established.

The maintainer contact is raban.heller@uni-ulm.de, explicitly approved by the user.
The user authorizes readiness verification only: do not submit to CRAN.
An ignored retry controller under admin/automation is scheduled with the user timer
cttir-readiness-retry.timer for 2026-09-28 11:00 Europe/Berlin and hourly thereafter.
It resumes this task while unfinished, suppresses recent activity, and honors STOP
and COMPLETE files. Disable the timer on completion or explicit pause.

reflowR source revision cd1243a068ff2c8fb6796e34b58f6c6ce6af87e8 was inspected and
its initializer run with Git/open/change_wd disabled. Generated setup auto-installs
a broad package set, and generated navigation YAML is malformed. The initial template adaptation is now implemented in milestone 6; complete
standard analysis integration remains pending.
Public organization discovery returned 40 repositories. The catalog was built
from materialized verified public revisions, not neighboring dirty worktrees.
The frozen source inventory and extraction evidence are retained locally.

## Milestone 1: offline foundation

Implemented and locally validated:

- Installable R package with generated namespace and reference documentation.
- `project()`: three-input offline scaffold, parent/child path semantics,
  staging plus rename, cooperative writer exclusion, portable slugs, identity
  preservation, read-only preview and repeat creation, existing edit preservation.
- `validate_config()` and `validate_spec()`: local JSON-schema and semantic
  validation, explicit null handling, keyed array merge, unknown-field rejection.
- `resources()`: parameterized read-only queries of the original 229-candidate
  research seed, physical integrity checking and explicit project-pin resolution.
- Project specification, build lock, file ownership metadata, analysis plan,
  registry/dictionary, publication trees and a read-only validation script.

Evidence: 89 passing assertions; source build; installation into a separate task
library; fresh-session installed smoke with primary, mixed/publication and
standard-profile examples; `R CMD check --no-manual --no-build-vignettes` reports
0 errors, 0 warnings, 0 notes on Linux x86_64, R 4.6.1.
`tools/verify_foundation.R` reproduces the installed example checks.

The source tarball and detailed logs are local under the repository root and
`artifacts/implementation/`. Example scaffolds are in `artifacts/examples/`.
The original ZIP and checksum-verified extraction remain ignored in
`admin/cttir-v7/`. Neither admin inputs nor build artifacts are published or
included in the source package. The selected schema and metadata resources are
intentionally bundled under `inst/`.

## Contract decisions

- The package and namespace use the requested case-sensitive name `cttiR`.
  Existing `cttir-project.yml`, `cttir-lock.json`, `.cttir/` and condition-class
  names preserve the supplied file-format contracts.
- Baseline schemas are adapted to draft-07 for the installed validator; their
  figure reference is inlined to avoid remote schema resolution. The conditional
  standard-profile/backend requirement is enforced and tested.
- Unsupported integration options raise explicit errors. Pending public APIs
  are not exported as stubs. First-milestone defaults use no scheduler, dependency
  environment, Git initialization or reporting backend.
- Standard profile selection records reflowR integration pending. It does not
  imply that reflowR was invoked or that a supported analysis adapter exists.
- Catalog candidates preserve their original verification levels. No workflow
  revisions are approved. Static API signatures are recorded separately from the resource seed.

## Remaining work

Full v7 acceptance is not complete. Continue in this order:

1. Complete CTTIR/standard/IMBI registry coverage and non-GitHub remote source refresh and release policy,
   source filtering, discovery, retained-history recovery and report schemas.
2. Build the reviewed reflowR template adapter and standard tidy/table/model
   adapters; test actual synthetic lm/GLM, mixed and survival workflows. Add
   capability routing, explicit mappings, dependency preparation and safe targets.
3. Store the licensed revision-aligned documentation/vignette corpus and tested
   role approvals. Current workflow approval count is zero, so G27 fails.
4. Implement grounded planning and complete the required usefulness/negative-case
   benchmark. A runtime smoke is not a planning qualification. Extend runtime
   installation beyond the currently live-tested Linux x86_64 backend.
5. Complete biological object/release compatibility and loss-aware bridges,
   accessible figure checks, patchwork and appropriate Seurat capability routing.
6. Complete typed UI questions, durable drafts, guided update/runtime apply,
   cancellation/recovery and exports. Finish screenshot visual review.
7. Expand audit profiles and full interruption/recovery fixtures, then build
   vignettes, documentation site, URL/spelling reports and the final gate matrix.

Package checks on the supported hosted matrix passed for milestones 3 through 6;
this does not qualify every runtime installer or scientific adapter. Full
scientific validation, production readiness and CRAN readiness are not claimed.
No CRAN or external-builder submission has been made. Creation and transaction
locks coordinate cooperating writers; hostile concurrent filesystem actors are
outside the current contract. Keep the exact acceptance gates in the ignored v7
specification and preserve the distinction between passed, partial and untested.

Authorization: validated milestones may be committed and pushed directly to main
using the repository's configured user identity. Preserve ignored admin inputs.
