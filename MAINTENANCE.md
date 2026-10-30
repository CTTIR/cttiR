# Public package readiness review — 30 October 2026

Scope: public package code at `7f52d4de3ad5992f9e1748eb464de784a204b5c7`, in an
isolated worktree. Existing source checkouts, private reference collections,
training artifacts and model qualification work are outside this review.

## Purpose and acceptance criteria

Researchers should be able to preview and create a deterministic project,
review revision-bound package approvals, update it without losing local work,
and recover safely from interrupted catalog or project operations. The two
strict synthetic study recipes must preserve accepted measurements and IDs
through compilation, computation and recorded results. Reports must consume
the pinned results, and changed inputs or declarations must be refused.

Local readiness requires an installed-package first-use exercise, both study
recipes with independent numerical comparisons, relevant failure paths, package
checks and live application checks. It does not establish scientific approval,
model qualification, hosted CI acceptance or deployment.

## Findings and strategy before implementation

1. **High, confirmed:** `R/study-spec.R` accepts matrix-valued continuous columns.
   A declared exposure can silently expand into two predictors, both labelled
   with the same declared variable name. Require one plain numeric value per
   observation, plain ID vectors, and ordinary categorical vectors. Validate
   supported integer, double and factor inputs as well as rejected shapes.
2. **High, confirmed:** `R/study-project.R` saves raw data with `write.csv()`.
   Its numeric conversion uses 15 digits regardless of `options(digits = 17)`;
   adjacent doubles collapse and small paired changes can disappear. The shared
   default JSON conversion also rounds recorded results. Use explicit 17-digit
   conversion, an accurate decimal reader, and verify the complete projected
   input round trip before creating a project. Preserve the CSV format and
   external provenance checks. Reject numeric IDs whose text forms collide.
   Validate measurements, results and report reuse against direct computation.
3. **Medium, confirmed:** README links to ignored, unavailable `CONCEPT.md` and
   `ROADMAP.md`. Replace these with this public record and runnable installation
   and first-use instructions; keep model and scientific limits explicit.
4. **Low, confirmed:** the baseline package check includes a nonstandard
   `pkgdown/` directory in the source package. Exclude website-only material
   while retaining source documentation and attribution.
5. **Medium, confirmed during first use:** `tools/verify_foundation.R` still
   requires the removed `reflowR_integration_pending` blocker. Current projects
   have approved standard adapters but remain incomplete scaffolds until their
   analysis settings and environment are prepared. Check those actual positive
   and negative contracts, then run the generated project validators.
6. **Medium, confirmed in the live browser:** the runtime smoke test assumes a
   Linux x86_64 host and times out on the correct macOS platform blocker. Check
   the documented blocker for the host while retaining the blocked-state,
   no-runtime-directory and offline-acquisition assertions.
7. **High, confirmed in the standard workflow:** CLI-created repositories on
   this machine use `main`, but git2r's empty-repository detection does not see
   the CLI's default-branch setting. Workflowr then tries to read a nonexistent
   HEAD and rendering fails. Retain the actual initial branch in local Git
   configuration for newly created projects. Verify a nondefault branch, empty
   history and index, unchanged repeated initialization, and real rendering.
8. **High, confirmed in the paired browser journey:** cttiR admits macOS system
   aliases such as `/var`, but the strict reflowR compiler rejects their symbolic
   spelling. Earlier installed tests normalized their temporary parents and
   missed the interface failure. Validate the caller's path first, then resolve
   only already-admitted aliases before fingerprinting and compilation. Retain
   arbitrary-link rejection and test the actual browser sequence and download.
9. **Low, confirmed in report rendering:** the generated workflowr index omits
   its documented `site: workflowr::wflow_site` declaration. Add it, update the
   generator identity, and rerun the real render and local-asset checks.
10. **Medium, confirmed visually:** the standard descriptive table renders with
    adjacent, unseparated headers and values on desktop and mobile. Apply the
    report framework's table styling and a labelled, keyboard-focusable scroll
    region. Hide the rendering plumbing while retaining the source and workflowr
    provenance. Verify the actual values, escaping and narrow-screen interaction.
11. **Medium, confirmed in browser requests:** standard workflowr reports load
    MathJax scripts and a workflowr badge from upstream servers. Local execution
    does not make the rendered report offline. Document this boundary and require
    deployment-specific asset review for confidential reports; retain mathematical
    rendering compatibility. Fully offline asset provisioning is deferred.
12. **Medium, confirmed against CRAN:** the preferred DescrTab2 2.1.16 backend is
    archived, so an ordinary `install.packages("DescrTab2")` cannot provision it.
    Preserve the reviewed pin, document its explicit archive installation and
    the existing `table_backend = "none"` base-table alternative, and exercise
    both routes. Archived availability is not evidence of current maintenance.

Milestones, in order:

- Repair study input and output integrity, with regression tests and precise
  user-facing contracts. Completion: unsupported column shapes stop and accepted
  measurements survive storage exactly for both recipes.
- Verify dependable daily use: project creation/update/conflict handling,
  catalog recovery, installed study execution/rendering and browser workflows.
  Completion: meaningful checks pass, or a concrete external limitation is
  recorded separately from a code defect.
- Prepare the release handover: clean archive, package checks, documentation,
  provenance and exact first-use commands. Completion: all locally verifiable
  criteria have evidence; remote checks and publication remain explicit gates.

Deferred: additional scientific designs, model selection, training, real-study
approval and deployment. These require independent evidence and decisions;
expanding the feature set would not repair the confirmed integrity failures.

The numeric-writing behavior is documented in the official
[R data-output reference](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/write.table.html).
Exact round-trip checks also protect against platform-dependent decimal-reader
rounding; merely printing more digits is insufficient evidence.

## Verification and remaining work

Baseline build and `R CMD check --as-cran` completed: 7,316 assertions passed,
zero failures or warnings, and 27 explicit skips. Its three check notes were
development/new-submission metadata (including the non-CRAN reflowR dependency),
the packaged website directory, and the system HTML validator's age. Seurat
packages were unavailable for that baseline; they and the archived DescrTab2
2.1.16 were installed before final validation.

The final archive passes `R CMD check --as-cran`: **zero errors, zero warnings,
one note** for the development/new-submission metadata and non-CRAN reflowR
dependency. **7,502 assertions pass with zero failures or warnings and 21 explicit
skips.** Vignette rebuilding, PDF manual and HTML manual validation pass. Browser,
environment, source-message and workflow lifecycle skips were exercised
separately below; the archived DescrTab2 branch also passed separately. The
complete interoperability module passes another **432 assertions, zero failures,
warnings or skips**, with Seurat 5.5.1 and SeuratObject 5.4.0, covering the
historical-version fixture skipped under the current Seurat 5.6.0 library.

Completed verification on macOS ARM64, R 4.6.1:

- Installed study integrations passed 104 assertions; integrity regressions
  passed 34, including matrix rejection, factor references, numeric-ID
  collisions, exact decimal storage and accepted/refused path aliases. Both
  recipes agreed with direct computation and preserved recorded results through
  rendering. Eight Git regression assertions verify the selected branch, empty
  repository/index and unchanged repeated initialization. Initial source-loaded
  compiler attempts correctly failed because
  this workflow requires an installed package; installed reruns passed.
- A separate 100,000-row round trip preserved all IDs, four numeric columns and
  fitted coefficients exactly. Writing took 1.052 seconds and reading 1.089
  seconds on this machine; this is a local measurement, not a performance bound.
- Live application tests passed 47 general/assistance assertions and eight
  paired-study assertions. The paired browser journey previewed, compiled,
  computed, rendered 12 pages and downloaded a declaration matching all recorded
  report hashes. Desktop and mobile layouts were inspected.
- Standard workflowr and targets lifecycle fixtures passed, including external
  data changes with unchanged size/mtime, added code, deleted outputs, reuse of
  unchanged computation, HTML escaping and refusal of incomplete analysis.
  The repaired table was inspected and exercised in Chromium: readable spacing,
  keyboard focus and horizontal scrolling, no document-wide mobile overflow,
  no JavaScript errors and no failed local asset requests.
- An installed first-use exercise created and validated three scaffold types.
  A separate archive installation produced a four-page report with the expected
  mean, all 68 local references present, no staged files and no Git commits.
  The upstream jqueryui demo file has one unused broken asset reference; the
  declared report pages do not link to that demo. This remains upstream debt.
- Explicit environment/interface checks passed 832 assertions, including offline
  preparation and restoration, blocked downloads, local fixture installation,
  and complete English/German message keys. Workflowr creation/migration checks
  passed 71 assertions; standard descriptive/model checks passed 85 with the
  pinned DescrTab2 backend installed.
- New test files pass the repository linters; existing source style findings
  remain. Workflow syntax and `git diff --check` pass. A narrow scan found no
  tracked private-key/common-token patterns; this is not a comprehensive secret
  or vulnerability assessment.
- All 47 generated documentation pages have their 1,823 local references and
  356 fragment targets present. The site builds successfully with upstream
  Pandoc math-option deprecation warnings retained. All 83 upstream notice files
  match their hashes, and the notice manifest matches all 14 catalog snapshots;
  this verifies preserved provenance, not a new legal or training-use approval.

The frozen source archive SHA-256 is
`577697590b0e577e4d2ef64ff2cfe3a86e656420cc141783d62016a1829ffabe`.
All 443 corresponding source files match; generated build metadata is checked
separately. Intermediate checks superseded by subsequent repairs were cancelled
and are not counted as passes.

### First use and repeatable checks

Use R 4.1 or newer and the installation instructions in README. From this source
directory, with `remotes`, `knitr` and `rmarkdown` installed:

```r
remotes::install_local(".", build_vignettes = TRUE)
p <- cttiR::project("Example Study", "primary_research",
  "Describe outcomes and plan an adjusted regression", path = tempdir(),
  options = list(workflow = list(table_backend = "none")))
p$readiness
```

Creation produces a scaffold; prepare its pinned environment and review the
analysis settings before executing study data. For the local interface install
`shiny` and `callr`, then run `cttiR::setup_app()`. Strict synthetic studies also
need Git, Pandoc, workflowr, git2r and the exact reflowR revision given in README.
Retain declarations outside those projects and follow the complete vignettes.

With the full test dependencies prepared, the release checks are:

```sh
R CMD build .
R CMD check --as-cran cttiR_0.1.1.9000.tar.gz
Rscript --vanilla tools/verify_foundation.R /path/to/installed/library /new/evidence/path
Rscript --vanilla tools/verify-workflowr-runner.R /new/workflowr/evidence
Rscript --vanilla tools/verify-workflowr-targets.R /new/targets/evidence
```

The last two require the standard workflow's exact dependency pins. Browser tests
are explicit (`CTTIR_LIVE_TESTS=true`) and need shinytest2 plus a Chromium binary;
they use temporary projects and a private package installation. Linux-only
runtime process checks and an actual qualified model remain external to this
macOS validation. No model was acquired or qualified by this review.

Strict dependency snapshots are not restoration manifests. Preserve the exact
prepared writer library together with the external declarations; changing or
reinstalling the runtime requires identity verification. Portable restoration
across different R environments remains outside this release's evidence.

Before release: obtain hosted checks for this candidate on the supported OS/R
matrix, choose release metadata and resolve any intended CRAN distribution of
reflowR. Confidential/offline report deployment needs asset provisioning and
review. Real-study interpretation and model qualification require their own
evidence. Existing pinned projects retain their original bytes/runtime; compile
a new strict project to use the repaired compiler. No changes from this review
have been pushed, submitted or deployed.

The local acceptance criteria for project scaffolding and the supported synthetic
workflows are satisfied. The remaining unexercised test conditions require Linux
runtime process behavior or a qualified model; they are not represented as passed.
