# cttiR 0.1.0

* Route every project through a deterministic capability router: the standard
  reflowR profile is selected when no approved CTTIR specialist adapter matches,
  approved specialist stages yield a hybrid profile, and infrastructure never
  counts as specialist evidence. Goal keywords suggest only unset fields.
* New projects use template 0.3.0: the adapted reflowR layout plus a reviewed
  stage library (delimited import, structural checks, dplyr role selection,
  DescrTab2 or base descriptive tables without tests, accessible
  ggplot2/patchwork figures, lm/glm/nlme/survival adapters, diagnostics and
  broom effects), a synthetic demonstration checked against independent
  references and a guarded study-data runner.
* Bundle the standard package family (tidyverse components, DescrTab2,
  ggplot2/patchwork/palettes, model engines, broom, rendering, targets, renv,
  Bioconductor containers, Seurat and R 4.6.1 base packages) from verified
  sources, with 56 role-scoped approvals bound to exact source revisions and
  the vignette sources of every approved revision stored as text.
* `ask()` returns grounded steps, prerequisites, approved revisions, citations
  and reviewed snippets validated against the approved function table.
* Optional targets pipelines, renv environments prepared in an isolated process
  and Git initialisation of the generated project only.
* Accessible figure policy validation, Bioconductor/Seurat interoperability
  helpers with loss reports and donor-level pseudobulk, and modality routing.
* CRAN and Bioconductor source backends, remote resource refresh, bounded
  discovery, pruning with tombstones and Bioconductor release policy in
  `update()`.
* Audit check registry with standard-workflow, resource and approval checks,
  allowlisted repairs (including catalog pointer recovery) and readiness levels
  derived from local receipts.
* A modular Shiny application with a bilingual conditional questionnaire,
  knowledge, resources, audit and runtime views.
* A schema-constrained local-model planner with deterministic fallback and a
  live benchmark; no tested model met the thresholds, so planning stays
  deterministic by default. A model plans only when the runtime manifest marks
  it qualified, and goals that read like instructions are never sent to it.

Changes from the independent pre-release audit:

* The study-data runner compared pinned versions as strings ("1.1-3" against
  "1.1.3") and refused every configured project; versions are now compared
  with `package_version()`. Study outputs follow the reflowR `output/` tree.
* The longitudinal demo check verifies the adapter's own REML fit, demo cases
  recount event coding from raw data, binary endpoints are described as
  counts, repeated measures at the first observation per subject, and a
  two-valued outcome under a continuous model fails the data checks.
* `ask()` reports precise gaps for method families without an adapter
  (competing risks, ordinal, GEE, count, quantile, propensity, Bayesian, GAM,
  prediction, meta-analysis), never returns a nearby engine's snippet, cites
  versioned sources, normalises lookalike characters before injection
  screening and no longer refuses benign wording. Capabilities are approved
  only when every callable they need is approved, so Seurat clustering is a gap.
* The code validator follows R's partial argument matching (`F =`, `FU =`),
  rejects more process, evaluation and connection calls and reports missing
  required arguments.
* `audit()` parses all project R code (scripts and R Markdown/Quarto chunks)
  for unsafe, unparseable or unresolved calls, fails on control metadata that
  differs from its regenerated baseline, compares the resource database with
  its JSON mirror row by row, and binds app repairs to the previewed plan.
* Stale creation and writer locks are recovered safely; an edited managed file
  no longer blocks unrelated syncs; unedited user-owned files (data registry,
  publication metadata) are updated and edited ones receive reviewable
  proposals; a hand-edited `cttir-project.yml` is previewed and accepted
  through `sync()`; analysis plans list research-type specific unknowns.
* The bundled catalog verifies under the C locale; rollback works when the
  active snapshot is damaged; `resources()` reports freshness and fetch status;
  the Bioconductor release policy is part of the atomic snapshot.
* Figures keep missing values visible with a distinct colour and shape, refuse
  merged legends with different meanings, and record unresolved accessibility
  findings in the demo receipt. Interop helpers refuse cells as pseudobulk
  replicates and check pinned versions.
* Schema validation no longer creates `.Random.seed` in the user's workspace.

# cttiR 0.0.18

* Record typed model choices separately from variable mappings: intercept/link,
  survival ties, and mixed-model structure and estimation. Report missing,
  unreviewed or inapplicable settings without fitting or approving models.

# cttiR 0.0.17

* Record static S3 registrations and implementation signatures separately from
  exported generic signatures. Duplicate or unresolved registrations remain
  unverified; method records do not establish installed dispatch or approval.
* Show method coverage and searchable declarations, and report removed or
  changed method evidence during catalog updates. Historical pins remain intact.

# cttiR 0.0.16

* Index explicitly registered local R distribution package sources without
  changing or evaluating them. Track the exact release and license hashes;
  refuse development versions, unknown substitutions and identity conflicts.

# cttiR 0.0.15

* Check explicitly supplied analysis data against typed mappings without changing
  data or fitting models. Report missingness, type/event-code problems and
  repeated-unit conflicts separately from workflow and scientific approval.

# cttiR 0.0.14

* Resolve nested-package evidence citations with exactly one source prefix.

* Refuse verified static callable signatures for reassigned, conditional or
  modified function bindings; never inspect or execute function bodies.
* Reindex the bundled catalog against hash-verified frozen sources while retaining
  the previous catalog for existing project pins.

# cttiR 0.0.13

* Index licensed Sweave, TeX and bibliography source documents as literal text.
  Embedded expressions are never evaluated; PDF assets remain metadata-only.

# cttiR 0.0.12

* Record typed dataset, variable, event-coding and missing-data mappings.
* Report missing scientific configuration and unsupported design/engine choices
  without reading datasets, evaluating formulas or fitting models.
* Check dataset references and conflicting variable roles in creation and sync.

# cttiR 0.0.11

* Export and restore bounded local JSON drafts without retaining accepted plans.
* Require JSON objects in UI configuration and drafts, preventing strings from
  being interpreted as local configuration file paths.
* Reject foreign project drafts and preview results whose answers changed while
  the background worker was running.

# cttiR 0.0.10

* Add an executable introductory vignette covering offline creation, synchronization,
  evidence queries, audit and explicit update/render/runtime boundaries.

# cttiR 0.0.9

* Journal catalog pointer activation and retain interrupted writer evidence.
* Recover locks only for verified stopped local writers with an intact active snapshot.
* Refuse live or unknown writers, conflicting journals and corrupt active snapshots.
* Keep catalog previews read-only even when an abandoned writer lock is present.

# cttiR 0.0.8

* Refresh explicitly registered public GitHub package sources at resolved commits.
* Verify Git blob hashes and reject truncated trees, links, unsafe paths and oversized sources.
* Preserve project pins and existing snapshots across remote fetch failures and rollback.
* Record remote resource observations separately from local observations without changing curation.

# cttiR 0.0.7

* Store bounded, revision-aligned documentation with explicit rights and omission records.
* Detect documentation-only edits and removals during transactional updates and rollback.
* Search literal documentation excerpts without executing content or granting workflow approval.
* Bundle 31 licensed reflowR documents and retain the historical catalog for existing project pins.

# cttiR 0.0.6

* Add a hashed, MIT-licensed adaptation of the pinned reflowR minimal layout.
* Render five local R Markdown pages explicitly, including an isolated synthetic fixture.
* Preserve accepted older templates during repeat creation and synchronization.
* Reject inconsistent template pins before project mutation.

# cttiR 0.0.5

* Add a local Fast/Detailed builder and Configure interface using the package APIs.
* Run read-only tasks in bounded background workers and refuse stale apply requests.
* Preserve project pins during repeat creation even when the global catalog is corrupt.
* Keep preview tables within narrow layouts and preserve missing-file conflict errors.

# cttiR 0.0.4

* Add staged local API/resource refresh and atomic composite activation.
* Preserve whole-revision API semantics, project pins and resource curation.
* Add immutable history and preview-first catalog rollback without package installation.
* Make catalog ordering locale-independent while retaining older snapshot identities.

# cttiR 0.0.3

* Add guarded interrupted-transaction recovery with hash and writer checks.
* Add explicit portable runtime setup, checksum/locality verification and a CPU structured-output probe.

* Index 30 public CTTIR package roots with 1,340 revision-scoped export records.
* Add packages(), search() and evidence retrieval with explicit static verification levels.
* Pin new projects to the API catalog and preserve unresolved exports without inventing signatures.
* Handle verified macOS system path aliases and normalized Windows path spellings.

# cttiR 0.0.2

* Add read-only synchronization previews, conflict detection, per-file backups and rollback.
* Add local integrity audits, structured reports and baseline-verified missing-file repair.
* Add a working maintainer contact and exclude the web license copy from package builds.

# cttiR 0.0.1.9000

* Add an offline, staged project builder with three required research inputs.
* Validate customization and resolved project documents with local JSON schemas.
* Preserve accepted project identity and existing edits on identical requests.
* Add publication scaffolding, data registry and read-only project validation.
* Bundle the supplied 229-candidate resource snapshot with read-only queries,
  integrity checking and explicit verification limits.
* Keep workflow integration, catalog lifecycle and application features pending.
