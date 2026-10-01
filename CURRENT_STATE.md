# cttiR development state

Updated: 2026-10-01. Version: 0.0.5. License: MIT.

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
a broad package set, and generated navigation YAML is malformed. An adapted,
reviewed template with provenance is required; integration remains unverified.
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

1. Complete bounded CTTIR/standard/IMBI remote source refresh and release policy,
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

Package checks on the supported hosted matrix passed for milestones 3 and 4;
this does not qualify every runtime installer or scientific adapter. Full
scientific validation, production readiness and CRAN readiness are not claimed.
No CRAN or external-builder submission has been made. Creation and transaction
locks coordinate cooperating writers; hostile concurrent filesystem actors are
outside the current contract. Keep the exact acceptance gates in the ignored v7
specification and preserve the distinction between passed, partial and untested.

Authorization: validated milestones may be committed and pushed directly to main
using the repository's configured user identity. Preserve ignored admin inputs.
