# cttiR development state

Updated: 2026-10-01. Version: 0.0.3. License: MIT.

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
Hosted milestone-2 Linux jobs passed; Windows/macOS path defects have working
fixes and require verification on the newly pushed candidate.

Retry history: hourly attempts ran, but the desktop session retained this thread's
write lock, so CLI resume was rejected. Timer execution is not evidence of task
progress. Direct interactive continuation resumed on 2026-10-01. The timer remains
configured; do not bypass another active writer or run duplicate modifications.

Next: implement coordinated immutable catalog/resource update and rollback,
complete standard source/documentation coverage and reviewed workflow adapters,
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
Public organization discovery returned 40 repositories. The live source inventory
is recorded locally; several local trees are dirty and must not be bundled as
public upstream source without obtaining verified public revisions.

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

WO-01 foundation is implemented for this milestone, with full catalog/audit/update
report schemas still pending. WO-02 offline creation is implemented; broader
interruption/recovery, cross-platform filesystem tests and full lifecycle gates
remain open. WO-06c has resource import/query functionality only.

Next: inspect the exact reflowR source and side effects, build a reviewed adapter,
then implement the real source/API inventory and grounded workflow routing.
Continue with immutable catalog lifecycle/update/rollback, approved documentation
corpus, runtime setup and inference, scientific adapters, synchronization,
Shiny and audit/repair. Follow the work orders in the local v7 specification.

Full v7 acceptance gates are **not complete**. No claims of Windows/macOS support,
hosted CI success, scientific validation, production readiness, CRAN readiness
or release/submission are made. Creation locking coordinates cooperating writers;
this milestone does not claim protection against a hostile concurrent filesystem
actor. Existing-project sync and rollback on caught write failures are tested. Recovery
after abrupt process termination remains pending and is not a release pass.

Authorization: validated milestones may be committed and pushed directly to main
using the repository's configured user identity. Preserve ignored admin inputs.
