# cttiR development state

Updated: 2026-09-27. Version: 0.0.1.9000. License: MIT.

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
  revisions are approved and no function signatures are verified in this seed.

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
actor. Existing-project writes/sync and journal recovery are not implemented.

Authorization: validated milestones may be committed and pushed directly to main
using the repository's configured user identity. Preserve ignored admin inputs.
