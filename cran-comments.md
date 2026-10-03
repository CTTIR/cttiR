# Draft submission notes — not submitted

Package: cttiR 0.1.1. These notes describe validated source 432fa34; they are
not an instruction to submit. Full product qualification remains open for the
optional planner and Seurat analysis workflow.

## Check results

Local Linux x86_64, R 4.6.1: full `R CMD check --as-cran --timings` completed
with 0 errors, 0 warnings and 1 incoming NOTE. Vignettes were rebuilt; PDF and
HTML manuals passed. Tests: 5,470 passed, 13 skipped, zero failures/warnings.
Skipped live runtime/browser/restore and unavailable optional-package checks
are identified in the retained test log; they are not counted as passes.

The incoming NOTE identifies a new submission and a Title beginning with the
package name. The current specified product title is "CTTIR Project Builder".

The hosted matrix for 432fa34 passed all five jobs: Linux release with all
Suggests, Linux oldrel-1, Linux devel, Windows release and macOS release.
Run: https://github.com/CTTIR/cttiR/actions/runs/37094093244
These are existing repository CI results, not win-builder or R-hub submissions.

## Scope and side effects

Ordinary examples and checks do not download models or start an inference
runtime. Runtime acquisition and model use are explicit opt-in operations;
planning remains deterministic by default. No CRAN or external-builder
submission has been made.

Local full-check archive SHA-256:
3a8d633a2bb6dafd19597b706c5d0e570bc5269196cffd60e690612256ae2730

The archive excludes private administration and validation artifacts.
