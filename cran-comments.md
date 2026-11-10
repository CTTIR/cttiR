## Submission

This is a new submission: cttiR 0.2.0.

## Test environments

* Local: macOS 27 on Apple silicon, R 4.6.1 (aarch64-apple-darwin),
  `R CMD check --as-cran --no-manual` on the built tarball with `NOT_CRAN`
  unset. The PDF manual was built locally with TinyTeX (TeX Live 2026) via
  `R CMD Rd2pdf` without errors; the check run itself used `--no-manual`.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release with all
  Suggests, R oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* Suggests or Enhances not in mainstream repositories: reflowR, available
  from https://cttir.r-universe.dev (Additional_repositories).

The installed size is about 10.7 Mb (extdata 6.6 Mb, R 1.5 Mb) and may be
reported on some platforms; the source package is about 4.5 Mb.

## Notes

* Suggests includes reflowR, which is not on CRAN; it is available from
  the repository named in `Additional_repositories`
  (https://cttir.r-universe.dev). The package uses it only conditionally
  and the tests that need it skip when it is absent.
* The optional local model runtime is never required, downloaded or
  started by examples, tests or vignettes. Runtime acquisition and model
  use are explicit opt-in operations with dry-run previews; examples point
  the catalog and runtime directories at `tempdir()`.
* No example uses `\dontrun{}`. The evidence registry and release preview
  examples build synthetic inputs below `tempdir()`. The strict study
  examples run under `\donttest{}` only when reflowR, workflowr, callr and
  git2r are installed (rendering also needs Pandoc), so they are skipped
  where reflowR is unavailable. `doctor()` reads the bundled catalog and the
  installed metadata of the cataloged packages and is wrapped in
  `\donttest{}`.
* Functions that write files (`project()`, `sync()`, `configure()`,
  `study_stage()`) have no default location; examples, tests and vignettes
  pass a directory below `tempdir()`.
* The slowest test files skip on CRAN so that the tests finish in a few
  minutes; they run in full in continuous integration.
* The bundled, compressed API catalog and resource catalog make up most
  of the installed size; they are needed offline at run time, because
  ask(), search() and project() must work without network access.
