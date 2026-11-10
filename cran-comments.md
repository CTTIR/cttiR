## Submission

This is a new submission: cttiR 0.2.0.

## Test environments

* Local: macOS 27 on Apple silicon, R 4.6.1 (aarch64-apple-darwin),
  `R CMD check --as-cran --no-manual` on the built tarball with `NOT_CRAN`
  unset. LaTeX is not installed on this machine, so the PDF manual was not
  built locally.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release with all
  Suggests, R oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* Suggests or Enhances not in mainstream repositories: reflowR, available
  from https://cttir.r-universe.dev (Additional_repositories).

The installed size is about 12.7 Mb (extdata 7.1 Mb, R 2.0 Mb, help
1.4 Mb) and may be reported on some platforms; the source package is about
5.8 Mb.

## Notes

* Suggests includes reflowR, which is not on CRAN; it is available from
  the repository named in `Additional_repositories`
  (https://cttir.r-universe.dev). The package uses it only conditionally
  and the tests that need it skip when it is absent.
* The optional local model runtime is never required, downloaded or
  started by examples, tests or vignettes. Runtime acquisition and model
  use are explicit opt-in operations with dry-run previews; examples point
  the catalog and runtime directories at `tempdir()`.
* Examples that need a locally prepared evidence registry, a published
  release manifest, or a prepared study library with Git are wrapped in
  `\dontrun{}` with a comment explaining the requirement. `doctor()` reads
  the bundled catalog and all installed package metadata and is wrapped in
  `\donttest{}`.
* The slowest test files skip on CRAN so that the tests finish in a few
  minutes; they run in full in continuous integration.
* The bundled, compressed API catalog and resource catalog make up most
  of the installed size; they are needed offline at run time, because
  ask(), search() and project() must work without network access.
