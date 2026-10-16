# Explicit scheduler lifecycle fixture; dependencies must already be installed.
# Rscript --vanilla tools/verify-workflowr-targets.R <new-evidence-directory>
pkgload::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !file.exists(args[[1]]))
Sys.setenv(CTTIR_TARGETS_EVIDENCE = file.path(normalizePath(dirname(args[[1]])), basename(args[[1]])))
testthat::test_file("tests/testthat/test-workflowr-targets-lifecycle.R", stop_on_failure = TRUE)
