# Run the explicit lifecycle fixture from the source root with installed dependencies.
# Rscript --vanilla tools/verify-workflowr-runner.R <new-evidence-directory>
pkgload::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !file.exists(args[[1]]))
Sys.setenv(CTTIR_WORKFLOWR_EVIDENCE = file.path(normalizePath(dirname(args[[1]]), mustWork = TRUE), basename(args[[1]])))
testthat::test_file("tests/testthat/test-workflowr-runner.R", stop_on_failure = TRUE)
