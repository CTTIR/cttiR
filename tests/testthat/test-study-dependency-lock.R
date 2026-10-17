test_that("study locks identify missing transitive dependencies without weakening the closure", {
  skip_if_not_installed("reflowR")
  skip_if_not_installed("workflowr")
  skip_if_not_installed("git2r")
  skip_if_not_installed("callr")
  installed <- utils::installed.packages()
  row <- match("cttiR", installed[, "Package"])
  expect_false(is.na(row))
  installed[row, "LinkingTo"] <- "cttirMissingBuildFixture"
  testthat::local_mocked_bindings(installed.packages = function(...) installed, .package = "utils")
  expect_error(study_dependency_lock(),
    "The prepared dependency closure is incomplete. Missing: cttirMissingBuildFixture", fixed = TRUE)
})
