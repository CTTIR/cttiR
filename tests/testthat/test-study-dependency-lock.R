test_that("study locks identify missing transitive dependencies without weakening the closure", {
  skip_if_not_installed("reflowR")
  skip_if_not_installed("workflowr")
  skip_if_not_installed("git2r")
  skip_if_not_installed("callr")
  installed <- library_dependency_db("cttiR", .libPaths())
  expect_true("cttiR" %in% rownames(installed))
  read_library <- library_dependency_db
  testthat::local_mocked_bindings(library_dependency_db = function(...) {
    db <- read_library(...)
    db["cttiR", "LinkingTo"] <- "cttirMissingBuildFixture"
    db
  })
  expect_error(study_dependency_lock(),
    "The prepared dependency closure is incomplete. Missing: cttirMissingBuildFixture", fixed = TRUE)
})

test_that("library records match installed.packages() for the dependency closure", {
  skip_on_cran()
  roots <- c("jsonlite", "yaml", "stats")
  db <- library_dependency_db(roots, .libPaths())
  reference <- utils::installed.packages()
  reference <- reference[!duplicated(reference[, "Package"]), , drop = FALSE]
  which <- c("Depends", "Imports", "LinkingTo")
  expect_identical(tools::package_dependencies(roots, db = db, which = which, recursive = TRUE),
    tools::package_dependencies(roots, db = reference, which = which, recursive = TRUE))
  expect_identical(unname(db[, "Version"]), unname(reference[match(db[, "Package"], reference[, "Package"]), "Version"]))
  expect_identical(installed_versions(c("jsonlite", "cttirAbsentPackage")),
    c(unname(reference[match("jsonlite", reference[, "Package"]), "Version"]), NA_character_))
  expect_identical(nrow(library_dependency_db("cttirAbsentPackage", .libPaths())), 0L)
})
