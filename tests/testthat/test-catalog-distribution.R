distribution_fixture <- function() {
  root <- tempfile("distribution-fixture-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  writeLines("4.6.1", file.path(root, "VERSION"))
  writeLines("Synthetic fixture license", file.path(root, "COPYING"))
  package <- file.path(root, "src", "library", "fixtureR")
  dir.create(file.path(package, "R"), recursive = TRUE)
  dir.create(file.path(package, "man"))
  writeLines(c("Package: fixtureR", "Version: @VERSION@", "Title: Fixture",
      "Description: Synthetic fixture.", "License: Part of R @VERSION@"), file.path(package, "DESCRIPTION.in"))
  writeLines("export(f)", file.path(package, "NAMESPACE"))
  writeLines(c("stop('Never execute acquired source')", "f <- function(x = stop('Never run default')) x"), file.path(package, "R", "f.R"))
  writeLines("\\name{f}\\alias{f}\\title{Fixture}", file.path(package, "man", "f.Rd"))
  list(id = "rsource", r_distribution = root, package = "fixtureR", documentation_rights = "Synthetic owned fixture")
}

test_that("distribution indexing preserves originals and records the exact R version", {
  record <- distribution_fixture()
  before <- tree_hashes(record$r_distribution)
  withr::local_options(cttiR.sources = list(record))
  entry <- update_sources(NULL, "fixtureR")[[1]]
  expect_equal(entry$version, "4.6.1")
  expect_equal(entry$license, "Part of R 4.6.1")
  expect_equal(entry$distribution$description_source, "DESCRIPTION.in")
  expect_equal(entry$exports[[1]]$verification, "static_api_verified")
  expect_equal(entry$coverage$approved, 0L)
  expect_equal(tree_hashes(record$r_distribution), before)
  expect_match(entry$revision, "^local-[a-f0-9]{64}$")
  doc <- Filter(function(x) x$path == "DESCRIPTION.in", entry$documentation_corpus$documents)[[1]]
  expect_match(doc$content, "@VERSION@", fixed = TRUE)
  writeLines("4.6.2", file.path(record$r_distribution, "VERSION"))
  changed <- update_sources(NULL, "fixtureR")[[1]]
  expect_equal(changed$version, "4.6.2")
  expect_false(identical(entry$source_hash, changed$source_hash))
  writeLines("Changed fixture license", file.path(record$r_distribution, "COPYING"))
  expect_false(identical(changed$source_hash, update_sources(NULL, "fixtureR")[[1]]$source_hash))
})

test_that("ambiguous or development distribution registrations fail closed", {
  record <- distribution_fixture()
  withr::local_options(cttiR.sources = list(record))
  writeLines("4.7.0 Under development", file.path(record$r_distribution, "VERSION"))
  expect_error(update_sources(NULL, NULL), "exact released")
  writeLines("4.6.1", file.path(record$r_distribution, "VERSION"))
  file <- file.path(record$r_distribution, "src/library/fixtureR/DESCRIPTION.in")
  writeLines(c(readLines(file), "Other: @UNKNOWN@"), file)
  expect_error(update_sources(NULL, NULL), "Unsupported distribution")
  record$path <- record$r_distribution
  options(cttiR.sources = list(record))
  expect_error(update_sources(NULL, NULL), "another location")
  record$path <- NULL
  record$package <- "../fixtureR"
  options(cttiR.sources = list(record))
  expect_error(update_sources(NULL, NULL), "Invalid distribution package")
})

test_that("distribution candidates participate in immutable update and rollback", {
  record <- distribution_fixture()
  cache <- file.path(new_parent(), "cache")
  withr::local_options(cttiR.sources = list(record), cttiR.catalog_dir = cache)
  preview <- update(packages = "fixtureR", dry_run = TRUE)
  expect_false(dir.exists(cache))
  result <- update(packages = "fixtureR", dry_run = FALSE)
  expect_true(result$activation)
  expect_equal(nrow(search("fixtureR::f")), 1L)
  expect_equal(packages()$approved[packages()$package == "fixtureR"], 0L)
  rollback_knowledge(result$previous_id, dry_run = FALSE)
  expect_equal(nrow(search("fixtureR::f")), 0L)
})


test_that("distribution source identity conflicts and missing license evidence are refused", {
  record <- distribution_fixture()
  file <- file.path(record$r_distribution, "src/library/fixtureR/DESCRIPTION.in")
  original <- readLines(file)
  writeLines(sub("@VERSION@", "4.5.0", original, fixed = TRUE), file)
  expect_error(r_distribution_source(record), "does not match")
  writeLines(sub("Package: fixtureR", "Package: otherR", original, fixed = TRUE), file)
  expect_error(r_distribution_source(record), "does not match")
  writeLines(original, file)
  unlink(file.path(record$r_distribution, "COPYING"))
  expect_error(r_distribution_source(record), class = "cttir_source_unavailable")
})
