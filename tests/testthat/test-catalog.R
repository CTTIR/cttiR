fixture_source <- function(path, code, exports) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  writeLines(c("Package: cttirFixtureA", "Version: 1.0.0", "Title: Synthetic Test Package", "License: MIT"), file.path(path, "DESCRIPTION"))
  writeLines(paste0("export(", exports, ")"), file.path(path, "NAMESPACE"))
  writeLines(code, file.path(path, "R", "api.R"))
  path
}

test_that("static extraction never executes code and preserves unresolved exports", {
  parent <- new_parent()
  sentinel <- file.path(parent, "should-not-exist")
  code <- c(
    paste0("writeLines('executed', ", encodeString(sentinel, quote = '"'), ")"),
    "keep <- function(x, value = stop('do not run default')) x", "old <- function(...) NULL"
  )
  source <- fixture_source(file.path(parent, "source"), code, c("old", "keep", "dynamic"))
  p <- extract_source(source, "fixture://cttirFixtureA", "v1")
  expect_false(file.exists(sentinel))
  expect_equal(p$coverage$exports, 3)
  expect_equal(p$coverage$resolved, 2)
  expect_false(any(vapply(p$exports, function(x) x$approved, logical(1))))
  unresolved <- Filter(function(x) x$name == "dynamic", p$exports)[[1]]
  expect_equal(unresolved$verification, "unknown")
  expect_match(Filter(function(x) x$name == "keep", p$exports)[[1]]$signature, "stop", fixed = TRUE)
  file <- file.path(parent, "catalog.json.gz")
  id <- write_catalog(list(p), file)
  expect_equal(read_catalog(file)$content_id, id)
  writeLines("corrupt", file)
  expect_error(read_catalog(file), class = "cttir_catalog_corrupt")
})

test_that("the public catalog exposes revision-scoped evidence without approval claims", {
  p <- packages()
  expect_gte(nrow(p), 28)
  expect_true(all(p$approved == 0L))
  hit <- search("reflowR::reflow_init")
  expect_equal(nrow(hit), 1L)
  expect_match(hit$snippet, "git = TRUE", fixed = TRUE)
  expect_match(hit$evidence, "/blob/[a-f0-9]{40}/R/reflow_init.R")
  expect_equal(hit$verification, "static_api_verified")
  expect_equal(nrow(search("reflowR::reflow_init", packages = "annotatR")), 0L)
  expect_equal(nrow(search("' OR 1=1 --")), 0L)
  expect_equal(ask("reflowR::reflow_init")$code, "")
  expect_error(search("", limit = 1), class = "cttir_input_error")
  expect_error(search("reflowR", limit = Inf), class = "cttir_input_error")
  parent <- new_parent()
  project <- project("Catalog pin", "methods", "Goal", parent)
  expect_equal(search("reflowR::reflow_init", path = project$path), hit)
})

test_that("multiple definitions in one source file cannot claim a verified signature", {
  f <- static_functions("f <- function(x) x; f <- function(y) y", "R/api.R")
  expect_match(f$f$signature, "unresolved", fixed = TRUE)
})
