method_fixture <- function() {
  path <- tempfile("methods-source-")
  withr::defer(unlink(path, recursive = TRUE), envir = parent.frame())
  fixture_source(path, c("f <- function(x, ...) UseMethod('f')", "f.thing <- function(x, scale = 2) x",
      "implementation <- function(x, other = NULL) x"), "f")
  writeLines(c("export(f)", "S3method(f,thing)", "S3method(print,thing,implementation)"), file.path(path, "NAMESPACE"))
  path
}

test_that("S3 declarations retain separate implementation signatures", {
  path <- method_fixture()
  package <- extract_source(path, "fixture://methods", "one")
  expect_equal(package$coverage$s3_declared, 2L)
  expect_equal(package$coverage$s3_resolved, 2L)
  expect_equal(package$s3_methods[[1]]$implementation, "f.thing")
  expect_match(package$s3_methods[[1]]$signature, "scale = 2", fixed = TRUE)
  expect_equal(package$s3_methods[[2]]$implementation, "implementation")
  expect_match(package$exports[[1]]$signature, "function(x, ...)", fixed = TRUE)
  expect_false(package$s3_methods[[1]]$approved)
  expect_equal(method_hits(package, "f.thing")[[1]]$verification, "static_method_verified")
})

test_that("unresolved and duplicate S3 registrations cannot claim resolution", {
  path <- method_fixture()
  writeLines(c("export(f)", "S3method(f,thing)", "S3method(f,thing,implementation)",
      "S3method(other,missing)", "S3method(other::generic,thing,implementation)"), file.path(path, "NAMESPACE"))
  package <- extract_source(path, "fixture://methods", "one")
  expect_equal(package$coverage$s3_declared, 4L)
  expect_equal(package$coverage$s3_resolved, 1L)
  expect_equal(package$s3_methods[[4]]$generic, "other::generic")
  expect_equal(package$s3_methods[[3]]$verification, "unknown")
  expect_equal(package$s3_methods[[1]]$arguments, list())
  writeLines(c(readLines(file.path(path, "R", "api.R")), "implementation <- 1"), file.path(path, "R", "api.R"))
  expect_equal(extract_source(path, "fixture://methods", "two")$coverage$s3_resolved, 0L)
})

test_that("method removals and implementation changes appear in API differences", {
  path <- method_fixture()
  a <- extract_source(path, "fixture://methods", "one")
  writeLines(c("export(f)", "S3method(f,thing)", "S3method(f,newclass,implementation)"), file.path(path, "NAMESPACE"))
  file <- list.files(file.path(path, "R"), full.names = TRUE)[[1]]
  writeLines(sub("scale = 2", "scale = 3", readLines(file), fixed = TRUE), file)
  b <- extract_source(path, "fixture://methods", "two")
  changes <- api_diff(list(packages = list(a)), list(packages = list(b)))
  expect_contains(changes$change, "method_declaration_removed")
  expect_contains(changes$change, "method_declaration_added")
  expect_contains(changes$change, "method_evidence_changed")
  expect_true(all(changes$breaking[changes$change == "method_evidence_changed"]))
  expect_false(any(changes$breaking[changes$change == "method_declaration_added"]))
})

test_that("method search does not present private implementations as exports", {
  path <- method_fixture()
  withr::local_options(cttiR.sources = list(list(id = "methods", path = path)),
    cttiR.catalog_dir = file.path(new_parent(), "cache"))
  update(dry_run = FALSE)
  hits <- search("f.thing")
  expect_true(any(hits$kind == "s3_method_declaration"))
  expect_false(any(grepl("::f.thing", hits$symbol, fixed = TRUE)))
  expect_false(any(hits$approved))
  expect_equal(nrow(ask("f.thing", verified_only = TRUE)$evidence), 0L)
  expect_equal(ask("f.thing")$code, "")
})
