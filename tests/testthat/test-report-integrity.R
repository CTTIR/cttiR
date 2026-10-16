report_integrity_library <- function() {
  env <- new.env(parent = baseenv())
  sys.source(system.file("templates", "workflowr-0.4.0", "code", "R", "report_integrity.R",
    package = "cttiR"), envir = env)
  env
}

test_that("compiled and standalone integrity helpers use the same contract", {
  standalone <- report_integrity_library()
  for (name in ls(standalone, all.names = TRUE)) {
    compiled <- get(name, envir = asNamespace("cttiR"), inherits = FALSE)
    expect_identical(formals(compiled), formals(standalone[[name]]), info = name)
    expect_identical(body(compiled), body(standalone[[name]]), info = name)
  }
})

test_that("report freshness uses content and the complete declared file set", {
  lib <- report_integrity_library()
  root <- new_parent()
  writeLines("input-a", file.path(root, "data.csv"))
  writeLines("result-a", file.path(root, "output.csv"))
  writeLines("code-a", file.path(root, "code.R"))
  inputs <- c("data.csv", "code.R")
  receipt <- lib$cr_snapshot(root, inputs, "output.csv")
  expect_true(lib$cr_assert_fresh(root, receipt, rev(inputs), "output.csv"))
  before <- file.info(file.path(root, "data.csv"))
  writeLines("input-b", file.path(root, "data.csv"))
  Sys.setFileTime(file.path(root, "data.csv"), before$mtime)
  expect_equal(file.info(file.path(root, "data.csv"))$size, before$size)
  expect_error(lib$cr_assert_fresh(root, receipt, inputs, "output.csv"), "changed")
  writeLines("input-a", file.path(root, "data.csv"))
  writeLines("code-b", file.path(root, "code.R"))
  expect_error(lib$cr_assert_fresh(root, receipt, inputs, "output.csv"), "changed")
  writeLines("code-a", file.path(root, "code.R"))
  writeLines("result-b", file.path(root, "output.csv"))
  expect_error(lib$cr_assert_fresh(root, receipt, inputs, "output.csv"), "changed")
  writeLines("result-a", file.path(root, "output.csv"))
  expect_error(lib$cr_assert_fresh(root, receipt, "data.csv", "output.csv"), "changed")
  expect_error(lib$cr_snapshot(root, c(inputs, "missing"), "output.csv"), "missing")
})

test_that("report identity refuses traversal, directories and linked files", {
  lib <- report_integrity_library()
  root <- new_parent()
  writeLines("example", file.path(root, "plain.txt"))
  dir.create(file.path(root, "nested"))
  for (path in c("../plain.txt", "/etc/passwd", "nested", "nested/../plain.txt", "C:/plain.txt")) {
    expect_error(lib$cr_identity(root, path))
  }
  expect_error(lib$cr_identity(root, c("plain.txt", "plain.txt")), "unique")
  if (.Platform$OS.type != "windows") {
    expect_true(file.symlink(file.path(root, "plain.txt"), file.path(root, "linked.txt")))
    expect_error(lib$cr_identity(root, "linked.txt"), "symbolic")
  }
})

test_that("external dataset content is verified without putting local paths in receipts", {
  lib <- report_integrity_library()
  root <- new_parent()
  external <- new_parent()
  data <- file.path(external, "private-location.csv")
  writeLines("first", data)
  writeLines("code", file.path(root, "code.R"))
  writeLines("result", file.path(root, "result.csv"))
  binding <- c(dataset_one = data)
  receipt <- lib$cr_snapshot(root, "code.R", "result.csv", binding)
  expect_true(lib$cr_assert_fresh(root, receipt, "code.R", "result.csv", binding))
  expect_false(grepl(external, jsonlite::toJSON(receipt), fixed = TRUE))
  expect_identical(receipt$external_inputs[[1]]$dataset_id, "dataset_one")
  before <- file.info(data)$mtime
  writeLines("other", data)
  Sys.setFileTime(data, before)
  expect_error(lib$cr_assert_fresh(root, receipt, "code.R", "result.csv", binding), "changed")
  expect_error(lib$cr_assert_fresh(root, receipt, "code.R", "result.csv"), "changed")
  expect_error(lib$cr_binding_identity(unname(binding)), "dataset IDs")
  expect_error(lib$cr_binding_identity(c(binding, binding)), "dataset IDs")
})

test_that("execution receipts detect changes during computation and new source files", {
  lib <- report_integrity_library()
  root <- new_parent()
  dir.create(file.path(root, "code"))
  dir.create(file.path(root, "config"))
  writeLines("source", file.path(root, "code/run.R"))
  writeLines("result", file.path(root, "output.csv"))
  before <- lib$cr_begin(root, character())
  receipt <- lib$cr_finish(root, before, "output.csv", character())
  expect_true(lib$cr_assert_fresh(root, receipt, lib$cr_execution_inputs(root), "output.csv"))
  writeLines("new source", file.path(root, "code/new.R"))
  expect_error(lib$cr_finish(root, before, "output.csv", character()), "during computation")
  expect_error(lib$cr_assert_fresh(root, receipt, lib$cr_execution_inputs(root), "output.csv"), "changed")
})

test_that("publication analysis and environment locks invalidate old report results", {
  lib <- report_integrity_library()
  root <- new_parent()
  dir.create(file.path(root, "publications/paper/analysis"), recursive = TRUE)
  dir.create(file.path(root, "publications/paper/manuscript"), recursive = TRUE)
  analysis <- "publications/paper/analysis/model.R"
  writeLines("x <- 1", file.path(root, analysis))
  writeLines("draft", file.path(root, "publications/paper/manuscript/draft.md"))
  writeLines("lock-a", file.path(root, "renv.lock"))
  writeLines("result", file.path(root, "result.csv"))
  receipt <- lib$cr_snapshot(root, lib$cr_execution_inputs(root), "result.csv")
  expect_contains(vapply(receipt$inputs, function(x) x$path, ""), c(analysis, "renv.lock"))
  writeLines("revised prose", file.path(root, "publications/paper/manuscript/draft.md"))
  expect_true(lib$cr_assert_fresh(root, receipt, lib$cr_execution_inputs(root), "result.csv"))
  mtime <- file.info(file.path(root, analysis))$mtime
  writeLines("x <- 2", file.path(root, analysis))
  Sys.setFileTime(file.path(root, analysis), mtime)
  expect_error(lib$cr_assert_fresh(root, receipt, lib$cr_execution_inputs(root), "result.csv"), "changed")
  writeLines("x <- 1", file.path(root, analysis))
  writeLines("lock-b", file.path(root, "renv.lock"))
  expect_error(lib$cr_assert_fresh(root, receipt, lib$cr_execution_inputs(root), "result.csv"), "changed")
  writeLines("lock-a", file.path(root, "renv.lock"))
  unlink(file.path(root, analysis))
  expect_error(lib$cr_assert_fresh(root, receipt, lib$cr_execution_inputs(root), "result.csv"), "changed")
})
