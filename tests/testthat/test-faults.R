# Builder fault matrix (spec 10 "Required verification", gate G13). Failures are
# injected with mocks so the tests are deterministic and never fill a disk.

parent_entries <- function(parent) list.files(parent, all.files = TRUE, no.. = TRUE)

running_as_root <- function() identical(unname(Sys.info()[["effective_user"]]), "root")

test_that("interruption before the staged rename leaves no target, staging or lock", {
  parent <- new_parent()
  local_mocked_bindings(file.rename = function(from, to) FALSE)
  expect_error(project("Interrupted", "methods", "Goal", parent), class = "cttir_transaction_conflict")
  expect_length(parent_entries(parent), 0L)
})

test_that("interruption after the staged rename leaves a complete readable project", {
  parent <- new_parent()
  local_mocked_bindings(file.rename = function(from, to) {
    base::file.rename(from, to)
    stop("interrupted after the commit point")
  })
  expect_error(project("Committed", "methods", "Goal", parent), "interrupted after the commit point")
  target <- file.path(parent, "committed")
  expect_identical(parent_entries(parent), "committed")
  expect_type(read_project(target)$spec$project$id, "character")
  state <- tree_state(target)
  repeat_run <- project("Committed", "methods", "Goal", parent)
  expect_true(all(repeat_run$plan$action == "skip"))
  expect_identical(tree_state(target), state)
  report <- audit(target, scope = "project")
  expect_false(report$overall_status %in% c("fail", "not_tested"))
})

test_that("a write failure while staging a new project leaves nothing behind", {
  parent <- new_parent()
  calls <- 0L
  local_mocked_bindings(writeBin = function(object, con, ...) {
    calls <<- calls + 1L
    if (calls == 3L) stop("no space left on device")
    base::writeBin(object, con, ...)
  })
  expect_error(project("Staging", "methods", "Goal", parent), "no space left")
  expect_equal(calls, 3L)
  expect_length(parent_entries(parent), 0L)
})

test_that("a sync interrupted mid-apply rolls back and preserves user files", {
  parent <- new_parent()
  p <- project("Mid apply", "methods", "Goal", parent)
  manuscript <- file.path(p$path, "publications/pub01_main/manuscript/README.md")
  writeLines("Reviewed manuscript text", manuscript)
  before <- tree_state(p$path)
  calls <- 0L
  real <- replace_file
  local_mocked_bindings(replace_file = function(from, to) {
    calls <<- calls + 1L
    if (calls == 2L) stop("injected failure on the second file")
    real(from, to)
  })
  expect_error(sync(p$path, options = list(analysis = list(aim = "descriptive")), dry_run = FALSE), "second file")
  after <- tree_state(p$path)
  expect_identical(after[names(before)], before)
  expect_true(all(startsWith(setdiff(names(after), names(before)), ".cttir/transactions")))
  journal <- list.files(file.path(p$path, ".cttir/transactions"), "journal.json", recursive = TRUE, full.names = TRUE)
  expect_equal(read_document(journal)$status, "rolled_back")
  expect_false(dir.exists(file.path(p$path, ".cttir/write-lock")))
  expect_equal(readLines(manuscript), "Reviewed manuscript text")
  expect_length(pending_transactions(p$path), 0L)
})

test_that("read-only parents and files fail with typed errors and no partial writes", {
  skip_on_os("windows")
  skip_if(running_as_root(), "Permission checks do not apply to root")
  parent <- new_parent()
  locked <- file.path(parent, "locked")
  dir.create(locked)
  Sys.chmod(locked, "0555")
  withr::defer(Sys.chmod(locked, "0755"))
  expect_error(project("Readonly", "methods", "Goal", locked), class = "cttir_error")
  expect_length(parent_entries(locked), 0L)
  p <- project("Readonly files", "methods", "Goal", parent)
  target <- file.path(p$path, "config/analysis.yml")
  Sys.chmod(target, "0444")
  withr::defer(Sys.chmod(target, "0644"))
  before <- tree_state(p$path)
  # base::file.copy() also warns about the denied write; the typed error is what matters.
  change <- list(analysis = list(aim = "descriptive"))
  suppressWarnings(expect_error(sync(p$path, options = change, dry_run = FALSE), class = "cttir_transaction_conflict"))
  after <- tree_state(p$path)
  expect_identical(after[names(before)], before)
  journal <- list.files(file.path(p$path, ".cttir/transactions"), "journal.json", recursive = TRUE, full.names = TRUE)
  expect_equal(read_document(journal)$status, "rolled_back")
})

test_that("concurrent writers yield typed conflicts and modify nothing", {
  parent <- new_parent()
  p <- project("Concurrent", "methods", "Goal", parent)
  dir.create(file.path(p$path, ".cttir/write-lock"))
  before <- tree_state(p$path)
  expect_error(sync(p$path, options = list(project = list(goal = "Changed")), dry_run = FALSE),
    class = "cttir_transaction_conflict")
  expect_identical(tree_state(p$path), before)
  checks <- audit(p$path, scope = "project")$checks
  expect_equal(checks$status[checks$id == "PRJ-007"], "fail")
  dir.create(file.path(parent, ".other.cttir-create-lock"))
  entries <- parent_entries(parent)
  expect_error(project("Other", "methods", "Goal", parent), class = "cttir_transaction_conflict")
  expect_identical(parent_entries(parent), entries)
})

test_that("unicode and spaces work while reserved device names are refused", {
  base <- new_parent()
  parent <- file.path(base, "Projekt Ordner \u00fc")
  dir.create(parent)
  p <- project("\u00c9tudes & Daten 2026", "methods", "Goal \u2713 with \u6570\u636e", parent)
  expect_match(basename(p$path), "^[a-z][a-z0-9_]*$")
  expect_identical(read_project(p$path)$spec$project$name, "\u00c9tudes & Daten 2026")
  expect_false(audit(p$path, scope = "project")$overall_status %in% c("fail", "not_tested"))
  for (name in c("con", "aux", "CON", "Aux", "nul", "lpt1", "com9")) {
    expect_error(project(name, "methods", "Goal", parent, dry_run = TRUE), class = "cttir_input_error")
  }
  for (path in c("aux/notes.md", "data/con.txt", "code/nul", "x/prn.R")) {
    expect_error(relative_file(path), class = "cttir_schema_error")
  }
  reserved <- list(publications = list(list(id = "pub01", slug = "con")))
  expect_error(project("Pubs", "methods", "Goal", parent, options = reserved, dry_run = TRUE), class = "cttir_input_error")
})

test_that("symlinked parents and roots and hard-linked managed files are refused", {
  skip_on_os("windows")
  base <- new_parent()
  outside <- new_parent()
  link <- file.path(base, "link")
  expect_true(file.symlink(outside, link))
  expect_error(project("Linked", "methods", "Goal", link), class = "cttir_path_conflict")
  expect_length(parent_entries(outside), 0L)
  p <- project("Hard link", "methods", "Goal", base)
  expect_true(file.symlink(p$path, file.path(base, "root-link")))
  expect_error(audit(file.path(base, "root-link"), scope = "project"), class = "cttir_path_conflict")
  managed <- file.path(p$path, "code/validate_project.R")
  external <- file.path(outside, "external.R")
  file.copy(managed, external)
  unlink(managed)
  expect_true(file.link(external, managed))
  before <- tree_state(p$path)
  external_hash <- digest::digest(file = external, algo = "sha256")
  expect_error(sync(p$path, dry_run = FALSE), class = "cttir_path_conflict")
  report <- audit(p$path, scope = "project", repair = TRUE)
  expect_equal(report$checks$status[report$checks$id == "PRJ-002"], "fail")
  expect_identical(jsonlite::fromJSON(report$checks$evidence[report$checks$id == "PRJ-002"])$unsafe, "code/validate_project.R")
  expect_identical(tree_state(p$path), before)
  expect_equal(digest::digest(file = external, algo = "sha256"), external_hash)
})

test_that("edited manuscripts are preserved and edited managed files conflict", {
  parent <- new_parent()
  p <- project("Edits", "methods", "Goal", parent)
  manuscript <- file.path(p$path, "publications/pub01_main/manuscript/README.md")
  managed <- file.path(p$path, "code/validate_project.R")
  original <- readLines(managed)
  writeLines("Reviewed manuscript", manuscript)
  writeLines("# Local managed edit", managed)
  before <- tree_state(p$path)
  s <- sync(p$path, options = list(project = list(goal = "Changed goal")), dry_run = FALSE)
  expect_equal(s$state, "conflict")
  expect_contains(s$conflicts, "code/validate_project.R")
  expect_identical(tree_state(p$path), before)
  writeLines(original, managed)
  applied <- sync(p$path, options = list(project = list(goal = "Changed goal")), dry_run = FALSE)
  expect_equal(applied$state, "applied")
  expect_equal(readLines(manuscript), "Reviewed manuscript")
  expect_equal(read_project(p$path)$spec$project$goal, "Changed goal")
})

test_that("a missing template resource raises a typed error before any write", {
  parent <- new_parent()
  imports <- parent.env(asNamespace("cttiR"))
  real <- if (exists("system.file", envir = imports, inherits = FALSE)) get("system.file", envir = imports) else base::system.file
  local_mocked_bindings(system.file = function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {
    if (identical(c(...)[1:2], c("templates", "reflowr-0.2.0"))) return("")
    real(..., package = package, lib.loc = lib.loc, mustWork = mustWork)
  })
  expect_error(project("Template", "methods", "Goal", parent), class = "cttir_api_mismatch")
  expect_length(parent_entries(parent), 0L)
  copy <- file.path(new_parent(), "reflowr-0.2.0")
  dir.create(copy)
  file.copy(list.files(real("templates", "reflowr-0.2.0", package = "cttiR"), full.names = TRUE), copy, recursive = TRUE)
  manifest <- read_document(file.path(copy, "manifest.json"))
  unlink(file.path(copy, names(manifest$files)[[1]]))
  local_mocked_bindings(system.file = function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {
    if (identical(c(...)[1:2], c("templates", "reflowr-0.2.0"))) return(copy)
    real(..., package = package, lib.loc = lib.loc, mustWork = mustWork)
  })
  expect_error(project("Template", "methods", "Goal", parent), "bundled template is missing", class = "cttir_api_mismatch")
  expect_length(parent_entries(parent), 0L)
})
