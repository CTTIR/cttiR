audit_row <- function(id, scope, status, required, message) {
  data.frame(id = id, scope = scope, status = status, required = required,
             message = message, stringsAsFactors = FALSE)
}

audit_overall <- function(checks) {
  if (any(checks$required & checks$status == "fail")) return("fail")
  if (any(checks$required & checks$status == "not_tested")) return("not_tested")
  if (all(checks$status == "not_applicable")) return("not_tested")
  if (any(checks$status %in% c("warning", "fail", "not_tested"))) return("warning")
  "pass"
}

repair_missing <- function(p) {
  desired <- project_bundle(p$spec, p$lock)$files
  rows <- list()
  for (entry in p$manifest$files) {
    if (entry$ownership != "managed" || !is.na(file_hash(file.path(p$path, entry$path)))) next
    content <- desired[[entry$path]]
    if (is.null(content) || !identical(content_hash(content), entry$baseline_sha256)) next
    rows <- append(rows, list(data.frame(path = entry$path, action = "create", old_hash = NA_character_,
                                        new_hash = entry$baseline_sha256, stringsAsFactors = FALSE)))
  }
  if (!length(rows)) return(list())
  plan <- do.call(rbind, rows)
  journal <- transact_files(p$path, desired, plan)
  list(list(id = "restore_missing_managed", paths = plan$path, journal = journal, status = "applied"))
}

#' Inspect installation, resources and project integrity
#'
#' Default inspection is local and read-only. It never opens referenced study
#' datasets, runs study pipelines, installs dependencies or starts services.
#' Missing managed files may be restored with `repair = TRUE` only when their
#' generated content matches the accepted baseline. Edited files are preserved.
#' Reports are written only to an explicit output directory with unique names.
#' @param path Optional exact existing project root.
#' @param scope Nonempty subset of `installation`, `knowledge`, `project`,
#'   `integration`. Integration opts into temporary synthetic files.
#' @param repair Apply the documented reversible repair allowlist.
#' @param output Optional directory for JSON and Markdown reports.
#' @param strict Throw `cttir_audit_failed` after reporting required failures or
#'   required unverified checks. The condition contains the full report.
#' @param live Allow live probes. No runtime probe is yet implemented; a selected
#'   live integration check is reported as unverified, never as passed.
#' @return A `cttir_audit` with check table, overall status, repairs and limitations.
#' @export
audit <- function(path = NULL, scope = c("installation", "knowledge", "project"),
                   repair = FALSE, output = NULL, strict = FALSE, live = FALSE) {
  for (key in c("repair", "strict", "live")) scalar_flag(get(key), key)
  allowed <- c("installation", "knowledge", "project", "integration")
  if (!is.character(scope) || !length(scope) || anyNA(scope) || any(!scope %in% allowed))
    abort_cttir("scope must be a nonempty subset of installation, knowledge, project and integration.")
  scope <- unique(scope)
  if (!is.null(path)) { scalar_text(path, "path"); assert_plain_path(path) }
  if (!is.null(output)) { scalar_text(output, "output"); assert_plain_path(output) }
  checks <- list()
  add <- function(id, area, status, required, message)
    checks[[length(checks) + 1L]] <<- audit_row(id, area, status, required, message)
  run <- function(id, area, expr) {
    tryCatch({ force(expr); add(id, area, "pass", TRUE, "Verified.") }, error = function(e) {
      add(id, area, "fail", TRUE, if (inherits(e, "cttir_error")) conditionMessage(e) else "Integrity check failed.")
    })
  }
  repairs <- list()
  if ("installation" %in% scope) {
    run("INS-001", "installation", {
      needed <- c("project", "resources", "validate_spec", "validate_config", "sync", "audit", "doctor")
      stopifnot(all(needed %in% getNamespaceExports("cttiR")))
      resource_file("schema", "project-spec.schema.json")
    })
    add("INS-002", "installation", "not_tested", FALSE, "Runtime setup and inference support are pending.")
  }
  if ("knowledge" %in% scope) {
    run("RES-001", "knowledge", {
      resources(limit = 1L)
      con <- DBI::dbConnect(RSQLite::SQLite(), resource_file("extdata", "package-resources.sqlite"), flags = RSQLite::SQLITE_RO)
      tryCatch({
        stopifnot(identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]], "ok"))
        stopifnot(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")) == 0L)
      }, finally = DBI::dbDisconnect(con))
    })
    add("RES-002", "knowledge", "warning", FALSE, "Resource observations are dated; remote freshness was not checked.")
    add("KB-004", "knowledge", "not_tested", FALSE, "Verified API corpus and workflow approvals are pending.")
  }
  if ("project" %in% scope) {
    if (is.null(path)) add("PRJ-001", "project", "not_applicable", TRUE, "No project root was supplied.")
    else {
      p <- NULL
      run("PRJ-001", "project", { p <- read_project(path) })
      if (!is.null(p)) {
        if (repair) repairs <- repair_missing(p)
        for (entry in p$manifest$files) {
          tryCatch({
            current <- file_hash(file.path(p$path, entry$path))
            status <- if (is.na(current)) "fail" else if (identical(current, entry$baseline_sha256)) "pass" else "warning"
            add(paste0("PRJ-002:", entry$path), "project", status, TRUE,
                if (status == "fail") "File is missing." else if (status == "warning") "Local edits preserved." else "Baseline matches.")
          }, error = function(e) add(paste0("PRJ-002:", entry$path), "project", "fail", TRUE, "Unsafe file path or links."))
        }
        for (pub in p$spec$publications) run(paste0("PRJ-003:", pub$id), "project", {
          for (dir in c("analysis", "manuscript", "figures", "tables", "supplement", "submission")) {
            folder <- file.path(p$path, "publications", pub$slug, dir)
            assert_plain_path(folder)
            stopifnot(dir.exists(folder))
          }
        })
        run("PRJ-004", "project", resources(path = p$path, limit = 1L))
        run("PRJ-007", "project", {
          if (length(pending_transactions(p$path)) || dir.exists(file.path(p$path, ".cttir/write-lock")))
            abort_cttir("An active or interrupted write needs review.", "cttir_transaction_conflict")
        })
        add("STD-002", "project", "not_tested", FALSE, "Workflow adapter validation remains pending.")
      }
    }
  }
  if ("integration" %in% scope) {
    run("INT-001", "integration", {
      parent <- tempfile("cttir-audit-")
      dir.create(parent)
      tryCatch({
        first <- project("Synthetic audit", "methods", "Check scaffold construction", parent)
        second <- project("Synthetic audit", "methods", "Check scaffold construction", parent)
        stopifnot(identical(first$spec, second$spec))
      }, finally = unlink(parent, recursive = TRUE))
    })
    if (live) add("INT-003", "integration", "not_tested", TRUE, "Live runtime verification is not implemented.")
  }
  checks <- do.call(rbind, checks)
  result <- structure(list(schema_version = 1L, timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    profile = "scaffold", scopes = scope, live = live, checks = checks, overall_status = audit_overall(checks),
    repairs = repairs, environment = list(R = as.character(getRversion()), platform = R.version$platform),
    limitations = c("Software integrity is not scientific validation.", "Full product and cross-platform gates remain open."),
    reports = character()), class = "cttir_audit")
  if (!is.null(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    stem <- file.path(output, paste0("cttir-audit-", uuid::UUIDgenerate()))
    result$reports <- paste0(stem, c(".json", ".md"))
    write_bytes(paste0(json_text(unclass(result), TRUE), "\n"), result$reports[[1]])
    lines <- c("# Project integrity report", "", paste("Status:", result$overall_status), "",
               "| Check | Status |", "|---|---|",
               paste0("| ", checks$id, " | ", checks$status, " |"))
    write_bytes(paste0(paste(lines, collapse = "\n"), "\n"), result$reports[[2]])
  }
  if (strict && result$overall_status %in% c("fail", "not_tested"))
    stop(structure(list(message = "Required audit checks failed or remain unverified.", call = NULL,
                        code = "audit_failed", report = result), class = c("cttir_audit_failed", "cttir_error", "error", "condition")))
  result
}

#' Brief read-only diagnostics
#' @param path Optional exact project root.
#' @return A `cttir_audit` from the shared inspection engine.
#' @export
doctor <- function(path = NULL) audit(path = path, repair = FALSE, live = FALSE)

#' @export
print.cttir_audit <- function(x, ...) {
  cat("Audit: ", x$overall_status, " (", x$profile, ")\n", sep = "")
  print(table(x$checks$status))
  invisible(x)
}

#' @export
as.data.frame.cttir_audit <- function(x, ...) x$checks
