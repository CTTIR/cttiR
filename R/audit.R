# Audit engine and check registry ---------------------------------------------
#
# Every audit row comes from a check definition created with audit_check().
# Built-in definitions are grouped by provider functions listed in
# audit_check_providers(); each provider returns a list of audit_check()
# definitions. To add a family of checks (for example STD-, RES- or documentation
# corpus checks), define a provider such as `audit_checks_standard()` in its own
# file and append it to audit_check_providers(). A check ID may be defined only
# once across all providers. For the current session only, tests and local
# extensions can call register_audit_check(audit_check(...)) and remove it with
# unregister_audit_check(id); built-in IDs cannot be shadowed that way.
#
# A check's `run(context)` returns audit_result(status, message, evidence).
# `context` is an environment holding `path`, `scopes`, `repair` and `live`;
# use audit_project(context), audit_spec(context) and audit_root(context) for
# memoized read-only project access. Errors raised by `run` become `fail` rows
# (or `not_tested` when the time budget is exceeded). `required` may be a flag or
# a function of the context deriving it from the declared readiness/workflow.
# `applies` may return TRUE, FALSE or a reason string for `not_applicable`.
# `read_effects` must use audit_effect_levels; declare every effect honestly.

audit_scope_levels <- c("installation", "knowledge", "project", "integration")
audit_status_levels <- c("pass", "warning", "fail", "not_tested", "not_applicable")
audit_severity_levels <- c("error", "warning", "info")
audit_effect_levels <- c(
  "reads_installation", "loads_namespaces", "reads_storage_metadata",
  "reads_runtime_metadata", "local_http_metadata", "local_model_inference",
  "reads_catalog_store", "reads_project_metadata", "reads_local_bindings",
  "reads_git_index", "writes_temp_files"
)
audit_readiness_levels <- c("scaffold_ready", "environment_ready", "demo_verified", "data_ready", "analysis_ready")

.audit_registry <- new.env(parent = emptyenv())
.audit_registry$checks <- list()

audit_or <- function(x, y) if (is.null(x)) y else x

audit_check <- function(id, scope, description, run, required = TRUE, severity = NULL,
  read_effects = "reads_installation", timeout_seconds = 60, repair_id = NULL,
  applies = NULL, evidence_schema = character()) {
  invalid <- function(message, field) abort_cttir(message, "cttir_schema_error", "invalid_audit_check", field)
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^[A-Z]{2,5}-[0-9]{3}$", id)) {
    invalid("Audit check IDs use the form ABC-001.", "id")
  }
  if (!is.character(scope) || length(scope) != 1L || !scope %in% audit_scope_levels) {
    invalid("Audit check scope must be installation, knowledge, project or integration.", "scope")
  }
  scalar_text(description, "description")
  if (!is.function(run)) invalid("Audit check runners must be functions of the context.", "run")
  if (!is.function(required) && (!is.logical(required) || length(required) != 1L || is.na(required))) {
    invalid("required must be TRUE, FALSE or a function of the context.", "required")
  }
  if (is.null(severity)) severity <- if (isFALSE(required)) "warning" else "error"
  if (!is.character(severity) || length(severity) != 1L || !severity %in% audit_severity_levels) {
    invalid("Audit check severity must be error, warning or info.", "severity")
  }
  if (!is.character(read_effects) || !length(read_effects) || anyNA(read_effects) ||
      any(!read_effects %in% audit_effect_levels) || anyDuplicated(read_effects)) {
    invalid("Declare read effects from audit_effect_levels.", "read_effects")
  }
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L || is.na(timeout_seconds) ||
      !is.finite(timeout_seconds) || timeout_seconds <= 0) {
    invalid("timeout_seconds must be a positive number.", "timeout_seconds")
  }
  known_repair <- is.character(repair_id) && length(repair_id) == 1L && repair_id %in% names(audit_repairs())
  if (!is.null(repair_id) && !known_repair) invalid("repair_id must name an allowlisted repair.", "repair_id")
  if (!is.null(applies) && !is.function(applies)) invalid("applies must be a function of the context.", "applies")
  if (!is.character(evidence_schema) || anyNA(evidence_schema)) invalid("evidence_schema lists evidence fields.", "evidence_schema")
  structure(list(
    id = id, scope = scope, description = description, required = required, severity = severity,
    read_effects = read_effects, timeout_seconds = timeout_seconds, run = run, repair_id = repair_id,
    applies = applies, evidence_schema = evidence_schema
  ), class = "cttir_audit_check")
}

audit_result <- function(status, message, evidence = NULL) {
  list(status = status, message = message, evidence = evidence)
}

# Integrators append further providers here, e.g. audit_checks_standard.
audit_check_providers <- function() {
  list(audit_checks_installation, audit_checks_knowledge, audit_checks_project, audit_checks_integration,
    audit_checks_standard)
}

audit_builtin_checks <- function() {
  checks <- do.call(c, lapply(audit_check_providers(), function(provider) provider()))
  for (check in checks) {
    if (!inherits(check, "cttir_audit_check")) {
      abort_cttir("Audit providers must return audit_check() definitions.", "cttir_schema_error", "invalid_audit_check")
    }
  }
  checks
}

audit_checks <- function(scope = NULL) {
  checks <- c(audit_builtin_checks(), unname(.audit_registry$checks))
  ids <- vapply(checks, function(x) x$id, character(1))
  if (anyDuplicated(ids)) {
    abort_cttir("Audit check IDs must be unique.", "cttir_schema_error", "duplicate_audit_check", ids[duplicated(ids)])
  }
  names(checks) <- ids
  scopes <- vapply(checks, function(x) x$scope, character(1))
  checks <- checks[order(match(scopes, audit_scope_levels), seq_along(checks))]
  if (!is.null(scope)) checks <- checks[vapply(checks, function(x) x$scope %in% scope, logical(1))]
  checks
}

register_audit_check <- function(check, replace = FALSE) {
  if (!inherits(check, "cttir_audit_check")) abort_cttir("Create audit checks with audit_check().")
  scalar_flag(replace, "replace")
  builtin <- vapply(audit_builtin_checks(), function(x) x$id, character(1))
  if (check$id %in% builtin) {
    abort_cttir("Built-in check IDs are extended through audit_check_providers().", "cttir_schema_error", "duplicate_audit_check", check$id)
  }
  if (!replace && !is.null(.audit_registry$checks[[check$id]])) {
    abort_cttir("This audit check ID is already registered.", "cttir_schema_error", "duplicate_audit_check", check$id)
  }
  .audit_registry$checks[[check$id]] <- check
  invisible(check)
}

unregister_audit_check <- function(id) {
  scalar_text(id, "id")
  .audit_registry$checks[[id]] <- NULL
  invisible(NULL)
}

# Context ----------------------------------------------------------------------

audit_context <- function(path, scopes, repair, live) {
  context <- new.env(parent = emptyenv())
  context$path <- path
  context$scopes <- scopes
  context$repair <- repair
  context$live <- live
  context$cache <- new.env(parent = emptyenv())
  context
}

audit_reset <- function(context) {
  rm(list = ls(context$cache, all.names = TRUE), envir = context$cache)
  invisible(context)
}

audit_cached <- function(context, key, fun) {
  if (!exists(key, envir = context$cache, inherits = FALSE)) {
    assign(key, tryCatch(fun(), error = function(e) e), envir = context$cache)
  }
  get(key, envir = context$cache, inherits = FALSE)
}

audit_root <- function(context) {
  if (is.null(context$path)) return(NULL)
  root <- audit_cached(context, "root", function() {
    if (dir.exists(context$path)) normalizePath(context$path, winslash = "/", mustWork = TRUE) else NULL
  })
  if (inherits(root, "error")) NULL else root
}

audit_project <- function(context) {
  if (is.null(context$path)) return(NULL)
  audit_cached(context, "project", function() read_project(context$path))
}

audit_spec <- function(context) {
  p <- audit_project(context)
  if (is.null(p) || inherits(p, "error")) NULL else p$spec
}

audit_with_project <- function(context, fun) {
  p <- audit_project(context)
  if (is.null(p) || inherits(p, "error")) {
    return(audit_result("not_tested", "Project metadata could not be read; see PRJ-001."))
  }
  fun(p)
}

audit_has_path <- function(context) {
  if (is.null(context$path)) "No project root was supplied." else TRUE
}

audit_readiness_at_least <- function(context, level) {
  spec <- audit_spec(context)
  if (is.null(spec)) return(FALSE)
  isTRUE(match(spec$workflow$readiness, audit_readiness_levels) >= match(level, audit_readiness_levels))
}

audit_requires_local_model <- function(context) {
  identical(audit_spec(context)$provenance$planner_mode, "local_llm")
}

audit_condition_message <- function(e) {
  if (inherits(e, "cttir_error")) conditionMessage(e) else "Integrity check failed."
}

# Execution --------------------------------------------------------------------

audit_with_budget <- function(run, context, seconds) {
  setTimeLimit(elapsed = seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  run(context)
}

audit_run_check <- function(check, context) {
  required <- check$required
  if (is.function(required)) required <- isTRUE(tryCatch(required(context), error = function(e) TRUE))
  started <- proc.time()[["elapsed"]]
  applicable <- if (is.null(check$applies)) TRUE else tryCatch(check$applies(context), error = function(e) TRUE)
  if (!isTRUE(applicable)) {
    outcome <- audit_result("not_applicable",
      if (is.character(applicable) && length(applicable) == 1L) applicable else "Not applicable to the selected inputs.")
  } else {
    outcome <- tryCatch(audit_with_budget(check$run, context, check$timeout_seconds), error = function(e) {
      if (proc.time()[["elapsed"]] - started >= check$timeout_seconds) {
        return(audit_result("not_tested", paste0("Check exceeded its ", check$timeout_seconds, "-second budget.")))
      }
      audit_result("fail", audit_condition_message(e), list(condition = class(e)[[1]]))
    })
  }
  valid <- is.list(outcome) && is.character(outcome$status) && length(outcome$status) == 1L &&
    outcome$status %in% audit_status_levels && is.character(outcome$message) &&
    length(outcome$message) == 1L && !is.na(outcome$message)
  if (!valid) outcome <- audit_result("fail", "The check returned an invalid result.")
  evidence <- tryCatch(if (is.null(outcome$evidence)) "{}" else json_text(outcome$evidence),
    error = function(e) "{\"error\":\"evidence could not be serialized\"}")
  data.frame(
    id = check$id, scope = check$scope, status = outcome$status, required = required,
    message = outcome$message, description = check$description, severity = check$severity,
    read_effects = paste(check$read_effects, collapse = ","),
    repair_id = audit_or(check$repair_id, NA_character_), evidence = evidence,
    duration_seconds = round(proc.time()[["elapsed"]] - started, 3),
    stringsAsFactors = FALSE
  )
}

audit_run_checks <- function(checks, context) {
  rows <- lapply(checks, audit_run_check, context = context)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

audit_overall <- function(checks) {
  if (any(checks$required & checks$status == "fail")) {
    return("fail")
  }
  if (any(checks$required & checks$status == "not_tested")) {
    return("not_tested")
  }
  if (all(checks$status == "not_applicable")) {
    return("not_tested")
  }
  if (any(checks$status %in% c("warning", "fail", "not_tested"))) {
    return("warning")
  }
  "pass"
}

audit_overall_reason <- function(checks) {
  ids <- function(x) paste(checks$id[x], collapse = ", ")
  required_fail <- checks$required & checks$status == "fail"
  required_unverified <- checks$required & checks$status == "not_tested"
  if (any(required_fail)) return(paste("Required checks failed:", ids(required_fail)))
  if (any(required_unverified)) return(paste("Required checks were not verified:", ids(required_unverified)))
  if (all(checks$status == "not_applicable")) return("No selected check was applicable, so nothing was verified.")
  advisory <- checks$status %in% c("warning", "fail", "not_tested")
  if (any(advisory)) return(paste("Optional or advisory findings:", ids(advisory)))
  "All applicable checks passed."
}

#' Inspect installation, resources and project integrity
#'
#' Runs registered checks with stable IDs (`INS-`, `KB-`, `RES-`, `PRJ-`,
#' `STD-` and `INT-`). Default inspection is local and read-only: it reads
#' package resources, catalog metadata, runtime state files and project metadata.
#' It never opens referenced study datasets or dereferences paths bound in
#' `.cttir/local.yml`, runs study pipelines, installs dependencies, starts
#' services, contacts a model daemon or queries remote servers. Declared Imports
#' are checked with [requireNamespace()], which may load trusted installed
#' namespaces. When the project root contains `.git` and Git is available,
#' tracked file names under ignored private paths are listed with
#' `git ls-files` (names only; no contents are read).
#'
#' Overall status precedence: a required `fail`, then a required `not_tested`,
#' then any warning or optional finding, then `pass`; when every check is
#' `not_applicable` the overall status is `not_tested`. Which checks are required
#' derives from the project's declared readiness and workflow.
#'
#' With `repair = TRUE`, only an allowlist of reversible repairs is attempted,
#' each only when its triggering check failed: rolling back an interrupted
#' project write whose recorded writer has stopped, restoring a missing managed
#' file whose generated content matches its accepted baseline, and repointing a
#' corrupt active catalog pointer to the most recently written retained manifest
#' that passes verification (under the catalog writer lock, journaled, never
#' deleting snapshots). Edited files are never overwritten. Each repair records
#' before/after hashes, its outcome and a recheck; a failed repair restores the
#' prior bytes. All checks except integration are rerun after an applied repair.
#'
#' The integration scope explicitly creates synthetic temporary projects and an
#' isolated temporary catalog store and removes them afterwards; it never uses
#' the configured catalog store for its fixture.
#'
#' Reports are written only to an explicit `output` directory, always under new
#' unique names. Report text is portable: paths under the project root become
#' relative, the home directory is shown as `~`, and Markdown control characters
#' in messages are escaped.
#' @param path Optional exact existing project root.
#' @param scope Nonempty subset of `installation`, `knowledge`, `project`,
#'   `integration`. Integration opts into temporary synthetic files.
#' @param repair Apply the documented reversible repair allowlist.
#' @param output Optional directory for JSON and Markdown reports.
#' @param strict Throw `cttir_audit_failed` after reporting required failures or
#'   required unverified checks. The condition contains the full report.
#' @param live Allow a bounded CPU structured-output probe of an already owned
#'   runtime when the integration scope is selected, and a loopback metadata
#'   query for model locality. Never installs, pulls or starts a runtime.
#' @return A `cttir_audit` with a check table (`id`, `scope`, `status`,
#'   `required`, `message`, `description`, `severity`, `read_effects`,
#'   `repair_id`, `evidence` as JSON text, `duration_seconds`), overall status
#'   and reason, repairs, package identity, effects and limitations.
#' @export
audit <- function(path = NULL, scope = c("installation", "knowledge", "project"),
  repair = FALSE, output = NULL, strict = FALSE, live = FALSE) {
  for (key in c("repair", "strict", "live")) scalar_flag(get(key), key)
  if (!is.character(scope) || !length(scope) || anyNA(scope) || any(!scope %in% audit_scope_levels)) {
    abort_cttir("scope must be a nonempty subset of installation, knowledge, project and integration.")
  }
  scope <- unique(scope)
  if (!is.null(path)) {
    scalar_text(path, "path")
    assert_plain_path(path)
  }
  if (!is.null(output)) {
    scalar_text(output, "output")
    assert_plain_path(output)
    if (file.exists(output) && !dir.exists(output)) {
      abort_cttir("output must be a directory, not an existing file.", "cttir_path_conflict", "output_not_directory")
    }
  }
  definitions <- audit_checks(scope)
  context <- audit_context(path, scope, repair, live)
  checks <- audit_run_checks(definitions, context)
  repairs <- list()
  if (repair) {
    repairs <- audit_apply_repairs(checks, context)
    if (any(vapply(repairs, function(x) identical(x$status, "applied"), logical(1)))) {
      rerun <- definitions[vapply(definitions, function(x) x$scope != "integration", logical(1))]
      fresh <- audit_run_checks(rerun, audit_context(path, scope, repair, live))
      at <- match(fresh$id, checks$id)
      checks[at, ] <- fresh
      for (i in seq_along(repairs)) {
        trigger <- repairs[[i]]$trigger
        repairs[[i]]$recheck <- as.list(stats::setNames(checks$status[match(trigger, checks$id)], trigger))
      }
    }
  }
  root <- audit_root(context)
  effects <- unique(unlist(strsplit(checks$read_effects[checks$status != "not_applicable"], ",", fixed = TRUE)))
  # Loopback model metadata and inference happen only when live = TRUE.
  if (!live) effects <- setdiff(effects, c("local_http_metadata", "local_model_inference"))
  if (!is.null(output)) effects <- c(effects, "writes_reports")
  if (any(vapply(repairs, function(x) x$status %in% c("applied", "failed"), logical(1)))) effects <- c(effects, "applies_repairs")
  result <- structure(list(
    schema_version = 2L, timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    profile = "scaffold", scopes = scope, live = live, checks = checks, overall_status = audit_overall(checks),
    repairs = repairs, environment = list(R = as.character(getRversion()), platform = R.version$platform),
    limitations = c(
      "Software integrity is not scientific validation.",
      "Default inspection reads metadata only; referenced datasets and bound paths are not opened.",
      "Full product and cross-platform gates remain open."
    ),
    reports = character(),
    overall_reason = audit_overall_reason(checks),
    package = audit_package_identity(), platform = R.version$platform,
    path = root, repair_requested = repair, effects = effects
  ), class = "cttir_audit")
  if (!is.null(output)) result$reports <- audit_write_reports(result, output, root)
  if (strict && result$overall_status %in% c("fail", "not_tested")) {
    blocking <- checks$id[checks$required & checks$status %in% c("fail", "not_tested")]
    stop(structure(list(
      message = "Required audit checks failed or remain unverified.", call = NULL,
      code = "audit_failed", field = blocking,
      remediation = "Inspect report$checks for messages and evidence; consider audit(repair = TRUE) for allowlisted repairs.",
      report = result
    ), class = c("cttir_audit_failed", "cttir_error", "error", "condition")))
  }
  result
}

audit_package_identity <- function() {
  description <- suppressWarnings(tryCatch(utils::packageDescription("cttiR"), error = function(e) NULL))
  built <- if (is.list(description) && !is.null(description$Built)) description$Built else NA_character_
  list(name = "cttiR", version = as.character(getNamespaceVersion("cttiR")), built = built)
}

#' Brief read-only diagnostics
#'
#' Runs the default audit scopes through the same check engine as [audit()],
#' never with repairs, live probes or reports.
#' @param path Optional exact project root.
#' @return A `cttir_audit` from the shared inspection engine.
#' @export
doctor <- function(path = NULL) audit(path = path, repair = FALSE, live = FALSE)

#' @export
print.cttir_audit <- function(x, ...) {
  cat("Audit: ", x$overall_status, " (", x$profile, ")\n", sep = "")
  print(table(x$checks$status))
  flagged <- x$checks[x$checks$status %in% c("fail", "warning", "not_tested"), , drop = FALSE]
  for (i in seq_len(nrow(flagged))) {
    cat(flagged$id[[i]], " ", flagged$status[[i]], ": ", flagged$message[[i]], "\n", sep = "")
  }
  invisible(x)
}

#' @export
as.data.frame.cttir_audit <- function(x, ...) x$checks

# Reports ----------------------------------------------------------------------

audit_report_stem <- function() {
  paste0("cttir-audit-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
    substr(gsub("-", "", uuid::UUIDgenerate(), fixed = TRUE), 1L, 12L))
}

audit_write_reports <- function(result, output, root) {
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output)) abort_cttir("The report directory could not be created.", "cttir_path_conflict", "output_unavailable")
  files <- NULL
  for (attempt in seq_len(20L)) {
    candidate <- file.path(output, paste0(audit_report_stem(), c(".json", ".md")))
    if (!any(file.exists(candidate))) {
      files <- candidate
      break
    }
  }
  if (is.null(files)) abort_cttir("Could not choose unused report names.", "cttir_path_conflict", "report_exists")
  portable <- audit_portable(unclass(result), root)
  portable$reports <- basename(files)
  write_bytes(paste0(json_text(portable, TRUE), "\n"), files[[1]])
  write_bytes(audit_markdown(portable), files[[2]])
  files
}

audit_path_prefixes <- function(path) {
  if (is.null(path) || !nzchar(path) || nchar(path) < 2L) return(character())
  normal <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) path)
  forms <- unique(c(normal, gsub("\\\\", "/", path), path))
  unique(c(forms, gsub("/", "\\\\", forms, fixed = TRUE)))
}

audit_redact_prefix <- function(text, prefix, replacement, relative) {
  # Match the prefix only as a whole path component, never inside a longer name.
  pattern <- gsub("([[:punct:]])", "\\\\\\1", prefix, perl = TRUE)
  ending <- "(?=$|[\"'\\s,;:)\\]}])"
  if (relative) {
    text <- gsub(paste0(pattern, "[/\\\\]"), "", text, perl = TRUE)
  } else {
    text <- gsub(paste0(pattern, "(?=[/\\\\])"), replacement, text, perl = TRUE)
  }
  gsub(paste0(pattern, ending), replacement, text, perl = TRUE)
}

audit_redact <- function(text, root) {
  if (!is.character(text) || !length(text)) return(text)
  missing <- is.na(text)
  for (prefix in audit_path_prefixes(root)) text <- audit_redact_prefix(text, prefix, ".", TRUE)
  home <- path.expand("~")
  if (!identical(home, "~") && nchar(home) > 1L) {
    for (prefix in audit_path_prefixes(home)) text <- audit_redact_prefix(text, prefix, "~", FALSE)
  }
  text[missing] <- NA_character_
  text
}

audit_portable <- function(x, root) {
  if (is.data.frame(x)) {
    for (column in names(x)) if (is.character(x[[column]])) x[[column]] <- audit_redact(x[[column]], root)
    return(x)
  }
  if (is.list(x)) {
    attrs <- attributes(x)
    x <- lapply(x, audit_portable, root = root)
    attributes(x) <- attrs
    if (!is.null(names(x))) names(x) <- audit_redact(names(x), root)
    return(x)
  }
  if (is.character(x)) return(audit_redact(x, root))
  x
}

md_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("|", "\\|", x, fixed = TRUE)
  x <- gsub("[", "\\[", x, fixed = TRUE)
  x <- gsub("]", "\\]", x, fixed = TRUE)
  x <- gsub("`", "\\`", x, fixed = TRUE)
  gsub("[\r\n\t]+", " ", x)
}

audit_markdown <- function(x) {
  checks <- x$checks
  lines <- c(
    "# cttiR audit report", "",
    paste0("- Overall status: ", md_escape(x$overall_status)),
    paste0("- Reason: ", md_escape(x$overall_reason)),
    paste0("- Generated (UTC): ", md_escape(x$timestamp)),
    paste0("- Package: cttiR ", md_escape(x$package$version)),
    paste0("- Scopes: ", md_escape(paste(x$scopes, collapse = ", "))),
    paste0("- Live: ", x$live, "; repair requested: ", x$repair_requested),
    paste0("- Effects: ", md_escape(paste(x$effects, collapse = ", "))),
    "", "| Check | Scope | Status | Required | Message |", "|---|---|---|---|---|",
    paste0("| ", md_escape(checks$id), " | ", md_escape(checks$scope), " | ", md_escape(checks$status),
      " | ", checks$required, " | ", md_escape(checks$message), " |")
  )
  if (length(x$repairs)) {
    lines <- c(lines, "", "## Repairs", "", "| Repair | Status | Reason |", "|---|---|---|")
    for (r in x$repairs) {
      lines <- c(lines, paste0("| ", md_escape(r$id), " | ", md_escape(r$status), " | ", md_escape(audit_or(r$reason, "")), " |"))
    }
  }
  lines <- c(lines, "", "## Limitations", "", paste0("- ", md_escape(x$limitations)))
  paste0(paste(lines, collapse = "\n"), "\n")
}
