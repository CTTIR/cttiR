readiness_levels <- c("scaffold_ready", "environment_ready", "demo_verified", "data_ready", "analysis_ready")

# Receipts count only when they were produced by the unmodified reviewed stage
# code of this project's template baseline.
receipt_matches_code <- function(p, receipt) {
  baseline <- stats::setNames(vapply(p$manifest$files, function(x) x$baseline_sha256, character(1)),
    vapply(p$manifest$files, function(x) x$path, character(1)))
  files <- c("code/R/cttir_workflow.R", "code/R/cttir_figures.R")
  for (file in files) {
    recorded <- receipt$code_sha256[[file]]
    current <- file_hash(file.path(p$path, file))
    if (is.null(recorded) || is.na(current) || !identical(recorded, current) ||
        !identical(current, unname(baseline[file]))) {
      return(FALSE)
    }
  }
  TRUE
}

read_receipt <- function(p, relative) {
  file <- file.path(p$path, relative)
  if (is.na(file_hash(file))) return(NULL)
  tryCatch(read_document(file), error = function(e) NULL)
}

# Readiness ladder from local evidence only: project metadata, installed package
# metadata, demo and workflow receipts. Datasets are never opened.
project_readiness <- function(path) {
  p <- read_project(path)
  checks <- list()
  add <- function(level, passed, reason) {
    checks[[length(checks) + 1L]] <<- list(level = level, passed = passed, reason = reason)
  }
  add("scaffold_ready", TRUE, "Specification, lock and ownership metadata validate.")
  if (!identical(p$spec$provenance$template_version, current_template_version)) {
    for (level in readiness_levels[-1]) add(level, FALSE, "This template version has no standard workflow stages.")
    return(list(level = "scaffold_ready", checks = checks))
  }
  environment <- environment_status(p$lock$dependencies)
  add("environment_ready", identical(environment$state, "installed_versions_match"),
    paste("Pinned dependencies:", environment$state))
  demo <- read_receipt(p, "demo/receipt.json")
  demo_ok <- !is.null(demo) && identical(demo$status, "passed") && isTRUE(demo$synthetic) &&
    receipt_matches_code(p, demo)
  add("demo_verified", demo_ok, if (demo_ok) "Synthetic demonstration passed with the reviewed stage code." else
    "Run Rscript code/run_demo.R with the unmodified stage code.")
  run <- read_receipt(p, "reports/workflow/receipt.json")
  data_ok <- !is.null(run) && identical(run$check$state, "passed") && receipt_matches_code(p, run)
  add("data_ready", data_ok, if (data_ok) "The last study-data run passed the structural data checks." else
    "No passing study-data run receipt from the reviewed stage code.")
  analysis <- analysis_configuration(p$spec)
  route <- project_route(p$spec)
  approved <- !length(route$approval_pending)
  analysis_ok <- data_ok && identical(analysis$state, "configuration_recorded") && isTRUE(p$spec$analysis$approved) && approved
  add("analysis_ready", analysis_ok, if (analysis_ok) "Mappings, reviewed settings, approval and stage approvals are recorded." else
    "Requires data readiness, complete reviewed configuration, recorded approval and approved stage adapters.")
  passed <- vapply(checks, function(x) x$passed, logical(1))
  reached <- if (all(passed)) length(passed) else which(!passed)[[1]] - 1L
  list(level = readiness_levels[[max(1L, reached)]], checks = checks,
    limitation = "Readiness reflects recorded evidence; it is not scientific validation of results.")
}
