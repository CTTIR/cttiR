# Explicit study runner with before/after content verification for report freshness.
# Existing stage prerequisites still control whether any study computation runs.
main <- function() {
  script <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(script) != 1L) stop("Invoke this file with Rscript --vanilla.")
  script <- gsub("~+~", " ", sub("^--file=", "", script), fixed = TRUE)
  root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
  previous <- setwd(root)
  on.exit(setwd(previous), add = TRUE)
  required <- c("cttir-project.yml", "config/workflow.yml", "code/R/cttir_workflow.R", "code/R/cttir_figures.R")
  if (!all(file.exists(required))) stop("Run from the project root.", call. = FALSE)
  source("code/R/cttir_figures.R", local = TRUE)
  source("code/R/cttir_workflow.R", local = TRUE)
  config <- cw_config(".")
  if (identical(config$analysis$aim, "descriptive")) {
    roles <- cw_roles(config$analysis$mapping)
    if (!length(c(unlist(roles$roles), unlist(roles$predictors)))) {
      stop("Descriptive analysis needs an explicit outcome or predictor mapping before data are read.", call. = FALSE)
    }
  }
  requirements <- cw_requirements(config)
  if (!requirements$ready) {
    stop(structure(class = c("cttir_missing_input", "error", "condition"), list(
      message = paste0("The study-data workflow is not ready. Missing:\n",
        paste("-", requirements$missing, collapse = "\n")),
      call = NULL, missing = requirements$missing)))
  }
  source("code/R/report_integrity.R", local = TRUE)
  dataset <- cw_dataset(config)
  bindings <- cr_project_bindings(".", dataset$id)
  report_state <- cr_file(".", ".cttir/report-receipt.json", must_exist = FALSE)
  if (file.exists(report_state)) {
    if (unlink(report_state) != 0L) stop("Could not invalidate the previous report receipt.")
  }
  before <- cr_begin(".", bindings)
  data <- cw_import(config)
  outputs <- cw_study_outputs(".")
  result <- cw_run(data, config$analysis, config$figures, outputs,
    backend = config$workflow$table_backend, synthetic = FALSE)
  cw_write_json(list(status = result$status, check = result$check, describe = result$describe,
    model = result$model, diagnostics = result$diagnostics, import = attr(data, "cw_import"),
    outputs = sub("^[.]/", "", result$outputs), session = cw_session(unlist(lapply(config$workflow$dependencies, function(x) x$package))),
    code_sha256 = cw_code_hashes(".")),
    outputs$receipt)
  if (identical(result$status, "completed")) {
    written <- list.files("output", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    paths <- if (length(written)) paste0("output/", written) else character()
    produced <- unique(sub("^[.]/", "", c(unlist(result$outputs), outputs$receipt)))
    if (!setequal(paths, produced)) {
      stop("Previous or untracked output files remain. Review and move them aside before rerunning; they cannot receive a fresh receipt.")
    }
    receipt <- cr_finish(".", before, produced, bindings)
    cw_write_json(receipt, report_state)
  }
  message("Workflow ", result$status, ". Review output/ before interpreting any estimate.")
  if (!identical(result$status, "completed")) quit(status = 1L)
}
main()
