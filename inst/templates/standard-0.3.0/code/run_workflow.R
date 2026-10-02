# Study-data workflow. Runs only when every prerequisite is recorded; otherwise
# it stops and lists the missing items. Run from the project root with
# `Rscript code/run_workflow.R`. Outputs go to reports/workflow/.
required <- c("cttir-project.yml", "config/workflow.yml", "code/R/cttir_workflow.R", "code/R/cttir_figures.R")
if (!all(file.exists(required))) stop("Run from the project root.", call. = FALSE)
source("code/R/cttir_figures.R", local = TRUE)
source("code/R/cttir_workflow.R", local = TRUE)
config <- cw_config(".")
requirements <- cw_requirements(config)
if (!requirements$ready) {
  stop(structure(class = c("cttir_missing_input", "error", "condition"), list(
    message = paste0("The study-data workflow is not ready. Missing:\n",
      paste("-", requirements$missing, collapse = "\n")),
    call = NULL, missing = requirements$missing)))
}
data <- cw_import(config)
result <- cw_run(data, config$analysis, config$figures, file.path("reports", "workflow"),
  backend = config$workflow$table_backend, synthetic = FALSE)
cw_write_json(list(status = result$status, check = result$check, describe = result$describe,
  model = result$model, diagnostics = result$diagnostics, import = attr(data, "cw_import"),
  outputs = basename(result$outputs), session = cw_session(unlist(lapply(config$workflow$dependencies, function(x) x$package)))),
  file.path("reports", "workflow", "receipt.json"))
message("Workflow ", result$status, ". Review reports/workflow/ before interpreting any estimate.")
if (!identical(result$status, "completed")) quit(status = 1L)
