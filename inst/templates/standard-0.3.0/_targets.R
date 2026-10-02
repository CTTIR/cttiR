# Reviewed targets pipeline for the standard workflow (template bundle standard-0.3.0).
#
# Run from the project root with `targets::tar_make()`. The pipeline checks the
# project structure without opening study data, runs the labelled synthetic
# demonstration into demo/ and lists what the study-data workflow still needs.
# The study target reads the bound dataset only when every prerequisite is
# recorded; otherwise it returns a `cttir_missing_input` status and reads
# nothing. Inspect it with `targets::tar_read(study)`.
#
# This is the only scheduler for these stages when you use it: it calls the
# stage library directly and never runs code/run_demo.R or code/run_workflow.R,
# which remain the entry points for projects without targets. Every non-base
# call is namespaced; nothing is attached with library().

ct_library <- c("code/R/cttir_figures.R", "code/R/cttir_workflow.R")
ct_structure <- c("cttir-project.yml", "cttir-lock.json", "config/workflow.yml", "config/analysis.yml",
  "metadata/data-registry.yml", "metadata/data-dictionary.csv")
if (!all(file.exists(c(ct_structure[[1]], ct_library)))) {
  stop("Run targets::tar_make() from the project root; the workflow stage library is missing.", call. = FALSE)
}
source(ct_library[[1]])
source(ct_library[[2]])

ct_dictionary_columns <- c("dataset_id", "variable", "type", "unit", "allowed_values", "missing_codes", "description")
ct_demo_packages <- c("yaml", "jsonlite", "dplyr", "ggplot2", "patchwork", "viridisLite", "RColorBrewer",
  "colorspace", "nlme", "survival", "broom", "broom.mixed")

ct_tag <- function(prefix, values) if (length(values)) paste0(prefix, values) else character()

# Configuration and metadata files whose changes invalidate downstream targets.
ct_inputs <- function() {
  files <- c(ct_structure, ".cttir/local.yml")
  files[file.exists(files)]
}

# Structural validation: presence and shape of metadata only, never study data.
ct_validate <- function(inputs) {
  issues <- ct_tag("missing_file:", setdiff(ct_structure, inputs))
  if ("metadata/data-dictionary.csv" %in% inputs) {
    header <- readLines("metadata/data-dictionary.csv", n = 1L, warn = FALSE, encoding = "UTF-8")
    columns <- if (length(header)) strsplit(header, ",", fixed = TRUE)[[1]] else character()
    issues <- c(issues, ct_tag("dictionary_column:", setdiff(ct_dictionary_columns, columns)))
  }
  if ("metadata/data-registry.yml" %in% inputs) {
    datasets <- cw_read_yaml("metadata/data-registry.yml")$datasets
    ids <- vapply(datasets, function(x) {
      if (is.list(x) && is.character(x$id) && length(x$id) == 1L) x$id else NA_character_
    }, character(1))
    if (anyNA(ids) || anyDuplicated(ids)) issues <- c(issues, "registry:dataset_ids_missing_or_duplicated")
  }
  if (length(issues)) {
    stop("Structural validation failed: ", paste(issues, collapse = ", "), call. = FALSE)
  }
  list(status = "passed", files = inputs, study_data_opened = FALSE)
}

ct_configuration <- function(inputs, validation) {
  config <- cw_config(".")
  if (!is.list(config$workflow$stages) || !length(config$workflow$stages)) {
    stop("config/workflow.yml lists no workflow stages.", call. = FALSE)
  }
  config
}

ct_missing <- function(stage, missing, reason) {
  structure(list(status = "blocked", stage = stage, reason = reason, missing = missing,
      data_read = FALSE), class = c("cttir_missing_input", "cttir_targets_status"))
}

# Synthetic demonstration with the same stage functions as code/run_demo.R.
ct_demo <- function(config) {
  absent <- ct_demo_packages[!vapply(ct_demo_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) {
    return(ct_missing("demo", ct_tag("package:", absent),
        "Install the synthetic demonstration dependencies explicitly, then rerun targets::tar_make()."))
  }
  receipt <- cw_run_demo(root = ".", figures_policy = config$figures, backend = config$workflow$table_backend)
  if (!identical(receipt$status, "passed")) {
    stop("The synthetic demonstration failed its reference checks; see demo/receipt.json.", call. = FALSE)
  }
  receipt
}

ct_demo_files <- function(demo) {
  if (inherits(demo, "cttir_missing_input")) return(character())
  files <- file.path("demo", "receipt.json")
  for (name in names(demo$cases)) {
    files <- c(files, file.path("demo", "outputs", name, unlist(demo$cases[[name]]$outputs)))
  }
  files[file.exists(files)]
}

# Re-evaluated on every run: bindings and installed versions live outside the
# tracked files. The dataset is fingerprinted by size and time stamp only.
ct_requirements <- function(config) {
  requirements <- cw_requirements(config)
  if (isTRUE(requirements$ready)) {
    dataset <- cw_dataset(config)
    info <- file.info(dataset$path)
    requirements$input <- list(id = dataset$id, size = info$size, modified = format(info$mtime, "%Y-%m-%dT%H:%M:%OS3"))
  }
  requirements
}

ct_study <- function(config, requirements) {
  if (!isTRUE(requirements$ready)) {
    message("Study-data workflow not run: ", length(requirements$missing),
      " prerequisites missing. See targets::tar_read(study).")
    return(ct_missing("study", requirements$missing,
        "The study-data workflow is not ready; no data were read and no analysis was run."))
  }
  outputs <- cw_study_outputs(".")
  data <- cw_import(config)
  result <- cw_run(data, config$analysis, config$figures, outputs,
    backend = config$workflow$table_backend, synthetic = FALSE)
  cw_write_json(list(status = result$status, scheduler = "targets", check = result$check,
      describe = result$describe, model = result$model, diagnostics = result$diagnostics,
      import = attr(data, "cw_import"), outputs = sub("^[.]/", "", result$outputs),
      session = cw_session(unlist(lapply(config$workflow$dependencies, function(x) x$package))),
      code_sha256 = cw_code_hashes(".")),
    outputs$receipt)
  if (!identical(result$status, "completed")) {
    stop("The study-data workflow stopped with status '", result$status,
      "'; review output/workflow-receipt.json.", call. = FALSE)
  }
  structure(list(status = result$status, receipt = outputs$receipt,
      outputs = sub("^[.]/", "", result$outputs), data_read = TRUE), class = "cttir_targets_status")
}

targets::tar_option_set(packages = character())

list(
  targets::tar_target(inputs, ct_inputs(), format = "file", cue = targets::tar_cue(mode = "always")),
  targets::tar_target(validation, ct_validate(inputs)),
  targets::tar_target(configuration, ct_configuration(inputs, validation)),
  targets::tar_target(demo, ct_demo(configuration)),
  targets::tar_target(demo_files, ct_demo_files(demo), format = "file"),
  targets::tar_target(requirements, ct_requirements(configuration), cue = targets::tar_cue(mode = "always")),
  targets::tar_target(study, ct_study(configuration, requirements))
)
