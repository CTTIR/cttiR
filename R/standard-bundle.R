standard_bundle_name <- "standard-0.3.0"

bundle_condition_met <- function(condition, spec) {
  if (is.null(condition)) return(TRUE)
  ok <- TRUE
  if (!is.null(condition$modality)) ok <- ok && isTRUE(spec$ecosystem$modality %in% unlist(condition$modality))
  if (!is.null(condition$pipeline)) ok <- ok && isTRUE(spec$workflow$pipeline %in% unlist(condition$pipeline))
  ok
}

# Static reviewed files selected by documented conditions, plus generated
# configuration. User text only appears in YAML data, never in code.
standard_bundle_files <- function(spec, route) {
  root <- system.file("templates", standard_bundle_name, package = "cttiR")
  if (!nzchar(root)) abort_cttir("The bundled template is missing.", "cttir_api_mismatch")
  manifest <- read_document(file.path(root, "manifest.json"))
  files <- list()
  for (path in names(manifest$files)) {
    if (!bundle_condition_met(manifest$conditional[[path]], spec)) next
    file <- file.path(root, path)
    if (!file.exists(file)) abort_cttir("A bundled template is missing.", "cttir_api_mismatch")
    text <- paste0(paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n")
    if (!identical(content_hash(text), manifest$files[[path]])) {
      abort_cttir("A bundled template failed its integrity check.", "cttir_api_mismatch")
    }
    files[[path]] <- text
  }
  files[["metadata/workflow-template.json"]] <- paste0(json_text(manifest, TRUE), "\n")
  files[["config/workflow.yml"]] <- workflow_config_text(spec, route)
  files
}

workflow_config_text <- function(spec, route) {
  catalog <- catalog_snapshot(spec$provenance$catalog_id)
  yaml::as.yaml(list(
    schema_version = 1L, bundle = standard_bundle_name, profile = route$profile,
    table_backend = spec$workflow$table_backend, pipeline = spec$workflow$pipeline,
    engine = route$engine, modality = route$modality,
    stages = lapply(route$stages, function(x) x[c("stage", "capability", "adapter", "packages", "enabled", "status")]),
    dependencies = lapply(route_dependencies(route, catalog), function(x) x[c("package", "version", "required", "stages")]),
    ecosystem_candidates = lapply(route$ecosystem, function(x) x[c("capability", "packages", "status")]),
    specialist_candidates = lapply(route$specialist, function(x) x[c("capability", "packages", "status")]),
    gaps = as.list(route$gaps)
  ))
}
