# Explicit rendering of reviewed pages. Run with Rscript --vanilla using this
# script's path from any working directory. No install, commit or publication.
script <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script) != 1L) stop("Invoke this file with Rscript --vanilla.", call. = FALSE)
# The Unix R launcher encodes spaces in --file with ~+~ before R starts.
script <- gsub("~+~", " ", sub("^--file=", "", script), fixed = TRUE)
root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
for (package in c("workflowr", "jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Prepare the rendering dependency explicitly: ", package)
}
if (as.character(utils::packageVersion("workflowr")) != "1.7.2") stop("Rendering requires pinned workflowr 1.7.2.")
if (!file.exists(file.path(root, "_workflowr.yml"))) stop("Missing root workflowr configuration.")
integrity <- new.env(parent = baseenv())
sys.source(file.path(root, "code/R/report_integrity.R"), envir = integrity)
config <- jsonlite::read_json(file.path(root, "config/report-pages.json"), simplifyVector = FALSE)
pages <- unlist(config$pages, use.names = FALSE)
if (!identical(config$schema_version, 1L) || !is.character(pages) || !length(pages) ||
    anyNA(pages) || anyDuplicated(pages) || any(!grepl("^analysis/[A-Za-z0-9_-]+[.]Rmd$", pages))) {
  stop("Invalid reviewed report page list.")
}
for (page in pages) integrity$cr_file(root, page)
outputs <- list.files(file.path(root, "output"), recursive = TRUE, all.files = TRUE,
  full.names = FALSE, no.. = TRUE)
outputs <- if (length(outputs)) paste0("output/", outputs) else character()
if (length(outputs)) {
  receipt_file <- file.path(root, ".cttir/report-receipt.json")
  if (!file.exists(receipt_file)) stop("Study outputs lack a content receipt; rerun the reviewed workflow.")
  receipt <- jsonlite::read_json(receipt_file, simplifyVector = FALSE)
  ids <- vapply(receipt$external_inputs, function(x) x$dataset_id, character(1))
  bindings <- integrity$cr_project_bindings(root, ids)
  integrity$cr_assert_fresh(root, receipt, integrity$cr_execution_inputs(root), outputs, bindings)
}
workflowr::wflow_build(files = file.path(root, pages), project = root, make = FALSE,
  update = FALSE, republish = FALSE, view = FALSE, local = FALSE,
  clean_fig_files = FALSE, delete_cache = FALSE,
  log_dir = file.path(root, "reports/render-logs"))
