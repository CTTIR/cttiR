# Regenerate inst/templates/standard-0.3.0/manifest.json from the reviewed files.
# Run from the package root after reviewing any template change.
root <- file.path("inst", "templates", "standard-0.3.0")
files <- sort(list.files(root, recursive = TRUE, all.files = TRUE), method = "radix")
files <- setdiff(files, "manifest.json")
hash <- function(path) {
  text <- paste0(paste(readLines(file.path(root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n")
  digest::digest(text, algo = "sha256", serialize = FALSE)
}
previous <- jsonlite::fromJSON(file.path("inst", "templates", "reflowr-0.2.0", "manifest.json"), simplifyVector = FALSE)
manifest <- list(
  template_version = "0.3.0", bundle = "standard-0.3.0",
  mode = "adapted_templates_with_reviewed_stage_library",
  source_repository = previous$source_repository, source_revision = previous$source_revision,
  license = previous$license, copyright = previous$copyright, renderer = "rmarkdown::render_site",
  initializer_invoked = FALSE, workflowr_invoked = FALSE,
  changes = c(previous$changes,
    "Add reviewed standard stages: delimited import, structural checks, dplyr role selection, DescrTab2 or base descriptive tables, accessible ggplot2/patchwork figures, lm/glm/nlme/survival adapters, diagnostics and broom effects.",
    "Add a synthetic demonstration with independent reference computations and a guarded study-data runner."),
  source_sha256 = previous$source_sha256,
  files = stats::setNames(lapply(files, hash), files),
  conditional = list(
    "code/R/cttir_interop.R" = list(modality = c("single_cell", "bulk_rna", "spatial", "multiomics", "cytometry", "proteomics")),
    "_targets.R" = list(pipeline = "targets")
  ),
  limitations = c(
    "Study-data runs require explicit mappings, reviewed settings, analysis approval, a local binding and approved pinned package revisions.",
    "Synthetic demonstration results are adapter checks, not scientific results.",
    "Prediction, causal, count and specialist analyses have no reviewed adapter in this bundle."),
  hash_normalization = "UTF-8 text with LF line endings and final newline"
)
manifest$conditional <- manifest$conditional[names(manifest$conditional) %in% files]
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), file.path(root, "manifest.json"), useBytes = TRUE)
cat("Wrote", length(files), "file hashes\n")
