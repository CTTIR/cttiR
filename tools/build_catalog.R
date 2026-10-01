# Build only from materialized, verified public revisions in an explicit inventory.
# Rscript tools/build_catalog.R INVENTORY.json OUTPUT.json.gz
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
pkgload::load_all(quiet = TRUE)
inventory <- jsonlite::fromJSON(args[[1]], simplifyVector = FALSE)
packages <- list()
report <- list()
for (source in inventory) {
  if (!identical(source$status, "public_revision_materialized")) {
    report <- append(report, list(list(repository = source$repository, status = source$status)))
    next
  }
  entry <- cttiR:::extract_source(source$path, source$url, source$revision, "CTTIR")
  if (!is.null(source$subdir)) {
    entry$source_subdir <- source$subdir
    for (i in seq_along(entry$exports)) {
      entry$exports[[i]]$source_path <- paste0(source$subdir, "/", entry$exports[[i]]$source_path)
    }
  }
  packages <- append(packages, list(entry))
  report <- append(report, list(list(repository = source$repository, revision = source$revision, status = "static_indexed")))
}
id <- cttiR:::write_catalog(packages, args[[2]], report)
cat("Catalog:", id, "\nPackages:", length(packages), "\n")
