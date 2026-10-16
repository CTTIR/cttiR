# Build a reviewable candidate from an already verified pinned source snapshot.
# This script never installs it as the bundled catalog or changes approvals.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
pkgload::load_all(quiet = TRUE)
ns <- asNamespace("cttiR")
source <- normalizePath(args[[1]], mustWork = TRUE)
out <- args[[2]]
if (dir.exists(out)) stop("Candidate evidence directory already exists")
dir.create(out, recursive = TRUE)
manifest <- ns$read_document(file.path(source, "manifest.json"))
catalog <- ns$resolve_catalog()
stopifnot(identical(manifest$catalog_id, catalog$content_id))
packages <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, ""))
reports <- list()
for (record in manifest$packages) {
  package <- packages[[record$package]]
  stopifnot(identical(package$revision, record$revision), identical(package$source_hash, record$catalog_source_hash))
  if (!identical(record$declared_license, "MIT + file LICENSE")) {
    reports[[record$package]] <- list(status = "separate_license_review_pending", license = record$declared_license)
    next
  }
  notices <- list()
  for (file in record$files) {
    path <- file.path(source, record$package, file$path)
    ns$assert_plain_path(path)
    stopifnot(identical(ns$file_hash(path), file$sha256))
    if (file$path %in% c("LICENSE", "LICENSE.md")) {
      notices[[length(notices) + 1L]] <- list(path = file$path, source_sha256 = file$sha256,
        revision = record$revision, content = ns$read_source_text(path))
    }
  }
  stopifnot(length(notices) >= 1L)
  rights <- paste("MIT documentation from the pinned public source revision; literal source text retained with",
    "the package documentation_notices. No source expressions are evaluated. Publication of model weights is separate.")
  corpus <- ns$document_inventory(file.path(source, record$package), rights)
  # API/source identities stay unchanged: retain only documents already covered
  # by the catalog's source-file hashes. Additional pinned notices are separate.
  corpus$documents <- Filter(function(doc) identical(doc$source_sha256, package$source_files[[doc$path]]), corpus$documents)
  corpus$coverage$discovered <- length(corpus$documents)
  corpus$coverage$stored <- sum(vapply(corpus$documents, function(d) identical(d$storage, "source_text"), logical(1)))
  corpus$coverage$metadata_only <- corpus$coverage$discovered - corpus$coverage$stored
  corpus$coverage$source_manifest_complete <- FALSE
  corpus$coverage$vignette_status <- "snapshot_scope_reference_and_metadata_only"
  ns$validate_document_corpus(corpus)
  package$documentation_corpus <- ns$merge_pinned_document_corpus(package, corpus)
  package$documentation_notices <- notices
  packages[[record$package]] <- package
  reports[[record$package]] <- list(status = "hash_matched_reference_text_added", references = length(corpus$documents),
    source_hash_unchanged = identical(package$source_hash, record$catalog_source_hash), notices = length(notices))
}
candidate <- file.path(out, "candidate.json.gz")
id <- ns$write_catalog(unname(packages), candidate, catalog$inventory, catalog$tombstones)
readback <- ns$read_catalog(candidate)
stopifnot(identical(ns$approved_callables(readback), ns$approved_callables(catalog)))
for (i in seq_along(catalog$packages)) {
  old <- catalog$packages[[i]]; new <- Filter(function(p) identical(p$name, old$name), readback$packages)[[1]]
  stopifnot(identical(old$exports, new$exports), identical(old$source_files, new$source_files),
    identical(old$source_hash, new$source_hash))
}
jsonlite::write_json(list(status = "candidate_verified_not_installed", previous = catalog$content_id, current = id,
  candidate_sha256 = ns$file_hash(candidate), source_manifest_sha256 = ns$file_hash(file.path(source, "manifest.json")),
  api_identities_unchanged = TRUE, approvals_unchanged = TRUE, packages = reports),
  file.path(out, "receipt.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Verified candidate", id, "without installing it\n")
