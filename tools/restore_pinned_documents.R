# Recover missing documentation from an immutable snapshot at identical source pins.
# This creates a candidate and receipt; activation is a separate reviewed action.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, !dir.exists(args[[2L]]))
pkgload::load_all(quiet = TRUE)
ns <- asNamespace("cttiR")
options(cttiR.catalog_dir = tempfile("unused-catalog-"))
current <- ns$resolve_catalog()
previous <- ns$catalog_snapshot(args[[1L]])
out <- args[[2L]]
dir.create(out, recursive = TRUE)
restored <- list()
packages <- lapply(current$packages, function(package) {
  old <- Filter(function(p) identical(p$name, package$name), previous$packages)
  if (length(old) != 1L) return(package)
  old <- old[[1L]]
  if (!identical(old$revision, package$revision) ||
      !identical(old$source_hash, package$source_hash) ||
      !identical(old$source_files, package$source_files) ||
      is.null(old$documentation_corpus) || is.null(package$documentation_corpus)) return(package)
  incoming <- package$documentation_corpus
  existing_paths <- vapply(incoming$documents, function(d) d$path, "")
  missing <- Filter(function(d) !d$path %in% existing_paths, old$documentation_corpus$documents)
  if (!length(missing)) return(package)
  stopifnot(all(vapply(missing, function(d)
    identical(d$source_sha256, package$source_files[[d$path]]), logical(1))))
  package$documentation_corpus <- old$documentation_corpus
  package$documentation_corpus <- ns$merge_pinned_document_corpus(package, incoming)
  restored[[package$name]] <<- lapply(missing, function(d)
    list(path = d$path, source_sha256 = d$source_sha256, storage = d$storage))
  package
})
stopifnot(length(restored) > 0L)
candidate <- file.path(out, "candidate.json.gz")
id <- ns$write_catalog(packages, candidate, current$inventory, current$tombstones)
readback <- ns$read_catalog(candidate)
stopifnot(identical(ns$approved_callables(readback), ns$approved_callables(current)))
for (i in seq_along(current$packages)) {
  a <- current$packages[[i]]; b <- readback$packages[[i]]
  stopifnot(identical(a$name, b$name), identical(a$revision, b$revision),
    identical(a$exports, b$exports), identical(a$source_hash, b$source_hash),
    identical(a$source_files, b$source_files))
}
jsonlite::write_json(list(status = "verified_candidate_not_activated",
  prior_catalog = current$content_id, recovery_catalog = previous$content_id,
  candidate_catalog = id, candidate_sha256 = ns$file_hash(candidate),
  restored = restored, api_and_approval_identities_unchanged = TRUE,
  model_requests_require_reconciliation = TRUE), file.path(out, "receipt.json"),
  auto_unbox = TRUE, pretty = TRUE)
cat("Verified recovery candidate", id, "\n")
