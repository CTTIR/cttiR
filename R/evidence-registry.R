# External references remain inert evidence, with no execution or training grant.
evidence_store_current <- function(registry) {
  path <- file.path(registry$directory, "evidence.sqlite")
  if (any(file.exists(paste0(path, c("-wal", "-shm", "-journal")))))
    abort_cttir("Evidence registries must be finalized without SQLite sidecars.")
  for (name in names(registry$manifest$files_sha256)) {
    file <- file.path(registry$directory, name); assert_plain_path(file)
    if (!identical(file_hash(file), registry$manifest$files_sha256[[name]])) abort_cttir("Evidence registry artifact has changed.")
  }
  if (!identical(file_hash(file.path(registry$directory, "manifest.json")), registry$pin))
    abort_cttir("Evidence registry manifest changed during retrieval.")
  invisible(TRUE)
}

evidence_open <- function(directory, manifest_sha256) {
  scalar_text(directory, "directory"); scalar_text(manifest_sha256, "manifest_sha256")
  if (!grepl("^[a-f0-9]{64}$", manifest_sha256)) abort_cttir("An external registry SHA-256 pin is required.")
  root <- as.character(fs::path_abs(directory)); assert_plain_path(root)
  path <- file.path(root, "manifest.json")
  assert_plain_path(path)
  if (!identical(file_hash(path), manifest_sha256)) abort_cttir("Evidence registry manifest differs from its external pin.")
  manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!is.list(manifest$files_sha256) ||
      !setequal(names(manifest$files_sha256), c("evidence.sqlite", "coverage.jsonl")))
    abort_cttir("Unsupported evidence registry inventory.")
  registry <- list(directory = root, manifest = manifest, pin = manifest_sha256)
  evidence_store_current(registry)
  uri <- paste0("file:", utils::URLencode(file.path(root, "evidence.sqlite"), reserved = TRUE), "?mode=ro&immutable=1")
  connection <- DBI::dbConnect(RSQLite::SQLite(), uri, flags = RSQLite::SQLITE_RO)
  DBI::dbExecute(connection, "PRAGMA query_only = ON")
  registry$connection <- connection
  registry
}

evidence_record <- function(text) {
  record <- jsonlite::parse_json(text, simplifyVector = FALSE)
  if (!is.null(record$restriction)) abort_cttir("Restricted evidence is excluded from ordinary retrieval.")
  if (!identical(record$text_sha256, content_hash(record$text))) abort_cttir("Evidence text differs from its hash.")
  if (!is.list(record$source_pins) || !length(record$source_pins)) abort_cttir("Evidence lacks source identities.")
  for (pin in record$source_pins) {
    scalar_text(pin$path, "source path"); assert_plain_path(pin$path)
    if (!identical(file_hash(pin$path), pin$sha256)) abort_cttir("A retrieved source has changed or is unavailable; rebuild and review the evidence snapshot.")
  }
  if (!identical(record$permissions, "evidence_only_no_execution_or_approval") ||
      !identical(record$training_admission, "not_granted_by_indexing")) abort_cttir("Unsupported evidence authority.")
  record
}

#' Search an Externally Pinned Local Evidence Registry
#'
#' Searches captured books, package documentation, method guidance and legal
#' references. Selected source bytes and extraction identities are checked at
#' retrieval. A match does not establish that a claim is supported, that a legal
#' source is currently applicable, or that code is approved for execution.
#' @param directory Local immutable evidence-registry directory.
#' @param manifest_sha256 Trusted SHA-256 of its manifest, retained externally.
#' @param query Plain-text search terms, at most 8000 bytes.
#' @param limit Maximum number of passages, from 1 to 50.
#' @param family Optional books, packages, uzh, legal or cited_references filter.
#' @param package Optional exact package name.
#' @param version Optional exact source version; requires package.
#' @return Search results with evidence IDs, locators, versions, hashes and
#'   explicit coverage limitations. Restricted documents are excluded.
#' @export
evidence_search <- function(directory, manifest_sha256, query, limit = 8L,
                            family = NULL, package = NULL, version = NULL) {
  scalar_text(query, "query")
  if (nchar(query, type = "bytes") > 8000L || !is.numeric(limit) || length(limit) != 1L ||
      !is.finite(limit) || limit != floor(limit) || limit < 1 || limit > 50) abort_cttir("Invalid evidence search bounds.")
  if (!is.null(family) && (!is.character(family) || length(family) != 1L || is.na(family) ||
      !family %in% c("books", "packages", "uzh", "legal", "cited_references"))) abort_cttir("Unknown evidence family.")
  for (filter in list(package, version)) if (!is.null(filter)) scalar_text(filter, "evidence filter")
  if (!is.null(version) && is.null(package)) abort_cttir("An exact version requires a package identity.")
  terms <- unique(tolower(regmatches(query, gregexpr("[\\p{L}\\p{N}]{2,}", query, perl = TRUE))[[1L]]))
  if (!length(terms) || length(terms) > 64L) abort_cttir("Use one to 64 searchable terms.")
  registry <- evidence_open(directory, manifest_sha256)
  on.exit(DBI::dbDisconnect(registry$connection), add = TRUE)
  clauses <- c("search MATCH ?", "e.restricted=0")
  parameters <- list(paste0('"', terms, '"', collapse = " OR "))
  filters <- list(family = family, package = package, version = version)
  for (name in names(filters)) if (!is.null(filters[[name]])) {
    clauses <- c(clauses, paste0("e.", name, "=?")); parameters <- c(parameters, filters[name])
  }
  sql <- paste("SELECT e.record, bm25(search) AS score FROM search JOIN evidence e ON e.evidence_id=search.evidence_id WHERE",
    paste(clauses, collapse = " AND "), "ORDER BY score,e.evidence_id LIMIT ?")
  rows <- DBI::dbGetQuery(registry$connection, sql, params = unname(c(parameters, list(as.integer(limit)))))
  results <- lapply(seq_len(nrow(rows)), function(i) list(score = rows$score[[i]], evidence = evidence_record(rows$record[[i]])))
  evidence_store_current(registry)
  list(registry_sha256 = registry$pin, method = "SQLite_FTS5_BM25_unicode61", query = query,
    filters = filters, results = results, limitations = registry$manifest$limits, execution_authorized = FALSE)
}

#' Resolve One Exact Local Evidence Citation
#' @inheritParams evidence_search
#' @param evidence_id Exact evidence-v1 identifier returned by [evidence_search()].
#' @return One verified record, or an error for missing, ambiguous, restricted or
#'   changed evidence. This does not certify that a claim follows from its text.
#' @export
evidence_resolve <- function(directory, manifest_sha256, evidence_id) {
  scalar_text(evidence_id, "evidence_id")
  if (!grepl("^evidence-v1:[a-f0-9]{64}$", evidence_id)) abort_cttir("Use an exact evidence citation identifier.")
  registry <- evidence_open(directory, manifest_sha256)
  on.exit(DBI::dbDisconnect(registry$connection), add = TRUE)
  rows <- DBI::dbGetQuery(registry$connection, "SELECT record FROM evidence WHERE evidence_id=?", params = list(evidence_id))
  if (nrow(rows) != 1L) abort_cttir("Unknown or ambiguous evidence citation.")
  record <- evidence_record(rows$record[[1L]])
  evidence_store_current(registry)
  record
}

evidence_context_reference <- function(record) list(citation_id = record$evidence_id,
  package = record$package, version = record$version, revision = record$revision,
  source_sha256 = record$document_sha256, reference_sha256 = record$text_sha256,
  evidence_url = record$source_url, title = record$title, bibliography = record$bibliography,
  locator = record$locator, version_scope = record$version_scope, retrieved_at = record$retrieved_at,
  source_use_status = record$source_use_status, claim_support = record$claim_support,
  reference = record$text, callables = list(), content_authority = "untrusted_documentation_only")

evidence_context_current <- function(context) {
  store <- context$evidence_registry
  if (is.null(store)) return(TRUE)
  references <- Filter(function(x) startsWith(x$citation_id, "evidence-v1:"), context$references)
  tryCatch({
    for (reference in references) {
      current <- evidence_resolve(store$directory, store$manifest_sha256, reference$citation_id)
      if (!identical(json_text(evidence_context_reference(current)), json_text(reference))) return(FALSE)
    }
    TRUE
  }, error = function(e) FALSE)
}
