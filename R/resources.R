#' Browse the bundled research package candidates
#'
#' Metadata reflects the supplied 2026-09-26 research snapshot. Candidate status
#' does not establish API verification or tested workflow support. This query is
#' read-only and offline; it does not load or install cataloged packages.
#' @param query Optional nonempty text; literal case-insensitive substring search.
#' @param domain Optional exact category filter, such as `cytometry`.
#' @param repository Optional exact repository filter, such as `Bioconductor`.
#' @param path Optional exact project root whose resource snapshot must match.
#' @param limit Positive integer row limit, at most 10000.
#' @return A data frame with source, observed version, verification and maturity
#'   fields. Filters combine with AND. Exact package names rank first.
#' @export
#' @examples
#' resources("cytometry", limit = 3L)
resources <- function(query = NULL, domain = NULL, repository = NULL,
  path = NULL, limit = 50L) {
  for (key in c("query", "domain", "repository", "path")) {
    value <- get(key)
    if (!is.null(value)) scalar_text(value, key)
  }
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
      !is.finite(limit) || limit < 1 || limit > 10000 || limit != as.integer(limit)) {
    abort_cttir("limit must be an integer between 1 and 10000.")
  }
  manifest <- jsonlite::fromJSON(resource_file("extdata", "resource-manifest.json"))
  if (!is.null(path)) {
    assert_plain_path(path)
    lockpath <- file.path(path, "cttir-lock.json")
    assert_plain_path(lockpath)
    pin <- read_document(lockpath)$resource_snapshot
    if (is.null(pin) || !identical(pin, manifest$content_id)) {
      abort_cttir("The pinned resource snapshot is unavailable.", "cttir_source_unavailable", "missing_pin")
    }
  }
  dbpath <- resource_file("extdata", "package-resources.sqlite")
  expected <- jsonlite::fromJSON(resource_file("extdata", "file-hashes.json"))[["package-resources.sqlite"]]
  if (!identical(digest::digest(file = dbpath, algo = "sha256"), expected)) {
    abort_cttir("The bundled resource database failed its integrity check.", "cttir_catalog_corrupt", "hash_mismatch")
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), dbpath, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  where <- "1 = 1"
  params <- list()
  if (!is.null(domain)) {
    where <- paste(where, "AND category = ?")
    params <- append(params, list(domain))
  }
  if (!is.null(repository)) {
    where <- paste(where, "AND repository = ?")
    params <- append(params, list(repository))
  }
  if (!is.null(query)) {
    where <- paste(where, "AND instr(lower(name || ' ' || purpose || ' ' || category), lower(?)) > 0")
    params <- append(params, list(query))
  }
  sql <- paste0("SELECT *, 'not_rechecked' AS freshness FROM resource_search WHERE ", where)
  if (!is.null(query)) {
    sql <- paste(sql, "ORDER BY CASE WHEN lower(name) = lower(?) THEN 0 ELSE 1 END, name, repository LIMIT ?")
    params <- append(params, list(query))
  } else {
    sql <- paste(sql, "ORDER BY name, repository LIMIT ?")
  }
  params <- append(params, list(as.integer(limit)))
  DBI::dbGetQuery(con, sql, params = params)
}
