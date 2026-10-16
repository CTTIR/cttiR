# Lossless bundled snapshots share a compressed stream. Local update snapshots
# retain their existing gzip representation and logical content identifiers.
catalog_archive_manifest <- function() {
  file <- resource_file("extdata", "catalog-archive", "manifest.json")
  assert_plain_path(file)
  if (file.info(file)$size > 1000000) abort_cttir("Catalog archive index exceeds the read limit.", "cttir_catalog_corrupt")
  index <- read_document(file)
  hex <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[a-f0-9]{64}$", x)
  number <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0 && x == floor(x)
  if (!identical(index$schema_version, 1L) || !hex(index$current_id) || !hex(index$archive_sha256) ||
      !number(index$uncompressed_bytes) || index$uncompressed_bytes > 250000000 ||
      !is.list(index$snapshots) || !length(index$snapshots) || length(index$snapshots) > 100L) {
    abort_cttir("Catalog archive index is invalid.", "cttir_catalog_corrupt")
  }
  offset <- 0
  ids <- character()
  for (record in index$snapshots) {
    if (!is.list(record) || !hex(record$content_id) || !hex(record$sha256) ||
        !number(record$offset) || !number(record$length) || record$length < 1 ||
        record$length > 25000000 || record$offset != offset) {
      abort_cttir("Catalog archive range is invalid.", "cttir_catalog_corrupt")
    }
    offset <- offset + record$length
    ids <- c(ids, record$content_id)
  }
  if (anyDuplicated(ids) || !index$current_id %in% ids || offset != index$uncompressed_bytes) {
    abort_cttir("Catalog archive identities or total size disagree.", "cttir_catalog_corrupt")
  }
  index
}

bundled_catalog_ids <- function() {
  vapply(catalog_archive_manifest()$snapshots, function(x) x$content_id, "")
}

read_bundled_catalog <- function(id = NULL) {
  index <- catalog_archive_manifest()
  if (is.null(id)) id <- index$current_id
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^[a-f0-9]{64}$", id)) {
    abort_cttir("Invalid catalog identifier.", "cttir_catalog_corrupt")
  }
  matches <- Filter(function(x) identical(x$content_id, id), index$snapshots)
  if (length(matches) != 1L) abort_cttir("Bundled catalog snapshot is unavailable.", "cttir_source_unavailable")
  record <- matches[[1L]]
  file <- resource_file("extdata", "catalog-archive", "catalogs.xz")
  assert_plain_path(file)
  if (dir.exists(file) || file.info(file)$size > 25000000 ||
      !identical(digest::digest(file = file, algo = "sha256"), index$archive_sha256)) {
    abort_cttir("Catalog archive checksum or size is invalid.", "cttir_catalog_corrupt")
  }
  key <- paste(index$archive_sha256, id, record$offset, record$length, record$sha256, sep = ":")
  if (exists(key, envir = .catalog_cache, inherits = FALSE)) return(get(key, envir = .catalog_cache, inherits = FALSE))
  con <- xzfile(file, "rb")
  on.exit(close(con), add = TRUE)
  remaining <- record$offset
  while (remaining > 0) {
    size <- min(remaining, 1048576)
    discarded <- readBin(con, "raw", n = size)
    if (length(discarded) != size) abort_cttir("Catalog archive is truncated.", "cttir_catalog_corrupt")
    remaining <- remaining - size
  }
  bytes <- readBin(con, "raw", n = record$length)
  if (length(bytes) != record$length ||
      !identical(digest::digest(bytes, algo = "sha256", serialize = FALSE), record$sha256)) {
    abort_cttir("Catalog archive snapshot checksum failed.", "cttir_catalog_corrupt")
  }
  result <- decode_catalog(bytes, key)
  if (!identical(result$content_id, id)) abort_cttir("Catalog archive identity mismatch.", "cttir_catalog_corrupt")
  result
}
