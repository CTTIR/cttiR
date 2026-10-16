# Rebuild the lossless installed archive from retained source-tree snapshots.
# Rscript tools/build_catalog_archive.R
build_catalog_archive <- function(root = ".") {
  root <- normalizePath(root, mustWork = TRUE)
  ext <- file.path(root, "inst", "extdata")
  paths <- c(file.path(ext, "api-catalog.json.gz"),
    sort(list.files(file.path(ext, "history"), "^[a-f0-9]{64}[.]json[.]gz$", full.names = TRUE)))
  stage <- tempfile("catalog-archive-"); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  archive <- file.path(stage, "catalogs.xz")
  con <- xzfile(archive, "wb", compression = 8)
  snapshots <- list(); offset <- 0
  tryCatch({
    for (path in paths) {
      input <- gzfile(path, "rb")
      raw <- tryCatch(readBin(input, "raw", n = 25000001L), finally = close(input))
      stopifnot(length(raw) > 0L, length(raw) <= 25000000L)
      text <- rawToChar(raw); Encoding(text) <- "UTF-8"
      id <- jsonlite::fromJSON(text, simplifyVector = FALSE)$content_id
      stopifnot(is.character(id), length(id) == 1L, grepl("^[a-f0-9]{64}$", id))
      snapshots[[length(snapshots) + 1L]] <- list(content_id = id, offset = offset,
        length = length(raw), sha256 = digest::digest(raw, algo = "sha256", serialize = FALSE),
        source = substring(path, nchar(root) + 2L), original_gzip_sha256 = digest::digest(file = path, algo = "sha256"))
      writeBin(raw, con); offset <- offset + length(raw)
    }
  }, finally = close(con))
  ids <- vapply(snapshots, function(x) x$content_id, "")
  stopifnot(!anyDuplicated(ids), offset <= 250000000)
  con <- xzfile(archive, "rb")
  tryCatch({
    for (record in snapshots) {
      raw <- readBin(con, "raw", n = record$length)
      stopifnot(length(raw) == record$length,
        identical(digest::digest(raw, algo = "sha256", serialize = FALSE), record$sha256))
    }
    stopifnot(!length(readBin(con, "raw", n = 1L)))
  }, finally = close(con))
  index <- list(schema_version = 1L, current_id = ids[[1L]], uncompressed_bytes = offset,
    archive_sha256 = digest::digest(file = archive, algo = "sha256"), snapshots = snapshots)
  jsonlite::write_json(index, file.path(stage, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  target <- file.path(ext, "catalog-archive")
  dir.create(target, showWarnings = FALSE)
  for (name in c("catalogs.xz", "manifest.json")) stopifnot(file.copy(file.path(stage, name), file.path(target, name), overwrite = TRUE))
  hashes_file <- file.path(ext, "file-hashes.json")
  hashes <- jsonlite::read_json(hashes_file)
  for (name in c("catalogs.xz", "manifest.json")) hashes[[paste0("catalog-archive/", name)]] <- digest::digest(file = file.path(target, name), algo = "sha256")
  jsonlite::write_json(hashes, hashes_file, auto_unbox = TRUE, pretty = TRUE)
  invisible(index)
}
if (sys.nframe() == 0L) build_catalog_archive()
