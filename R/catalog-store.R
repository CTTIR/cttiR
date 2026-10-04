catalog_pointer <- function() {
  file <- file.path(catalog_store(), "active.json")
  assert_plain_path(file)
  if (!file.exists(file)) {
    return(NULL)
  }
  value <- read_document(file)
  if (!is.null(value$resource_id) || !is.null(value$manifest_id)) value <- validate_manifest(value)
  value
}

current_catalog_manifest <- function() {
  pointer <- catalog_pointer()
  base_resources <- read_document(resource_file("extdata", "resource-manifest.json"))$content_id
  if (is.null(pointer)) {
    return(snapshot_manifest(read_catalog(resource_file("extdata", "api-catalog.json.gz"))$content_id, base_resources))
  }
  if (!is.null(pointer$manifest_id)) return(validate_manifest(pointer))
  snapshot_manifest(catalog_snapshot(pointer$content_id)$content_id, base_resources)
}

resource_snapshot <- function(path = NULL) {
  base_id <- read_document(resource_file("extdata", "resource-manifest.json"))$content_id
  id <- base_id
  if (!is.null(path)) {
    assert_plain_path(path)
    file <- file.path(path, "cttir-lock.json")
    assert_plain_path(file)
    id <- read_document(file)$resource_snapshot
  } else {
    pointer <- catalog_pointer()
    if (!is.null(pointer$resource_id)) id <- pointer$resource_id
  }
  resource_snapshot_by_id(id)
}

resource_history <- function() {
  file <- resource_file("extdata", "resource-history", "index.json")
  expected <- read_document(resource_file("extdata", "file-hashes.json"))[["resource-history/index.json"]]
  if (!identical(file_hash(file), expected)) {
    abort_cttir("The retained resource index failed its integrity check.", "cttir_catalog_corrupt")
  }
  read_document(file)$snapshots
}

valid_resource_id <- function(id) {
  is.character(id) && length(id) == 1L && !is.na(id) &&
    (identical(id, read_document(resource_file("extdata", "resource-manifest.json"))$content_id) ||
        grepl("^[a-f0-9]{64}$", id) || id %in% names(resource_history()))
}

resource_snapshot_by_id <- function(id) {
  if (!valid_resource_id(id)) {
    abort_cttir("The pinned resource snapshot is unavailable.", "cttir_source_unavailable")
  }
  base_id <- read_document(resource_file("extdata", "resource-manifest.json"))$content_id
  history <- resource_history()[[id]]
  if (identical(id, base_id)) {
    file <- resource_file("extdata", "package-resources.sqlite")
    expected <- read_document(resource_file("extdata", "file-hashes.json"))[["package-resources.sqlite"]]
  } else if (!is.null(history)) {
    expected <- history$sha256
    if (!is.character(expected) || length(expected) != 1L || is.na(expected) || !grepl("^[a-f0-9]{64}$", expected)) {
      abort_cttir("Invalid retained resource identity.", "cttir_catalog_corrupt")
    }
    file <- resource_file("extdata", "resource-history", paste0(expected, ".sqlite"))
  } else {
    file <- file.path(catalog_store(), "resource-snapshots", id, "package-resources.sqlite")
    expected <- id
  }
  assert_plain_path(file)
  if (!file.exists(file)) abort_cttir("The pinned resource snapshot is unavailable.", "cttir_source_unavailable")
  if (!identical(digest::digest(file = file, algo = "sha256"), expected)) {
    abort_cttir("The resource database failed its integrity check.", "cttir_catalog_corrupt")
  }
  list(id = id, file = file, sha256 = expected)
}

# The recorded Bioconductor release policy is part of the composite state, so it
# is activated and rolled back atomically with both catalogs. A manifest without
# `bioc_release` records no policy (the release is derived from the running R).
snapshot_manifest <- function(api_id, resource_id, bioc_release = NULL) {
  x <- list(schema_version = 1L, content_id = api_id, resource_id = resource_id)
  if (!is.null(bioc_release)) x$bioc_release <- bioc_release
  x$manifest_id <- content_hash(json_text(x))
  x
}

validate_manifest <- function(x) {
  id <- x$manifest_id
  x$manifest_id <- NULL
  release <- x$bioc_release
  if (!identical(x$schema_version, 1L) || !identical(content_hash(json_text(x)), id) ||
      !is.character(x$content_id) || length(x$content_id) != 1L || !grepl("^[a-f0-9]{64}$", x$content_id) ||
      !valid_resource_id(x$resource_id) ||
      (!is.null(release) && (!is.character(release) || length(release) != 1L || !release %in% bioc_release_table()$release))) {
    abort_cttir("Invalid composite catalog manifest.", "cttir_catalog_corrupt")
  }
  x$manifest_id <- id
  x
}

# bioc-policy.json mirrors the active manifest for readers of the store policy
# file; the manifest stays authoritative. Run after every activation.
sync_bioc_policy <- function(manifest, root = catalog_store()) {
  release <- manifest$bioc_release
  if (is.null(release)) {
    file <- bioc_policy_file(root)
    if (file.exists(file) && !file.remove(file)) {
      abort_cttir("Could not clear the Bioconductor release policy of the previous snapshot.", "cttir_transaction_conflict")
    }
  } else if (!identical(tryCatch(read_bioc_policy(root)$release, cttir_error = function(e) NULL), release)) {
    write_bioc_policy(release, root)
  }
  invisible(manifest)
}

# Retained composite snapshot IDs, newest first.
retained_manifest_ids <- function(root = catalog_store()) {
  directory <- file.path(root, "manifests")
  assert_plain_path(directory)
  files <- list.files(directory, pattern = "^[a-f0-9]{64}[.]json$", full.names = TRUE)
  files <- files[order(-as.numeric(file.info(files)$mtime), basename(files))]
  sub("[.]json$", "", basename(files))
}

verified_manifest_ids <- function(root = catalog_store()) {
  Filter(function(id) {
    file <- file.path(root, "manifests", paste0(id, ".json"))
    !is.null(tryCatch(validate_snapshot_content(read_document(file)), error = function(e) NULL))
  }, retained_manifest_ids(root))
}

# The verified snapshot named by the active pointer, or the reason it does not
# verify. update() refuses a damaged snapshot; rollback may replace it.
active_snapshot <- function() {
  failed <- function(e) list(catalog = NULL, resources = NULL, problem = conditionMessage(e))
  tryCatch(list(catalog = resolve_catalog(), resources = resource_snapshot(), problem = NULL),
    cttir_catalog_corrupt = failed, cttir_source_unavailable = failed)
}

catalog_snapshot <- function(id) {
  base <- read_catalog(resource_file("extdata", "api-catalog.json.gz"))
  if (identical(id, base$content_id)) {
    return(base)
  }
  if (!is.character(id) || length(id) != 1L || !grepl("^[a-f0-9]{64}$", id)) {
    abort_cttir("Invalid catalog identifier.", "cttir_catalog_corrupt")
  }
  historical <- system.file("extdata", "history", paste0(id, ".json.gz"), package = "cttiR")
  if (nzchar(historical)) {
    result <- read_catalog(historical)
    if (!identical(result$content_id, id)) abort_cttir("Historical catalog identity mismatch.", "cttir_catalog_corrupt")
    return(result)
  }
  result <- read_catalog(file.path(catalog_store(), "snapshots", id, "api-catalog.json.gz"))
  if (!identical(result$content_id, id)) abort_cttir("Catalog snapshot identity mismatch.", "cttir_catalog_corrupt")
  result
}

retain_api_snapshot <- function(catalog, root) {
  target <- file.path(root, "snapshots", catalog$content_id)
  assert_plain_path(target)
  if (dir.exists(target)) {
    existing <- read_catalog(file.path(target, "api-catalog.json.gz"))
    if (!identical(existing$content_id, catalog$content_id)) abort_cttir("Snapshot collision.", "cttir_catalog_corrupt")
    return(invisible(target))
  }
  dir.create(file.path(root, "snapshots"), showWarnings = FALSE)
  stage <- tempfile("staged-", tmpdir = file.path(root, "snapshots"))
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  con <- gzfile(file.path(stage, "api-catalog.json.gz"), "wb", compression = 9)
  tryCatch(writeBin(charToRaw(catalog_json(catalog)), con), finally = close(con))
  id <- read_catalog(file.path(stage, "api-catalog.json.gz"))$content_id
  if (!identical(id, catalog$content_id) || !file.rename(stage, target)) {
    abort_cttir("Could not retain the complete API snapshot.", "cttir_transaction_conflict")
  }
  invisible(target)
}

activate_catalog <- function(manifest, previous) {
  root <- catalog_store()
  manifest <- validate_manifest(manifest)
  # Pointer identity only: callers verify content, and rollback may replace a
  # pointer whose snapshot no longer verifies.
  if (!identical(current_catalog_manifest(), previous)) {
    abort_cttir("The active catalog changed during staging.", "cttir_transaction_conflict")
  }
  directory <- file.path(root, "manifests")
  dir.create(directory, showWarnings = FALSE)
  for (x in list(previous, manifest)) {
    file <- file.path(directory, paste0(x$manifest_id, ".json"))
    assert_plain_path(file)
    if (file.exists(file)) {
      if (!identical(validate_manifest(read_document(file)), x)) abort_cttir("Stored manifest differs from its identity.", "cttir_catalog_corrupt")
    } else {
      write_bytes(paste0(json_text(x), "\n"), file)
    }
  }
  active <- file.path(root, "active.json")
  assert_plain_path(active)
  staged <- tempfile("active-", tmpdir = root, fileext = ".json")
  on.exit(unlink(staged), add = TRUE)
  write_bytes(paste0(json_text(manifest), "\n"), staged)
  write_bytes(paste0(json_text(list(schema_version = 1L, previous = previous, candidate = manifest)), "\n"),
    file.path(root, "write-lock", "activation.json"))
  # Same-directory rename replaces the pointer only after both immutable outputs
  # are verified. Failed rename preserves the prior pointer; orphan snapshots are safe.
  tryCatch(fs::file_move(staged, active), error = function(e) {
    abort_cttir("Catalog activation failed; the prior pointer remains authoritative.", "cttir_transaction_conflict")
  })
  invisible(manifest)
}

catalog_lock <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  guard <- file.path(root, "recovery-lock")
  assert_plain_path(guard)
  if (file.exists(guard)) abort_cttir("Catalog recovery may be active; retry after it finishes.", "cttir_transaction_conflict")
  lock <- file.path(root, "write-lock")
  assert_plain_path(lock)
  if (dir.exists(lock)) recover_catalog_lock(root)
  if (!dir.create(lock, showWarnings = FALSE)) {
    abort_cttir("Catalog has an active or interrupted writer; inspect the lock owner before recovery.", "cttir_transaction_conflict")
  }
  write_bytes(paste0(json_text(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]])), "\n"), file.path(lock, "owner.json"))
  lock
}
