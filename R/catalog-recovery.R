validate_snapshot_content <- function(manifest) {
  manifest <- validate_manifest(manifest)
  catalog_snapshot(manifest$content_id)
  base <- read_document(resource_file("extdata", "resource-manifest.json"))$content_id
  file <- if (identical(manifest$resource_id, base)) {
    resource_file("extdata", "package-resources.sqlite")
  } else {
    file.path(catalog_store(), "resource-snapshots", manifest$resource_id, "package-resources.sqlite")
  }
  expected <- if (identical(manifest$resource_id, base)) {
    read_document(resource_file("extdata", "file-hashes.json"))[["package-resources.sqlite"]]
  } else {
    manifest$resource_id
  }
  if (!identical(file_hash(file), expected)) {
    abort_cttir("Recovery requires a complete verified resource snapshot.", "cttir_catalog_corrupt")
  }
  invisible(manifest)
}

recover_catalog_lock <- function(root) {
  lock <- file.path(root, "write-lock")
  owner_file <- file.path(lock, "owner.json")
  owner_hash <- file_hash(owner_file)
  if (is.na(owner_hash)) abort_cttir("Catalog writer ownership is unknown; recovery refused.", "cttir_transaction_conflict")
  owner <- read_document(owner_file)
  if (!identical(owner$host, Sys.info()[["nodename"]]) || !is.numeric(owner$pid) ||
      length(owner$pid) != 1L || is.na(owner$pid) || owner$pid <= 0 || owner$pid > .Machine$integer.max || owner$pid != as.integer(owner$pid)) {
    abort_cttir("Catalog writer ownership cannot be verified.", "cttir_transaction_conflict")
  }
  if (tryCatch(owner$pid %in% ps::ps_pids(), error = function(e) TRUE)) {
    abort_cttir("The catalog writer is still running; recovery refused.", "cttir_transaction_conflict")
  }
  guard <- file.path(root, "recovery-lock")
  assert_plain_path(guard)
  if (!dir.create(guard, showWarnings = FALSE)) abort_cttir("Another catalog recovery may be active.", "cttir_transaction_conflict")
  on.exit(unlink(guard, recursive = TRUE), add = TRUE)
  current <- current_catalog_manifest()
  validate_snapshot_content(current)
  journal_file <- file.path(lock, "activation.json")
  assert_plain_path(journal_file)
  if (file.exists(journal_file)) {
    journal <- read_document(journal_file)
    if (!identical(journal$schema_version, 1L)) abort_cttir("Unsupported catalog activation journal.", "cttir_catalog_corrupt")
    previous <- validate_manifest(journal$previous)
    candidate <- validate_manifest(journal$candidate)
    if (!identical(current, previous) && !identical(current, candidate)) {
      abort_cttir("The catalog pointer does not match the interrupted transaction.", "cttir_transaction_conflict")
    }
  }
  if (!identical(owner_hash, file_hash(owner_file))) {
    abort_cttir("Catalog ownership changed during recovery.", "cttir_transaction_conflict")
  }
  retained <- file.path(root, "recovered-locks")
  assert_plain_path(retained)
  dir.create(retained, showWarnings = FALSE)
  destination <- tempfile("writer-", tmpdir = retained)
  if (!file.rename(lock, destination)) abort_cttir("Could not retain interrupted writer evidence.", "cttir_transaction_conflict")
  invisible(destination)
}
