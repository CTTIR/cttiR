runtime_storage_files <- function(root) {
  rows <- list()
  add <- function(path, role, retained) {
    assert_plain_path(path)
    if (dir.exists(path)) abort_cttir("Unexpected directory in runtime file storage.", "cttir_path_conflict")
    info <- file.info(path)
    rows[[length(rows) + 1L]] <<- data.frame(path = path, role = role,
      bytes = as.double(info$size), retained = retained, stringsAsFactors = FALSE)
  }
  protected <- character()
  releases <- file.path(root, "releases"); assert_plain_path(releases)
  for (directory in list.files(releases, full.names = TRUE, all.files = TRUE, no.. = TRUE)) {
    assert_plain_path(directory)
    identity <- basename(directory)
    if (!grepl("^[a-f0-9]{64}$", identity) || !dir.exists(directory)) abort_cttir("Unrecognized retained release entry.", "cttir_catalog_corrupt")
    manifest <- file.path(directory, "manifest.json")
    release <- read_model_release(manifest, identity)
    protected <- c(protected, release$model$sha256)
    add(manifest, "release_manifest", TRUE)
  }
  for (area in c("release-downloads", "models/blobs")) {
    directory <- file.path(root, area); assert_plain_path(directory)
    for (path in list.files(directory, full.names = TRUE, all.files = TRUE, no.. = TRUE)) {
      name <- basename(path)
      download <- area == "release-downloads"
      known <- download && grepl("^[a-f0-9]{64}\\.gguf(\\.part)?$", name)
      pinned <- known && substr(name, 1L, 64L) %in% protected
      add(path, if (!download) "model_blob" else if (known) "model_download" else "unrecognized",
        !download || !known || pinned)
    }
  }
  for (name in c("ollama-linux-amd64.tar.zst", "ollama-linux-amd64.tar.zst.part")) {
    path <- file.path(root, name); assert_plain_path(path)
    if (file.exists(path)) add(path, "runtime_archive", TRUE)
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(path = character(), role = character(), bytes = double(), retained = logical())
}

#' Inspect managed model storage without contacting a daemon
#'
#' Lists release downloads, runtime archives, model blobs and retained release manifests. Runtime
#' libraries, logs and model-tag metadata are outside this size inventory. All
#' retained release manifests protect their model downloads, including older
#' versions kept for rollback. Model blobs are always retained. This call performs
#' no HTTP requests, starts no process and does not create storage.
#' @return A list with storage root, file inventory, inventoried bytes, current
#'   setup lock state and locally verified owned process identity when available.
#' @export
runtime_storage <- function() {
  root <- runtime_directory()
  files <- runtime_storage_files(root)
  owner <- tryCatch(runtime_owner(root, runtime_endpoint()), error = function(e) NULL)
  list(root = root, files = files, inventoried_bytes = sum(files$bytes),
    setup_locked = file.exists(file.path(root, "setup-lock")),
    process = if (is.null(owner)) NULL else owner[c("pid", "host", "endpoint")],
    scope = "Model downloads, runtime archives, blobs and release manifests only; no readiness or qualification claim.")
}

#' Preview or remove reviewed unreferenced model downloads
#'
#' Only files directly under the owned `release-downloads` directory can be
#' removed. Retained release pins, model blobs, runtimes and other files are
#' protected. Preview first and pass its named `expected` hashes to a separate
#' consenting call. A setup lock prevents concurrent package acquisition; all
#' selected hashes are rechecked before deletion. Failed or partial cleanup leaves
#' a durable receipt. No process is stopped and no model tag is removed.
#' @param files Exact download basenames to inspect or remove.
#' @param expected Named SHA-256 vector returned by the preview; required for
#'   removal and checked again under the setup lock.
#' @param dry_run Preview without writes (default `TRUE`).
#' @param consent Explicit consent to irreversible removal (default `FALSE`).
#' @return A list with state, selected paths/bytes, expected hashes and, after a
#'   removal attempt, a receipt path. Omitted files are never selected implicitly.
#' @export
runtime_cleanup <- function(files, expected = NULL, dry_run = TRUE, consent = FALSE) {
  scalar_flag(dry_run, "dry_run"); scalar_flag(consent, "consent")
  if (!is.character(files) || !length(files) || anyNA(files) || anyDuplicated(files) ||
      any(!grepl("^[a-f0-9]{64}\\.gguf(\\.part)?$", files))) abort_cttir("Select exact model download basenames.")
  root <- runtime_directory()
  plan <- function() {
    inventory <- runtime_storage_files(root)
    paths <- file.path(root, "release-downloads", files)
    index <- match(paths, inventory$path)
    if (anyNA(index) || any(inventory$retained[index])) abort_cttir("Selected files are absent or protected by retained releases.", "cttir_path_conflict")
    hashes <- stats::setNames(vapply(paths, file_hash, character(1)), files)
    list(state = "planned", paths = paths, bytes = sum(inventory$bytes[index]), expected = hashes)
  }
  result <- plan()
  if (dry_run) return(result)
  if (!consent) abort_cttir("Download removal requires explicit consent.")
  if (!identical(expected, result$expected)) abort_cttir("Download selection changed or lacks reviewed hashes.", "cttir_path_conflict")
  lock <- file.path(root, "setup-lock"); assert_plain_path(lock)
  if (!dir.create(lock, showWarnings = FALSE)) abort_cttir("Runtime setup is locked; preserve the active writer.", "cttir_transaction_conflict")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  fresh <- plan()
  if (!identical(fresh$expected, expected)) abort_cttir("Downloads changed after preview.", "cttir_path_conflict")
  directory <- file.path(root, "cleanup-receipts"); assert_plain_path(directory)
  dir.create(directory, showWarnings = FALSE)
  result$receipt <- file.path(directory, paste0(uuid::UUIDgenerate(), ".json"))
  result$state <- "started"; result$removed <- character()
  release_write_record(result, result$receipt)
  for (i in seq_along(files)) {
    path <- fresh$paths[[i]]
    if (!identical(file_hash(path), expected[[i]]) || unlink(path) != 0L || file.exists(path)) {
      result$state <- "partial_failure"
      release_write_record(result, result$receipt)
      return(result)
    }
    result$removed <- c(result$removed, files[[i]])
    release_write_record(result, result$receipt)
  }
  result$state <- "removed"
  release_write_record(result, result$receipt)
  result
}

#' Preview or stop the verified owned local daemon
#'
#' A separate consenting call must supply the identity from the preview. Ownership,
#' binary, model directory and process creation time are rechecked under the setup
#' lock. Sends a graceful termination signal only to that process. No lookup by
#' port/name, forced kill, model deletion or service-manager change is performed.
#' All files and release pins remain available for a subsequent explicit setup.
#' @param dry_run Preview without signals or writes (default `TRUE`).
#' @param consent Explicit consent to stop the previewed daemon (default `FALSE`).
#' @param expected Exact identity checksum returned by the preview.
#' @return A list with state, process identity and, after a stop attempt, a durable
#'   receipt. `stopping` means the process remained alive after five seconds;
#'   it is not a claim that all resources or child processes were released.
#' @export
runtime_stop <- function(dry_run = TRUE, consent = FALSE, expected = NULL) {
  scalar_flag(dry_run, "dry_run"); scalar_flag(consent, "consent")
  root <- runtime_directory(); endpoint <- runtime_endpoint()
  inspect <- function() {
    owner <- runtime_owner(root, endpoint)
    if (is.null(owner)) abort_cttir("No verified owned daemon is available to stop.", "cttir_runtime_unavailable")
    handle <- ps::ps_handle(owner$pid)
    identity <- list(owner = owner, created = as.numeric(ps::ps_create_time(handle)))
    list(owner = owner, handle = handle, expected = content_hash(json_text(identity)))
  }
  observed <- inspect()
  result <- list(state = "planned", process = observed$owner[c("pid", "host", "endpoint")], expected = observed$expected)
  if (dry_run) return(result)
  if (!consent) abort_cttir("Stopping the owned daemon requires explicit consent.")
  if (!identical(expected, observed$expected)) abort_cttir("Daemon identity changed or was not previewed.", "cttir_path_conflict")
  lock <- file.path(root, "setup-lock"); assert_plain_path(lock)
  if (!dir.create(lock, showWarnings = FALSE)) abort_cttir("Runtime setup is locked; preserve the active writer.", "cttir_transaction_conflict")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  fresh <- inspect()
  if (!identical(fresh$expected, expected)) abort_cttir("Daemon identity changed after preview.", "cttir_path_conflict")
  directory <- file.path(root, "lifecycle-receipts"); assert_plain_path(directory)
  dir.create(directory, showWarnings = FALSE)
  result$receipt <- file.path(directory, paste0(uuid::UUIDgenerate(), ".json"))
  result$state <- "stop_requested"
  release_write_record(result, result$receipt)
  ps::ps_terminate(fresh$handle)
  for (i in seq_len(50L)) {
    if (!ps::ps_is_running(fresh$handle)) break
    Sys.sleep(0.1)
  }
  result$state <- if (ps::ps_is_running(fresh$handle)) "stopping" else "stopped"
  release_write_record(result, result$receipt)
  result
}
