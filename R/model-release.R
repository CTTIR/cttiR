# Release manifests bind transport identity; qualification remains a separate gate.
read_model_release <- function(path, sha256) {
  scalar_text(path, "release manifest"); scalar_text(sha256, "manifest checksum")
  assert_plain_path(path)
  if (!grepl("^[a-f0-9]{64}$", sha256) || !file.exists(path) || dir.exists(path) ||
      file.info(path)$size > 1048576 || !identical(file_hash(path), sha256)) {
    abort_cttir("Release manifest does not match the trusted checksum.", "cttir_catalog_corrupt")
  }
  release <- validate_document(read_document(path), "model-release")
  if (!valid_model_tag(release$model$tag) ||
      !identical(content_hash(release$model$template), release$model$template_sha256) ||
      release$resources$required_disk_bytes < 2 * release$model$bytes) {
    abort_cttir("Release identity, template or storage budget is inconsistent.", "cttir_schema_error")
  }
  release
}

release_file_verified <- function(path, model) {
  assert_plain_path(path)
  if (!file.exists(path) || dir.exists(path) || file.info(path)$size != model$bytes ||
      !identical(file_hash(path), model$sha256)) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con))
  identical(readBin(con, "raw", n = 4L), charToRaw("GGUF"))
}

release_write_record <- function(value, path) {
  assert_plain_path(path)
  staged <- tempfile(".record-", tmpdir = dirname(path))
  on.exit(unlink(staged), add = TRUE)
  write_bytes(paste0(json_text(value, TRUE), "\n"), staged)
  assert_plain_path(path)
  if (!file.rename(staged, path)) abort_cttir("Could not activate the local release record.", "cttir_path_conflict")
}

# Downloads are resumable checkpoints. Failed parts remain available for review;
# only the complete trusted digest is ever renamed to the reusable model path.
release_acquire_model <- function(root, model, local_file = NULL, offline = FALSE) {
  cache <- file.path(root, "release-downloads")
  assert_plain_path(cache); dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(cache, paste0(model$sha256, ".gguf"))
  if (file.exists(target)) {
    if (!release_file_verified(target, model)) abort_cttir("Existing model cache failed integrity checks; preserve it for review.", "cttir_catalog_corrupt")
    return(target)
  }
  part <- paste0(target, ".part"); assert_plain_path(part)
  if (!is.null(local_file)) {
    if (!release_file_verified(local_file, model)) abort_cttir("Local model file failed its size, SHA-256 or GGUF check.", "cttir_catalog_corrupt")
    if (file.exists(part)) abort_cttir("A partial model transfer already exists; review it before local import.", "cttir_path_conflict")
    if (!file.copy(local_file, part, overwrite = FALSE)) abort_cttir("Could not stage the local model.", "cttir_path_conflict")
  } else {
    if (offline || is.null(model$url)) abort_cttir("The verified model is absent and no permitted download is available.", "cttir_runtime_unavailable")
    executable <- Sys.which("curl")
    if (!nzchar(executable)) abort_cttir("Resumable model acquisition requires the curl command-line tool.", "cttir_runtime_unavailable")
    if (file.exists(part) && (dir.exists(part) || file.info(part)$size >= model$bytes)) {
      abort_cttir("A completed or oversized unverified transfer requires review.", "cttir_path_conflict")
    }
    # Disable curl startup files, URL globbing, credentials, proxies and non-HTTPS redirects.
    args <- c("--disable", "--globoff", "--fail", "--silent", "--show-error",
      "--proto", "=https", "--proto-redir", "=https", "--location", "--max-redirs", "5",
      "--proxy", "", "--connect-timeout", "15", "--max-time", "3600",
      "--max-filesize", format(model$bytes, scientific = FALSE, trim = TRUE),
      "--continue-at", "-", "--output", part, "--url", model$url)
    result <- processx::run(executable, args, error_on_status = FALSE, timeout = 3610)
    if (result$status != 0L) abort_cttir("Model download failed; the partial transfer is retained for retry.", "cttir_runtime_unavailable", "download_incomplete")
  }
  if (!release_file_verified(part, model)) abort_cttir("Downloaded model failed its trusted integrity check; the transfer is retained.", "cttir_catalog_corrupt")
  assert_plain_path(target)
  if (file.exists(target) || !file.rename(part, target)) abort_cttir("Model cache activation conflicted with another writer.", "cttir_path_conflict")
  target
}

release_start_runtime <- function(root, endpoint, manifest, install_ollama, offline) {
  owner <- runtime_owner(root, endpoint)
  if (!is.null(owner)) return(list(owner = owner, process = NULL))
  reachable <- tryCatch({runtime_request(endpoint, "version", timeout = 2); TRUE}, error = function(e) FALSE)
  if (reachable) abort_cttir("The port belongs to an unmanaged daemon; choose an unused loopback port.", "cttir_runtime_unavailable")
  executable <- file.path(root, paste0("ollama-", manifest$runtime_version), "bin", "ollama")
  assert_plain_path(executable)
  if (!file.exists(executable)) {
    if (offline || !install_ollama) abort_cttir("The verified runtime is absent and acquisition is disabled.", "cttir_runtime_unavailable")
    executable <- acquire_runtime(root, manifest)
  }
  if (!identical(file_hash(executable), manifest$binary_sha256)) abort_cttir("Runtime binary checksum mismatch.", "cttir_runtime_unavailable")
  executable <- normalizePath(executable, mustWork = TRUE)
  for (path in c(file.path(root, "models"), file.path(root, "release-server.log"))) assert_plain_path(path)
  dir.create(file.path(root, "models"), showWarnings = FALSE)
  process <- processx::process$new(executable, "serve", env = c("current", OLLAMA_NO_CLOUD = "1",
    OLLAMA_HOST = endpoint, OLLAMA_MODELS = normalizePath(file.path(root, "models")),
    OLLAMA_MAX_LOADED_MODELS = "1", OLLAMA_NUM_PARALLEL = "1"),
    stdout = file.path(root, "release-server.log"), stderr = "2>&1", cleanup = FALSE)
  ready <- FALSE
  on.exit(if (!ready && process$is_alive()) process$kill(), add = TRUE)
  for (i in seq_len(40L)) {
    ready <- tryCatch({runtime_request(endpoint, "version", timeout = 1); TRUE}, error = function(e) FALSE)
    if (ready || !process$is_alive()) break
    Sys.sleep(0.25)
  }
  if (!ready) abort_cttir("Owned runtime did not become ready.", "cttir_runtime_unavailable")
  owner <- list(pid = process$get_pid(), host = Sys.info()[["nodename"]], executable = executable,
    binary_sha256 = manifest$binary_sha256, endpoint = endpoint, locality = "managed_cloud_disabled")
  assign(as.character(owner$pid), process, envir = .owned_runtimes)
  list(owner = owner, process = process)
}

# Only local upload/create operations are accepted. Bodies are streamed from disk.
release_import_model <- function(endpoint, model, path) {
  if (!planner_valid_endpoint(endpoint) || !release_file_verified(path, model)) abort_cttir("Unverified model import request.", "cttir_runtime_unavailable")
  perform <- function(route, body = NULL, file = NULL) {
    request <- httr2::request(paste0(endpoint, "/api/", route))
    request <- httr2::req_options(request, proxy = "", followlocation = FALSE, connecttimeout = 3, maxfilesize = 1048576)
    request <- httr2::req_timeout(request, 900)
    if (!is.null(file)) request <- httr2::req_body_file(request, file, type = "application/octet-stream") else
      request <- httr2::req_body_json(request, body, auto_unbox = TRUE)
    response <- httr2::req_perform(request)
    if (httr2::resp_has_body(response) && length(httr2::resp_body_raw(response)) > 1048576L) abort_cttir("Import response exceeded its size limit.", "cttir_runtime_unavailable")
    response
  }
  perform(paste0("blobs/sha256:", model$sha256), file = path)
  if (!release_file_verified(path, model)) abort_cttir("Model file changed during upload.", "cttir_catalog_corrupt")
  body <- list(model = model$tag,
    files = stats::setNames(list(paste0("sha256:", model$sha256)), model$file),
    parameters = model$parameters, stream = FALSE)
  # Native GGUF templates reported by show are not necessarily valid Go import
  # templates. NULL retains the embedded template; verify the served one later.
  if (!is.null(model$import_template)) body$template <- model$import_template
  response <- perform("create", body = body)
  if (!identical(httr2::resp_body_json(response)$status, "success")) abort_cttir("Local model import did not complete.", "cttir_runtime_unavailable")
  invisible(TRUE)
}

#' Preview or install an exact local model release
#'
#' Validates a locally supplied release manifest against a separately trusted
#' SHA-256 value. The default preview performs no HTTP requests or writes.
#' Installation requires both `dry_run = FALSE` and `consent = TRUE`. The model
#' can be imported offline from a verified GGUF, or downloaded from the immutable
#' Hugging Face revision in the manifest. Downloads and failed imports are retained
#' for diagnosis and retry. Existing model tags are never overwritten.
#'
#' Acquisition is supported on Linux x86_64. Other platforms return explicit
#' blockers. The function starts or reuses only the package's verified, owned,
#' cloud-disabled runtime, never changes login services and never modifies an
#' unmanaged daemon. Successful import establishes artifact identity and a bounded
#' structured-output probe; it does not grant planner or assistance qualification.
#' Previous release artifacts remain available for rollback by selecting their
#' original manifests. Research-project dependency preparation is separate.
#' @param manifest Path to a local release JSON manifest.
#' @param manifest_sha256 Trusted SHA-256 of that exact manifest, obtained through
#'   a separately reviewed release record rather than trusting a download alone.
#' @param model_file Optional local GGUF matching the manifest for offline import.
#' @param dry_run Return the setup preview without side effects (default `TRUE`).
#' @param consent Explicit noninteractive consent to the previewed acquisition,
#'   owned daemon startup and import. Required for a non-preview call.
#' @param offline Forbid model and runtime downloads.
#' @param install_ollama Permit acquisition of the pinned runtime when absent.
#' @return A `cttir_setup` containing storage/resource requirements, runtime/model
#'   identity, steps and blockers. No qualification follows from manifest labels.
#' @export
setup_release <- function(manifest, manifest_sha256, model_file = NULL, dry_run = TRUE,
                          consent = FALSE, offline = FALSE, install_ollama = TRUE) {
  for (key in c("dry_run", "consent", "offline", "install_ollama")) scalar_flag(get(key), key)
  release <- read_model_release(manifest, manifest_sha256)
  if (!is.null(model_file)) {scalar_text(model_file, "local model"); assert_plain_path(model_file)}
  root <- runtime_directory(); endpoint <- runtime_endpoint()
  runtime <- read_document(resource_file("runtime", "manifest.json"))
  resources <- runtime_resources(root)
  # Allow for the retained download, unpacked runtime and model blob staging.
  # This is an admission estimate, not a reservation or measured peak usage.
  required_disk <- max(release$resources$required_disk_bytes,
    3 * release$model$bytes + 4 * runtime$archive_bytes + 1073741824)
  blockers <- character()
  if (!identical(release$package_version, as.character(utils::packageVersion("cttiR"))) ||
      !identical(release$catalog_id, resolve_catalog()$content_id) ||
      !identical(release$policy_sha256, file_hash(resource_file("extdata", "code-policy-v1.json"))) ||
      !identical(release$prompt_version, assistant_prompt_version) ||
      !identical(release$runtime_version, runtime$runtime_version)) blockers <- c(blockers, "release_compatibility_mismatch")
  if (!identical(resources$platform, release$platform) || !identical(resources$arch, release$arch)) blockers <- c(blockers, "platform_unverified")
  for (field in c("memory", "disk")) {
    available <- resources[[paste0("available_", field, "_bytes")]]
    required <- if (field == "disk") required_disk else release$resources$minimum_memory_bytes
    if (!is.numeric(available) || length(available) != 1L || !is.finite(available) || available < required) blockers <- c(blockers, paste0(field, "_headroom_unavailable"))
  }
  result <- structure(list(state = "planned", steps = list(), runtime = list(version = runtime$runtime_version, endpoint = endpoint),
    model = list(name = release$model$tag, digest = release$model$digest, validation = "not_qualified_for_assistance"),
    release = list(id = release$release_id, manifest_sha256 = manifest_sha256),
    download_estimate = list(allowed = !offline, runtime_archive_bytes = runtime$archive_bytes,
      model_bytes = if (is.null(model_file)) release$model$bytes else 0,
      scope = "Full acquisition sizes; offline preview does not query existing model tags. Verified caches can reduce transfer."),
    storage = list(root = root, model_bytes = release$model$bytes, required_free_bytes = required_disk,
      estimate_scope = "Conservative allowance for retained downloads, unpacked runtime and model staging; not a disk reservation.",
      available_bytes = resources$available_disk_bytes, local_file = model_file),
    actions = c("release_verify_files", "release_start_runtime", "release_import_model", "release_verify_response"),
    blockers = blockers), class = "cttir_setup")
  if (dry_run) return(result)
  if (!consent) result$blockers <- c(result$blockers, "explicit_setup_consent_required")
  if (length(result$blockers)) {result$state <- "blocked"; return(result)}
  process <- NULL; attempt <- NULL
  tryCatch({
    assert_plain_path(root); dir.create(root, recursive = TRUE, showWarnings = FALSE)
    lock <- file.path(root, "setup-lock"); assert_plain_path(lock)
    if (!dir.create(lock, showWarnings = FALSE)) abort_cttir("Runtime setup is already locked; preserve the other writer.", "cttir_transaction_conflict")
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    write_bytes(paste0(json_text(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]])), "\n"), file.path(lock, "owner.json"))
    attempt <- file.path(root, "release-attempts", uuid::UUIDgenerate())
    assert_plain_path(attempt); dir.create(attempt, recursive = TRUE)
    for (name in c("runtime-state.json", "current-release.json")) {
      old <- file.path(root, name); assert_plain_path(old)
      if (file.exists(old)) stopifnot(file.copy(old, file.path(attempt, paste0("previous-", name))))
    }
    release_write_record(list(status = "started", manifest_sha256 = manifest_sha256, release_id = release$release_id), file.path(attempt, "receipt.json"))
    # Validate local input before starting a service, even if the tag already exists.
    if (!is.null(model_file) && !release_file_verified(model_file, release$model)) abort_cttir("Local model file failed integrity checks.", "cttir_catalog_corrupt")
    started <- release_start_runtime(root, endpoint, runtime, install_ollama, offline)
    process <- started$process; owner <- started$owner
    verify_owner <- function() {
      if (!runtime_state_verified(root, endpoint, owner)) abort_cttir("Owned runtime identity or model storage changed during setup.", "cttir_runtime_unavailable")
    }
    verify_owner()
    if (!identical(runtime_request(endpoint, "version")$version, release$runtime_version)) abort_cttir("Serving runtime version differs from the release.", "cttir_runtime_unavailable")
    existing <- local_model(endpoint, release$model$tag)
    if (!is.null(existing) && !identical(existing$digest, release$model$digest)) abort_cttir("The model tag already has different content; it will not be overwritten.", "cttir_path_conflict")
    if (is.null(existing)) {
      path <- release_acquire_model(root, release$model, model_file, offline)
      # Recheck the destination immediately before the mutating import.
      if (!is.null(local_model(endpoint, release$model$tag))) abort_cttir("The model tag appeared during setup; retry after review.", "cttir_path_conflict")
      verify_owner()
      release_import_model(endpoint, release$model, path)
    }
    actual <- local_model(endpoint, release$model$tag)
    if (is.null(actual) || !identical(actual$digest, release$model$digest)) abort_cttir("Imported model digest differs from the release.", "cttir_catalog_corrupt")
    shown <- runtime_request(endpoint, "show", list(model = release$model$tag))
    if (!is.character(shown$template) || !identical(content_hash(shown$template), release$model$template_sha256)) abort_cttir("Served prompt template differs from the release.", "cttir_catalog_corrupt")
    probe <- runtime_probe(endpoint, release$model$tag)
    fresh <- local_model(endpoint, release$model$tag)
    if (is.null(fresh) || !identical(fresh$digest, actual$digest)) abort_cttir("Model identity changed during setup.", "cttir_catalog_corrupt")
    verify_owner()
    retained <- file.path(root, "releases", manifest_sha256)
    assert_plain_path(retained); dir.create(retained, recursive = TRUE, showWarnings = FALSE)
    retained_manifest <- file.path(retained, "manifest.json"); assert_plain_path(retained_manifest)
    if (file.exists(retained_manifest)) {
      if (!identical(file_hash(retained_manifest), manifest_sha256)) abort_cttir("Retained manifest differs from its identity.", "cttir_catalog_corrupt")
    } else if (!file.copy(manifest, retained_manifest)) abort_cttir("Could not preserve the release manifest.", "cttir_path_conflict")
    if (!identical(file_hash(retained_manifest), manifest_sha256)) abort_cttir("Manifest changed during setup.", "cttir_catalog_corrupt")
    owner$model <- release$model$tag; owner$model_digest <- actual$digest; owner$probe <- probe
    owner$release_manifest_sha256 <- manifest_sha256
    result$state <- "model_ready"
    result$runtime$pid <- owner$pid; result$runtime$locality <- owner$locality
    result$steps <- list(list(id = "release_identity", state = "verified"), list(id = "structured_inference", state = "verified", evidence = probe))
    result$blockers <- "model_independent_qualification_pending"
    result$receipt <- file.path(attempt, "receipt.json")
    pending <- unclass(result); pending$state <- "verified_awaiting_activation"
    release_write_record(pending, result$receipt)
    # One authoritative atomic pointer binds runtime, model and release manifest.
    release_write_record(owner, file.path(root, "runtime-state.json"))
    process <- NULL
    finalized <- tryCatch({release_write_record(unclass(result), result$receipt); TRUE}, error = function(e) FALSE)
    if (!finalized) result$receipt_state <- "activation_recorded_final_receipt_pending"
    result
  }, error = function(e) {
    if (!is.null(process) && process$is_alive()) process$kill()
    result$state <- "blocked"
    result$blockers <- if (inherits(e, "cttir_error")) conditionMessage(e) else "Release setup failed; retained files and receipts require review."
    if (!is.null(attempt) && dir.exists(attempt)) release_write_record(unclass(result), file.path(attempt, "receipt.json"))
    result
  })
}
