# Read-only preview. Applying a reviewed draft is a separate transaction.
assistant_suggestion_preview <- function(advice, path) {
  if (!identical(advice$state, "proposal") || is.null(advice$context)) {
    abort_cttir("A validated suggestion is required for preview.")
  }
  context <- advice$context
  checked <- assistant_validate_response(json_text(advice$proposal), context,
    language = advice$requested_language, kind = "suggestion")
  if (!isTRUE(checked$ok)) abort_cttir("Suggestion no longer passes validation.", "cttir_schema_error")
  controls <- c("cttir-project.yml", "cttir-lock.json", ".cttir/managed-files.json", ".cttir/state.json")
  control_hashes <- stats::setNames(vapply(controls, function(file) file_hash(file.path(path, file)), ""), controls)
  project <- read_project(path)
  if (!identical(project$lock$catalog_id, context$catalog_id)) {
    abort_cttir("Suggestion differs from the project catalog.", "cttir_transaction_conflict", "catalog_mismatch")
  }
  records <- stats::setNames(project$manifest$files,
    vapply(project$manifest$files, function(x) tolower(x$path), ""))
  changes <- lapply(checked$proposal$changes, function(change) {
    relative_file(change$path)
    destination <- file.path(project$path, change$path)
    current <- file_hash(destination)
    expected <- if (is.null(change$original_sha256)) NA_character_ else change$original_sha256
    record <- records[[tolower(change$path)]]
    managed <- !is.null(record) && identical(record$ownership, "managed")
    status <- if (managed) "managed_file" else if (!identical(current, expected)) "stale_content" else "ready_for_review"
    before <- NULL
    if (!is.na(current)) {
      size <- file.info(destination)$size
      if (is.na(size) || size > 1048576L) abort_cttir("Suggestion target exceeds the preview size limit.")
      bytes <- readBin(destination, "raw", size)
      if (any(bytes == as.raw(0))) abort_cttir("Suggestion target is not a text file.")
      before <- rawToChar(bytes)
      if (!identical(digest::digest(bytes, algo = "sha256", serialize = FALSE), current) ||
          !identical(file_hash(destination), current)) abort_cttir("Suggestion target changed while being read.",
        "cttir_transaction_conflict", "stale_plan")
    }
    list(path = change$path, role = if (grepl("/manuscript/", change$path, fixed = TRUE)) "manuscript" else "analysis",
      action = if (is.na(current)) "create" else "update", status = status,
      old_hash = current, expected_hash = expected, new_hash = content_hash(change$content),
      before = before, after = change$content, reason = change$reason)
  })
  fresh_controls <- vapply(controls, function(file) file_hash(file.path(project$path, file)), "")
  if (!identical(unname(fresh_controls), unname(control_hashes))) {
    abort_cttir("Project metadata changed during preview.", "cttir_transaction_conflict", "stale_plan")
  }
  result <- list(schema_version = 1L, project_id = project$state$project_id, control_hashes = as.list(control_hashes),
    catalog_id = context$catalog_id, proposal_sha256 = content_hash(json_text(checked$proposal)),
    provenance = advice$exchange$provenance, qualification = advice$exchange$qualification,
    validation_level = checked$validation_level, changes = changes,
    conflicts = any(vapply(changes, function(x) !identical(x$status, "ready_for_review"), logical(1))),
    execution_authorized = FALSE, applied = FALSE)
  result$preview_sha256 <- content_hash(json_text(result))
  result
}

# An explicit apply requires the exact reviewed preview fingerprint. Content is
# written through the shared journal/rollback machinery and never evaluated.
assistant_suggestion_apply <- function(advice, path, reviewed_preview_sha256) {
  scalar_text(reviewed_preview_sha256, "reviewed_preview_sha256")
  if (!grepl("^[a-f0-9]{64}$", reviewed_preview_sha256)) abort_cttir("Invalid reviewed preview hash.")
  preview <- assistant_suggestion_preview(advice, path)
  if (!identical(preview$preview_sha256, reviewed_preview_sha256)) {
    abort_cttir("Suggestion changed after review; request a fresh preview.", "cttir_transaction_conflict", "stale_plan")
  }
  if (isTRUE(preview$conflicts)) abort_cttir("Suggestion conflicts must be resolved before applying.",
    "cttir_transaction_conflict", "suggestion_conflict")
  rows <- lapply(preview$changes, function(change) data.frame(path = change$path, action = change$action,
    old_hash = change$old_hash, new_hash = change$new_hash, stringsAsFactors = FALSE))
  controls <- names(preview$control_hashes)
  guards <- data.frame(path = controls, action = "skip", old_hash = unlist(preview$control_hashes),
    new_hash = unlist(preview$control_hashes), stringsAsFactors = FALSE, row.names = NULL)
  plan <- rbind(do.call(rbind, rows), guards)
  files <- stats::setNames(lapply(preview$changes, function(change) change$after),
    vapply(preview$changes, function(change) change$path, ""))
  suggestion_paths <- names(files)
  receipt_path <- paste0(".cttir/suggestions/", reviewed_preview_sha256, ".json")
  receipt <- list(schema_version = 1L, operation = "apply_reviewed_suggestion",
    project_id = preview$project_id, catalog_id = preview$catalog_id,
    proposal_sha256 = preview$proposal_sha256, reviewed_preview_sha256 = reviewed_preview_sha256,
    reported_model_provenance = preview$provenance, qualification = preview$qualification,
    validation_level = preview$validation_level, executed = FALSE,
    changes = lapply(preview$changes, function(change) change[c("path", "role", "old_hash", "new_hash")]),
    references = lapply(advice$context$references, function(ref) ref[c("citation_id", "package", "revision",
      "source_path", "source_sha256", "reference_sha256")]))
  receipt_text <- paste0(json_text(receipt, TRUE), "\n")
  files[[receipt_path]] <- receipt_text
  plan <- rbind(plan, data.frame(path = receipt_path, action = "create", old_hash = NA_character_,
    new_hash = content_hash(receipt_text), stringsAsFactors = FALSE))
  journal <- transact_files(path, files, plan)
  list(applied = TRUE, executed = FALSE, execution_authorized = FALSE,
    reviewed_preview_sha256 = reviewed_preview_sha256, journal = journal,
    paths = suggestion_paths, receipt = receipt_path, validation_level = preview$validation_level,
    qualification = preview$qualification, provenance = preview$provenance)
}
