relative_file <- function(path) {
  scalar_text(path, "managed path")
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (grepl("^[/\\\\]|^[A-Za-z]:|\\\\", path) ||
      grepl(":", path, fixed = TRUE) || any(grepl("[. ]$", parts)) ||
      any(grepl("^(con|prn|aux|nul|com[0-9]|lpt[0-9])($|\\.)", tolower(parts))) ||
      any(parts %in% c("", ".", ".."))) {
    abort_cttir("Invalid relative managed path.", "cttir_schema_error", "unsafe_managed_path")
  }
  invisible(path)
}

file_hash <- function(path) {
  assert_plain_path(path)
  if (!file.exists(path)) {
    return(NA_character_)
  }
  if (dir.exists(path)) abort_cttir("A file path was replaced by a directory.", "cttir_path_conflict")
  links <- fs::file_info(path)$hard_links
  if (!is.na(links) && links > 1L) {
    abort_cttir("Managed files cannot be hard linked.", "cttir_path_conflict", "hard_link")
  }
  digest::digest(file = path, algo = "sha256")
}

abort_not_project_root <- function(root) {
  if (file.exists(file.path(root, ".cttir", "state.json"))) {
    abort_cttir("The project file cttir-project.yml is missing.", "cttir_path_conflict", "incomplete_project",
      field = "cttir-project.yml", remediation = "Restore it from version control or a backup.")
  }
  enclosing <- dirname(root)
  while (!identical(enclosing, dirname(enclosing)) && !file.exists(file.path(enclosing, "cttir-project.yml"))) {
    enclosing <- dirname(enclosing)
  }
  if (file.exists(file.path(enclosing, "cttir-project.yml"))) {
    abort_cttir(sprintf("%s is inside the project %s, not its root.", root, enclosing), "cttir_path_conflict",
      "not_project_root", field = "path", remediation = sprintf("Pass the project root: %s.", r_literal(enclosing)))
  }
  abort_cttir(sprintf("%s contains no cttir-project.yml, so it is not the root of a cttiR project.", root),
    "cttir_path_conflict", "not_project_root", field = "path",
    remediation = "Pass the directory that project() created (the one holding cttir-project.yml).")
}

read_control_file <- function(root, file) {
  tryCatch(read_document(file.path(root, file)), cttir_error = function(e) {
    abort_cttir(sprintf("The project control file %s cannot be read.", file), "cttir_schema_error", "metadata_mismatch",
      field = file, remediation = "Restore it from version control or a backup; cttiR does not regenerate control metadata.")
  })
}

# Reads cttir-project.yml with messages that name the offending field and the
# next step, since this file is meant to be edited by hand.
read_project_spec <- function(root) {
  next_step <- sprintf("then preview with cttiR::sync(%s).", r_literal(root))
  tryCatch(validate_spec(file.path(root, "cttir-project.yml")), cttir_error = function(e) {
    if (identical(e$code, "parse_error")) {
      abort_cttir("cttir-project.yml is not valid YAML.", "cttir_schema_error", "parse_error", field = "cttir-project.yml",
        remediation = paste("Fix the YAML syntax (indentation, quoting) or restore the file from version control,", next_step))
    }
    if (identical(e$code, "schema_validation") && is.data.frame(e$field)) {
      detail <- schema_error_detail(e$field)
      abort_cttir(paste0("cttir-project.yml is not a valid project specification: ", detail$text, "."),
        "cttir_schema_error", "schema_validation", field = detail$pointer,
        remediation = paste0("Correct ", detail$pointer, " in cttir-project.yml or restore the file from version control, ",
          next_step))
    }
    if (inherits(e, "cttir_input_error") && !is.null(e$field)) {
      abort_cttir(paste("cttir-project.yml:", conditionMessage(e)), "cttir_schema_error", e$code, field = e$field,
        remediation = paste("Correct the field in cttir-project.yml or restore the file from version control,", next_step))
    }
    stop(e)
  })
}

# The verified copy of the accepted spec, used to review hand edits field by
# field; NULL when absent (older projects) or when it no longer matches the lock.
accepted_spec <- function(root, lock) {
  file <- file.path(root, ".cttir", "accepted-spec.yml")
  if (is.na(file_hash(file))) return(NULL)
  spec <- tryCatch(validate_spec(file), error = function(e) NULL)
  if (is.null(spec) || !identical(content_hash(json_text(spec)), lock$spec_sha256)) return(NULL)
  spec
}

# A spec whose hash differs from the lock while the control files agree with
# each other was edited by hand. Builder-owned identity must still match.
check_spec_edit <- function(root, spec, lock, state) {
  restore <- function(field, value) {
    value <- encodeString(format(value), quote = "'")
    message <- sprintf("%s in cttir-project.yml was changed by hand; it is assigned by cttiR and must stay %s.", field, value)
    abort_cttir(message, "cttir_input_error", "invalid_spec_edit", field = field,
      remediation = sprintf("Restore %s to %s, then preview the remaining edit with cttiR::sync(%s).",
        field, value, r_literal(root)))
  }
  if (!identical(spec$project$id, state$project_id)) restore("/project/id", state$project_id)
  if (!identical(spec$provenance$template_version, lock$template_version)) {
    restore("/provenance/template_version", lock$template_version)
  }
  if (!identical(spec$provenance$catalog_id, lock$catalog_id)) restore("/provenance/catalog_id", lock$catalog_id)
  invisible(TRUE)
}

read_project <- function(path, edited = FALSE) {
  scalar_text(path, "path")
  assert_plain_path(path)
  if (!dir.exists(path)) abort_cttir("Project directory does not exist.", "cttir_path_conflict")
  root <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(root, "cttir-project.yml"))) abort_not_project_root(root)
  for (f in c("cttir-project.yml", "cttir-lock.json", ".cttir/managed-files.json", ".cttir/state.json")) {
    if (is.na(file_hash(file.path(root, f)))) {
      abort_cttir(sprintf("The project control file %s is missing.", f), "cttir_path_conflict", "incomplete_project",
        field = f, remediation = "Restore it from version control or a backup; cttiR does not regenerate control metadata.")
    }
  }
  spec <- read_project_spec(root)
  lock <- read_control_file(root, "cttir-lock.json")
  manifest <- read_control_file(root, ".cttir/managed-files.json")
  state <- read_control_file(root, ".cttir/state.json")
  if (!identical(lock$schema_version, 1L) || !identical(manifest$schema_version, 1L) ||
      !identical(state$spec_sha256, lock$spec_sha256) || !is.character(lock$spec_sha256)) {
    abort_cttir("Project control metadata (cttir-lock.json, .cttir/state.json, .cttir/managed-files.json) do not agree.",
      "cttir_schema_error", "metadata_mismatch",
      remediation = "Restore the control files from version control or a backup; cttiR does not regenerate them.")
  }
  edited_spec <- NULL
  if (!identical(lock$spec_sha256, content_hash(json_text(spec)))) {
    check_spec_edit(root, spec, lock, state)
    accepted <- accepted_spec(root, lock)
    preview <- sprintf("cttiR::sync(%s)", r_literal(root))
    if (!edited) {
      message <- paste0("cttir-project.yml was edited after it was last accepted. Preview the edit with ", preview,
        " and accept it with dry_run = FALSE.")
      abort_cttir(message, "cttir_schema_error", "spec_edited", field = "cttir-project.yml",
        remediation = paste0("Run ", preview, " to review the edit, then ", sub(")$", ", dry_run = FALSE)", preview),
          " to accept it; or restore cttir-project.yml from version control."))
    }
    if (is.null(accepted)) {
      message <- paste("cttir-project.yml was edited after it was last accepted, and this project has no verified",
        "copy of the accepted specification (.cttir/accepted-spec.yml) to review the edit against.")
      abort_cttir(message, "cttir_schema_error", "spec_edit_unverifiable", field = "cttir-project.yml",
        remediation = paste("Restore cttir-project.yml from version control (or undo the edit) and make the change with",
          "cttiR::sync(path, options = list(...), dry_run = FALSE), which validates it."))
    }
    edited_spec <- spec
    spec <- accepted
  }
  if (!identical(state$project_id, spec$project$id) ||
      !identical(lock$template_version, spec$provenance$template_version)) {
    abort_cttir("Project specification and control metadata do not agree.", "cttir_schema_error", "metadata_mismatch",
      remediation = "Restore cttir-project.yml and the control files from version control or a backup.")
  }
  paths <- vapply(manifest$files, function(x) {
    relative_file(x$path)
    if (!x$ownership %in% c("user", "managed") ||
        !is.character(x$baseline_sha256) || length(x$baseline_sha256) != 1L ||
        !grepl("^[0-9a-f]{64}$", x$baseline_sha256)) {
      abort_cttir("Invalid file ownership record.", "cttir_schema_error")
    }
    x$path
  }, character(1))
  if (anyDuplicated(tolower(paths))) abort_cttir("Duplicate managed paths.", "cttir_schema_error")
  list(path = root, spec = spec, lock = lock, manifest = manifest, state = state, edited_spec = edited_spec)
}

write_bytes <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw(enc2utf8(text)), path)
}

replace_file <- function(from, to) {
  if (!file.copy(from, to, overwrite = TRUE, copy.mode = FALSE)) {
    abort_cttir("Could not replace a transaction file.", "cttir_transaction_conflict", "write_failed")
  }
}

pending_transactions <- function(root) {
  directory <- file.path(root, ".cttir/transactions")
  assert_plain_path(directory)
  if (!dir.exists(directory)) {
    return(character())
  }
  dirs <- list.dirs(directory, recursive = FALSE, full.names = TRUE)
  pending <- character()
  for (dir in dirs) {
    assert_plain_path(dir)
    journal <- file.path(dir, "journal.json")
    assert_plain_path(journal)
    if (!file.exists(journal) || !read_document(journal)$status %in% c("committed", "rolled_back")) {
      pending <- c(pending, dir)
    }
  }
  pending
}

transact_files <- function(root, files, plan) {
  lockdir <- file.path(root, ".cttir/write-lock")
  assert_plain_path(lockdir)
  if (!dir.create(lockdir, showWarnings = FALSE)) abort_writer_lock(root)
  on.exit(unlink(lockdir, recursive = TRUE), add = TRUE)
  write_lock_owner(lockdir)
  if (length(pending_transactions(root))) {
    abort_cttir("An interrupted transaction needs review before another write.", "cttir_transaction_conflict", "pending_journal",
      remediation = sprintf("Review it with cttiR::audit(%s, scope = \"project\") and roll it back with repair = TRUE.",
        r_literal(root)))
  }
  # Recheck all preview preimages while owning the writer lock.
  for (i in seq_len(nrow(plan))) {
    current <- file_hash(file.path(root, plan$path[[i]]))
    if (!identical(current, plan$old_hash[[i]])) {
      abort_cttir("A project file changed after planning.", "cttir_transaction_conflict", "stale_plan")
    }
  }
  actions <- plan[plan$action %in% c("create", "update"), , drop = FALSE]
  if (!nrow(actions)) {
    return(NULL)
  }
  journal_dir <- file.path(root, ".cttir/transactions", uuid::UUIDgenerate())
  dir.create(journal_dir, recursive = TRUE, showWarnings = FALSE)
  journal_path <- file.path(journal_dir, "journal.json")
  journal <- list(schema_version = 1L, status = "staging", actions = actions)
  write_journal <- function() write_bytes(paste0(json_text(journal, TRUE), "\n"), journal_path)
  write_journal()
  applied <- integer()
  committed <- FALSE
  on.exit(
    {
      if (!committed) {
        intact <- TRUE
        for (i in rev(applied)) {
          dest <- file.path(root, actions$path[[i]])
          now <- tryCatch(file_hash(dest), error = function(e) "unsafe")
          if (!identical(now, actions$new_hash[[i]]) && !identical(now, actions$old_hash[[i]])) {
            intact <- FALSE
            next
          }
          # A file whose replacement never happened already holds its preimage.
          if (identical(now, actions$old_hash[[i]])) next
          if (is.na(actions$old_hash[[i]])) {
            unlink(dest)
          } else if (!file.copy(file.path(journal_dir, "backup", as.character(i)), dest, overwrite = TRUE)) {
            intact <- FALSE
          }
        }
        journal$status <- if (intact) "rolled_back" else "recovery_required"
        write_journal()
      }
    },
    add = TRUE
  )
  for (i in seq_len(nrow(actions))) {
    dest <- file.path(root, actions$path[[i]])
    stage <- file.path(journal_dir, "stage", as.character(i))
    write_bytes(files[[actions$path[[i]]]], stage)
    if (!is.na(actions$old_hash[[i]])) {
      backup <- file.path(journal_dir, "backup", as.character(i))
      dir.create(dirname(backup), showWarnings = FALSE)
      if (!file.copy(dest, backup)) abort_cttir("Could not back up project file.", "cttir_transaction_conflict")
    }
    if (!identical(file_hash(stage), actions$new_hash[[i]])) {
      abort_cttir("Staged content hash mismatch.", "cttir_transaction_conflict")
    }
  }
  journal$status <- "applying"
  write_journal()
  for (i in seq_len(nrow(actions))) {
    dest <- file.path(root, actions$path[[i]])
    if (!identical(file_hash(dest), actions$old_hash[[i]])) {
      abort_cttir("A project file changed during the transaction.", "cttir_transaction_conflict")
    }
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    applied <- c(applied, i)
    replace_file(file.path(journal_dir, "stage", as.character(i)), dest)
    if (!identical(file_hash(dest), actions$new_hash[[i]])) {
      abort_cttir("Written content hash mismatch; recovery is required.", "cttir_transaction_conflict")
    }
  }
  journal$status <- "committed"
  write_journal()
  committed <- TRUE
  journal_dir
}
