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

read_project <- function(path) {
  scalar_text(path, "path")
  assert_plain_path(path)
  if (!dir.exists(path)) abort_cttir("Project directory does not exist.", "cttir_path_conflict")
  root <- normalizePath(path, winslash = "/", mustWork = TRUE)
  for (f in c("cttir-project.yml", "cttir-lock.json", ".cttir/managed-files.json", ".cttir/state.json")) {
    file_hash(file.path(root, f))
  }
  spec <- validate_spec(file.path(root, "cttir-project.yml"))
  lock <- read_document(file.path(root, "cttir-lock.json"))
  manifest <- read_document(file.path(root, ".cttir/managed-files.json"))
  state <- read_document(file.path(root, ".cttir/state.json"))
  if (!identical(lock$schema_version, 1L) || !identical(manifest$schema_version, 1L) ||
      !identical(state$project_id, spec$project$id) ||
      !identical(lock$template_version, spec$provenance$template_version) ||
      !identical(lock$spec_sha256, content_hash(json_text(spec))) ||
      !identical(state$spec_sha256, lock$spec_sha256)) {
    abort_cttir("Project specification and control metadata do not agree.", "cttir_schema_error", "metadata_mismatch")
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
  list(path = root, spec = spec, lock = lock, manifest = manifest, state = state)
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
  if (!dir.create(lockdir, showWarnings = FALSE)) {
    abort_cttir("The project has an active or interrupted writer.", "cttir_transaction_conflict", "writer_lock")
  }
  on.exit(unlink(lockdir, recursive = TRUE), add = TRUE)
  write_bytes(
    paste0(json_text(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]]), TRUE), "\n"),
    file.path(lockdir, "owner.json")
  )
  if (length(pending_transactions(root))) {
    abort_cttir("An interrupted transaction needs review before another write.", "cttir_transaction_conflict", "pending_journal")
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
