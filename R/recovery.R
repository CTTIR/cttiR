# Read-only validation of interrupted project transactions. Returns the pending
# journals with verified rows, or aborts when automatic recovery is unsafe.
transaction_recovery_plan <- function(root) {
  assert_plain_path(root)
  journals <- pending_transactions(root)
  lockdir <- file.path(root, ".cttir/write-lock")
  if (!length(journals)) {
    return(list(journals = list(), lockdir = lockdir))
  }
  owner_file <- file.path(lockdir, "owner.json")
  assert_plain_path(owner_file)
  if (!file.exists(owner_file)) {
    abort_cttir("Interrupted writer ownership is unknown; automatic recovery is refused.", "cttir_transaction_conflict")
  }
  owner <- read_document(owner_file)
  if (!identical(owner$host, Sys.info()[["nodename"]]) || !is.numeric(owner$pid) ||
      length(owner$pid) != 1L || is.na(owner$pid) || owner$pid <= 0 || owner$pid != as.integer(owner$pid)) {
    abort_cttir("Interrupted writer ownership cannot be verified.", "cttir_transaction_conflict")
  }
  alive <- tryCatch(owner$pid %in% ps::ps_pids(), error = function(e) TRUE)
  if (alive) abort_cttir("The recorded writer is still running; recovery is refused.", "cttir_transaction_conflict")
  recovery_lock <- file.path(root, ".cttir/recovery-lock")
  assert_plain_path(recovery_lock)
  if (dir.exists(recovery_lock)) abort_cttir("Another recovery may be active.", "cttir_transaction_conflict")
  prepared <- list()
  for (dir in journals) {
    record <- read_document(file.path(dir, "journal.json"))
    if (!identical(record$schema_version, 1L) || !record$status %in% c("staging", "applying", "recovery_required")) {
      abort_cttir("Unsupported transaction journal.", "cttir_transaction_conflict")
    }
    rows <- record$actions
    for (i in seq_along(rows)) {
      row <- rows[[i]]
      relative_file(row$path)
      if (grepl("^(data/|administration/private/|\\.cttir/local)", row$path)) {
        abort_cttir("Automatic recovery cannot alter private data or bindings.", "cttir_transaction_conflict")
      }
      for (value in list(row$new_hash, row$old_hash)) {
        if (!is.null(value) && (!is.character(value) || length(value) != 1L || !grepl("^[a-f0-9]{64}$", value))) {
          abort_cttir("Invalid journal content hash.", "cttir_transaction_conflict")
        }
      }
      if (is.null(row$new_hash)) abort_cttir("Missing journal postimage hash.", "cttir_transaction_conflict")
      old <- if (is.null(row$old_hash)) NA_character_ else row$old_hash
      now <- file_hash(file.path(root, row$path))
      if (!identical(now, old) && !identical(now, row$new_hash)) {
        abort_cttir("A file changed after interruption; manual recovery is required.", "cttir_transaction_conflict")
      }
      if (!is.na(old) && !identical(file_hash(file.path(dir, "backup", as.character(i))), old)) {
        abort_cttir("A recovery backup is missing or corrupt.", "cttir_transaction_conflict")
      }
      rows[[i]]$old_hash <- old
    }
    prepared <- append(prepared, list(list(dir = dir, record = record, rows = rows)))
  }
  list(journals = prepared, lockdir = lockdir)
}

# Applies a validated plan. Every file and journal touched is first copied to an
# undo area inside the recovery guard; if any step fails, all touched bytes are
# restored, so an unsuccessful recovery leaves the prior state unchanged.
recover_transactions <- function(root) {
  plan <- transaction_recovery_plan(root)
  if (!length(plan$journals)) {
    return(list())
  }
  recovery_lock <- file.path(root, ".cttir/recovery-lock")
  if (!dir.create(recovery_lock, showWarnings = FALSE)) {
    abort_cttir("Another recovery may be active.", "cttir_transaction_conflict")
  }
  undo_state <- new.env(parent = emptyenv())
  undo_state$saved <- list()
  undo_state$keep_guard <- FALSE
  on.exit(if (!undo_state$keep_guard) unlink(recovery_lock, recursive = TRUE), add = TRUE)
  undo <- file.path(recovery_lock, "undo")
  dir.create(undo)
  preserve <- function(dest) {
    copy <- file.path(undo, as.character(length(undo_state$saved) + 1L))
    if (!file.copy(dest, copy) || !identical(file_hash(copy), file_hash(dest))) {
      abort_cttir("Could not preserve a file before recovery.", "cttir_transaction_conflict")
    }
    undo_state$saved[[length(undo_state$saved) + 1L]] <- list(dest = dest, copy = copy, hash = file_hash(dest))
  }
  tryCatch(
    {
      for (entry in plan$journals) {
        for (i in rev(seq_along(entry$rows))) {
          row <- entry$rows[[i]]
          dest <- file.path(root, row$path)
          now <- file_hash(dest)
          if (identical(now, row$old_hash)) next
          if (!identical(now, row$new_hash)) abort_cttir("File changed during recovery.", "cttir_transaction_conflict")
          preserve(dest)
          if (is.na(row$old_hash)) {
            if (unlink(dest) != 0L) abort_cttir("Could not remove a transaction-owned file.", "cttir_transaction_conflict")
          } else {
            replace_file(file.path(entry$dir, "backup", as.character(i)), dest)
          }
          if (!identical(file_hash(dest), row$old_hash)) abort_cttir("Recovery verification failed.", "cttir_transaction_conflict")
        }
      }
      for (entry in plan$journals) {
        journal <- file.path(entry$dir, "journal.json")
        preserve(journal)
        entry$record$status <- "rolled_back"
        write_bytes(paste0(json_text(entry$record, TRUE), "\n"), journal)
      }
    },
    error = function(e) {
      for (item in rev(undo_state$saved)) {
        restored <- tryCatch(
          {
            if (!isTRUE(file.copy(item$copy, item$dest, overwrite = TRUE))) stop("copy")
            identical(file_hash(item$dest), item$hash)
          },
          error = function(x) FALSE
        )
        if (!restored) undo_state$keep_guard <- TRUE
      }
      if (undo_state$keep_guard) {
        abort_cttir("Recovery failed and could not restore every file; the recovery guard keeps undo copies for manual review.",
          "cttir_transaction_conflict", "recovery_incomplete")
      }
      stop(e)
    }
  )
  unlink(plan$lockdir, recursive = TRUE)
  lapply(plan$journals, function(x) {
    list(id = "recover_interrupted_transaction", journal = x$dir, status = "rolled_back",
      paths = vapply(x$rows, function(r) r$path, character(1)))
  })
}
