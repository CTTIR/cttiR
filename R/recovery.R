recover_transactions <- function(root) {
  assert_plain_path(root)
  journals <- pending_transactions(root)
  if (!length(journals)) {
    return(list())
  }
  lockdir <- file.path(root, ".cttir/write-lock")
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
  if (!dir.create(recovery_lock, showWarnings = FALSE)) {
    abort_cttir("Another recovery may be active.", "cttir_transaction_conflict")
  }
  on.exit(unlink(recovery_lock, recursive = TRUE), add = TRUE)
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
  for (entry in prepared) {
    for (i in rev(seq_along(entry$rows))) {
      row <- entry$rows[[i]]
      dest <- file.path(root, row$path)
      now <- file_hash(dest)
      if (identical(now, row$old_hash)) next
      if (!identical(now, row$new_hash)) abort_cttir("File changed during recovery.", "cttir_transaction_conflict")
      if (is.na(row$old_hash)) {
        if (unlink(dest) != 0L) abort_cttir("Could not remove a transaction-owned file.", "cttir_transaction_conflict")
      } else {
        replace_file(file.path(entry$dir, "backup", as.character(i)), dest)
      }
      if (!identical(file_hash(dest), row$old_hash)) abort_cttir("Recovery verification failed.", "cttir_transaction_conflict")
    }
    entry$record$status <- "rolled_back"
    write_bytes(paste0(json_text(entry$record, TRUE), "\n"), file.path(entry$dir, "journal.json"))
  }
  unlink(lockdir, recursive = TRUE)
  lapply(prepared, function(x) list(id = "recover_interrupted_transaction", journal = x$dir, status = "rolled_back"))
}
