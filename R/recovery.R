# Lock ownership ---------------------------------------------------------------
#
# Project creation and project writes take a directory lock and record their
# owner in it. A lock is stale only when its owner is verified gone on this
# host, or when it has no owner record (older cttiR, or a crash right after
# taking it) and is older than stale_lock_seconds, which exceeds the longest
# environment preparation. Anything else counts as held.

stale_lock_seconds <- 24 * 3600

write_lock_owner <- function(lockdir, extra = list()) {
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  owner <- c(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]], started_at = started), extra)
  write_bytes(paste0(json_text(owner, TRUE), "\n"), file.path(lockdir, "owner.json"))
}

lock_state <- function(lockdir) {
  assert_plain_path(lockdir)
  if (!dir.exists(lockdir)) return(list(state = "absent"))
  owner_file <- file.path(lockdir, "owner.json")
  assert_plain_path(owner_file)
  if (!file.exists(owner_file)) {
    modified <- file.info(lockdir)$mtime
    age <- as.numeric(difftime(Sys.time(), modified, units = "secs"))
    # The modification time is the identity of an ownerless lock.
    fingerprint <- paste0("ownerless:", format(as.numeric(modified), digits = 15))
    if (isTRUE(age > stale_lock_seconds)) {
      return(list(state = "stale", reason = "ownerless_lock_expired", owner = NULL, fingerprint = fingerprint))
    }
    return(list(state = "unverified", reason = "ownerless_lock", owner = NULL, fingerprint = fingerprint))
  }
  fingerprint <- file_hash(owner_file)
  owner <- tryCatch(read_document(owner_file), error = function(e) NULL)
  pid <- if (is.list(owner)) owner$pid else NULL
  if (!is.numeric(pid) || length(pid) != 1L || is.na(pid) || pid <= 0 || pid > .Machine$integer.max || pid != round(pid)) {
    return(list(state = "unverified", reason = "unreadable_owner", owner = NULL, fingerprint = fingerprint))
  }
  if (!identical(owner$host, Sys.info()[["nodename"]])) {
    return(list(state = "unverified", reason = "other_host", owner = owner, fingerprint = fingerprint))
  }
  alive <- tryCatch(pid %in% ps::ps_pids(), error = function(e) TRUE)
  list(state = if (alive) "active" else "stale", reason = if (alive) "owner_running" else "owner_gone",
    owner = owner, fingerprint = fingerprint)
}

lock_fingerprint <- function(lockdir) {
  state <- tryCatch(lock_state(lockdir), error = function(e) NULL)
  if (is.null(state$fingerprint)) NA_character_ else state$fingerprint
}

# Moves a stale lock aside so that only one process can take it over, then
# confirms that the moved lock is the one judged stale; otherwise it is put back.
claim_stale_lock <- function(lockdir, state) {
  aside <- paste0(lockdir, ".stale-", substr(gsub("-", "", uuid::UUIDgenerate(), fixed = TRUE), 1L, 12L))
  if (!file.rename(lockdir, aside)) return(NULL)
  if (!identical(lock_fingerprint(aside), state$fingerprint)) {
    file.rename(aside, lockdir)
    return(NULL)
  }
  aside
}

lock_held_message <- function(state, label) {
  if (identical(state$state, "absent")) return(sprintf("%s could not be created; check that the directory is writable.", label))
  switch(state$state,
    active = sprintf("%s is held by a running cttiR process (PID %s on this host).", label, format(state$owner$pid)),
    stale = sprintf("%s was left by a stopped process.", label),
    switch(state$reason,
      other_host = sprintf("%s was taken on host '%s'; its owner cannot be checked from here.", label, format(state$owner$host)),
      unreadable_owner = sprintf("%s has an unreadable owner record.", label),
      sprintf("%s has no owner record; it may belong to an operation that is still starting or to one that stopped.", label)))
}

# Releases a project writer lock whose owner is verified gone when no journal
# was started: the writer had not changed any project file yet.
release_stale_writer_lock <- function(root) {
  lockdir <- file.path(root, ".cttir", "write-lock")
  state <- lock_state(lockdir)
  if (!identical(state$state, "stale") || length(pending_transactions(root))) return(NULL)
  aside <- claim_stale_lock(lockdir, state)
  if (is.null(aside)) return(NULL)
  unlink(aside, recursive = TRUE)
  list(id = "release_stale_writer_lock", lock = ".cttir/write-lock", reason = state$reason, owner = state$owner)
}

abort_writer_lock <- function(root) {
  lockdir <- file.path(root, ".cttir", "write-lock")
  state <- lock_state(lockdir)
  if (identical(state$state, "absent")) {
    abort_cttir("The project writer lock (.cttir/write-lock) could not be created.", "cttir_transaction_conflict",
      "write_failed", remediation = "Check that the project directory is writable, then retry.")
  }
  journals <- length(pending_transactions(root))
  call <- sprintf("cttiR::audit(%s, scope = \"project\", repair = TRUE)", r_literal(root))
  remediation <- if (identical(state$state, "active")) {
    "Wait for the other cttiR operation to finish, then retry."
  } else if (journals) {
    paste0("An interrupted write left a transaction journal. Review it with cttiR::audit() and roll it back with ", call, ".")
  } else if (identical(state$state, "stale")) {
    paste0("Retry; the lock is released automatically, or run ", call, ".")
  } else {
    paste0("If no cttiR operation is writing to this project, remove the directory .cttir/write-lock and retry. ",
      "Locks without a verifiable owner are released automatically after 24 hours.")
  }
  message <- lock_held_message(state, "The project writer lock (.cttir/write-lock)")
  if (journals) message <- paste(message, "An interrupted transaction journal is pending.")
  abort_cttir(message, "cttir_transaction_conflict", "writer_lock", remediation = remediation)
}

creation_lock_path <- function(parent, slug) file.path(parent, paste0(".", slug, ".cttir-create-lock"))

abort_creation_lock <- function(lockdir, state) {
  remediation <- if (identical(state$state, "active")) {
    "Wait for the other project() call to finish, then retry."
  } else {
    paste0("If no other project() call is creating this project, remove the directory ", basename(lockdir),
      " and any matching .<slug>-stage-* directory in the parent directory, then retry. ",
      "Locks without a verifiable owner are recovered automatically after 24 hours.")
  }
  abort_cttir(lock_held_message(state, paste0("The creation lock ", basename(lockdir))),
    "cttir_transaction_conflict", "writer_lock", remediation = remediation)
}

# Recovers the creation lock of a project() call that stopped before publishing
# its staged scaffold. Only the lock and that call's staging directory are
# removed: the recorded one, or for an ownerless lock from an older cttiR, the
# staging directories of this slug (no other call could stage while it held).
recover_creation_lock <- function(parent, slug) {
  lockdir <- creation_lock_path(parent, slug)
  state <- lock_state(lockdir)
  if (identical(state$state, "absent")) {
    abort_cttir("The creation lock could not be created in the parent directory.", "cttir_path_conflict", "write_failed",
      remediation = "Check that the parent directory exists and is writable, then retry.")
  }
  if (!identical(state$state, "stale")) abort_creation_lock(lockdir, state)
  pattern <- paste0("^\\.", slug, "-stage-[0-9a-f]+$")
  stages <- if (is.null(state$owner)) {
    list.files(parent, pattern, all.files = TRUE)
  } else {
    recorded <- state$owner$stage
    if (is.character(recorded) && length(recorded) == 1L && grepl(pattern, recorded)) recorded else character()
  }
  aside <- claim_stale_lock(lockdir, state)
  if (is.null(aside)) abort_creation_lock(lockdir, lock_state(lockdir))
  removed <- character()
  for (stage in stages) {
    path <- file.path(parent, stage)
    link <- Sys.readlink(path)
    if (dir.exists(path) && !is.na(link) && !nzchar(link) && unlink(path, recursive = TRUE) == 0L) {
      removed <- c(removed, stage)
    }
  }
  unlink(aside, recursive = TRUE)
  list(id = "recover_creation_lock", lock = basename(lockdir), reason = state$reason,
    owner = state$owner, staging_removed = as.list(removed))
}

# Read-only validation of interrupted project transactions. Returns the pending
# journals with verified rows, or aborts when automatic recovery is unsafe. A
# stale writer lock without any journal is listed as its own entry, so that
# callers acting on pending recovery entries (audit repair) also release it.
transaction_recovery_plan <- function(root) {
  assert_plain_path(root)
  journals <- pending_transactions(root)
  lockdir <- file.path(root, ".cttir/write-lock")
  if (!length(journals)) {
    state <- lock_state(lockdir)
    if (!identical(state$state, "stale")) return(list(journals = list(), lockdir = lockdir))
    entry <- list(kind = "stale_writer_lock", dir = lockdir, record = NULL, rows = list(), lock = state)
    return(list(journals = list(entry), lockdir = lockdir, stale_lock = state))
  }
  owner_file <- file.path(lockdir, "owner.json")
  assert_plain_path(owner_file)
  if (!file.exists(owner_file)) {
    abort_cttir("Interrupted writer ownership is unknown; automatic recovery is refused.", "cttir_transaction_conflict")
  }
  state <- lock_state(lockdir)
  if (identical(state$state, "unverified")) {
    abort_cttir("Interrupted writer ownership cannot be verified.", "cttir_transaction_conflict")
  }
  if (identical(state$state, "active")) {
    abort_cttir("The recorded writer is still running; recovery is refused.", "cttir_transaction_conflict")
  }
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
    prepared <- append(prepared, list(list(kind = "journal", dir = dir, record = record, rows = rows)))
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
  if (!is.null(plan$stale_lock)) {
    released <- release_stale_writer_lock(root)
    if (is.null(released)) abort_cttir("The writer lock changed during recovery; audit again.", "cttir_transaction_conflict")
    record <- list(id = "recover_interrupted_transaction", journal = NA_character_, status = "released_stale_lock",
      paths = character(), lock = released)
    return(list(record))
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
