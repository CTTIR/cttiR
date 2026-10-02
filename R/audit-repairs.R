# Allowlisted audit repairs, in priority order. A repair runs only when a check
# naming it via `repair_id` failed and `repair = TRUE` was requested. Each entry
# has `precondition(context)` returning NULL or a skip reason (read-only),
# `targets(context)` returning named absolute files whose bytes are hashed
# before and after, `apply(context)` returning list(message, journal), and a
# description of its rollback. A failed apply must restore the prior bytes.

audit_repairs <- function() {
  list(
    recover_interrupted_transaction = list(
      scope = "project",
      description = "Roll back an interrupted project write whose recorded local writer has stopped.",
      rollback = "Files reverted before a failure are restored from a temporary undo copy; journals and the writer lock are left unchanged.",
      precondition = function(context) {
        root <- audit_root(context)
        if (is.null(root)) return("The project directory does not exist.")
        plan <- tryCatch(transaction_recovery_plan(root), error = function(e) e)
        if (inherits(plan, "error")) return(audit_condition_message(plan))
        if (!length(plan$journals)) return("No interrupted transaction journals are pending.")
        NULL
      },
      targets = function(context) {
        root <- audit_root(context)
        plan <- transaction_recovery_plan(root)
        paths <- unique(unlist(lapply(plan$journals, function(x) vapply(x$rows, function(r) r$path, character(1)))))
        stats::setNames(file.path(root, paths), paths)
      },
      apply = function(context) {
        records <- recover_transactions(audit_root(context))
        list(message = paste(length(records), "interrupted transactions were rolled back to their verified preimages."),
          journal = vapply(records, function(x) x$journal, character(1)))
      }
    ),
    restore_missing_managed = list(
      scope = "project",
      description = "Restore missing managed files whose regenerated content equals the accepted baseline.",
      rollback = "The common transaction journal removes files created before a failure; a verified rollback journal is then discarded.",
      precondition = function(context) {
        p <- audit_project(context)
        if (is.null(p) || inherits(p, "error")) return("Project metadata could not be read.")
        if (is.null(missing_restore_plan(p))) {
          return("No missing managed file has a verified baseline; user-owned or edited files are never regenerated.")
        }
        NULL
      },
      targets = function(context) {
        p <- audit_project(context)
        paths <- missing_restore_plan(p)$rows$path
        stats::setNames(file.path(p$path, paths), paths)
      },
      apply = function(context) {
        records <- repair_missing(audit_project(context))
        list(message = paste(length(records[[1]]$paths), "missing managed files were restored from their accepted baseline."),
          journal = records[[1]]$journal)
      }
    ),
    repoint_active_catalog = list(
      scope = "knowledge",
      description = "Repoint a corrupt active catalog pointer to the last verified retained manifest.",
      rollback = "The prior pointer bytes are kept in the repair journal and restored if verification fails; snapshots are never deleted.",
      precondition = function(context) {
        root <- catalog_store()
        problem <- audit_pointer_problem(root)
        if (is.null(problem)) return("The active catalog pointer is valid or absent.")
        if (dir.exists(file.path(root, "write-lock"))) {
          return("A catalog writer lock is present; inspect the interrupted writer before repairing the pointer.")
        }
        if (is.null(last_verified_manifest(root))) {
          return("No retained manifest passes verification; restore a snapshot manually before rolling back.")
        }
        NULL
      },
      targets = function(context) c("active.json" = file.path(catalog_store(), "active.json")),
      apply = function(context) repoint_active_catalog()
    )
  )
}

audit_target_hashes <- function(targets) {
  lapply(targets, function(file) {
    tryCatch(file_hash(file), error = function(e) "unsafe")
  })
}

audit_apply_repairs <- function(checks, context) {
  failing <- checks[checks$status == "fail" & !is.na(checks$repair_id), , drop = FALSE]
  if (!nrow(failing)) return(list())
  catalog <- audit_repairs()
  records <- list()
  for (id in names(catalog)) {
    if (!id %in% failing$repair_id) next
    repair <- catalog[[id]]
    audit_reset(context)
    record <- list(id = id, status = "skipped", trigger = failing$id[failing$repair_id == id],
      reason = NULL, targets = character(), before = list(), after = list(),
      rollback = repair$rollback, journal = NULL, recheck = list())
    reason <- tryCatch(repair$precondition(context), error = function(e) audit_condition_message(e))
    if (!is.null(reason)) {
      record$reason <- reason
      records[[length(records) + 1L]] <- record
      next
    }
    targets <- repair$targets(context)
    record$targets <- names(targets)
    record$before <- audit_target_hashes(targets)
    outcome <- tryCatch(repair$apply(context), error = function(e) e)
    record$after <- audit_target_hashes(targets)
    if (inherits(outcome, "error")) {
      record$status <- "failed"
      record$reason <- audit_condition_message(outcome)
    } else {
      record$status <- "applied"
      record$reason <- outcome$message
      record$journal <- outcome$journal
    }
    records[[length(records) + 1L]] <- record
  }
  audit_reset(context)
  records
}

missing_restore_plan <- function(p) {
  desired <- project_bundle(p$spec, p$lock)$files
  rows <- list()
  for (entry in p$manifest$files) {
    if (entry$ownership != "managed" || !is.na(file_hash(file.path(p$path, entry$path)))) next
    content <- desired[[entry$path]]
    if (is.null(content) || !identical(content_hash(content), entry$baseline_sha256)) next
    rows[[length(rows) + 1L]] <- data.frame(
      path = entry$path, action = "create", old_hash = NA_character_,
      new_hash = entry$baseline_sha256, stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(NULL)
  list(rows = do.call(rbind, rows), files = desired)
}

repair_missing <- function(p) {
  plan <- missing_restore_plan(p)
  if (is.null(plan)) return(list())
  transactions <- file.path(p$path, ".cttir/transactions")
  existed <- dir.exists(transactions)
  before <- if (existed) list.dirs(transactions, recursive = FALSE) else character()
  journal <- tryCatch(transact_files(p$path, plan$files, plan$rows), error = function(e) {
    # Discard only this operation's own journal, and only after a verified rollback.
    after <- if (dir.exists(transactions)) list.dirs(transactions, recursive = FALSE) else character()
    for (dir in setdiff(after, before)) {
      status <- tryCatch(read_document(file.path(dir, "journal.json"))$status, error = function(x) NULL)
      if (identical(status, "rolled_back")) unlink(dir, recursive = TRUE)
    }
    if (!existed && dir.exists(transactions) && !length(list.files(transactions, all.files = TRUE, no.. = TRUE))) {
      unlink(transactions, recursive = TRUE)
    }
    stop(e)
  })
  list(list(id = "restore_missing_managed", paths = plan$rows$path, journal = journal, status = "applied"))
}

last_verified_manifest <- function(root = catalog_store()) {
  directory <- file.path(root, "manifests")
  assert_plain_path(directory)
  files <- list.files(directory, pattern = "^[a-f0-9]{64}[.]json$", full.names = TRUE)
  if (!length(files)) return(NULL)
  # The most recently written retained manifest that verifies completely wins.
  files <- files[order(-as.numeric(file.info(files)$mtime), basename(files))]
  for (file in files) {
    manifest <- tryCatch(
      {
        assert_plain_path(file)
        x <- validate_manifest(read_document(file))
        if (!identical(paste0(x$manifest_id, ".json"), basename(file))) {
          abort_cttir("Retained manifest identity mismatch.", "cttir_catalog_corrupt")
        }
        store_manifest_verified(x, root)
        x
      },
      error = function(e) NULL
    )
    if (!is.null(manifest)) return(manifest)
  }
  NULL
}

repoint_active_catalog <- function() {
  root <- catalog_store()
  active <- file.path(root, "active.json")
  assert_plain_path(active)
  before <- file_hash(active)
  if (is.na(before) || is.null(audit_pointer_problem(root))) {
    abort_cttir("The active catalog pointer does not need repair.", "cttir_transaction_conflict")
  }
  lock <- catalog_lock(root)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (!identical(file_hash(active), before) || is.null(audit_pointer_problem(root))) {
    abort_cttir("The active pointer changed during repair planning.", "cttir_transaction_conflict")
  }
  target <- last_verified_manifest(root)
  if (is.null(target)) abort_cttir("No verified retained manifest is available.", "cttir_catalog_corrupt")
  repairs <- file.path(root, "repairs")
  assert_plain_path(repairs)
  existed <- dir.exists(repairs)
  journal_dir <- file.path(repairs, uuid::UUIDgenerate())
  dir.create(journal_dir, recursive = TRUE)
  backup <- file.path(journal_dir, "active.json.before")
  journal_file <- file.path(journal_dir, "journal.json")
  journal <- list(schema_version = 1L, repair = "repoint_active_catalog", status = "applying",
    before_sha256 = before, target = target)
  staged <- tempfile("active-", tmpdir = root, fileext = ".json")
  committed <- FALSE
  on.exit(
    {
      if (!committed) {
        unlink(staged)
        now <- tryCatch(file_hash(active), error = function(e) "unsafe")
        restorable <- file.exists(backup) && identical(file_hash(backup), before)
        restored <- identical(now, before)
        if (!restored && restorable && isTRUE(file.copy(backup, active, overwrite = TRUE))) {
          restored <- identical(file_hash(active), before)
        }
        if (restored) {
          unlink(journal_dir, recursive = TRUE)
          if (!existed && !length(list.files(repairs, all.files = TRUE, no.. = TRUE))) unlink(repairs, recursive = TRUE)
        } else {
          journal$status <- "recovery_required"
          try(write_bytes(paste0(json_text(journal, TRUE), "\n"), journal_file), silent = TRUE)
        }
      }
    },
    add = TRUE, after = FALSE
  )
  if (!file.copy(active, backup) || !identical(file_hash(backup), before)) {
    abort_cttir("Could not preserve the prior active pointer.", "cttir_transaction_conflict")
  }
  write_bytes(paste0(json_text(journal, TRUE), "\n"), journal_file)
  write_bytes(paste0(json_text(target), "\n"), staged)
  tryCatch(fs::file_move(staged, active), error = function(e) {
    abort_cttir("Could not replace the active pointer.", "cttir_transaction_conflict")
  })
  if (!is.null(audit_pointer_problem(root)) || !identical(current_catalog_manifest(), target)) {
    abort_cttir("The repointed catalog failed verification.", "cttir_catalog_corrupt")
  }
  journal$status <- "committed"
  journal$after_sha256 <- file_hash(active)
  write_bytes(paste0(json_text(journal, TRUE), "\n"), journal_file)
  committed <- TRUE
  list(message = paste("The active catalog now points to retained manifest", target$manifest_id, "after verification."),
    journal = journal_dir)
}
