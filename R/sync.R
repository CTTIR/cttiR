# A hand edit of cttir-project.yml is reviewed against the verified accepted
# copy and becomes a change set like config input: fields that config may set
# can change; schema version, identity, provenance and decisions stay with cttiR.
hand_edit_changes <- function(root, accepted, edited) {
  restore <- function(field, why) {
    abort_cttir(sprintf("%s in cttir-project.yml %s.", field, why), "cttir_input_error", "invalid_spec_edit", field = field,
      remediation = sprintf("Restore %s as recorded in .cttir/accepted-spec.yml, then preview again with cttiR::sync(%s).",
        field, r_literal(root)))
  }
  builder <- c("schema_version", "decisions", "provenance")
  for (key in builder) {
    if (!identical(json_text(edited[[key]]), json_text(accepted[[key]]))) {
      restore(paste0("/", key), "is recorded by cttiR and cannot be edited")
    }
  }
  for (key in c("id", "slug", "created_at")) {
    if (!identical(edited$project[[key]], accepted$project[[key]])) {
      restore(paste0("/project/", key), "is assigned once at creation and cannot be edited")
    }
  }
  editable <- setdiff(union(names(accepted), names(edited)), builder)
  pick <- function(x) stats::setNames(lapply(editable, function(key) x[[key]]), editable)
  changed <- spec_changes(pick(accepted), pick(edited))$pointer
  hand <- list()
  for (key in unique(sub("^/([^/]+).*$", "\\1", changed))) {
    hand[key] <- list(if (identical(key, "project")) {
      edited$project[setdiff(names(edited$project), c("id", "slug", "created_at"))]
    } else {
      edited[[key]]
    })
  }
  if (!length(hand)) return(hand)
  tryCatch(validate_config(hand), cttir_error = function(e) {
    if (!identical(e$code, "schema_validation") || !is.data.frame(e$field)) stop(e)
    detail <- schema_error_detail(e$field)
    abort_cttir(paste0("The edit of cttir-project.yml is not valid configuration: ", detail$text, "."),
      "cttir_schema_error", "schema_validation", field = detail$pointer,
      remediation = sprintf("Correct %s in cttir-project.yml, then preview again with cttiR::sync(%s).",
        detail$pointer, r_literal(root)))
  })
}

apply_hand_edit <- function(saved, hand) {
  for (key in names(hand)) {
    if (identical(key, "project")) saved$project[names(hand$project)] <- hand$project else saved[key] <- list(hand[[key]])
  }
  saved
}

# Precedence for existing projects (file 04): explicit options > config > a
# reviewed hand edit of cttir-project.yml > the accepted spec.
sync_spec <- function(saved, config, options, edited = NULL, root = ".") {
  config <- validate_config(if (is.null(config)) list() else config)
  options <- validate_config(options)
  hand <- if (is.null(edited)) list() else hand_edit_changes(root, saved, edited)
  for (x in list(hand, config, options)) {
    check_empty_strings(x, saved)
    if (any(c("id", "slug", "created_at") %in% names(x$project))) {
      abort_cttir("Synchronization preserves project identity and paths.")
    }
    if (isTRUE(x$knowledge$refresh)) {
      abort_cttir("Knowledge migration is not yet supported.", "cttir_api_mismatch")
    }
    if (identical(x$workflow$profile, "auto")) abort_cttir("Synchronization keeps the resolved profile; 'auto' is a creation-time request.", "cttir_api_mismatch")
  }
  base <- apply_hand_edit(saved, hand)
  merged <- merge_config(merge_config(base, config), options)
  merged$knowledge <- NULL
  # Arrays merge by stable ID, so an entry can only disappear by an explicit
  # removal (an empty list) or by deleting it from cttir-project.yml.
  for (key in c("publications", "data_sources")) {
    kept <- vapply(merged[[key]], function(x) x$id, character(1))
    gone <- setdiff(vapply(saved[[key]], function(x) x$id, character(1)), kept)
    if (length(gone)) {
      abort_cttir(sprintf("Removing %s entries (%s) requires an explicit archive plan.", key, paste(gone, collapse = ", ")),
        "cttir_path_conflict", field = paste0("/", key),
        remediation = "Keep the entries; reclassify them instead, or archive them manually after review.")
    }
  }
  # Only these workflow options of the current standard bundle may change in
  # place; they alter managed configuration, pinned dependencies and explicit
  # environment/Git steps, never user files. The profile stays fixed.
  changeable <- if (identical(saved$provenance$template_version, current_template_version)) {
    c("table_backend", "pipeline", "environment", "prepare_environment", "network", "git")
  } else {
    character()
  }
  fixed <- setdiff(union(names(merged$workflow), names(saved$workflow)), changeable)
  moved <- fixed[!vapply(fixed, function(key) identical(merged$workflow[[key]], saved$workflow[[key]]), logical(1))]
  if (length(moved)) {
    abort_cttir(sprintf("workflow.%s cannot change in an existing project; workflow migration is not yet supported.", moved[[1]]),
      "cttir_api_mismatch", field = paste0("/workflow/", moved[[1]]),
      remediation = if (length(changeable)) {
        paste0("Keep the recorded value. Only workflow.", paste(changeable, collapse = ", workflow."), " may change.")
      } else {
        "Keep the recorded value; this template version allows no workflow changes."
      })
  }
  if (!identical(merged$packages, saved$packages)) {
    abort_cttir("Dependency migration is not yet supported; packages cannot change in an existing project.",
      "cttir_api_mismatch", field = "/packages", remediation = "Keep the recorded packages.")
  }
  if (!identical(merged$workflow, saved$workflow)) check_supported_workflow(merged, list())
  for (pub in saved$publications) {
    ids <- vapply(merged$publications, function(p) p$id, character(1))
    if (!identical(merged$publications[[match(pub$id, ids)]]$slug, pub$slug)) {
      abort_cttir("Publication paths are stable; a rename requires a migration plan.", "cttir_path_conflict")
    }
  }
  decision <- function(reason) list(field = "/", origin = "explicit", reason = reason, evidence_ids = list())
  decisions <- saved$decisions
  if (length(hand)) decisions <- append(decisions, list(decision("Accepted a reviewed hand edit of cttir-project.yml.")))
  if (!identical(json_text(merged), json_text(base))) {
    decisions <- append(decisions, list(decision("Accepted explicit configuration synchronization.")))
  }
  if (!identical(decisions, saved$decisions)) merged$decisions <- decisions
  validate_spec(merged)
}

sync_control_files <- c("cttir-project.yml", "cttir-lock.json", ".cttir/managed-files.json", ".cttir/state.json",
  ".cttir/accepted-spec.yml")

# Three-way comparison of the recorded baseline, the current file and the newly
# rendered content. Only a file changed both by its user and by the new
# rendering needs attention: a managed file then conflicts, while a user-owned
# file is kept and its update is proposed for review. `accepted` lists files
# whose current content is a reviewed hand edit and therefore the new base.
sync_plan <- function(project, bundle, accepted = character()) {
  old <- project$manifest$files
  oldpaths <- vapply(old, function(x) x$path, character(1))
  rows <- lapply(names(bundle$files), function(path) {
    actual <- file_hash(file.path(project$path, path))
    desired <- content_hash(bundle$files[[path]])
    index <- match(path, oldpaths)
    control <- path %in% sync_control_files
    baseline <- if (is.na(index)) NA_character_ else old[[index]]$baseline_sha256
    ownership <- if (control) "managed" else if (is.na(index)) "new" else old[[index]]$ownership
    if ((control && is.na(baseline)) || path %in% accepted) baseline <- actual
    # A file that differs only by CRLF line endings (saved on Windows) is
    # compared by its LF content; old_hash keeps the exact bytes for the writer.
    current <- actual
    if (!is.na(actual) && !identical(actual, baseline) && !identical(actual, desired)) {
      current <- lf_file_hash(file.path(project$path, path), actual)
    }
    edited <- !identical(current, baseline)
    if (identical(current, desired)) {
      action <- c("skip", "unchanged")
    } else if (is.na(actual)) {
      action <- c("create", if (is.na(index) && !control) "new_file" else "missing_file")
    } else if (identical(ownership, "new")) {
      # Do not silently replace a hand-created file at a newly generated path.
      action <- c("conflict", "unrecorded_file_at_new_path")
    } else if (!edited) {
      action <- c("update", if (path %in% accepted) "accepted_hand_edit" else "unedited_since_baseline")
    } else if (identical(desired, baseline) && !control) {
      action <- c("preserve", "edit_kept_rendering_unchanged")
    } else if (identical(ownership, "user")) {
      action <- c("preserve", "pending_update")
    } else {
      action <- c("conflict", "edited_and_rendering_changed")
    }
    data.frame(path = path, action = action[[1]], reason = action[[2]], old_hash = actual, new_hash = desired,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

proposal_path <- function(path) file.path(".cttir", "proposed", path)

#' Preview or apply project configuration changes
#'
#' Preserves identity, catalog pins and edits. A file changed both by its user
#' and by the new rendering is never overwritten: a managed file yields a
#' conflict, and no writes occur while any conflict is present; a user-owned
#' file (protocol, metadata, publication and analysis text) is kept and the
#' rendered update is written to `.cttir/proposed/<path>` for review. Edited
#' files whose rendering did not change are simply kept. Unedited user-owned
#' files, such as `metadata/data-registry.yml` or `publication.yml`, are updated
#' like managed files. Applied changes have per-file backups and a journal. The
#' operation is not a multi-file atomic filesystem transaction. Interrupted
#' writers require review; a writer lock left by a stopped process that had not
#' started its journal is released.
#' @param path Exact project root.
#' @param config Optional configuration list or YAML/JSON path.
#' @param options Configuration overrides taking precedence over `config`.
#' @param dry_run Return a read-only plan by default.
#' @details For current standard projects, `workflow` options `pipeline`,
#'   `environment`, `prepare_environment`, `network`, `git` and `table_backend`
#'   may change; the profile stays fixed. An applied sync then runs the explicit
#'   environment and Git steps; dry runs only read their state. `renv.lock` is
#'   derived output and is never planned as a template file. A lockfile whose
#'   project library is absent on this machine is not rewritten: the recovery
#'   command restores it with `renv::restore()`.
#'
#'   `cttir-project.yml` may be edited by hand. `project()` and `audit()` then
#'   report the edit, and `sync(path)` previews it against the accepted copy in
#'   `.cttir/accepted-spec.yml`: the edit is checked with the same rules as
#'   configuration input (schema version, identity, provenance and decisions
#'   stay as recorded; entries are not removed; publication paths are stable).
#'   `sync(path, dry_run = FALSE)` accepts it; `config` and `options` passed in
#'   the same call take precedence over the hand edit.
#' @return A `cttir_sync` with actions (with a `reason` per file), conflicts,
#'   changed files, `preserved` edited files, `pending_updates` (kept user files
#'   with a proposed update), `spec_edit` (a reviewed hand edit and its changed
#'   JSON pointers), `recovered` stale locks, journal, the computed readiness
#'   level, blockers, `environment` and `git` status and an `analysis`
#'   configuration assessment. Recording mappings does not approve or execute an
#'   analysis.
#' @export
sync <- function(path = ".", config = NULL, options = list(), dry_run = TRUE) {
  sync_impl(path, config, options, dry_run)
}

sync_impl <- function(path = ".", config = NULL, options = list(), dry_run = TRUE, expected_plan = NULL) {
  scalar_flag(dry_run, "dry_run")
  options <- utf8_input(options)
  p <- read_project(path, edited = TRUE)
  spec <- sync_spec(p$spec, config, options, p$edited_spec, p$path)
  bundle <- carry_user_records(project_bundle(spec, p$lock), p, refresh = TRUE)
  plan <- sync_plan(p, bundle, if (is.null(p$edited_spec)) character() else "cttir-project.yml")
  pending <- plan[plan$reason == "pending_update", , drop = FALSE]
  files <- bundle$files
  for (path in pending$path) {
    proposal <- proposal_path(path)
    files[[proposal]] <- bundle$files[[path]]
    current <- file_hash(file.path(p$path, proposal))
    desired <- content_hash(files[[proposal]])
    action <- if (identical(current, desired)) "skip" else if (is.na(current)) "create" else "update"
    row <- data.frame(path = proposal, action = action, reason = "proposed_update", old_hash = current,
      new_hash = desired, stringsAsFactors = FALSE)
    plan <- rbind(plan, row)
  }
  if (!is.null(expected_plan) && !identical(content_hash(json_text(plan)), expected_plan))
    abort_cttir("Project files changed after preview; preview again before applying.", "cttir_transaction_conflict")
  conflicts <- plan$path[plan$action == "conflict"]
  journal <- NULL
  recovered <- list()
  # Dry runs and conflicts only read the environment and Git state from disk.
  environment <- environment_status(bundle$lock$dependencies, p$path, spec$workflow$environment)
  git <- git_status(p$path, isTRUE(spec$workflow$git))
  if (!dry_run && !length(conflicts)) {
    released <- release_stale_writer_lock(p$path)
    if (!is.null(released)) recovered <- list(released)
    journal <- transact_files(p$path, files, plan)
    environment <- environment_step(p$path, spec, bundle$lock$dependencies)
    if (isTRUE(spec$workflow$git)) git <- git_initialize(p$path)
  }
  readiness <- readiness_with(list(), spec, bundle, environment, git)
  writer <- lock_state(file.path(p$path, ".cttir", "write-lock"))
  structure(list(
    path = p$path, actions = plan, conflicts = conflicts,
    changed_files = if (dry_run || length(conflicts)) character() else plan$path[plan$action %in% c("create", "update")],
    state = if (length(conflicts)) "conflict" else if (dry_run) "planned" else "applied",
    preserved = plan$path[plan$action == "preserve"],
    pending_updates = data.frame(path = pending$path, proposal = vapply(pending$path, proposal_path, character(1)),
      current_hash = pending$old_hash, proposed_hash = pending$new_hash, stringsAsFactors = FALSE, row.names = NULL),
    spec_edit = if (!is.null(p$edited_spec)) {
      list(state = if (dry_run || length(conflicts)) "previewed" else "accepted",
        changes = spec_changes(p$spec[setdiff(names(p$spec), "decisions")], spec[setdiff(names(spec), "decisions")]))
    },
    recovered = recovered,
    writer_lock = if (!identical(writer$state, "absent")) writer[c("state", "reason")],
    journal = journal, readiness = readiness$level, blockers = readiness$blockers,
    environment = environment, git = git,
    analysis = analysis_configuration(spec)
  ), class = "cttir_sync")
}

#' @export
print.cttir_sync <- function(x, ...) {
  cat("Project synchronization: ", x$state, "\n", sep = "")
  cat(length(x$changed_files), " files changed; ", length(x$conflicts), " conflicts; ",
    length(x$preserved), " edited files kept; ", NROW(x$pending_updates), " pending updates\n", sep = "")
  if (NROW(x$pending_updates)) {
    cat("Kept your edits; review the proposed content in ",
      paste(x$pending_updates$proposal, collapse = ", "), "\n", sep = "")
  }
  if (!is.null(x$spec_edit)) {
    cat("Hand edit of cttir-project.yml: ", x$spec_edit$state, " (", nrow(x$spec_edit$changes), " fields)\n", sep = "")
  }
  if (length(x$recovered)) cat("Released a stale writer lock left by a stopped process.\n")
  invisible(x)
}
