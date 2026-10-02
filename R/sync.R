sync_spec <- function(saved, config, options) {
  config <- validate_config(if (is.null(config)) list() else config)
  options <- validate_config(options)
  for (x in list(config, options)) {
    if (any(c("id", "slug", "created_at") %in% names(x$project))) {
      abort_cttir("Synchronization preserves project identity and paths.")
    }
    if (isTRUE(x$knowledge$refresh)) {
      abort_cttir("Knowledge migration is not yet supported.", "cttir_api_mismatch")
    }
    if (any(c("publications", "data_sources") %in% names(x))) {
      for (key in intersect(c("publications", "data_sources"), names(x))) {
        if (!length(x[[key]]) && length(saved[[key]])) {
          abort_cttir("Removing entries requires an explicit archive plan.", "cttir_path_conflict")
        }
      }
    }
  }
  for (x in list(config, options)) {
    if (identical(x$workflow$profile, "auto")) abort_cttir("Synchronization keeps the resolved profile; 'auto' is a creation-time request.", "cttir_api_mismatch")
  }
  merged <- merge_config(merge_config(saved, config), options)
  merged$knowledge <- NULL
  # Only these workflow options of the current standard bundle may change in
  # place; they alter managed configuration, pinned dependencies and explicit
  # environment/Git steps, never user files. The profile stays fixed.
  changeable <- if (identical(saved$provenance$template_version, current_template_version)) {
    c("table_backend", "pipeline", "environment", "prepare_environment", "network", "git")
  } else {
    character()
  }
  fixed <- setdiff(union(names(merged$workflow), names(saved$workflow)), changeable)
  if (!identical(merged$workflow[fixed], saved$workflow[fixed]) || !identical(merged$packages, saved$packages)) {
    abort_cttir("Workflow and dependency migration are not yet supported.", "cttir_api_mismatch")
  }
  if (!identical(merged$workflow, saved$workflow)) check_supported_workflow(merged, list())
  for (pub in saved$publications) {
    ids <- vapply(merged$publications, function(p) p$id, character(1))
    if (!identical(merged$publications[[match(pub$id, ids)]]$slug, pub$slug)) {
      abort_cttir("Publication paths are stable; a rename requires a migration plan.", "cttir_path_conflict")
    }
  }
  if (!identical(json_text(merged), json_text(saved))) {
    merged$decisions <- append(saved$decisions, list(list(
      field = "/", origin = "explicit",
      reason = "Accepted explicit configuration synchronization.", evidence_ids = list()
    )))
  }
  validate_spec(merged)
}

sync_plan <- function(project, bundle) {
  old <- project$manifest$files
  oldpaths <- vapply(old, function(x) x$path, character(1))
  control <- c("cttir-project.yml", "cttir-lock.json", ".cttir/managed-files.json", ".cttir/state.json")
  rows <- lapply(names(bundle$files), function(path) {
    actual <- file_hash(file.path(project$path, path))
    desired <- content_hash(bundle$files[[path]])
    index <- match(path, oldpaths)
    baseline <- if (is.na(index)) NA_character_ else old[[index]]$baseline_sha256
    ownership <- if (path %in% control) "managed" else if (is.na(index)) "new" else old[[index]]$ownership
    if (path %in% control && is.na(baseline)) baseline <- actual
    action <- if (identical(actual, desired)) {
      "skip"
    } else if (is.na(actual)) {
      "create"
    } else if (ownership == "user") {
      "preserve"
    } else if (identical(actual, baseline)) {
      "update"
    } else {
      "conflict"
    }
    # Do not silently replace a hand-created file at a newly generated path.
    if (ownership == "new" && !is.na(actual) && !identical(actual, desired)) action <- "conflict"
    data.frame(path = path, action = action, old_hash = actual, new_hash = desired, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Preview or apply project configuration changes
#'
#' Preserves identity, catalog pins and user-owned files. Edited managed files
#' yield a conflict; no writes occur if any conflict is present. Applied changes
#' have per-file backups and a journal. The operation is not a multi-file atomic
#' filesystem transaction. Interrupted writers require review.
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
#' @return A `cttir_sync` with actions, conflicts, changed files, journal, the
#'   computed readiness level, blockers, `environment` and `git` status and an
#'   `analysis` configuration assessment. Recording mappings does not approve or
#'   execute an analysis.
#' @export
sync <- function(path = ".", config = NULL, options = list(), dry_run = TRUE) {
  sync_impl(path, config, options, dry_run)
}

sync_impl <- function(path = ".", config = NULL, options = list(), dry_run = TRUE, expected_plan = NULL) {
  scalar_flag(dry_run, "dry_run")
  p <- read_project(path)
  spec <- sync_spec(p$spec, config, options)
  bundle <- project_bundle(spec, p$lock)
  # Keep accepted baselines and ownership of user files, including edited ones.
  old_paths <- vapply(p$manifest$files, function(f) f$path, character(1))
  for (i in seq_along(bundle$manifest)) {
    at <- match(bundle$manifest[[i]]$path, old_paths)
    if (!is.na(at) && p$manifest$files[[at]]$ownership == "user") {
      bundle$manifest[[i]] <- p$manifest$files[[at]]
    }
  }
  bundle$files[[".cttir/managed-files.json"]] <- paste0(json_text(list(schema_version = 1L, files = bundle$manifest), TRUE), "\n")
  plan <- sync_plan(p, bundle)
  if (!is.null(expected_plan) && !identical(content_hash(json_text(plan)), expected_plan))
    abort_cttir("Project files changed after preview; preview again before applying.", "cttir_transaction_conflict")
  conflicts <- plan$path[plan$action == "conflict"]
  journal <- NULL
  # Dry runs and conflicts only read the environment and Git state from disk.
  environment <- environment_status(bundle$lock$dependencies, p$path, spec$workflow$environment)
  git <- git_status(p$path, isTRUE(spec$workflow$git))
  if (!dry_run && !length(conflicts)) {
    journal <- transact_files(p$path, bundle$files, plan)
    environment <- environment_step(p$path, spec, bundle$lock$dependencies)
    if (isTRUE(spec$workflow$git)) git <- git_initialize(p$path)
  }
  readiness <- readiness_with(list(), spec, bundle, environment, git)
  structure(list(
    path = p$path, actions = plan, conflicts = conflicts,
    changed_files = if (dry_run || length(conflicts)) character() else plan$path[plan$action %in% c("create", "update")],
    state = if (length(conflicts)) "conflict" else if (dry_run) "planned" else "applied",
    journal = journal, readiness = readiness$level, blockers = readiness$blockers,
    environment = environment, git = git,
    analysis = analysis_configuration(spec)
  ), class = "cttir_sync")
}

#' @export
print.cttir_sync <- function(x, ...) {
  cat("Project synchronization: ", x$state, "\n", sep = "")
  cat(length(x$changed_files), "files changed;", length(x$conflicts), "conflicts\n")
  invisible(x)
}
