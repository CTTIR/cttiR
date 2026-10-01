update_sources <- function(sources, packages) {
  registry <- getOption("cttiR.sources", list())
  if (!is.list(registry)) abort_cttir("cttiR.sources must be a list of source records.")
  for (entry in registry) {
    if (!is.list(entry)) abort_cttir("Invalid source record.")
    for (key in c("id", "path")) scalar_text(entry[[key]], paste("source", key))
    if (!grepl("^[A-Za-z][A-Za-z0-9._-]*$", entry$id)) abort_cttir("Invalid registry source ID.")
  }
  ids <- vapply(registry, function(x) x$id, character(1))
  if (anyDuplicated(ids)) abort_cttir("Duplicate source registry IDs.")
  if (!is.null(sources)) {
    if (!is.character(sources) || !length(sources) || anyNA(sources) || any(!sources %in% ids)) {
      abort_cttir("sources must select registered source IDs.")
    }
    registry <- registry[ids %in% sources]
  }
  if (!is.null(packages) && (!is.character(packages) || !length(packages) || anyNA(packages) || any(!nzchar(packages)))) {
    abort_cttir("packages must contain exact package names.")
  }
  result <- lapply(registry, function(x) {
    entry <- extract_source(x$path, paste0("local-source:", x$id), "local", "configured_local")
    entry$revision <- paste0("local-", entry$source_hash)
    entry$freshness <- "remote_currency_unknown"
    entry
  })
  names <- vapply(result, function(x) x$name, character(1))
  if (anyDuplicated(names)) abort_cttir("Multiple source records resolve to the same package.")
  if (!is.null(packages)) {
    if (any(!packages %in% names)) abort_cttir("A selected package has no available registered local source.", "cttir_source_unavailable")
    result <- result[names %in% packages]
  }
  result
}

api_diff <- function(before, after) {
  out <- data.frame(package = character(), change = character(), symbol = character(), breaking = logical())
  old <- stats::setNames(before$packages, vapply(before$packages, function(x) x$name, character(1)))
  new <- stats::setNames(after$packages, vapply(after$packages, function(x) x$name, character(1)))
  for (name in union(names(old), names(new))) {
    a <- old[[name]]
    b <- new[[name]]
    if (identical(a, b)) next
    if (is.null(a) || is.null(b)) {
      out[nrow(out) + 1L, ] <- list(name, if (is.null(a)) "package_added" else "package_removed", "", is.null(b))
      next
    }
    out[nrow(out) + 1L, ] <- list(name, "revision_changed", "", TRUE)
    a_exports <- stats::setNames(a$exports, vapply(a$exports, function(x) x$name, character(1)))
    b_exports <- stats::setNames(b$exports, vapply(b$exports, function(x) x$name, character(1)))
    for (symbol in union(names(a_exports), names(b_exports))) {
      x <- a_exports[[symbol]]
      y <- b_exports[[symbol]]
      change <- if (is.null(x)) "export_added" else if (is.null(y)) "export_removed" else if (!identical(x, y)) "export_changed" else NULL
      if (!is.null(change)) out[nrow(out) + 1L, ] <- list(name, change, symbol, change != "export_added")
    }
  }
  out
}

refresh_resource_observations <- function(file, selected) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA foreign_keys=ON")
  DBI::dbWithTransaction(con, {
    for (entry in selected) {
      id <- DBI::dbGetQuery(con, "SELECT package_id FROM packages WHERE name = ?", params = list(entry$name))$package_id
      if (!length(id)) next
      observation <- paste0("local:", id)
      previous <- DBI::dbGetQuery(con, "SELECT source_sha256 FROM observations WHERE observation_id = ?", params = list(observation))$source_sha256
      if (length(previous) && identical(previous, entry$source_hash)) next
      DBI::dbExecute(con, "DELETE FROM observations WHERE observation_id = ?", params = list(observation))
      DBI::dbExecute(con, paste(
        "INSERT INTO observations (observation_id, package_id, repository, observed_version, title, license,",
        "source_url, documentation_url, observed_at, fetch_status, source_sha256, freshness)",
        "VALUES (?, ?, 'Local', ?, ?, ?, ?, ?, ?, 'local_source_read', ?, 'remote_currency_unknown')"
      ), params = list(
        observation, id, entry$version, entry$title, entry$license,
        entry$repository, paste0(entry$repository, "#DESCRIPTION"), format(Sys.time(), tz = "UTC", usetz = TRUE), entry$source_hash
      ))
    }
  })
  if (!identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]], "ok") || nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check"))) {
    abort_cttir("Candidate resource integrity checks failed.", "cttir_catalog_corrupt")
  }
  invisible(file)
}

#' Refresh explicitly registered local package sources
#'
#' Builds complete immutable API and resource snapshots before one atomic pointer
#' activation. Never installs packages, executes source code, changes project pins
#' or promotes workflow approvals. Removed exports do not fall back to bundled
#' revisions. Failed extraction preserves the previous active catalog.
#' @param sources Optional vector of registered source IDs.
#' @param packages Optional vector of exact package names within selected sources.
#' @param mode `local` reads configured sources without network access. `remote`
#'   is reserved and currently fails before any mutation.
#' @param dry_run Compute the candidate and diff in disposable temporary storage.
#' @param include_embeddings Request derived embeddings. No embedding backend is
#'   yet configured; lexical search remains usable with a warning.
#' @param prune Retention pruning is not yet supported and is explicitly rejected.
#' @param catalogs Nonempty subset of `knowledge` and `resources`.
#' @param discover Discovery is not yet supported and is explicitly rejected.
#' @param bioc_version Release migration is not supported by the local updater.
#' @details Register trusted local source directories with
#'   `options(cttiR.sources = list(list(id = "local-example", path = source_dir)))`.
#'   Sources are parsed statically and assigned a content-derived local revision,
#'   including same-version edits. No checkout is modified. Local observations
#'   preserve curated resource fields and never claim remote currency. Packages
#'   absent from the resource registry are indexed as APIs only, without discovery.
#' @return A `cttir_update` with previous/new composite IDs, per-catalog state,
#'   source identities, conservative API diff, activation and warnings.
#' @export
update <- function(
  sources = NULL, packages = NULL, mode = c("local", "remote"),
  dry_run = FALSE, include_embeddings = FALSE, prune = FALSE,
  catalogs = c("knowledge", "resources"), discover = FALSE, bioc_version = NULL
) {
  mode <- match.arg(mode)
  for (key in c("dry_run", "include_embeddings", "prune", "discover")) scalar_flag(get(key), key)
  if (!is.character(catalogs) || !length(catalogs) || anyNA(catalogs) || any(!catalogs %in% c("knowledge", "resources"))) {
    abort_cttir("catalogs must select knowledge and/or resources.")
  }
  if (mode != "local" || prune || discover || !is.null(bioc_version)) {
    abort_cttir("This updater supports local registered sources without pruning, discovery or release migration.", "cttir_source_unavailable", "unsupported_update_policy")
  }
  root <- catalog_store()
  if (!dry_run) {
    lock <- catalog_lock(root)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  }
  before <- resolve_catalog()
  old_resources <- resource_snapshot()
  previous <- snapshot_manifest(before$content_id, old_resources$id)
  selected <- update_sources(sources, packages)
  after <- before
  if ("knowledge" %in% catalogs) {
    indexed <- stats::setNames(before$packages, vapply(before$packages, function(x) x$name, character(1)))
    for (entry in selected) indexed[[entry$name]] <- entry
    after$packages <- unname(indexed)
  }
  stage <- tempfile("cttir-catalog-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  api_file <- file.path(stage, "api-catalog.json.gz")
  if (identical(after, before)) {
    id <- before$content_id
  } else {
    id <- write_catalog(after$packages, api_file, after$inventory)
    after <- read_catalog(api_file)
  }
  resource_file <- file.path(stage, "package-resources.sqlite")
  if (!file.copy(old_resources$file, resource_file)) abort_cttir("Could not stage resource database.", "cttir_transaction_conflict")
  if ("resources" %in% catalogs) refresh_resource_observations(resource_file, selected)
  resource_hash <- digest::digest(file = resource_file, algo = "sha256")
  resource_id <- if (identical(resource_hash, old_resources$sha256)) old_resources$id else resource_hash
  manifest <- snapshot_manifest(id, resource_id)
  changed <- !identical(previous, manifest)
  warnings <- "Remote currency was not checked; workflow approvals are not granted by extraction."
  if (!length(selected)) warnings <- c(warnings, "No local source records are configured or selected.")
  if (include_embeddings) warnings <- c(warnings, "Embedding backend unavailable; lexical catalog retained.")
  if (changed && !dry_run) {
    retain_api_snapshot(before, root)
    retain_api_snapshot(after, root)
    if (!identical(resource_id, old_resources$id)) {
      target <- file.path(root, "resource-snapshots", resource_id)
      assert_plain_path(target)
      if (!dir.exists(target)) {
        dir.create(target, recursive = TRUE)
        if (!file.copy(resource_file, file.path(target, "package-resources.sqlite"))) {
          abort_cttir("Could not retain candidate resources.", "cttir_transaction_conflict")
        }
      }
      if (!identical(digest::digest(file = file.path(target, "package-resources.sqlite"), algo = "sha256"), resource_hash)) {
        abort_cttir("Retained resource snapshot is corrupt.", "cttir_catalog_corrupt")
      }
    }
    activate_catalog(manifest, previous)
  }
  structure(list(
    status = if (dry_run) "planned" else if (changed) "succeeded" else "unchanged",
    previous_id = previous$manifest_id, new_id = manifest$manifest_id,
    catalogs = list(
      knowledge = list(previous = before$content_id, current = id),
      resources = list(previous = old_resources$id, current = resource_id)
    ),
    sources = lapply(selected, function(x) list(package = x$name, revision = x$revision, status = "static_extracted")),
    api_diff = api_diff(before, after), activation = changed && !dry_run, warnings = warnings
  ), class = "cttir_update")
}

#' @rdname update
#' @param ... Arguments forwarded unchanged to [update()].
#' @export
update_knowledge <- function(...) update(...)

#' Restore a retained composite catalog without changing project pins
#' @param version Composite snapshot ID returned by [update()].
#' @param dry_run Preview only; the default makes no changes.
#' @return A `cttir_update` containing IDs, API impact and activation outcome.
#' @export
rollback_knowledge <- function(version, dry_run = TRUE) {
  scalar_text(version, "version")
  scalar_flag(dry_run, "dry_run")
  if (!grepl("^[a-f0-9]{64}$", version)) abort_cttir("Invalid composite snapshot ID.")
  root <- catalog_store()
  if (!dry_run) {
    lock <- catalog_lock(root)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  }
  file <- file.path(root, "manifests", paste0(version, ".json"))
  assert_plain_path(file)
  target <- validate_manifest(read_document(file))
  if (!identical(target$manifest_id, version)) abort_cttir("Snapshot ID mismatch.", "cttir_catalog_corrupt")
  before <- resolve_catalog()
  after <- catalog_snapshot(target$content_id)
  previous <- snapshot_manifest(before$content_id, resource_snapshot()$id)
  base_resource <- read_document(resource_file("extdata", "resource-manifest.json"))$content_id
  if (!identical(target$resource_id, base_resource)) {
    resource <- file.path(root, "resource-snapshots", target$resource_id, "package-resources.sqlite")
    assert_plain_path(resource)
    if (!file.exists(resource) || !identical(digest::digest(file = resource, algo = "sha256"), target$resource_id)) {
      abort_cttir("Rollback resource snapshot is missing or corrupt.", "cttir_catalog_corrupt")
    }
  }
  changed <- !identical(previous, target)
  if (changed && !dry_run) activate_catalog(target, previous)
  structure(list(
    status = if (dry_run) "planned" else if (changed) "succeeded" else "unchanged",
    previous_id = previous$manifest_id, new_id = target$manifest_id,
    api_diff = api_diff(before, after), activation = changed && !dry_run,
    warnings = "Existing project pins and installed packages are unchanged."
  ), class = "cttir_update")
}

#' @export
print.cttir_update <- function(x, ...) {
  cat("Catalog update: ", x$status, "\n", sep = "")
  cat("Active pointer changed: ", x$activation, "\n", sep = "")
  invisible(x)
}
