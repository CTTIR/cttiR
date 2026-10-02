update_sources <- function(sources, packages, mode = "local") {
  registry <- getOption("cttiR.sources", list())
  if (!is.list(registry)) abort_cttir("cttiR.sources must be a list of source records.")
  for (entry in registry) {
    if (!is.list(entry)) abort_cttir("Invalid source record.")
    scalar_text(entry$id, "source id")
    if (!is.null(entry$package)) scalar_text(entry$package, "source package")
    if (!is.null(entry$r_distribution)) {
      scalar_text(entry$r_distribution, "R distribution path")
      scalar_text(entry$package, "source package")
      if (!is.null(entry$path) || !is.null(entry$github)) abort_cttir("A distribution source cannot declare another location.")
    } else if (is.null(entry$github)) {
      scalar_text(entry$path, "source path")
    } else {
      scalar_text(entry$github, "source github")
      scalar_text(entry$package, "source package")
      if (!is.null(entry$path)) abort_cttir("A source cannot declare both local and remote locations.")
    }
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
    if (!is.null(packages) && !is.null(x$package) && !x$package %in% packages) return(NULL)
    if (!is.null(x$github)) {
      if (!is.null(packages) && !x$package %in% packages) return(NULL)
      if (mode != "remote") {
        if (!is.null(sources) || !is.null(packages)) abort_cttir("A selected remote source requires mode = 'remote'.", "cttir_source_unavailable")
        return(NULL)
      }
      entry <- github_source(x)
      if (!identical(entry$name, x$package)) abort_cttir("Remote package identity differs from its registration.", "cttir_source_unavailable")
      return(entry)
    }
    if (!is.null(x$r_distribution)) {
      entry <- r_distribution_source(x)
    } else {
      entry <- extract_source(x$path, paste0("local-source:", x$id), "local", "configured_local", x$documentation_rights)
    }
    if (!is.null(x$package) && !identical(entry$name, x$package)) {
      abort_cttir("Local package identity differs from its registration.", "cttir_source_unavailable")
    }
    entry$revision <- paste0("local-", entry$source_hash)
    entry$freshness <- "remote_currency_unknown"
    entry
  })
  result <- Filter(Negate(is.null), result)
  result <- lapply(result, attach_approvals, decisions = approval_decisions())
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
    a <- without_approvals(old[[name]])
    b <- without_approvals(new[[name]])
    if (identical(a, b)) next
    if (is.null(a) || is.null(b)) {
      out[nrow(out) + 1L, ] <- list(name, if (is.null(a)) "package_added" else "package_removed", "", is.null(b))
      next
    }
    out[nrow(out) + 1L, ] <- list(name, "revision_changed", "", TRUE)
    for (declaration in union(unlist(a$methods), unlist(b$methods))) {
      if (!declaration %in% unlist(a$methods)) {
        out[nrow(out) + 1L, ] <- list(name, "method_declaration_added", declaration, FALSE)
      } else if (!declaration %in% unlist(b$methods)) {
        out[nrow(out) + 1L, ] <- list(name, "method_declaration_removed", declaration, TRUE)
      }
    }
    if (!is.null(a$s3_methods) && !is.null(b$s3_methods)) {
      for (method in a$s3_methods) {
        matches <- Filter(function(x) identical(x$declaration, method$declaration), b$s3_methods)
        if (length(matches) == 1L && !identical(method, matches[[1]])) {
          out[nrow(out) + 1L, ] <- list(name, "method_evidence_changed", method$declaration, TRUE)
        }
      }
    }
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
      remote <- identical(entry$family, "configured_github")
      observation <- paste0(if (remote) "github:" else "local:", id)
      previous <- DBI::dbGetQuery(con, "SELECT source_sha256 FROM observations WHERE observation_id = ?", params = list(observation))$source_sha256
      if (length(previous) && identical(previous, entry$source_hash)) next
      DBI::dbExecute(con, "DELETE FROM observations WHERE observation_id = ?", params = list(observation))
      DBI::dbExecute(con, paste(
        "INSERT INTO observations (observation_id, package_id, repository, observed_version, title, license,",
        "source_url, documentation_url, observed_at, fetch_status, source_sha256, freshness)",
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ), params = list(
        observation, id, if (remote) "GitHub" else "Local", entry$version, entry$title, entry$license,
        entry$repository, paste0(entry$repository, "#DESCRIPTION"), format(Sys.time(), tz = "UTC", usetz = TRUE),
        if (remote) "public_commit_fetched" else "local_source_read", entry$source_hash, entry$freshness
      ))
    }
  })
  if (!identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]], "ok") || nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check"))) {
    abort_cttir("Candidate resource integrity checks failed.", "cttir_catalog_corrupt")
  }
  invisible(file)
}

#' Refresh explicitly registered package sources
#'
#' Builds complete immutable API and resource snapshots before one atomic pointer
#' activation. Never installs packages, executes source code, changes project pins
#' or promotes workflow approvals. Removed exports do not fall back to bundled
#' revisions. Failed extraction preserves the previous active catalog.
#' @param sources Optional vector of registered source IDs.
#' @param packages Optional vector of exact package names within selected sources.
#' @param mode `local` reads configured local sources without network access.
#'   `remote` also fetches registered public GitHub repositories at one resolved
#'   commit, verifying each file against its Git blob hash.
#' @param dry_run Compute the candidate and diff in disposable temporary storage.
#' @param include_embeddings Request derived embeddings. No embedding backend is
#'   yet configured; lexical search remains usable with a warning.
#' @param prune Retention pruning is not yet supported and is explicitly rejected.
#' @param catalogs Nonempty subset of `knowledge` and `resources`.
#' @param discover Discovery is not yet supported and is explicitly rejected.
#' @param bioc_version Release migration is not supported by the local updater.
#' @details Register trusted local source directories with
#'   `options(cttiR.sources = list(list(id = "local-example", path = source_dir)))`.
#'   For public GitHub sources, replace `path` with `github = "owner/repository"`
#'   and `package = "ExpectedPackageName"`; optional `ref` selects a commit,
#'   tag or branch (default `HEAD`), and `subdir` selects a nested package root.
#'   Local mode skips remote registrations unless explicitly selected, in which
#'   case it reports that remote mode is required. Remote mode still reads local
#'   registrations locally. Redirects and private authentication are not used.
#'   Remote fetches allow at most 5000 tree entries, 250 selected files, 20 MB
#'   total, and one MB per file. A two-minute source budget is checked between
#'   requests, each of which has a 30-second timeout. Truncated trees fail closed.
#'   A newly added resource observation records its actual observation time, so
#'   its preview and applied composite IDs can differ even when API content is
#'   identical. Repeating an already applied unchanged source retains its ID.
#'   Applied updates can recover a stopped local writer lock only after checking
#'   its owner, the active snapshot and any activation journal. The old lock is
#'   retained under `recovered-locks`. Active or unknown writers, corrupt pointers
#'   and conflicting journals are refused. Preview never recovers locks. Recovery
#'   itself uses a guard; an interrupted recovery guard requires manual review.
#'   A local released R source tree can be registered with `r_distribution`
#'   (the directory containing `VERSION` and `COPYING`) and an exact `package`.
#'   This reads `src/library/<package>/DESCRIPTION.in`, replacing only the literal
#'   `@VERSION@` marker in memory. Original files are never changed or evaluated.
#'   Version/license hashes participate in source identity. Development versions,
#'   unknown substitutions and conflicting location fields are refused. This
#'   indexes the package subtree, not the distribution manuals or NEWS, and does
#'   not authenticate a local tree as canonical or approve a workflow.
#'   A source record may include `documentation_rights`, a nonempty description
#'   of the reviewed rights basis for storing that source's documentation text.
#'   Without it only document hashes and inventory are retained. This declaration
#'   must cover the selected documents, including any third-party material.
#'   Documentation is stored literally, never rendered or evaluated. Each text
#'   file is bounded to one megabyte and each package corpus to ten megabytes;
#'   binary assets and rendered `inst/doc` HTML/PDF up to 25 MB are hashed but
#'   never stored, and oversize renders are reported as such. Missing
#'   vignettes are reported as absent from the source, not proven unpublished.
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
  if (prune || discover || !is.null(bioc_version)) {
    abort_cttir("Pruning, discovery and release migration are not yet supported.", "cttir_source_unavailable", "unsupported_update_policy")
  }
  root <- catalog_store()
  if (!dry_run) {
    lock <- catalog_lock(root)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  }
  before <- resolve_catalog()
  old_resources <- resource_snapshot()
  previous <- snapshot_manifest(before$content_id, old_resources$id)
  selected <- update_sources(sources, packages, mode)
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
  warnings <- if (mode == "local") "Remote currency was not checked; workflow approvals are not granted by extraction." else "Only registered sources were checked; fetched revisions remain unapproved."
  if (!length(selected)) warnings <- c(warnings, "No available source records are configured or selected.")
  if (include_embeddings) warnings <- c(warnings, "Embedding backend unavailable; lexical catalog retained.")
  api_changes <- api_diff(before, after)
  doc_changes <- documentation_diff(before, after)
  approval_changes <- approval_diff(before, after)
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
    api_diff = api_changes, documentation_diff = doc_changes,
    approval_diff = approval_changes,
    activation = changed && !dry_run, warnings = warnings
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
  api_changes <- api_diff(before, after)
  doc_changes <- documentation_diff(before, after)
  approval_changes <- approval_diff(before, after)
  if (changed && !dry_run) activate_catalog(target, previous)
  structure(list(
    status = if (dry_run) "planned" else if (changed) "succeeded" else "unchanged",
    previous_id = previous$manifest_id, new_id = target$manifest_id,
    approval_diff = approval_changes,
    api_diff = api_changes, documentation_diff = doc_changes, activation = changed && !dry_run,
    warnings = "Existing project pins and installed packages are unchanged."
  ), class = "cttir_update")
}

#' @export
print.cttir_update <- function(x, ...) {
  cat("Catalog update: ", x$status, "\n", sep = "")
  cat("Active pointer changed: ", x$activation, "\n", sep = "")
  invisible(x)
}
