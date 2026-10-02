update_sources <- function(sources, packages, mode = "local", registry = getOption("cttiR.sources", list()),
  context = new_fetch_context(tempfile("cttir-fetch-"))) {
  if (!is.list(registry)) abort_cttir("cttiR.sources must be a list of source records.")
  for (entry in registry) {
    if (!is.list(entry)) abort_cttir("Invalid source record.")
    scalar_text(entry$id, "source id")
    if (!is.null(entry$package)) scalar_text(entry$package, "source package")
    if (!is.null(entry$optional)) scalar_flag(entry$optional, "source optional")
    if (!is.null(entry$cran) || !is.null(entry$bioc)) {
      validate_repository_record(entry)
    } else if (!is.null(entry$r_distribution)) {
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
    name <- registered_package(x)
    if (!is.null(packages) && !is.null(name) && !name %in% packages) return(NULL)
    if (!is.null(x$github) || !is.null(x$cran) || !is.null(x$bioc)) {
      if (mode != "remote") {
        if (!is.null(sources) || !is.null(packages)) abort_cttir("A selected remote source requires mode = 'remote'.", "cttir_source_unavailable")
        return(NULL)
      }
      entry <- remote_source(x, context)
      if (is.null(entry)) return(NULL)
      if (!identical(entry$name, name)) abort_cttir("Remote package identity differs from its registration.", "cttir_source_unavailable")
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
    unavailable <- vapply(context$report, function(x) if (identical(x$status, "unavailable_optional")) x$package else NA_character_, character(1))
    if (any(!packages %in% c(names, unavailable))) abort_cttir("A selected package has no available registered local source.", "cttir_source_unavailable")
    result <- result[names %in% packages]
  }
  result
}

within_freshness <- function(x) {
  x$freshness <- NULL
  x
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
    if (identical(within_freshness(a), within_freshness(b))) {
      out[nrow(out) + 1L, ] <- list(name, "freshness_changed", "", FALSE)
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
      if (!length(id) || entry$family %in% c("configured_cran", "configured_bioconductor")) next
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

retain_resource_snapshot <- function(file, root, id) {
  directory <- file.path(root, "resource-snapshots")
  target <- file.path(directory, id)
  assert_plain_path(target)
  if (!dir.exists(target)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    staged <- tempfile("staged-", tmpdir = directory)
    dir.create(staged)
    on.exit(unlink(staged, recursive = TRUE), add = TRUE)
    if (!file.copy(file, file.path(staged, "package-resources.sqlite")) ||
        !identical(digest::digest(file = file.path(staged, "package-resources.sqlite"), algo = "sha256"), id) ||
        !file.rename(staged, target)) {
      abort_cttir("Could not retain candidate resources.", "cttir_transaction_conflict")
    }
  }
  if (!identical(digest::digest(file = file.path(target, "package-resources.sqlite"), algo = "sha256"), id)) {
    abort_cttir("Retained resource snapshot is corrupt.", "cttir_catalog_corrupt")
  }
  invisible(target)
}

# Splits retired registry declarations from fetchable records and applies the
# caller's source/package selection to both.
registry_plan <- function(sources, packages, prune) {
  registry <- getOption("cttiR.sources", list())
  if (!is.list(registry)) abort_cttir("cttiR.sources must be a list of source records.")
  retired_flag <- vapply(registry, function(x) {
    if (!is.list(x)) abort_cttir("Invalid source record.")
    if (!is.null(x$retired)) scalar_flag(x$retired, "source retired")
    isTRUE(x$retired)
  }, logical(1))
  retired <- registry[retired_flag]
  active <- registry[!retired_flag]
  for (x in retired) {
    scalar_text(x$id, "source id")
    if (!grepl("^[A-Za-z][A-Za-z0-9._-]*$", x$id)) abort_cttir("Invalid registry source ID.")
    name <- registered_package(x)
    scalar_text(name, "retired source package")
    if (!is.null(x$retirement_evidence)) scalar_text(x$retirement_evidence, "retirement_evidence")
  }
  ids <- vapply(registry, function(x) if (is.character(x$id) && length(x$id) == 1L) x$id else NA_character_, character(1))
  retired_ids <- ids[retired_flag]
  retired_names <- vapply(retired, registered_package, character(1))
  active_names <- unlist(lapply(active, function(x) {
    name <- registered_package(x)
    if (is.character(name) && length(name) == 1L) name else NULL
  }))
  if (anyDuplicated(retired_ids) || any(retired_ids %in% ids[!retired_flag]) || anyDuplicated(retired_names) ||
      any(retired_names %in% active_names)) {
    abort_cttir("A retired source must have a unique ID and package that is not also actively registered.")
  }
  active_sources <- NULL
  if (!is.null(sources)) {
    if (!is.character(sources) || !length(sources) || anyNA(sources) || any(!sources %in% ids)) {
      abort_cttir("sources must select registered source IDs.")
    }
    retired <- retired[retired_ids %in% sources]
    active_sources <- setdiff(sources, retired_ids)
  }
  active_packages <- NULL
  if (!is.null(packages)) {
    if (!is.character(packages) || !length(packages) || anyNA(packages) || any(!nzchar(packages))) {
      abort_cttir("packages must contain exact package names.")
    }
    retired <- retired[vapply(retired, registered_package, character(1)) %in% packages]
    active_packages <- setdiff(packages, retired_names)
  }
  if (prune) {
    for (x in retired) {
      if (is.null(x$retirement_evidence)) {
        abort_cttir("Pruning requires nonempty retirement_evidence for every selected retired source.",
          "cttir_input_error", "missing_retirement_evidence", "retirement_evidence",
          "Record authoritative retirement evidence in the registry, or keep prune = FALSE.")
      }
    }
  }
  list(active = active, sources = active_sources, packages = active_packages, requested = active_packages, retired = retired,
    skip = (!is.null(sources) && !length(active_sources)) || (!is.null(packages) && !length(active_packages)))
}

# A resources-only run observes CRAN/Bioconductor registrations through the
# repository indices; their tarballs are only fetched for the knowledge catalog.
defer_repository_sources <- function(plan) {
  repository <- vapply(plan$active, function(x) !is.null(x$cran) || !is.null(x$bioc), logical(1))
  if (!any(repository)) return(plan)
  records <- plan$active[repository]
  for (x in records) {
    scalar_text(x$id, "source id")
    validate_repository_record(x)
  }
  ids <- vapply(records, function(x) x$id, character(1))
  names <- vapply(records, registered_package, character(1))
  plan$active <- plan$active[!repository]
  if (!is.null(plan$sources)) {
    plan$deferred <- names[ids %in% plan$sources]
    plan$sources <- setdiff(plan$sources, ids)
    plan$skip <- plan$skip || !length(plan$sources)
  }
  if (!is.null(plan$packages)) {
    plan$packages <- setdiff(plan$packages, names)
    plan$skip <- plan$skip || !length(plan$packages)
  }
  plan
}

#' Refresh explicitly registered package sources and resource metadata
#'
#' Builds complete immutable API and resource snapshots before one atomic pointer
#' activation. Never installs or upgrades packages, runs `install.packages()` or
#' `BiocManager`, executes downloaded or source code, changes project pins or
#' promotes workflow approvals. Removed exports do not fall back to bundled
#' revisions. A failed required source or catalog preserves both previous
#' catalogs; the typed error carries a failed `cttir_update` in `$report`.
#' @param sources Optional vector of registered source IDs.
#' @param packages Optional vector of exact package names within selected sources.
#' @param mode `local` reads configured local sources without any network access
#'   and marks remote currency unknown. `remote` also fetches registered public
#'   GitHub, CRAN and Bioconductor sources and refreshes resource observations
#'   from the official repository indices.
#' @param dry_run Compute the candidate and diff in disposable temporary storage.
#'   Remote previews download only into that storage and delete it afterwards.
#' @param include_embeddings Request derived embeddings. No embedding backend is
#'   yet configured; lexical search remains usable with a warning.
#' @param prune When `TRUE`, registry records declaring `retired = TRUE` with a
#'   nonempty `retirement_evidence` are removed from the new current API snapshot
#'   and recorded as tombstones. Without evidence the update is refused. When
#'   `FALSE` (default), retired or unavailable packages stay current with a stale
#'   freshness flag.
#' @param catalogs Nonempty subset of `knowledge` and `resources`.
#' @param discover When `TRUE` (remote mode only), adds a bounded number of new
#'   quarantined `discovered_candidate` rows matching configured keywords.
#' @param bioc_version Optional exact Bioconductor release such as `"3.23"`.
#'   `NULL` keeps the catalog-store policy or derives the release compatible with
#'   the running R. Aliases such as `release`/`devel` and unknown releases fail.
#' @details
#' **Registry.** Register trusted sources with `options(cttiR.sources = list(...))`.
#' Each record has an `id` and exactly one location:
#' * `path`: a trusted local package source directory.
#' * `github = "owner/repository"` with `package`; optional `ref` (commit, tag or
#'   branch, default `HEAD`) and `subdir`. Fetches resolve one commit and verify
#'   each file against its Git blob hash. At most 5000 tree entries, 250 files,
#'   20 MB in total and one MB per file; 30-second request timeouts and a
#'   two-minute source budget. Truncated trees fail closed.
#' * `cran = "pkg"`, optional exact `version` and `md5`. The current version is
#'   fetched from `https://cloud.r-project.org/src/contrib/<pkg>_<ver>.tar.gz`
#'   and verified against the `MD5sum` in `https://cloud.r-project.org/src/contrib/PACKAGES`.
#'   Other versions use `.../src/contrib/Archive/<pkg>/<pkg>_<ver>.tar.gz`; CRAN
#'   publishes no checksum for those, so a registered `md5` is verified when
#'   given and otherwise the locally computed MD5 is labelled as unverified.
#' * `bioc = "pkg"`, optional `bioc_version` and `version`: the tarball listed in
#'   `https://bioconductor.org/packages/<release>/bioc/src/contrib/PACKAGES`,
#'   verified against its `MD5sum`. An exact earlier version of the same release
#'   is fetched from that release's `Archive/` directory, verified against a
#'   registered `md5` when given and otherwise labelled unverified.
#' * `r_distribution`: a local released R source tree (directory containing
#'   `VERSION` and `COPYING`) with an exact `package`. This reads
#'   `src/library/<package>/DESCRIPTION.in`, replacing only the literal
#'   `@VERSION@` marker in memory; development versions are refused and the tree
#'   is not authenticated as canonical.
#'
#' MD5 values are repository integrity metadata, not publisher authenticity.
#' Repository fetches use only those fixed HTTPS endpoints, never follow
#' redirects or send credentials, time out after 60 seconds per request, and
#' bound each index to 50 MB and each tarball to 30 MB (120 MB uncompressed).
#' Archives are parsed in memory and fully listed first; absolute or parent
#' paths, links, devices, duplicate or out-of-package members refuse the whole
#' archive. Only DESCRIPTION, NAMESPACE, top-level `R/*.R`, `man`, vignettes,
#' `inst/doc`, `inst/CITATION` and top-level README/NEWS/licence files are
#' written to a temporary stage; documentation files above one MB are omitted
#' with their size and hash recorded, and oversize API files fail closed.
#'
#' Optional records may set `optional = TRUE`: an unavailable optional remote
#' source keeps its previous revision marked `source_unavailable` and the result
#' is `partial`; a required failure aborts before activation. A record may also
#' declare `retired = TRUE` with `retirement_evidence` (see `prune`). Local mode
#' skips remote registrations unless they are explicitly selected, in which case
#' it reports that remote mode is required.
#'
#' **Resources.** In remote mode the resource catalog reads the CRAN index and
#' the selected Bioconductor release software index once each per run and
#' refreshes observations of CRAN and Bioconductor software packages already in
#' the resource database (version, licence, R and dependency floors,
#' compilation flag, observation time and fetch status). Curated purpose, tiers,
#' notes and verification states are preserved. Absence from an index is
#' recorded as `missing_from_selected_index`, never as retirement. Version,
#' licence and dependency-floor changes and dual CRAN/Bioconductor listings with
#' different versions are reported in `resource_changes`. An unavailable index
#' fails the update unless declared optional via
#' `options(cttiR.resource_refresh = list(optional = c("cran", "bioconductor")))`;
#' its previous observations are then kept as `source_unavailable`. The same
#' option takes `exclude`, exact package names that are never refreshed or
#' discovered. Discovery uses `options(cttiR.discovery = list(keywords = ...,
#' biocViews = ..., limit = 25L))` (limit at most 100) against package names
#' and any Title/biocViews fields present in the fetched indices; the current
#' official PACKAGES files carry neither field, so matching is effectively by
#' name. Discovered rows are quarantined and never promoted. An explicit
#' `bioc_version` that differs from the stored policy stages a separate
#' observation set (release in each observation ID) and records the new policy
#' in the catalog store only after a successful applied run; project pins keep
#' their resource snapshots. Upstream-only, base R, historical Bioconductor and
#' data packages are not refreshed by index. A resources-only run observes CRAN
#' and Bioconductor registrations through the indices without downloading their
#' tarballs.
#'
#' **Not supported.** Discovery from Bioconductor VIEWS or package pages,
#' versions from other Bioconductor releases, alternate CRAN paths (such as recommended
#' package copies), private repositories, conditional HTTP requests, embeddings
#' and automatic promotion of candidates.
#'
#' Documentation text is stored only for records with `documentation_rights`, a
#' nonempty description of the reviewed rights basis covering all selected
#' documents; otherwise only hashes and inventory are kept. Documents are stored
#' literally, never rendered or evaluated (one MB per file, ten MB per package);
#' binary assets and rendered `inst/doc` HTML/PDF up to 25 MB are hashed but never
#' stored. Reviewed workflow approvals (see `inst/extdata/approvals.json` and
#' `options(cttiR.approvals)`) attach only to the exact extracted revision; the
#' result reports approvals added, removed or invalidated in `approval_diff`.
#' Missing vignettes are reported as absent from the source, not unpublished.
#' Sources are parsed statically; local sources receive a content-derived
#' revision, including same-version edits. Packages absent from the resource
#' registry are indexed as APIs only.
#'
#' Observation times are part of the resource snapshot, so a preview and its
#' applied run can have different composite IDs even when API content matches.
#' Repeating an unchanged update retains its ID. Applied updates can recover a
#' stopped local writer lock only after checking its owner, the active snapshot
#' and any activation journal; the old lock is retained under `recovered-locks`.
#' Active or unknown writers, corrupt pointers and conflicting journals are
#' refused, and previews never recover locks.
#' @return A `cttir_update` with `status` (`planned`, `unchanged`, `succeeded`,
#'   `partial`; failures raise with a `failed` report), previous/new composite
#'   IDs, per-catalog IDs, per-source status and report, repository index
#'   reports, the exact repository URLs requested, `resource_changes`, API and
#'   documentation diffs, new `tombstones`, the Bioconductor release policy,
#'   activation and warnings.
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
  if (!is.null(bioc_version)) validate_bioc_release(bioc_version)
  if (discover && !"resources" %in% catalogs) abort_cttir("Discovery requires the resources catalog.", field = "discover")
  if (mode != "remote" && (discover || !is.null(bioc_version))) {
    abort_cttir("Discovery and an explicit bioc_version require mode = 'remote'; local mode never contacts repositories.",
      "cttir_source_unavailable", "requires_remote_mode", if (discover) "discover" else "bioc_version")
  }
  resource_policy <- resource_refresh_policy()
  discovery <- if (discover) discovery_policy() else NULL
  plan <- registry_plan(sources, packages, prune)
  if (!"knowledge" %in% catalogs) plan <- defer_repository_sources(plan)
  root <- catalog_store()
  policy <- bioc_release_policy(bioc_version, root)
  if (!dry_run) {
    lock <- catalog_lock(root)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  }
  before <- resolve_catalog()
  old_resources <- resource_snapshot()
  previous <- snapshot_manifest(before$content_id, old_resources$id)
  stage <- tempfile("cttir-catalog-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  context <- new_fetch_context(file.path(stage, "fetch"), policy)
  activated <- FALSE
  tryCatch({
    selected <- if (plan$skip) list() else update_sources(plan$sources, plan$packages, mode, plan$active, context)
    unavailable <- Filter(function(x) identical(x$status, "unavailable_optional"), context$report)
    source_rows <- lapply(selected, function(x) list(package = x$name, revision = x$revision, status = "static_extracted"))
    for (x in unavailable) source_rows[[length(source_rows) + 1L]] <- list(package = x$package, revision = NA_character_, status = "unavailable_optional")
    tombstones <- list()
    after <- before
    if ("knowledge" %in% catalogs) {
      indexed <- stats::setNames(before$packages, vapply(before$packages, function(x) x$name, character(1)))
      for (entry in selected) indexed[[entry$name]] <- entry
      for (x in unavailable) {
        if (!is.null(indexed[[x$package]])) indexed[[x$package]]$freshness <- "source_unavailable"
      }
      for (record in plan$retired) {
        name <- registered_package(record)
        current <- indexed[[name]]
        status <- if (is.null(current)) "retired_absent" else if (prune) "retired_pruned" else "retired_not_pruned"
        if (!is.null(current) && prune) {
          tombstones[[length(tombstones) + 1L]] <- list(package = name, last_revision = current$revision,
            last_version = current$version, reason = "retired_by_registry", evidence = record$retirement_evidence,
            source_id = record$id, removed_at = utc_timestamp())
          indexed[[name]] <- NULL
        } else if (!is.null(current)) {
          indexed[[name]]$freshness <- "retirement_declared_not_pruned"
        }
        source_rows[[length(source_rows) + 1L]] <- list(package = name,
          revision = if (is.null(current)) NA_character_ else current$revision, status = status)
      }
      after$packages <- unname(indexed)
      if (length(tombstones)) after$tombstones <- c(before$tombstones, tombstones)
    }
    api_file <- file.path(stage, "api-catalog.json.gz")
    if (identical(after, before)) {
      id <- before$content_id
    } else {
      id <- write_catalog(after$packages, api_file, after$inventory, after$tombstones)
      after <- read_catalog(api_file)
    }
    resource_file <- file.path(stage, "package-resources.sqlite")
    if (!file.copy(old_resources$file, resource_file)) abort_cttir("Could not stage resource database.", "cttir_transaction_conflict")
    resource_result <- list(changes = empty_resource_changes(), partial = FALSE, unavailable = character())
    if ("resources" %in% catalogs) {
      refresh_resource_observations(resource_file, selected)
      if (mode == "remote") {
        targets <- if (!is.null(plan$requested)) {
          plan$requested
        } else if (!is.null(plan$sources) || plan$skip) {
          extracted <- vapply(selected, function(x) x$name, character(1))
          reported <- unlist(lapply(context$report, function(x) x$package))
          unique(c(extracted, reported, plan$deferred))
        }
        resource_result <- refresh_repository_resources(resource_file, context, policy$release, targets, discovery, resource_policy)
      }
    }
    resource_hash <- digest::digest(file = resource_file, algo = "sha256")
    resource_id <- if (identical(resource_hash, old_resources$sha256)) old_resources$id else resource_hash
    manifest <- snapshot_manifest(id, resource_id)
    changed <- !identical(previous, manifest)
    partial <- length(unavailable) > 0L || isTRUE(resource_result$partial)
    api_changes <- api_diff(before, after)
    doc_changes <- documentation_diff(before, after)
    approval_changes <- approval_diff(before, after)
    if (changed && !dry_run) {
      retain_api_snapshot(before, root)
      retain_api_snapshot(after, root)
      if (!identical(resource_id, old_resources$id)) retain_resource_snapshot(resource_file, root, resource_id)
      activate_catalog(manifest, previous)
      activated <- TRUE
    }
    recorded <- !dry_run && policy$changes_policy
    if (recorded) write_bioc_policy(bioc_version, root)
    warnings <- update_warnings(mode, selected, include_embeddings, unavailable, resource_result, tombstones,
      plan, prune, policy, discover, dry_run)
    structure(list(
      status = if (dry_run) "planned" else if (partial) "partial" else if (changed) "succeeded" else "unchanged",
      previous_id = previous$manifest_id, new_id = manifest$manifest_id,
      catalogs = list(
        knowledge = list(previous = before$content_id, current = id),
        resources = list(previous = old_resources$id, current = resource_id)
      ),
      sources = source_rows, source_report = unname(context$report),
      resource_sources = index_reports(context, resource_policy$optional), requests = context$calls,
      resource_changes = resource_result$changes,
      api_diff = api_changes, documentation_diff = doc_changes, approval_diff = approval_changes,
      tombstones = tombstones,
      bioc_release = list(release = policy$release, source = policy$source, r_minor = policy$r_minor,
        running_r = policy$running_r, compatible = policy$compatible, recorded = recorded),
      partial = partial, activation = changed && !dry_run, warnings = warnings
    ), class = "cttir_update")
  }, cttir_error = function(e) {
    e$report <- structure(list(
      status = "failed", previous_id = previous$manifest_id, new_id = NA_character_,
      sources = unname(context$report), source_report = unname(context$report),
      resource_sources = index_reports(context, resource_policy$optional), requests = context$calls,
      activation = activated,
      warnings = if (activated) "Activation completed before a later step failed; inspect the report." else
        "No catalog was activated; the previous API and resource snapshots remain active."
    ), class = "cttir_update")
    stop(e)
  })
}

update_warnings <- function(mode, selected, include_embeddings, unavailable, resource_result, tombstones,
  plan, prune, policy, discover, dry_run) {
  warnings <- if (mode == "local") "Remote currency was not checked; workflow approvals are not granted by extraction." else "Only registered sources and existing resource entries were checked; fetched revisions remain unapproved."
  if (!length(selected)) warnings <- c(warnings, "No available source records are configured or selected.")
  if (include_embeddings) warnings <- c(warnings, "Embedding backend unavailable; lexical catalog retained.")
  for (x in unavailable) {
    warnings <- c(warnings, paste0("Optional source '", x$id, "' was unavailable; its previous revision is retained and marked stale."))
  }
  for (family in resource_result$unavailable) {
    warnings <- c(warnings, paste0("Optional ", family, " index was unavailable; previous observations are retained and marked stale."))
  }
  impact_types <- c("version_changed", "license_changed", "dependency_floor_increased", "repository_conflict", "missing_from_selected_index")
  impacts <- resource_result$changes[resource_result$changes$change %in% impact_types, , drop = FALSE]
  if (nrow(impacts)) warnings <- c(warnings, paste0(nrow(impacts), " resource impact warnings; see resource_changes. Project pins are unchanged."))
  if (length(tombstones)) {
    warnings <- c(warnings, paste0(length(tombstones), " retired package(s) were removed from the new current snapshot with tombstones; historical snapshots and project pins are unchanged."))
  }
  if (!prune && length(plan$retired)) warnings <- c(warnings, "Retired registry packages were kept with a stale flag because prune = FALSE.")
  for (entry in selected) {
    if (identical(entry$archive$checksum_source, "none_published")) {
      warnings <- c(warnings, paste0("No repository checksum is published for ", entry$name, " ", entry$version, "; integrity is unverified."))
    }
    if (length(entry$archive$omitted_documents)) {
      warnings <- c(warnings, paste0(length(entry$archive$omitted_documents), " oversize documentation file(s) of ", entry$name, " were omitted and recorded."))
    }
  }
  if (mode == "remote" && !is.na(policy$release) && !policy$compatible) {
    message <- paste0("Bioconductor ", policy$release, " targets R ", policy$r_minor, " but R ", policy$running_r,
      " is running; observations are metadata only and no environment change is made.")
    warnings <- c(warnings, message)
  }
  if (discover) warnings <- c(warnings, "Discovered candidates are quarantined: unreviewed, API-unverified and never installed or promoted.")
  if (dry_run && mode == "remote") warnings <- c(warnings, "Remote preview downloads were disposable and have been deleted.")
  warnings
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
  if (is.data.frame(x$resource_changes) && nrow(x$resource_changes)) {
    cat("Resource changes: ", nrow(x$resource_changes), "\n", sep = "")
  }
  if (length(x$tombstones)) cat("Tombstones: ", length(x$tombstones), "\n", sep = "")
  invisible(x)
}
