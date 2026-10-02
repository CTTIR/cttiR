# Built-in audit check definitions. See R/audit.R for the registry contract.

audit_contract_exports <- c(
  "project", "setup", "doctor", "update", "update_knowledge", "ask", "search", "packages",
  "resources", "setup_app", "configure", "sync", "audit", "rollback_knowledge",
  "validate_spec", "validate_config"
)

audit_private_paths <- c("data/raw", "data/interim", "data/processed", ".cttir/local.yml", "administration/private")

# Installation -----------------------------------------------------------------

audit_checks_installation <- function() {
  list(
    audit_check("INS-001", "installation",
      "Package version, contract exports, bundled resources and declared Imports are available.",
      audit_ins_package, required = TRUE, read_effects = c("reads_installation", "loads_namespaces"),
      evidence_schema = c("version", "exports", "missing_exports", "missing_resources", "imports", "unavailable_imports")),
    audit_check("INS-002", "installation",
      "Owned local runtime executable, process and model metadata (no inference request).",
      audit_ins_runtime, required = audit_requires_local_model, severity = "warning",
      read_effects = "reads_runtime_metadata", evidence_schema = c("endpoint", "model")),
    audit_check("INS-003", "installation",
      "Local-only model policy is verified from owned runtime state, or reported unknown.",
      audit_ins_locality, required = audit_requires_local_model, severity = "warning",
      read_effects = c("reads_runtime_metadata", "local_http_metadata"),
      evidence_schema = c("recorded_locality", "process_verified", "model_metadata")),
    audit_check("INS-004", "installation",
      "Configured catalog and runtime storage paths: type, links and permission metadata.",
      audit_ins_storage, required = FALSE, read_effects = "reads_storage_metadata",
      evidence_schema = c("catalog", "runtime"))
  )
}

audit_ins_package <- function(context) {
  version <- as.character(getNamespaceVersion("cttiR"))
  exports <- getNamespaceExports("cttiR")
  missing_exports <- setdiff(audit_contract_exports, exports)
  resources <- list(
    c("schema", "project-spec.schema.json"), c("schema", "config.schema.json"),
    c("extdata", "api-catalog.json.gz"), c("extdata", "package-resources.sqlite"),
    c("extdata", "resource-manifest.json"), c("extdata", "file-hashes.json"),
    c("runtime", "manifest.json"), c("templates", "reflowr-0.2.0", "manifest.json")
  )
  present <- vapply(resources, function(x) !inherits(tryCatch(do.call(resource_file, as.list(x)), error = function(e) e), "error"), logical(1))
  missing_resources <- vapply(resources[!present], paste, character(1), collapse = "/")
  imports <- audit_declared_imports()
  unavailable <- character()
  versions <- list()
  if (!is.null(imports)) {
    available <- vapply(imports, requireNamespace, logical(1), quietly = TRUE)
    unavailable <- imports[!available]
    versions <- lapply(imports[available], function(x) as.character(getNamespaceVersion(x)))
    names(versions) <- imports[available]
  }
  evidence <- list(version = version, exports = length(exports), missing_exports = missing_exports,
    missing_resources = missing_resources, imports = versions, unavailable_imports = unavailable)
  if (length(missing_exports) || length(missing_resources) || length(unavailable)) {
    message <- paste0("cttiR ", version, " is incomplete: ",
      length(missing_exports), " contract exports, ", length(missing_resources), " bundled resources and ",
      length(unavailable), " declared Imports are unavailable.")
    return(audit_result("fail", message, evidence))
  }
  if (is.null(imports)) {
    return(audit_result("not_tested", paste0("cttiR ", version, " exports and resources are present; declared Imports could not be read."), evidence))
  }
  message <- paste0("cttiR ", version, ": contract exports, bundled resources and ",
    length(imports), " declared Imports are available.")
  audit_result("pass", message, evidence)
}

audit_declared_imports <- function() {
  description <- suppressWarnings(tryCatch(utils::packageDescription("cttiR"), error = function(e) NULL))
  if (!is.list(description) || is.null(description$Imports)) return(NULL)
  imports <- trimws(strsplit(gsub("\\([^)]*\\)", "", description$Imports), ",", fixed = TRUE)[[1]])
  imports[nzchar(imports)]
}

audit_ins_runtime <- function(context) {
  owner <- tryCatch(runtime_owner(runtime_directory(), runtime_endpoint()), error = function(e) NULL)
  if (is.null(owner)) {
    return(audit_result("not_tested", "No verified owned runtime; offline project creation remains available."))
  }
  audit_result("pass", "Owned local runtime identity verified without an inference request.",
    list(endpoint = owner$endpoint, model = owner$model))
}

audit_ins_locality <- function(context) {
  root <- runtime_directory()
  file <- file.path(root, "runtime-state.json")
  assert_plain_path(file)
  if (!file.exists(file)) {
    evidence <- list(recorded_locality = NULL, process_verified = FALSE)
    return(audit_result("not_tested", "No owned runtime state is recorded; model locality is unknown.", evidence))
  }
  state <- read_document(file)
  if (!identical(state$locality, "managed_cloud_disabled")) {
    evidence <- list(recorded_locality = state$locality, process_verified = FALSE)
    return(audit_result("fail", "The recorded runtime policy does not disable cloud execution.", evidence))
  }
  endpoint <- runtime_endpoint()
  owner <- runtime_owner(root, endpoint)
  if (is.null(owner)) {
    message <- paste("The recorded policy disables cloud execution, but no verified owned runtime is running,",
      "so the process environment is unverified.")
    return(audit_result("not_tested", message, list(recorded_locality = state$locality, process_verified = FALSE)))
  }
  if (!context$live || is.null(owner$model)) {
    message <- "The owned runtime runs with cloud execution disabled on a loopback endpoint; model metadata was not queried."
    evidence <- list(recorded_locality = state$locality, process_verified = TRUE, model_metadata = "not_queried")
    return(audit_result("pass", message, evidence))
  }
  entry <- local_model(endpoint, owner$model)
  if (is.null(entry) || !identical(entry$digest, owner$model_digest)) {
    evidence <- list(recorded_locality = state$locality, process_verified = TRUE, model_metadata = "mismatch")
    return(audit_result("fail", "The configured local model is absent or its digest changed.", evidence))
  }
  audit_result("pass", "The owned runtime disables cloud execution and reports a local downloaded model.",
    list(recorded_locality = state$locality, process_verified = TRUE, model_metadata = "local_verified"))
}

audit_storage_metadata <- function(get_path) {
  path <- tryCatch(get_path(), error = function(e) e)
  if (inherits(path, "error")) {
    return(list(status = "fail", note = audit_condition_message(path)))
  }
  info <- list(path = path, exists = file.exists(path))
  if (info$exists && !dir.exists(path)) {
    return(c(info, status = "fail", note = "The configured storage path is a file, not a directory."))
  }
  probe <- path
  while (!file.exists(probe)) {
    parent <- dirname(probe)
    if (identical(parent, probe)) break
    probe <- parent
  }
  mode <- file.info(probe)$mode
  info$checked <- if (identical(probe, path)) "path" else "nearest_existing_parent"
  info$writable <- isTRUE(unname(file.access(probe, 2L) == 0L))
  info$mode <- if (is.na(mode)) NA_character_ else format(mode)
  info$world_writable <- info$exists && !is.na(mode) && bitwAnd(as.integer(mode), 2L) != 0L
  status <- if (!info$writable || info$world_writable) "warning" else "pass"
  note <- if (!info$writable) {
    "The storage location is not writable; updates or setup cannot persist changes."
  } else if (info$world_writable) {
    "The storage directory is world-writable; other accounts could alter it."
  } else if (info$exists) {
    "Existing directory with write permission."
  } else {
    "Absent; it will be created under a writable parent on first explicit write."
  }
  c(info, status = status, note = note)
}

audit_ins_storage <- function(context) {
  entries <- list(catalog = audit_storage_metadata(catalog_store), runtime = audit_storage_metadata(runtime_directory))
  statuses <- vapply(entries, function(x) x$status, character(1))
  status <- if (any(statuses == "fail")) "fail" else if (any(statuses == "warning")) "warning" else "pass"
  message <- paste0("Catalog storage: ", entries$catalog$note, " Runtime storage: ", entries$runtime$note,
    " Only metadata was inspected; no write probe was performed.")
  audit_result(status, message, entries)
}

# Knowledge --------------------------------------------------------------------

audit_checks_knowledge <- function() {
  list(
    audit_check("KB-001", "knowledge",
      "Bundled, active and pinned catalog hashes, identities and schemas.",
      audit_kb_hashes, required = TRUE, read_effects = c("reads_installation", "reads_catalog_store", "reads_project_metadata"),
      repair_id = "repoint_active_catalog",
      evidence_schema = c("bundled_catalog", "history", "active", "pinned", "problems")),
    audit_check("KB-002", "knowledge",
      "Resource database integrity/foreign keys and catalog reference consistency for the selected revisions.",
      audit_kb_references, required = TRUE, read_effects = c("reads_installation", "reads_catalog_store", "reads_project_metadata"),
      evidence_schema = c("resources", "catalogs", "retained_manifests")),
    audit_check("KB-003", "knowledge",
      "Registered source coverage in the active catalog, freshness and last update result.",
      audit_kb_registry, required = FALSE, read_effects = "reads_catalog_store",
      evidence_schema = c("configured", "indexed", "missing", "freshness", "last_activation", "last_update_result")),
    audit_check("KB-004", "knowledge",
      "Export/signature evidence and installed-versus-cataloged version agreement.",
      function(context) audit_kb_evidence(context), required = function(context) audit_readiness_at_least(context, "analysis_ready"),
      severity = "warning", read_effects = c("reads_catalog_store", "reads_installation"),
      evidence_schema = c("packages", "exports", "resolved", "documented", "mismatches")),
    audit_check("KB-005", "knowledge",
      "Documentation corpus integrity, removed-export tombstones and no cross-revision fallback.",
      audit_kb_tombstones, required = TRUE, read_effects = c("reads_installation", "reads_catalog_store", "reads_project_metadata"),
      evidence_schema = c("packages", "tombstones", "retained_snapshots", "fallback")),
    audit_check("RES-001", "knowledge",
      "Bundled resource database queries, integrity and foreign keys.",
      audit_res_bundled, required = TRUE, read_effects = c("reads_installation", "reads_catalog_store")),
    audit_check("RES-002", "knowledge",
      "Resource observation dates and unknown remote freshness.",
      function(context) audit_result("warning", "Resource observations are dated; remote freshness was not checked."),
      required = FALSE, read_effects = "reads_installation")
  )
}

audit_pointer_problem <- function(root = catalog_store()) {
  active <- file.path(root, "active.json")
  assert_plain_path(active)
  if (!file.exists(active)) return(NULL)
  tryCatch(
    {
      store_manifest_verified(current_catalog_manifest(), root)
      resolve_catalog()
      NULL
    },
    error = function(e) if (inherits(e, "cttir_error")) conditionMessage(e) else "The active catalog pointer could not be read."
  )
}

store_manifest_verified <- function(manifest, root = catalog_store()) {
  manifest <- validate_manifest(manifest)
  catalog <- read_catalog(file.path(root, "snapshots", manifest$content_id, "api-catalog.json.gz"))
  if (!identical(catalog$content_id, manifest$content_id)) {
    abort_cttir("Retained snapshot identity mismatch.", "cttir_catalog_corrupt")
  }
  validate_snapshot_content(manifest)
  invisible(manifest)
}

audit_kb_hashes <- function(context) {
  problems <- character()
  attempt <- function(label, expr) {
    tryCatch(expr, error = function(e) {
      problems <<- c(problems, paste0(label, ": ", audit_condition_message(e)))
      NULL
    })
  }
  evidence <- list()
  attempt("Bundled hashes", {
    hashes <- read_document(resource_file("extdata", "file-hashes.json"))
    for (name in names(hashes)) {
      relative_file(name)
      if (!identical(digest::digest(file = resource_file("extdata", name), algo = "sha256"), hashes[[name]])) {
        problems <- c(problems, paste("Bundled", name, "failed its recorded hash."))
      }
    }
  })
  evidence$bundled_catalog <- attempt("Bundled API catalog", read_catalog(resource_file("extdata", "api-catalog.json.gz"))$content_id)
  attempt("Schemas", for (kind in c("project-spec", "config", "figure-policy")) {
    jsonlite::fromJSON(resource_file("schema", paste0(kind, ".schema.json")), simplifyVector = FALSE)
  })
  history <- system.file("extdata", "history", package = "cttiR")
  evidence$history <- character()
  if (nzchar(history)) {
    for (file in list.files(history, "^[a-f0-9]{64}[.]json[.]gz$", full.names = TRUE)) {
      id <- attempt("Historical snapshot", read_catalog(file)$content_id)
      if (!is.null(id) && !identical(paste0(id, ".json.gz"), basename(file))) {
        problems <- c(problems, "A historical snapshot does not match its identity.")
      }
      evidence$history <- c(evidence$history, id)
    }
  }
  root <- catalog_store()
  pointer <- audit_pointer_problem(root)
  if (!is.null(pointer)) {
    problems <- c(problems, paste("Active pointer:", pointer))
    evidence$active <- list(state = "invalid")
  } else if (file.exists(file.path(root, "active.json"))) {
    evidence$active <- list(state = "verified", manifest_id = current_catalog_manifest()$manifest_id)
  } else {
    evidence$active <- list(state = "bundled_default")
  }
  p <- audit_project(context)
  if (!is.null(p) && !inherits(p, "error")) {
    evidence$pinned <- attempt("Pinned project snapshot", {
      catalog <- if (identical(p$lock$catalog_id, "unavailable")) NULL else catalog_snapshot(p$lock$catalog_id)
      list(catalog_id = p$lock$catalog_id, catalog_verified = !is.null(catalog),
        resource_id = resource_snapshot(p$path)$id)
    })
  }
  evidence$problems <- problems
  if (length(problems)) {
    return(audit_result("fail", paste(problems, collapse = " "), evidence))
  }
  audit_result("pass", "Bundled resources, catalog identities, schemas and selected snapshots verified.", evidence)
}

audit_sqlite_integrity <- function(file) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tables <- DBI::dbListTables(con)
  list(
    integrity = identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]], "ok"),
    foreign_key_violations = nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")),
    required_tables = all(c("packages", "observations", "resource_search") %in% tables)
  )
}

audit_catalog_references <- function(catalog) {
  names <- vapply(catalog$packages, function(p) p$name, character(1))
  documentation <- 0L
  unresolved <- 0L
  for (p in catalog$packages) {
    files <- names(p$source_files)
    for (entry in p$exports) {
      if (!is.null(entry$documentation) && !identical(p$source_files[[entry$documentation$path]], entry$documentation$sha256)) {
        documentation <- documentation + 1L
      }
      path <- entry$source_path
      # Older snapshots recorded repository-relative paths with a package subdirectory.
      if (!identical(path, "NAMESPACE") && !path %in% files && !any(endsWith(path, paste0("/", files)))) {
        unresolved <- unresolved + 1L
      }
    }
  }
  list(content_id = catalog$content_id, packages = length(names),
    duplicate_packages = unique(names[duplicated(names)]),
    documentation_mismatches = documentation, unresolved_source_paths = unresolved)
}

audit_kb_references <- function(context) {
  found <- new.env(parent = emptyenv())
  found$failures <- character()
  found$warnings <- character()
  found$resources <- list()
  found$catalogs <- list()
  inspect_resources <- function(label, file) {
    result <- audit_sqlite_integrity(file)
    found$resources[[label]] <- result
    if (!result$integrity || result$foreign_key_violations > 0L || !result$required_tables) {
      found$failures <- c(found$failures, paste("The", label, "resource database failed integrity, foreign-key or schema checks."))
    }
  }
  inspect_catalog <- function(label, catalog) {
    result <- audit_catalog_references(catalog)
    found$catalogs[[label]] <- result
    if (length(result$duplicate_packages) || result$documentation_mismatches > 0L) {
      found$failures <- c(found$failures, paste("The", label, "catalog has duplicate package revisions or misaligned documentation references."))
    }
    if (result$unresolved_source_paths > 0L) {
      found$warnings <- c(found$warnings, paste0("The ", label, " catalog has ", result$unresolved_source_paths, " export source locators outside its file manifest."))
    }
  }
  inspect_resources("active", resource_snapshot()$file)
  inspect_catalog("active", resolve_catalog())
  p <- audit_project(context)
  if (!is.null(p) && !inherits(p, "error")) {
    inspect_resources("pinned", resource_snapshot(p$path)$file)
    if (!identical(p$lock$catalog_id, "unavailable")) inspect_catalog("pinned", catalog_snapshot(p$lock$catalog_id))
  }
  root <- catalog_store()
  directory <- file.path(root, "manifests")
  base_resource <- read_document(resource_file("extdata", "resource-manifest.json"))$content_id
  manifests <- if (dir.exists(directory)) list.files(directory, full.names = TRUE) else character()
  dangling <- 0L
  invalid <- 0L
  for (file in manifests) {
    manifest <- tryCatch(
      {
        assert_plain_path(file)
        x <- validate_manifest(read_document(file))
        if (!identical(paste0(x$manifest_id, ".json"), basename(file))) stop("identity")
        x
      },
      error = function(e) NULL
    )
    if (is.null(manifest)) {
      invalid <- invalid + 1L
      next
    }
    content <- file.exists(file.path(root, "snapshots", manifest$content_id, "api-catalog.json.gz"))
    resource <- identical(manifest$resource_id, base_resource) ||
      file.exists(file.path(root, "resource-snapshots", manifest$resource_id, "package-resources.sqlite"))
    if (!content || !resource) dangling <- dangling + 1L
  }
  evidence <- list(resources = found$resources, catalogs = found$catalogs,
    retained_manifests = list(count = length(manifests), invalid = invalid, dangling = dangling))
  failures <- found$failures
  warnings <- found$warnings
  if (invalid || dangling) {
    warnings <- c(warnings, paste0(invalid, " retained manifests are invalid and ", dangling, " reference missing snapshots; they cannot be rolled back to."))
  }
  if (length(failures)) return(audit_result("fail", paste(c(failures, warnings), collapse = " "), evidence))
  if (length(warnings)) return(audit_result("warning", paste(warnings, collapse = " "), evidence))
  audit_result("pass", "Resource databases, catalog references and retained manifests are consistent.", evidence)
}

audit_source_repository <- function(record) {
  if (!is.null(record$github)) paste0("https://github.com/", record$github) else paste0("local-source:", record$id)
}

audit_kb_registry <- function(context) {
  registry <- getOption("cttiR.sources", list())
  catalog <- resolve_catalog()
  repositories <- vapply(catalog$packages, function(p) audit_or(p$repository, NA_character_), character(1))
  names <- vapply(catalog$packages, function(p) p$name, character(1))
  freshness <- table(vapply(catalog$packages, function(p) audit_or(p$freshness, "unknown"), character(1)))
  active <- file.path(catalog_store(), "active.json")
  last <- if (file.exists(active)) format(file.info(active)$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else NULL
  evidence <- list(configured = 0L, indexed = 0L, missing = character(),
    freshness = as.list(freshness), last_activation = last, last_update_result = "not_recorded")
  if (!is.list(registry) || !length(registry)) {
    message <- "No source registry is configured; only the bundled or active catalog coverage is known."
    return(audit_result("not_tested", message, evidence))
  }
  missing <- character()
  for (record in registry) {
    if (!is.list(record) || !is.character(record$id) || length(record$id) != 1L) {
      return(audit_result("fail", "The configured source registry contains an invalid record.", evidence))
    }
    covered <- audit_source_repository(record) %in% repositories ||
      (!is.null(record$package) && isTRUE(record$package %in% names))
    if (!covered) missing <- c(missing, record$id)
  }
  evidence$configured <- length(registry)
  evidence$indexed <- length(registry) - length(missing)
  evidence$missing <- missing
  if (length(missing)) {
    message <- paste0(length(missing), " of ", length(registry),
      " registered sources are not in the active catalog; run update() to index them.")
    return(audit_result("warning", message, evidence))
  }
  message <- paste0("All ", length(registry),
    " registered sources are indexed; remote currency is only known where recorded as fetched.")
  audit_result("pass", message, evidence)
}

audit_kb_evidence <- function(context) {
  catalog <- resolve_catalog()
  installed <- utils::installed.packages(noCache = TRUE)
  versions <- installed[!duplicated(installed[, "Package"]), "Version"]
  names(versions) <- installed[!duplicated(installed[, "Package"]), "Package"]
  exports <- 0L
  resolved <- 0L
  documented <- 0L
  mismatches <- list()
  for (p in catalog$packages) {
    exports <- exports + length(p$exports)
    resolved <- resolved + sum(vapply(p$exports, function(x) identical(x$verification, "static_api_verified"), logical(1)))
    documented <- documented + sum(vapply(p$exports, function(x) !is.null(x$documentation), logical(1)))
    have <- versions[p$name]
    if (!is.na(have) && !identical(unname(have), p$version)) {
      mismatches[[p$name]] <- list(installed = unname(have), cataloged = p$version)
    }
  }
  evidence <- list(packages = length(catalog$packages), exports = exports, resolved = resolved,
    documented = documented, mismatches = mismatches)
  message <- paste0(length(catalog$packages), " cataloged packages; ", resolved, " of ", exports,
    " exports have static signatures and ", documented, " have reference topics; ",
    length(mismatches), " installed versions differ from the cataloged revision. Workflow approvals remain separate.")
  audit_result(if (length(mismatches)) "warning" else "pass", message, evidence)
}

audit_retained_catalog_files <- function(root) {
  history <- system.file("extdata", "history", package = "cttiR")
  files <- c(
    if (nzchar(history)) list.files(history, "^[a-f0-9]{64}[.]json[.]gz$", full.names = TRUE),
    Sys.glob(file.path(root, "snapshots", "*", "api-catalog.json.gz"))
  )
  if (length(files) > 50L) files <- files[order(file.info(files)$mtime, decreasing = TRUE)][seq_len(50L)]
  files
}

audit_kb_tombstones <- function(context) {
  root <- catalog_store()
  active <- resolve_catalog()
  catalogs <- list(active = active)
  p <- audit_project(context)
  if (!is.null(p) && !inherits(p, "error") && !identical(p$lock$catalog_id, "unavailable")) {
    catalogs$pinned <- catalog_snapshot(p$lock$catalog_id)
  }
  for (catalog in catalogs) {
    for (package in catalog$packages) validate_document_corpus(package$documentation_corpus)
  }
  names <- vapply(active$packages, function(x) x$name, character(1))
  if (anyDuplicated(names)) {
    evidence <- list(duplicates = unique(names[duplicated(names)]))
    return(audit_result("fail", "The active catalog contains more than one revision of a package.", evidence))
  }
  current <- stats::setNames(lapply(active$packages, function(x) vapply(x$exports, function(e) e$name, character(1))), names)
  revisions <- stats::setNames(lapply(active$packages, function(x) x$source_hash), names)
  tombstones <- list()
  files <- audit_retained_catalog_files(root)
  for (file in files) {
    retained <- tryCatch(read_catalog(file), error = function(e) NULL)
    if (is.null(retained)) next
    for (package in retained$packages) {
      if (!package$name %in% names || identical(package$source_hash, revisions[[package$name]])) next
      removed <- setdiff(vapply(package$exports, function(e) e$name, character(1)), current[[package$name]])
      if (length(removed)) tombstones[[package$name]] <- sort(unique(c(tombstones[[package$name]], removed)))
    }
  }
  resurrected <- names(tombstones)[vapply(names(tombstones), function(x) any(tombstones[[x]] %in% current[[x]]), logical(1))]
  fallback <- !file.exists(file.path(root, "active.json")) &&
    length(Sys.glob(file.path(root, "manifests", "*.json"))) > 0L
  evidence <- list(packages = length(names), tombstones = lapply(tombstones, length),
    retained_snapshots = length(files), fallback = fallback)
  if (length(resurrected)) {
    return(audit_result("fail", "A removed export is resolvable in the current revision.", evidence))
  }
  if (fallback) {
    return(audit_result("warning", paste(
      "Retained catalog manifests exist but no active pointer is set, so reads fall back to the bundled catalog",
      "and exports removed by later updates are visible again. Select a retained snapshot with rollback_knowledge()."), evidence))
  }
  message <- paste0("Documentation corpora are intact; ", sum(lengths(tombstones)),
    " exports removed in other retained revisions stay absent from the current revision.")
  audit_result("pass", message, evidence)
}

audit_res_bundled <- function(context) {
  resources(limit = 1L)
  result <- audit_sqlite_integrity(resource_file("extdata", "package-resources.sqlite"))
  if (!result$integrity || result$foreign_key_violations > 0L) {
    return(audit_result("fail", "The bundled resource database failed integrity or foreign-key checks.", result))
  }
  audit_result("pass", "Verified.", result)
}

# Project ----------------------------------------------------------------------

audit_checks_project <- function() {
  project_check <- function(id, description, run, ...) {
    audit_check(id, "project", description, run, applies = audit_has_path, ...)
  }
  list(
    project_check("PRJ-001", "ProjectSpec, lock and control metadata validate and agree.",
      audit_prj_spec, required = TRUE, read_effects = "reads_project_metadata",
      evidence_schema = c("project_id", "schema_version", "template_version", "readiness")),
    project_check("PRJ-002", "Managed file hashes, preserved user edits and unsafe links.",
      audit_prj_files, required = TRUE, read_effects = "reads_project_metadata",
      repair_id = "restore_missing_managed", evidence_schema = c("files", "missing", "edited", "unsafe")),
    project_check("PRJ-003", "Publication IDs, metadata and required subproject folders.",
      audit_prj_publications, required = TRUE, read_effects = "reads_project_metadata",
      evidence_schema = c("publications", "missing")),
    project_check("PRJ-004", "Declared dependency environment and pinned catalog availability.",
      audit_prj_pins, required = TRUE, read_effects = c("reads_project_metadata", "reads_catalog_store", "reads_installation"),
      evidence_schema = c("catalog_id", "resource_id", "environment_status", "declared_packages")),
    project_check("PRJ-005", "Data registry metadata and local binding references, without opening data or bound paths.",
      audit_prj_data, required = function(context) audit_readiness_at_least(context, "data_ready"),
      severity = "warning", read_effects = c("reads_project_metadata", "reads_local_bindings"),
      evidence_schema = c("datasets", "declared", "bindings", "unknown_keys", "unregistered", "undeclared", "unbound_references", "machine_uris")),
    project_check("PRJ-006", "Private paths are ignored by Git and none are already tracked (file names only).",
      audit_prj_git, required = function(context) isTRUE(audit_spec(context)$workflow$git),
      severity = "warning", read_effects = c("reads_project_metadata", "reads_git_index"),
      evidence_schema = c("missing_ignores", "git", "tracked")),
    project_check("PRJ-007", "No active or interrupted project writer.",
      audit_prj_writer, required = TRUE, read_effects = "reads_project_metadata",
      repair_id = "recover_interrupted_transaction", evidence_schema = c("pending", "write_lock", "recovery_lock")),
    project_check("PRJ-008", "Interrupted transaction journals are recoverable from verified backups.",
      audit_prj_journals, required = TRUE, read_effects = c("reads_project_metadata", "reads_runtime_metadata"),
      repair_id = "recover_interrupted_transaction", evidence_schema = c("journals", "paths")),
    project_check("STD-002", "Standard workflow adapter and reflowR integration evidence.",
      audit_std_adapter, required = FALSE, read_effects = "reads_project_metadata"),
    project_check("STD-003", "Recorded analysis configuration completeness (no data or model validation).",
      audit_std_analysis, required = FALSE, read_effects = "reads_project_metadata")
  )
}

audit_prj_spec <- function(context) {
  p <- audit_project(context)
  if (inherits(p, "error")) stop(p)
  audit_result("pass", "Specification, lock and control metadata validate and agree.",
    list(project_id = p$spec$project$id, schema_version = p$spec$schema_version,
      template_version = p$spec$provenance$template_version, readiness = p$spec$workflow$readiness))
}

audit_prj_files <- function(context) {
  audit_with_project(context, function(p) {
    missing <- character()
    edited <- character()
    unsafe <- character()
    managed_missing <- character()
    for (entry in p$manifest$files) {
      current <- tryCatch(file_hash(file.path(p$path, entry$path)), error = function(e) "unsafe")
      if (identical(current, "unsafe")) {
        unsafe <- c(unsafe, entry$path)
      } else if (is.na(current)) {
        missing <- c(missing, entry$path)
        if (identical(entry$ownership, "managed")) managed_missing <- c(managed_missing, entry$path)
      } else if (!identical(current, entry$baseline_sha256)) {
        edited <- c(edited, entry$path)
      }
    }
    evidence <- list(files = length(p$manifest$files), missing = missing, managed_missing = managed_missing,
      edited = edited, unsafe = unsafe)
    status <- if (length(missing) || length(unsafe)) "fail" else if (length(edited)) "warning" else "pass"
    message <- paste0(length(p$manifest$files) - length(missing) - length(edited) - length(unsafe),
      " recorded files match their baseline; ", length(edited), " edited files are preserved; ",
      length(missing), " are missing; ", length(unsafe), " have unsafe paths or links.")
    audit_result(status, message, evidence)
  })
}

audit_prj_publications <- function(context) {
  audit_with_project(context, function(p) {
    missing <- character()
    for (pub in p$spec$publications) {
      for (dir in c("analysis", "manuscript", "figures", "tables", "supplement", "submission")) {
        folder <- file.path(p$path, "publications", pub$slug, dir)
        ok <- tryCatch(
          {
            assert_plain_path(folder)
            dir.exists(folder)
          },
          error = function(e) FALSE
        )
        if (!ok) missing <- c(missing, paste0("publications/", pub$slug, "/", dir))
      }
      metadata <- file.path(p$path, "publications", pub$slug, "publication.yml")
      if (!isTRUE(tryCatch(file.exists(assert_plain_path(metadata)), error = function(e) FALSE))) {
        missing <- c(missing, paste0("publications/", pub$slug, "/publication.yml"))
      }
    }
    ids <- vapply(p$spec$publications, function(x) x$id, character(1))
    evidence <- list(publications = ids, missing = missing)
    if (length(missing)) {
      return(audit_result("fail", paste0(length(missing), " required publication folders or metadata files are missing or unsafe."), evidence))
    }
    audit_result("pass", paste0(length(ids), " publications have their own metadata and six subproject folders."), evidence)
  })
}

audit_prj_pins <- function(context) {
  audit_with_project(context, function(p) {
    status <- "pass"
    notes <- character()
    if (identical(p$lock$catalog_id, "unavailable")) {
      status <- "warning"
      notes <- c(notes, "The project predates API catalog pins.")
    } else {
      resolve_catalog(p$path)
    }
    resources(path = p$path, limit = 1L)
    environment <- audit_or(p$lock$environment_status, "unknown")
    if (audit_readiness_at_least(context, "environment_ready") && !environment %in% c("ready", "materialized")) {
      status <- "fail"
      notes <- c(notes, "The declared readiness requires a materialized dependency environment.")
    }
    notes <- c(notes, paste0("Pinned API and resource snapshots are available; dependency environment: ", environment, "."))
    evidence <- list(catalog_id = p$lock$catalog_id, resource_id = p$lock$resource_snapshot,
      environment_status = environment, declared_packages = length(p$spec$packages))
    audit_result(status, paste(notes, collapse = " "), evidence)
  })
}

audit_registry_fields <- c(
  "id", "label", "logical_uri", "format", "access_class", "checksum", "checksum_status", "schema_ref",
  "origin", "provenance", "responsible_role", "checksum_algorithm", "description"
)

audit_prj_data <- function(context) {
  audit_with_project(context, function(p) {
    file <- file.path(p$path, "metadata", "data-registry.yml")
    assert_plain_path(file)
    if (!file.exists(file)) {
      return(audit_result("warning", "The data registry metadata file is missing."))
    }
    registry <- read_document(file)
    if (!is.list(registry) || is.null(names(registry)) || !"datasets" %in% names(registry) ||
        !(is.null(registry$datasets) || is.list(registry$datasets))) {
      return(audit_result("fail", "The data registry must be a mapping with a `datasets` list."))
    }
    datasets <- audit_or(registry$datasets, list())
    ids <- character()
    unknown_keys <- character()
    machine <- 0L
    for (entry in datasets) {
      if (!is.list(entry) || is.null(names(entry)) || !is.character(entry$id) || length(entry$id) != 1L ||
          is.na(entry$id) || !nzchar(entry$id)) {
        return(audit_result("fail", "Every data registry entry needs a nonempty string `id`."))
      }
      ids <- c(ids, entry$id)
      unknown_keys <- union(unknown_keys, setdiff(names(entry), audit_registry_fields))
      uri <- entry$logical_uri
      # Only the form of the URI is inspected; it is never resolved or opened.
      if (is.character(uri) && length(uri) == 1L && grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\|file:)", uri)) machine <- machine + 1L
    }
    if (anyDuplicated(tolower(ids))) {
      return(audit_result("fail", "Data registry IDs must be unique."))
    }
    declared <- vapply(p$spec$data_sources, function(x) x$id, character(1))
    bindings <- list()
    local <- file.path(p$path, ".cttir", "local.yml")
    assert_plain_path(local)
    if (file.exists(local)) {
      parsed <- read_document(local)
      if (!is.null(parsed) && (!is.list(parsed) || !(is.null(parsed$bindings) || is.list(parsed$bindings)))) {
        return(audit_result("fail", "`.cttir/local.yml` must be a mapping with a `bindings` list."))
      }
      bindings <- audit_or(parsed$bindings, list())
    }
    bound <- vapply(bindings, function(x) {
      id <- if (is.list(x)) audit_or(x$dataset_id, x$id) else NULL
      if (is.character(id) && length(id) == 1L && !is.na(id)) id else NA_character_
    }, character(1))
    mapping <- p$spec$analysis$mapping$data_source_id
    evidence <- list(
      datasets = length(ids), declared = length(declared), bindings = length(bindings),
      unknown_keys = unknown_keys, unregistered = setdiff(declared, ids), undeclared = setdiff(ids, declared),
      unbound_references = unique(c(setdiff(stats::na.omit(bound), ids), if (anyNA(bound)) "<binding without dataset_id>")),
      mapping_reference = if (is.null(mapping) || mapping %in% ids) "ok" else "unregistered",
      machine_uris = machine
    )
    findings <- c(
      if (length(evidence$unregistered)) "datasets declared in cttir-project.yml are absent from the registry",
      if (length(evidence$undeclared)) "registry datasets are not declared in cttir-project.yml",
      if (length(evidence$unbound_references)) "local bindings reference unknown dataset IDs",
      if (identical(evidence$mapping_reference, "unregistered")) "the analysis mapping references an unregistered dataset",
      if (length(unknown_keys)) "registry entries contain unrecognized fields",
      if (machine) "logical URIs look like machine paths, which belong in .cttir/local.yml"
    )
    suffix <- " Datasets and bound paths were not opened or resolved."
    if (length(findings)) {
      return(audit_result("warning", paste0("Data registry findings: ", paste(findings, collapse = "; "), ".", suffix), evidence))
    }
    message <- paste0(length(ids), " registered datasets and ", length(bindings),
      " local bindings have consistent IDs.", suffix)
    audit_result("pass", message, evidence)
  })
}

audit_git_tracked <- function(root) {
  git <- Sys.which("git")
  if (!nzchar(git)) return(NULL)
  result <- processx::run(git,
    c("-c", "core.fsmonitor=false", "-c", "core.quotePath=false", "-C", root, "ls-files", "--", audit_private_paths),
    env = c("current", GIT_OPTIONAL_LOCKS = "0", GIT_TERMINAL_PROMPT = "0",
      GIT_CEILING_DIRECTORIES = dirname(root)),
    error_on_status = FALSE, timeout = 15, windows_hide_window = TRUE
  )
  if (!identical(result$status, 0L)) return(NA)
  # Names only; unusual characters arrive C-quoted by Git.
  files <- strsplit(result$stdout, "\n", fixed = TRUE)[[1]]
  files[nzchar(files)]
}

audit_prj_git <- function(context) {
  root <- audit_root(context)
  if (is.null(root)) return(audit_result("not_tested", "The project directory does not exist; see PRJ-001."))
  ignore <- file.path(root, ".gitignore")
  assert_plain_path(ignore)
  lines <- if (file.exists(ignore)) trimws(readLines(ignore, warn = FALSE, encoding = "UTF-8")) else character()
  normalized <- sub("^/", "", sub("/+$", "", lines))
  missing <- audit_private_paths[!audit_private_paths %in% normalized]
  evidence <- list(missing_ignores = missing, git = "not_present", tracked = character())
  git_dir <- file.path(root, ".git")
  if (file.exists(git_dir)) {
    tracked <- tryCatch(audit_git_tracked(root), error = function(e) NA)
    if (is.null(tracked)) {
      evidence$git <- "unavailable"
    } else if (identical(tracked, NA)) {
      evidence$git <- "unreadable"
    } else {
      evidence$git <- "inspected"
      evidence$tracked <- tracked
    }
  }
  findings <- c(
    if (length(missing)) paste0(".gitignore does not list ", paste(missing, collapse = ", ")),
    if (length(evidence$tracked)) paste0(length(evidence$tracked), " files under private paths are already tracked; .gitignore cannot untrack them (use git rm --cached after review)")
  )
  if (length(findings)) return(audit_result("warning", paste0(paste(findings, collapse = "; "), "."), evidence))
  if (evidence$git %in% c("unavailable", "unreadable")) {
    return(audit_result("not_tested", "Private paths are ignored, but the Git index could not be inspected for already-tracked files.", evidence))
  }
  audit_result("pass", if (identical(evidence$git, "inspected")) {
    "Private paths are ignored and none are tracked in the Git index."
  } else {
    "Private paths are ignored; the project has no Git repository."
  }, evidence)
}

audit_prj_writer <- function(context) {
  root <- audit_root(context)
  if (is.null(root)) return(audit_result("not_tested", "The project directory does not exist; see PRJ-001."))
  pending <- pending_transactions(root)
  lock <- dir.exists(file.path(root, ".cttir/write-lock"))
  recovery <- dir.exists(file.path(root, ".cttir/recovery-lock"))
  evidence <- list(pending = length(pending), write_lock = lock, recovery_lock = recovery)
  if (length(pending) || lock || recovery) {
    return(audit_result("fail", "An active or interrupted write needs review.", evidence))
  }
  audit_result("pass", "No active or interrupted project writer.", evidence)
}

audit_prj_journals <- function(context) {
  root <- audit_root(context)
  if (is.null(root)) return(audit_result("not_tested", "The project directory does not exist; see PRJ-001."))
  plan <- transaction_recovery_plan(root)
  if (!length(plan$journals)) return(audit_result("pass", "No interrupted transaction journals."))
  paths <- unique(unlist(lapply(plan$journals, function(x) vapply(x$rows, function(r) r$path, character(1)))))
  message <- paste0(length(plan$journals),
    " interrupted transactions can be rolled back from verified backups with audit(repair = TRUE).")
  journals <- basename(vapply(plan$journals, function(x) x$dir, character(1)))
  audit_result("warning", message, list(journals = journals, paths = paths))
}

audit_std_adapter <- function(context) {
  audit_with_project(context, function(p) {
    audit_result("not_tested", "Workflow adapter validation remains pending.")
  })
}

audit_std_analysis <- function(context) {
  audit_with_project(context, function(p) {
    analysis <- analysis_configuration(p$spec)
    audit_result(if (analysis$state == "incomplete") "warning" else "pass",
      paste("Analysis configuration:", analysis$state,
        "- missing fields:", length(analysis$missing_fields),
        "- capability gaps:", length(analysis$capability_gaps),
        "; no data or model validation performed."))
  })
}

# Integration ------------------------------------------------------------------

audit_checks_integration <- function() {
  list(
    audit_check("INT-001", "integration", "Synthetic temporary project creation and idempotent repeat.",
      audit_int_project, required = TRUE, read_effects = c("writes_temp_files", "reads_catalog_store"),
      timeout_seconds = 300),
    audit_check("INT-002", "integration",
      "Update, export removal and rollback on a synthetic package in an isolated temporary catalog store.",
      audit_int_update, required = TRUE, read_effects = c("writes_temp_files", "reads_installation"),
      timeout_seconds = 300, evidence_schema = c("first", "second", "removed_exports")),
    audit_check("INT-003", "integration", "Bounded live structured-output probe of an owned local model.",
      audit_int_live, required = function(context) isTRUE(context$live),
      applies = function(context) if (isTRUE(context$live)) TRUE else "Live probe not requested (live = FALSE).",
      read_effects = c("reads_runtime_metadata", "local_http_metadata", "local_model_inference"),
      timeout_seconds = 180)
  )
}

audit_int_project <- function(context) {
  parent <- tempfile("cttir-audit-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  first <- project("Synthetic audit", "methods", "Check scaffold construction", parent)
  second <- project("Synthetic audit", "methods", "Check scaffold construction", parent)
  if (!identical(first$spec, second$spec) || !all(second$plan$action == "skip")) {
    abort_cttir("A repeated synthetic project was not an idempotent no-op.", "cttir_transaction_conflict")
  }
  audit_result("pass", "A synthetic temporary project was created and repeated as a no-op, then removed.")
}

audit_int_update <- function(context) {
  parent <- tempfile("cttir-audit-update-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  source <- file.path(parent, "source")
  write_bytes("Package: cttirAuditFixture\nVersion: 1.0.0\nTitle: Synthetic Audit Fixture\nLicense: MIT\n",
    file.path(source, "DESCRIPTION"))
  write_bytes("export(retired)\nexport(kept)\n", file.path(source, "NAMESPACE"))
  write_bytes("retired <- function(x) x\nkept <- function(x = 1) x\n", file.path(source, "R", "api.R"))
  previous <- options(cttiR.catalog_dir = file.path(parent, "store"),
    cttiR.sources = list(list(id = "audit-fixture", path = source)))
  on.exit(options(previous), add = TRUE, after = FALSE)
  exported <- function() {
    found <- Filter(function(p) identical(p$name, "cttirAuditFixture"), resolve_catalog()$packages)
    if (length(found) != 1L) abort_cttir("The synthetic package is not indexed exactly once.", "cttir_catalog_corrupt")
    vapply(found[[1]]$exports, function(e) e$name, character(1))
  }
  first <- update()
  if (!isTRUE(first$activation) || !setequal(exported(), c("kept", "retired"))) {
    abort_cttir("The synthetic source was not activated with its exports.", "cttir_catalog_corrupt")
  }
  write_bytes("export(kept)\n", file.path(source, "NAMESPACE"))
  write_bytes("kept <- function(x = 2) x\n", file.path(source, "R", "api.R"))
  second <- update()
  removed <- any(second$api_diff$change == "export_removed" & second$api_diff$symbol == "retired")
  if (!isTRUE(second$activation) || !removed || "retired" %in% exported()) {
    abort_cttir("A removed export was not withdrawn from the new revision.", "cttir_catalog_corrupt")
  }
  rolled <- rollback_knowledge(first$new_id, dry_run = FALSE)
  if (!isTRUE(rolled$activation) || !identical(current_catalog_manifest()$manifest_id, first$new_id) ||
      !"retired" %in% exported()) {
    abort_cttir("Rollback did not restore the retained snapshot.", "cttir_catalog_corrupt")
  }
  message <- paste("In an isolated temporary store, a synthetic update removed an export from the new revision,",
    "and rollback restored the retained snapshot; the store was then removed.")
  audit_result("pass", message, list(first = first$new_id, second = second$new_id, removed_exports = "retired"))
}

audit_int_live <- function(context) {
  owner <- runtime_owner(runtime_directory(), runtime_endpoint())
  if (is.null(owner) || is.null(owner$model)) {
    abort_cttir("Explicit setup is required before a live runtime audit.", "cttir_runtime_unavailable")
  }
  entry <- local_model(runtime_endpoint(), owner$model)
  if (is.null(entry) || !identical(entry$digest, owner$model_digest)) {
    abort_cttir("The configured model identity has changed.", "cttir_runtime_unavailable")
  }
  probe <- runtime_probe(runtime_endpoint(), owner$model)
  audit_result("pass", "Bounded structured-output probe passed on the owned local model.", probe)
}
