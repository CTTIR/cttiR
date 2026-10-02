project_route <- function(spec) {
  if (!identical(spec$provenance$template_version, current_template_version)) return(NULL)
  route_workflow(spec, spec$workflow$profile, catalog_snapshot(spec$provenance$catalog_id))
}

project_bundle <- function(spec, prior_lock = NULL) {
  route <- project_route(spec)
  files <- render_project(spec, route)
  lock <- list(
    schema_version = 1L, spec_sha256 = content_hash(json_text(spec)),
    template_version = spec$provenance$template_version, catalog_id = spec$provenance$catalog_id, model = NULL,
    resource_snapshot = if (is.null(prior_lock)) resource_snapshot()$id else prior_lock$resource_snapshot,
    dependencies = list(), environment_status = "pending"
  )
  if (!is.null(prior_lock)) {
    lock <- prior_lock
    lock$spec_sha256 <- content_hash(json_text(spec))
  }
  if (!is.null(route)) {
    # Derived from the accepted spec and the pinned catalog only, never from
    # whatever happens to be installed or active globally.
    lock$dependencies <- route_dependencies(route, catalog_snapshot(spec$provenance$catalog_id))
    lock$workflow <- route_summary(route)
  }
  files[["cttir-lock.json"]] <- paste0(json_text(lock, TRUE), "\n")
  manifest <- project_manifest(files, spec$provenance$template_version)
  files[[".cttir/managed-files.json"]] <- paste0(json_text(list(schema_version = 1L, files = manifest), TRUE), "\n")
  files[[".cttir/state.json"]] <- paste0(json_text(list(
    schema_version = 1L, project_id = spec$project$id,
    status = "created", spec_sha256 = lock$spec_sha256
  ), TRUE), "\n")
  list(files = files, manifest = manifest, lock = lock, route = route)
}

assert_plain_path <- function(path) {
  # Check every existing lexical component before normalizing away links.
  path <- path.expand(path)
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) path <- file.path(getwd(), path)
  parts <- strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]]
  if (any(parts == "..")) abort_cttir("Parent traversal is not allowed in project paths.")
  current <- path
  repeat {
    link <- Sys.readlink(current)
    system_alias <- identical(Sys.info()[["sysname"]], "Darwin") &&
      current %in% c("/var", "/tmp", "/etc") &&
      identical(normalizePath(current, winslash = "/", mustWork = FALSE), paste0("/private", current))
    if (!is.na(link) && nzchar(link) && !system_alias) {
      abort_cttir("Project paths cannot pass through symbolic links.", "cttir_path_conflict", "symlink")
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  invisible(path)
}

render_project <- function(spec, route = project_route(spec)) {
  version <- spec$provenance$template_version
  if (!version %in% c("0.1.0", "0.2.0", "0.3.0")) {
    abort_cttir("This template version is not supported.", "cttir_api_mismatch")
  }
  # User text stays in structured YAML, never executable code or Markdown markup.
  files <- list(
    "README.md" = paste0(
      "# Research project\n\n",
      "The name, research type and objective are recorded in `cttir-project.yml`.\n\n",
      "Status: scaffold ready. Workflow integration and dependency preparation are pending.\n",
      "No data, scientific findings or approvals have been inferred.\n\n",
      "1. Review `cttir-project.yml` and `protocol/analysis-plan.md`.\n",
      "2. Define datasets in `metadata/data-registry.yml`; keep local paths in `.cttir/local.yml`.\n",
      "3. Run `Rscript code/validate_project.R` from this project directory.\n",
      "4. Review variable mappings before adding or running an analysis.\n\n",
      "This scaffold has no initialized reflowR backend or installed analysis dependencies.\n"
    ),
    "cttir-project.yml" = yaml::as.yaml(spec),
    ".gitignore" = paste0(
      "data/raw/\ndata/interim/\ndata/processed/\n.cttir/local.yml\n",
      "administration/private/\n.Rhistory\n.RData\n.Rproj.user/\n"
    ),
    ".cttir/local.yml" = "# Machine-specific data bindings; do not commit this file.\nbindings: []\n",
    "metadata/data-registry.yml" = yaml::as.yaml(list(datasets = spec$data_sources)),
    "metadata/data-dictionary.csv" = "dataset_id,variable,type,unit,allowed_values,missing_codes,description\n",
    "config/analysis.yml" = yaml::as.yaml(spec$analysis),
    "protocol/analysis-plan.md" = paste0(
      "# Analysis plan\n\n",
      "Record the question, design, sampling unit and provenance.\n",
      "Specify outcomes, variable roles, missingness handling and dependence structure.\n",
      "Describe validation, diagnostics, multiplicity and sensitivity analyses where relevant.\n",
      "These decisions remain unknown until reviewed. No analysis is approved by creation.\n"
    ),
    "code/validate_project.R" = paste0(
      "# Read-only structural validation; no study data are opened.\n",
      "required <- c('cttir-project.yml', 'cttir-lock.json', 'metadata/data-registry.yml',\n",
      "              'metadata/data-dictionary.csv', 'config/analysis.yml')\n",
      "missing <- required[!file.exists(required)]\n",
      "if (length(missing)) stop(paste('Missing project files:', paste(missing, collapse = ', ')))\n",
      "if (!requireNamespace('cttiR', quietly = TRUE)) stop('Install cttiR to validate the specification.')\n",
      "cttiR::validate_spec('cttir-project.yml')\n",
      "message('Scaffold structure validated. Data and analysis readiness remain unverified.')\n"
    ),
    "analysis/README.md" = "# Shared analysis\n\nDefine shared preprocessing and quality checks here. No analysis has run.\n",
    "data/README.md" = "# Data\n\nRaw data remain external or in ignored storage. Record metadata without copying datasets.\n",
    "administration/README.md" = "# Administration\n\nRecord responsibilities, funding and approvals when known. Private files belong in private/.\n",
    "reports/README.md" = "# Reports\n\nStore reviewed reports here. No scientific results are generated by project creation.\n"
  )
  for (pub in spec$publications) {
    root <- paste0("publications/", pub$slug, "/")
    files[[paste0(root, "publication.yml")]] <- yaml::as.yaml(pub)
    files[[paste0(root, "README.md")]] <- "# Publication\n\nSee publication.yml for identity and taxonomy. Reference shared datasets by registry ID.\n"
    for (dir in c("analysis", "manuscript", "figures", "tables", "supplement", "submission")) {
      files[[paste0(root, dir, "/README.md")]] <- paste0("# ", dir, "\n\nPublication-specific work belongs here.\n")
    }
  }
  if (version == "0.2.0") {
    files <- c(files, reflowr_templates())
    files[["README.md"]] <- paste0(files[["README.md"]],
      "\nReviewed reflowR layout adaptation: see `metadata/reflowr-template.json`.\n",
      "Run `Rscript code/render_report.R` explicitly to render the placeholder\n",
      "pages and labelled synthetic fixture. Standard analysis adapters remain pending.\n")
  }
  if (version == "0.3.0") {
    files <- c(files, standard_bundle_files(spec, route))
    files[["README.md"]] <- paste0(
      "# Research project\n\n",
      "The name, research type and objective are recorded in `cttir-project.yml`.\n",
      "Workflow profile, stages, package pins and approval status are in `config/workflow.yml`.\n\n",
      "Status: scaffold ready. No data, scientific findings or approvals have been inferred.\n\n",
      "1. Review `cttir-project.yml`, `config/workflow.yml` and `protocol/analysis-plan.md`.\n",
      "2. Define datasets in `metadata/data-registry.yml`; keep local paths in `.cttir/local.yml`.\n",
      "3. Run `Rscript code/validate_project.R` from this project directory.\n",
      "4. Run `Rscript code/run_demo.R` to exercise every stage on labelled synthetic data.\n",
      "5. Record mappings, reviewed model settings and approval with `cttiR::sync()`; then run\n",
      "   `Rscript code/run_workflow.R`, which stops and lists anything still missing.\n",
      "6. Run `Rscript code/render_report.R` to render the pages in `analysis/`.\n\n",
      "The layout adapts the pinned reflowR minimal template (see `metadata/workflow-template.json`);\n",
      "reflow_init and workflowr are not invoked. Dependencies are listed, not installed.\n")
  }
  files
}

project_manifest <- function(files, template_version = "0.1.0") {
  lapply(names(files), function(path) {
    ownership <- if (grepl("^(protocol/|analysis/|publications/|metadata/|administration/|reports/|data/)", path) ||
        path == ".cttir/local.yml") {
      "user"
    } else {
      "managed"
    }
    if (path == "metadata/reflowr-template.json") ownership <- "managed"
    list(path = path, ownership = ownership, baseline_sha256 = content_hash(files[[path]]), template_version = template_version)
  })
}

#' Create an offline research project scaffold
#'
#' The foundation build uses deterministic templates, without package
#' installation or execution of study code. It makes no network calls and no
#' model inference unless the local planner policy enables one loopback
#' exchange with an owned local model (see Details). The planned
#' standard workflow is explicitly pending reflowR integration. Advanced workflow
#' options that are not implemented raise a typed error instead of being ignored.
#'
#' @param name Nonempty project title. A portable child-directory slug is derived.
#' @param type One of `primary_research`, `secondary_research`, `methods`,
#'   `review`, `software`, `mixed`, or `other`.
#' @param goal Nonempty research objective.
#' @param path Existing parent directory; never the project directory itself.
#' @param config Optional named configuration list or local JSON/YAML file.
#' @param options Named configuration list taking precedence over `config`.
#' @param dry_run If TRUE, return a plan without writing any files.
#' @return A `cttir_project` containing path, spec, plan, readiness, manifest and
#'   warnings. An identical repeat is read-only and preserves user edits.
#'   `readiness$analysis` reports candidate routing, missing fields and capability
#'   gaps. It never opens data or executes a model, even with `analysis$approved`.
#' @details Optional `analysis$mapping` records `data_source_id`, `outcome`,
#'   `predictors`, `subject`, `time`, `event`, `event_value`, `non_event_value`,
#'   `estimand`, `time_origin`, `time_unit` and `missing_data`. Dataset IDs must
#'   refer to `data_sources`. Column names are literal data, not R expressions.
#'   An explicitly empty predictor list denotes an intercept-only intention.
#'   Missing-data choices are `unknown`, `fail` and `complete_case`; these record
#'   intent and never remove observations during planning. Predictive and causal
#'   designs are marked as requiring additional supported adapters. Scientific
#'   and revision-specific workflow approval remain separate prerequisites.
#'
#'   Unset aim, outcome family, unit structure and modality are filled from goal
#'   keywords by default. Only when `options(cttiR.planner = "local_llm")` is
#'   set and the owned runtime prepared by [setup()] and its recorded model
#'   digest verify, one bounded read-only exchange with that local model (at
#'   most one repair) proposes the same unset fields, also in dry runs. It never
#'   starts, pulls or installs anything; explicit values always win; failures
#'   fall back to the keyword rules, and replies showing signs of embedded
#'   instructions leave the fields unknown. The reason is recorded in
#'   `spec$decisions`.
#' @export
#' @examples
#' p <- project("Example", "methods", "Plan a reproducible comparison",
#'   path = tempdir(), dry_run = TRUE
#' )
#' print(p)
project <- function(name, type, goal, path = getwd(), config = NULL,
  options = list(), dry_run = FALSE) {
  project_impl(name, type, goal, path, config, options, dry_run)
}

project_impl <- function(name, type, goal, path = getwd(), config = NULL,
  options = list(), dry_run = FALSE, expected_catalog = NULL) {
  scalar_flag(dry_run, "dry_run")
  scalar_text(path, "path")
  assert_plain_path(path)
  if (!dir.exists(path)) abort_cttir("The parent directory must already exist.")
  parent <- normalizePath(path, winslash = "/", mustWork = TRUE)
  target <- file.path(parent, safe_slug(name))
  assert_plain_path(target)
  exists <- file.exists(target)
  saved <- NULL
  prior_lock <- NULL
  context <- if (!exists) current_catalog_manifest() else NULL
  if (!exists && !is.null(expected_catalog) && !identical(content_hash(json_text(context)), expected_catalog))
    abort_cttir("The catalog changed after preview; preview again before creation.", "cttir_transaction_conflict")
  if (exists) {
    if (!dir.exists(target)) abort_cttir("The project target is an existing file.", "cttir_path_conflict")
    spec_file <- file.path(target, "cttir-project.yml")
    assert_plain_path(spec_file)
    if (!file.exists(spec_file)) abort_cttir("The target is not a recognized project.", "cttir_path_conflict")
    saved <- validate_spec(spec_file)
    for (file in c("cttir-lock.json", ".cttir/state.json", ".cttir/managed-files.json")) {
      actual <- file.path(target, file)
      assert_plain_path(actual)
      if (!file.exists(actual) || dir.exists(actual))
        abort_cttir("An existing project control file is missing or replaced by a directory.", "cttir_path_conflict", "incomplete_project")
    }
    prior_lock <- read_project(target)$lock
    spec <- resolve_existing(saved, name, type, goal, config, options)
  } else {
    spec <- resolve_spec(name, type, goal, config, options)
  }
  bundle <- project_bundle(spec, prior_lock)
  if (!exists && (!identical(spec$provenance$catalog_id, context$content_id) ||
        !identical(bundle$lock$resource_snapshot, context$resource_id))) {
    abort_cttir("The catalog changed during planning; retry against one complete snapshot.", "cttir_transaction_conflict")
  }
  files <- bundle$files
  manifest <- bundle$manifest
  plan <- data.frame(
    path = names(files), action = if (exists) "skip" else "create",
    sha256 = vapply(files, content_hash, character(1)), stringsAsFactors = FALSE, row.names = NULL
  )
  if (exists) {
    for (file in names(files)) {
      actual <- file.path(target, file)
      assert_plain_path(actual)
      if (!file.exists(actual) || dir.exists(actual)) {
        abort_cttir("An existing project file is missing or replaced by a directory.", "cttir_path_conflict", "incomplete_project")
      }
    }
    # Control metadata must match the accepted spec and generated baseline.
    for (file in c("cttir-lock.json", ".cttir/state.json", ".cttir/managed-files.json")) {
      if (!identical(digest::digest(file = file.path(target, file), algo = "sha256"), content_hash(files[[file]]))) {
        abort_cttir("Project control metadata differs from its accepted baseline.", "cttir_path_conflict", "changed_metadata")
      }
    }
  }
  blockers <- project_blockers(spec, bundle)
  warnings <- blockers
  if (exists) {
    changed <- vapply(names(files), function(f) !identical(digest::digest(file = file.path(target, f), algo = "sha256"), content_hash(files[[f]])), logical(1))
    if (any(changed)) warnings <- c(warnings, "existing_edits_preserved")
  }
  result <- structure(list(
    path = target, spec = spec, plan = plan,
    readiness = list(
      level = "scaffold_ready", materialized = exists || !dry_run,
      blockers = blockers, analysis = analysis_configuration(spec),
      workflow = if (is.null(bundle$route)) NULL else route_summary(bundle$route),
      environment = environment_status(bundle$lock$dependencies)
    ), manifest = manifest,
    warnings = warnings, dry_run = dry_run
  ), class = "cttir_project")
  if (dry_run || exists) {
    return(result)
  }
  lockdir <- file.path(parent, paste0(".", spec$project$slug, ".cttir-create-lock"))
  if (!dir.create(lockdir, showWarnings = FALSE)) {
    abort_cttir("Another creation operation may own this destination.", "cttir_transaction_conflict", "writer_lock")
  }
  on.exit(unlink(lockdir, recursive = TRUE), add = TRUE)
  stage <- tempfile(pattern = paste0(".", spec$project$slug, "-stage-"), tmpdir = parent)
  if (!dir.create(stage, mode = "0700")) abort_cttir("Could not create staging directory.", "cttir_path_conflict")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (file in names(files)) {
    dest <- file.path(stage, file)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(files[[file]])), dest)
    if (!identical(digest::digest(file = dest, algo = "sha256"), content_hash(files[[file]]))) {
      abort_cttir("Staging verification failed.", "cttir_transaction_conflict", "hash_mismatch")
    }
  }
  assert_plain_path(target)
  if (file.exists(target) || !file.rename(stage, target)) {
    abort_cttir("The destination changed or publication of staged files failed.", "cttir_transaction_conflict", "rename_failed")
  }
  result
}

project_blockers <- function(spec, bundle) {
  if (is.null(bundle$route)) return(c("reflowR_integration_pending", "environment_pending", "knowledge_catalog_pending"))
  c(if (length(bundle$route$approval_pending)) "workflow_approval_pending",
    if (!identical(environment_status(bundle$lock$dependencies)$state, "installed_versions_match")) "environment_pending",
    if (length(bundle$route$gaps)) "capability_gaps_recorded")
}

# Reads installed package metadata only (no namespace loading) and compares it
# with the pinned dependency versions. Never installs anything.
environment_status <- function(dependencies) {
  if (!length(dependencies)) return(list(state = "no_dependencies_recorded", missing = list(), mismatched = list()))
  installed <- utils::installed.packages(fields = "Version")
  missing <- character()
  mismatched <- character()
  for (dep in dependencies) {
    index <- match(dep$package, installed[, "Package"])
    if (is.na(index)) {
      missing <- c(missing, dep$package)
    } else if (!is.null(dep$version) && !identical(unname(installed[index, "Version"]), dep$version)) {
      mismatched <- c(mismatched, paste0(dep$package, " ", installed[index, "Version"], " != ", dep$version))
    }
  }
  unpinned <- vapply(dependencies, function(dep) is.null(dep$version), logical(1))
  list(state = if (length(missing)) "dependencies_missing" else if (length(mismatched) || any(unpinned)) "versions_unverified" else "installed_versions_match",
    missing = as.list(missing), mismatched = as.list(mismatched),
    unpinned = as.list(vapply(dependencies[unpinned], function(dep) dep$package, character(1))),
    limitation = "Installed versions are read from package metadata; no environment was prepared or restored.")
}

#' @export
print.cttir_project <- function(x, ...) {
  cat(if (x$dry_run) "Planned project: " else "Project: ", x$path, "\n", sep = "")
  cat("Readiness: ", x$readiness$level, "\n", sep = "")
  if (!is.null(x$readiness$workflow)) cat("Workflow: ", x$readiness$workflow$profile, "\n", sep = "")
  cat("Pending: ", paste(x$readiness$blockers, collapse = ", "), "\n", sep = "")
  invisible(x)
}
