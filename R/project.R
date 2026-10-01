project_bundle <- function(spec, prior_lock = NULL) {
  files <- render_project(spec)
  lock <- list(
    schema_version = 1L, spec_sha256 = content_hash(json_text(spec)),
    template_version = "0.1.0", catalog_id = spec$provenance$catalog_id, model = NULL,
    resource_snapshot = if (is.null(prior_lock)) resource_snapshot()$id else prior_lock$resource_snapshot,
    dependencies = list(), environment_status = "pending"
  )
  if (!is.null(prior_lock)) {
    lock <- prior_lock
    lock$spec_sha256 <- content_hash(json_text(spec))
  }
  files[["cttir-lock.json"]] <- paste0(json_text(lock, TRUE), "\n")
  manifest <- project_manifest(files)
  files[[".cttir/managed-files.json"]] <- paste0(json_text(list(schema_version = 1L, files = manifest), TRUE), "\n")
  files[[".cttir/state.json"]] <- paste0(json_text(list(
    schema_version = 1L, project_id = spec$project$id,
    status = "created", spec_sha256 = lock$spec_sha256
  ), TRUE), "\n")
  list(files = files, manifest = manifest, lock = lock)
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

render_project <- function(spec) {
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
  files
}

project_manifest <- function(files) {
  lapply(names(files), function(path) {
    ownership <- if (grepl("^(protocol/|analysis/|publications/|metadata/|administration/|reports/|data/)", path) ||
        path == ".cttir/local.yml") {
      "user"
    } else {
      "managed"
    }
    list(path = path, ownership = ownership, baseline_sha256 = content_hash(files[[path]]), template_version = "0.1.0")
  })
}

#' Create an offline research project scaffold
#'
#' The foundation build uses deterministic templates, without network calls,
#' model inference, package installation or execution of study code. The planned
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
#' @export
#' @examples
#' p <- project("Example", "methods", "Plan a reproducible comparison",
#'   path = tempdir(), dry_run = TRUE
#' )
#' print(p)
project <- function(name, type, goal, path = getwd(), config = NULL,
  options = list(), dry_run = FALSE) {
  scalar_flag(dry_run, "dry_run")
  scalar_text(path, "path")
  assert_plain_path(path)
  if (!dir.exists(path)) abort_cttir("The parent directory must already exist.")
  parent <- normalizePath(path, winslash = "/", mustWork = TRUE)
  spec <- resolve_spec(name, type, goal, config, options)
  target <- file.path(parent, spec$project$slug)
  assert_plain_path(target)
  exists <- file.exists(target)
  saved <- NULL
  if (exists) {
    if (!dir.exists(target)) abort_cttir("The project target is an existing file.", "cttir_path_conflict")
    spec_file <- file.path(target, "cttir-project.yml")
    assert_plain_path(spec_file)
    if (!file.exists(spec_file)) abort_cttir("The target is not a recognized project.", "cttir_path_conflict")
    saved <- validate_spec(spec_file)
    spec <- resolve_spec(name, type, goal, config, options, saved$project, saved$provenance)
    if (!identical(json_text(spec), json_text(saved))) {
      abort_cttir("This project has different accepted inputs; use sync() to preview explicit changes.",
        "cttir_path_conflict", "different_spec",
        remediation = "Use sync() to preview changes or choose another project name."
      )
    }
  }
  bundle <- project_bundle(spec)
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
  warnings <- c("reflowR_integration_pending", "environment_pending", "knowledge_catalog_pending")
  if (exists) {
    changed <- vapply(names(files), function(f) !identical(digest::digest(file = file.path(target, f), algo = "sha256"), content_hash(files[[f]])), logical(1))
    if (any(changed)) warnings <- c(warnings, "existing_edits_preserved")
  }
  result <- structure(list(
    path = target, spec = spec, plan = plan,
    readiness = list(
      level = "scaffold_ready", materialized = exists || !dry_run,
      blockers = warnings[1:3]
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

#' @export
print.cttir_project <- function(x, ...) {
  cat(if (x$dry_run) "Planned project: " else "Project: ", x$path, "\n", sep = "")
  cat("Readiness: ", x$readiness$level, "\n", sep = "")
  cat("Pending: ", paste(x$readiness$blockers, collapse = ", "), "\n", sep = "")
  invisible(x)
}
