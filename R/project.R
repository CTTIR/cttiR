project_route <- function(spec) {
  if (!identical(spec$provenance$template_version, current_template_version)) return(NULL)
  route_workflow(spec, spec$workflow$profile, catalog_snapshot(spec$provenance$catalog_id))
}

project_bundle <- function(spec, prior_lock = NULL) {
  route <- project_route(spec)
  files <- render_project(spec, route)
  # The lock pins inputs; it cannot claim an installed environment (file 04).
  # Environment readiness is derived on each machine from renv.lock and the
  # project library (environment_status()); locks written by earlier versions
  # keep their historical `environment_status` field unchanged.
  lock <- list(
    schema_version = 1L, spec_sha256 = content_hash(json_text(spec)),
    template_version = spec$provenance$template_version, catalog_id = spec$provenance$catalog_id, model = NULL,
    resource_snapshot = if (is.null(prior_lock)) resource_snapshot()$id else prior_lock$resource_snapshot,
    dependencies = list()
  )
  if (!is.null(prior_lock)) {
    lock <- prior_lock
    lock$spec_sha256 <- content_hash(json_text(spec))
  }
  if (identical(spec$provenance$planner_mode, "local_llm")) {
    lock$model <- list(id = spec$provenance$model_id, digest = spec$provenance$model_digest,
      prompt_version = spec$provenance$prompt_version, qualification = spec$provenance$model_qualification,
      options = spec$provenance$planner_options)
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
  # A verified copy of the accepted spec lets sync() review hand edits of
  # cttir-project.yml field by field. Older template versions stay unchanged.
  if (identical(spec$provenance$template_version, current_template_version)) {
    files[[".cttir/accepted-spec.yml"]] <- paste0(
      "# Accepted copy of cttir-project.yml, kept by cttiR to review hand edits. Do not edit.\n",
      files[["cttir-project.yml"]])
  }
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
    "protocol/analysis-plan.md" = if (identical(version, current_template_version)) analysis_plan_text(spec) else paste0(
      "# Analysis plan\n\n",
      "Record the question, design, sampling unit and provenance.\n",
      "Specify outcomes, variable roles, missingness handling and dependence structure.\n",
      "Describe validation, diagnostics, multiplicity and sensitivity analyses where relevant.\n",
      "These decisions remain unknown until reviewed. No analysis is approved by creation.\n"
    ),
    "code/validate_project.R" = if (identical(version, "0.3.0")) validate_script_0.3.0 else paste0(
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
      "reflow_init and workflowr are not invoked. Dependencies are listed, not installed.\n\n",
      "Optional reproducibility features are enabled only through explicit `workflow` options\n",
      "(the `options` argument of `cttiR::project()` or `cttiR::sync()`):\n\n",
      "- `pipeline: targets` adds `_targets.R`; `targets::tar_make()` validates the structure,\n",
      "  runs the synthetic demonstration and reports missing study inputs without reading data.\n",
      "- `environment: renv` with `prepare_environment: true` installs only the listed\n",
      "  dependencies into a project library and writes `renv.lock` from the installed versions.\n",
      "  `renv.lock` is derived output that sync never overwrites as a template; recreate the\n",
      "  library elsewhere with `renv::restore()`. `network: offline` (default) downloads nothing.\n",
      "- `git: true` runs `git init` in this directory only; nothing is staged or committed.\n",
      "  `.gitignore` excludes raw data, private records, local bindings, project libraries and\n",
      "  study-data reports, but it cannot untrack files that are already committed.\n")
    files[[".gitignore"]] <- paste0(
      "# Study data, private records and machine-local state\n",
      "data/raw/\ndata/interim/\ndata/processed/\nadministration/private/\n",
      ".cttir/local.yml\n.cttir/environment.json\n",
      "# Project library and pipeline store (rebuild with renv::restore() and targets::tar_make())\n",
      "renv/library/\nrenv/local/\nrenv/staging/\n_targets/\n",
      "# Derived outputs; study-data reports may contain results\n",
      "demo/outputs/\ndemo/receipt.json\noutput/\n",
      ".Rhistory\n.RData\n.Rproj.user/\n")
  }
  files
}

# Falls back to structural YAML checks when cttiR is not in the active library
# (for example inside a project renv library).
validate_script_0.3.0 <- paste0(
  "# Read-only structural validation; no study data are opened.\n",
  "required <- c('cttir-project.yml', 'cttir-lock.json', 'metadata/data-registry.yml',\n",
  "              'metadata/data-dictionary.csv', 'config/analysis.yml', 'config/workflow.yml')\n",
  "missing <- required[!file.exists(required)]\n",
  "if (length(missing)) stop(paste('Missing project files:', paste(missing, collapse = ', ')))\n",
  "if (requireNamespace('cttiR', quietly = TRUE)) {\n",
  "  cttiR::validate_spec('cttir-project.yml')\n",
  "  message('Specification validated with cttiR. Data and analysis readiness remain unverified.')\n",
  "} else {\n",
  "  for (file in grep('[.]yml$', required, value = TRUE)) yaml::read_yaml(file, eval.expr = FALSE)\n",
  "  message('YAML structure parsed; install cttiR for full schema validation. Data remain unverified.')\n",
  "}\n"
)

# Research-type specific unknowns for protocol/analysis-plan.md (file 11). The
# items are prompts only: each stays a TODO until the researcher records it,
# and no user text is interpolated.
analysis_plan_sections <- list(
  primary_research = list(
    "Question and design" = c(
      "Objectives and hypotheses",
      "Confirmatory or exploratory status of each question",
      "Study design",
      "Population, setting and eligibility criteria",
      "Data origin (new collection or existing dataset) and analysis role"),
    "Variables and estimands" = c(
      "Exposure or intervention, and comparator",
      "Primary outcome and its timepoint",
      "Secondary outcomes",
      "Estimand: population, variable, handling of intercurrent events and summary measure",
      "Covariates and confounders, with the reason for each adjustment"),
    "Data structure" = c(
      "Unit of analysis",
      "Repeated measures, clustering or other dependence"),
    "Analysis" = c(
      "Statistical model and its assumptions",
      "Missing-data handling",
      "Multiplicity",
      "Sensitivity analyses",
      "Sample size or precision rationale"),
    "Ethics and reporting" = c(
      "Ethics approval and consent status",
      "Reporting guideline, if any",
      "Preregistration or protocol registration (this file does not claim one)")),
  secondary_research = list(
    "Review question" = c(
      "Review question and its elements (population, intervention or exposure, comparator, outcomes)",
      "Review type: systematic review, scoping review, meta-analysis or other",
      "Protocol registration (this file does not claim one)"),
    "Search and selection" = c(
      "Eligibility criteria",
      "Information sources and search dates",
      "Search strategy for each source",
      "Screening process: number of reviewers and how disagreements are resolved"),
    "Extraction and appraisal" = c(
      "Data extraction items and process",
      "Risk-of-bias or quality appraisal tool"),
    "Synthesis" = c(
      "Effect measures",
      "Synthesis method (narrative or meta-analytic) and model",
      "Heterogeneity and sensitivity assessment",
      "Certainty-of-evidence assessment",
      "Reporting guideline, if any")),
  methods = list(
    "Problem and method" = c(
      "Methodological problem and the claim to be tested",
      "Proposed method and its assumptions",
      "Comparator methods"),
    "Validation" = c(
      "Validation strategy: simulation study, benchmark data or both",
      "Simulation design: data-generating mechanisms, scenarios and number of repetitions",
      "Benchmark datasets and their provenance",
      "Performance measures",
      "Monte Carlo uncertainty of the reported performance"),
    "Implementation" = c(
      "Software implementation and its test plan",
      "Reproducibility: random seeds and computing environment",
      "Licensing decision for code and outputs")),
  software = list(
    "Scope" = c(
      "Purpose and intended users",
      "Interface (API) design",
      "Supported inputs, outputs and platforms"),
    "Quality" = c(
      "Test plan: unit, integration and validation against reference results",
      "Benchmarks",
      "Dependencies and their versions"),
    "Release" = c(
      "Licensing decision",
      "Documentation and release plan")),
  other = list(
    "Plan" = c(
      "Question or objective",
      "Design or approach",
      "Inputs and data sources",
      "Planned outputs",
      "Validation or quality checks",
      "Whether ethics or other approvals apply"))
)

analysis_plan_text <- function(spec) {
  type <- spec$project$type
  labels <- c(primary_research = "primary research", secondary_research = "secondary research (evidence synthesis)",
    methods = "methods", review = "review (evidence synthesis; the review type is unknown)", software = "software",
    mixed = "mixed (publications of several classes)", other = "other")
  todo <- function(sections, suffix = "") {
    unlist(lapply(names(sections), function(title) {
      c(paste0("## ", title, suffix), "", paste0("- TODO: ", sections[[title]]), "")
    }))
  }
  body <- if (identical(type, "mixed")) {
    classes <- unique(vapply(spec$publications, function(p) p$research_class, character(1)))
    classes <- intersect(c("primary_research", "secondary_research", "methods", "software"), classes)
    if (!length(classes)) classes <- "other"
    c("## Shared across publications", "",
      "- TODO: Which publication answers which question",
      "- TODO: Shared data sources, preprocessing and quality checks (root `analysis/`)", "",
      "Publication-specific plans belong in `publications/<slug>/analysis/`.", "",
      unlist(lapply(classes, function(class) {
        todo(analysis_plan_sections[[class]], paste0(" (", gsub("_", " ", class), " publications)"))
      })))
  } else {
    key <- if (identical(type, "review")) "secondary_research" else if (type %in% names(analysis_plan_sections)) type else "other"
    todo(analysis_plan_sections[[key]])
  }
  synthesis <- type %in% c("secondary_research", "review") ||
    (identical(type, "mixed") && any(vapply(spec$publications, function(p) identical(p$research_class, "secondary_research"), logical(1))))
  paste0(paste(c(
    "# Analysis plan", "",
    paste0("Research type: ", labels[[type]], "."), "",
    "Every item below is unknown until you record it. Replace `TODO` with the reviewed decision,",
    "or with `not applicable` and the reason. Nothing here was inferred from the project goal,",
    "and creating this file approves no analysis.", "",
    body,
    if (synthesis) c("No literature search has been run, and no studies or citations are recorded.", ""),
    "Record reviewed choices in `cttir-project.yml` with `cttiR::sync()`; analysis mappings and",
    "approval go to `config/analysis.yml` the same way."), collapse = "\n"), "\n")
}

# User ownership is sticky, and a user-owned file keeps its accepted baseline
# while it differs from it, so its edits stay recognisable. sync() moves an
# unedited user file to its new baseline together with its update (`refresh`);
# a repeat project() only reads and keeps every recorded user baseline.
carry_user_records <- function(bundle, p, refresh = FALSE) {
  recorded <- vapply(p$manifest$files, function(f) f$path, character(1))
  for (i in seq_along(bundle$manifest)) {
    at <- match(bundle$manifest[[i]]$path, recorded)
    if (is.na(at) || !identical(p$manifest$files[[at]]$ownership, "user")) next
    old <- p$manifest$files[[at]]
    current <- if (refresh) file_hash(file.path(p$path, old$path)) else NULL
    if (refresh && (is.na(current) || current %in% c(old$baseline_sha256, bundle$manifest[[i]]$baseline_sha256))) {
      bundle$manifest[[i]]$ownership <- "user"
    } else {
      bundle$manifest[[i]] <- old
    }
  }
  bundle$files[[".cttir/managed-files.json"]] <- paste0(json_text(list(schema_version = 1L, files = bundle$manifest), TRUE), "\n")
  bundle
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
#' Explicit `workflow` options add reviewed integrations. `pipeline = "targets"`
#' writes `_targets.R` and adds targets to the dependency manifest.
#' `environment = "renv"` with `prepare_environment = TRUE` prepares a project
#' library in an isolated R process after the scaffold has been published,
#' honoring `network` (`"offline"` by default). `git = TRUE` runs `git init` in
#' the new project root only, never inside another work tree, and never stages
#' or commits. Failures of these steps keep the scaffold and are reported as
#' readiness blockers with a recovery command.
#'
#' Creation stages the scaffold next to the target and publishes it with one
#' rename while holding a creation lock that records its process. A lock left
#' by a stopped process on this host (or an ownerless lock from an older
#' version, after 24 hours) is recovered on the next attempt: only the lock and
#' that attempt's staging directory are removed, and `recovered` reports it.
#'
#' @param name Nonempty project title, kept as written. A portable
#'   child-directory slug is derived once: ASCII transliteration where
#'   possible; when letters have no transliteration (for example CJK script) or
#'   the slug would exceed 80 characters, a short hash of the name is appended
#'   (`project_<hash>` when nothing transliterates).
#' @param type One of `primary_research`, `secondary_research`, `methods`,
#'   `review`, `software`, `mixed`, or `other`.
#' @param goal Nonempty research objective.
#' @param path Existing parent directory; never the project directory itself.
#' @param config Optional named configuration list or local JSON/YAML file.
#' @param options Named configuration list taking precedence over `config`.
#' @param dry_run If TRUE, return a plan without writing any files.
#' @return A `cttir_project` containing path, spec, plan, readiness, manifest and
#'   warnings. An identical repeat is read-only and preserves user edits.
#'   `readiness$level` becomes `environment_ready` only when `renv.lock` and the
#'   project library match the pinned dependencies; `readiness$environment` and
#'   `readiness$git` report the evidence and any recovery command. The
#'   environment state is derived on each machine from `renv.lock`, the project
#'   library and `.cttir/environment.json`; `cttir-lock.json` pins versions but
#'   never records an installed environment.
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
  name <- utf8_input(name)
  type <- utf8_input(type)
  goal <- utf8_input(goal)
  options <- utf8_input(options)
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
    for (file in c("cttir-lock.json", ".cttir/state.json", ".cttir/managed-files.json")) {
      actual <- file.path(target, file)
      assert_plain_path(actual)
      if (!file.exists(actual) || dir.exists(actual))
        abort_cttir("An existing project control file is missing or replaced by a directory.", "cttir_path_conflict", "incomplete_project")
    }
    existing <- read_project(target)
    prior_lock <- existing$lock
    spec <- resolve_existing(existing$spec, name, type, goal, config, options)
  } else {
    spec <- resolve_spec(name, type, goal, config, options)
  }
  bundle <- project_bundle(spec, prior_lock)
  if (exists) bundle <- carry_user_records(bundle, existing)
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
    # Generated files of an earlier build of the same template version differ
    # from this build's rendering; sync() previews and applies the difference.
    refresh <- sprintf("If an earlier cttiR build created this project, preview its regenerated files with cttiR::sync(%s) %s",
      r_literal(target), "and apply them with dry_run = FALSE; otherwise restore the file from version control.")
    for (file in names(files)) {
      actual <- file.path(target, file)
      assert_plain_path(actual)
      if (!file.exists(actual) || dir.exists(actual)) {
        abort_cttir(sprintf("The existing project file %s is missing or replaced by a directory.", file),
          "cttir_path_conflict", "incomplete_project", field = file, remediation = refresh)
      }
    }
    # Control metadata must match the accepted spec and generated baseline.
    for (file in c("cttir-lock.json", ".cttir/state.json", ".cttir/managed-files.json")) {
      if (!identical(digest::digest(file = file.path(target, file), algo = "sha256"), content_hash(files[[file]]))) {
        abort_cttir(sprintf("Project control metadata (%s) differs from its accepted baseline.", file),
          "cttir_path_conflict", "changed_metadata", field = file, remediation = refresh)
      }
    }
  }
  environment <- environment_status(bundle$lock$dependencies, if (exists) target else NULL, spec$workflow$environment)
  git <- git_status(if (exists) target else NULL, spec$workflow$git, parent)
  readiness <- readiness_with(list(level = "scaffold_ready", materialized = exists || !dry_run), spec, bundle, environment, git)
  blockers <- readiness$blockers
  warnings <- unique(c(blockers, spec$provenance$fallback_reason))
  if (exists) {
    changed <- vapply(names(files), function(f) !identical(digest::digest(file = file.path(target, f), algo = "sha256"), content_hash(files[[f]])), logical(1))
    if (any(changed)) warnings <- c(warnings, "existing_edits_preserved")
  }
  result <- structure(list(
    path = target, spec = spec, plan = plan,
    readiness = list(
      level = readiness$level, materialized = exists || !dry_run,
      blockers = blockers, analysis = analysis_configuration(spec),
      planning = list(mode = spec$provenance$planner_mode, fallback_reason = spec$provenance$fallback_reason,
        local_runtime = "not_checked", guidance = "Use setup(dry_run = TRUE) to inspect optional local planning prerequisites."),
      workflow = if (is.null(bundle$route)) NULL else route_summary(bundle$route),
      environment = environment, git = git
    ), manifest = manifest,
    warnings = warnings, dry_run = dry_run
  ), class = "cttir_project")
  lockdir <- creation_lock_path(parent, spec$project$slug)
  if (dry_run && !exists) {
    lock <- lock_state(lockdir)$state
    if (!identical(lock, "absent")) {
      result$warnings <- c(result$warnings, if (identical(lock, "stale")) "stale_creation_lock" else "creation_lock_held")
    }
  }
  if (dry_run || exists) {
    return(result)
  }
  recovered <- list()
  if (!dir.create(lockdir, showWarnings = FALSE)) {
    recovered <- list(recover_creation_lock(parent, spec$project$slug))
    if (!dir.create(lockdir, showWarnings = FALSE)) abort_creation_lock(lockdir, lock_state(lockdir))
  }
  on.exit(unlink(lockdir, recursive = TRUE), add = TRUE)
  stage <- tempfile(pattern = paste0(".", spec$project$slug, "-stage-"), tmpdir = parent)
  write_lock_owner(lockdir, list(stage = basename(stage)))
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
  # Explicit options act on the published scaffold; their failures stay pending.
  environment <- environment_step(target, spec, bundle$lock$dependencies)
  git <- if (isTRUE(spec$workflow$git)) git_initialize(target) else git_status(target, FALSE)
  result$readiness <- readiness_with(result$readiness, spec, bundle, environment, git)
  result$warnings <- c(result$readiness$blockers, if (length(recovered)) "stale_creation_lock_recovered")
  result$recovered <- recovered
  result
}

project_blockers <- function(spec, bundle, environment = environment_status(bundle$lock$dependencies)) {
  if (is.null(bundle$route)) return(c("reflowR_integration_pending", "environment_pending", "knowledge_catalog_pending"))
  c(if (length(bundle$route$approval_pending)) "workflow_approval_pending",
    if (!environment$state %in% c("installed_versions_match", "environment_ready")) "environment_pending",
    if (length(bundle$route$gaps)) "capability_gaps_recorded")
}

# Reads installed package metadata only (no namespace loading) and compares it
# with the pinned dependency versions. Never installs anything.
environment_status <- function(dependencies, root = NULL, mode = "none") {
  if (identical(mode, "renv")) return(renv_environment_status(dependencies, root))
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
  if (!is.null(x$readiness$planning)) {
    cat("Planning: ", x$readiness$planning$mode, "; optional local runtime not checked.\n", sep = "")
    cat(x$readiness$planning$guidance, "\n")
  }
  if (!is.null(x$readiness$workflow)) cat("Workflow: ", x$readiness$workflow$profile, "\n", sep = "")
  cat("Pending: ", paste(x$readiness$blockers, collapse = ", "), "\n", sep = "")
  if (length(x$recovered)) cat("Recovered: a creation lock left by a stopped process\n")
  invisible(x)
}
