safe_slug <- function(x) {
  scalar_text(x, "name")
  if (grepl("[/\\\\]", x) || x %in% c(".", "..")) {
    abort_cttir("Names cannot contain path separators or traversal.")
  }
  slug <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = "_"))
  slug <- gsub("[^a-z0-9]+", "_", slug)
  slug <- gsub("^_+|_+$", "", slug)
  if (!nzchar(slug)) abort_cttir("Supply a name containing letters or numbers.")
  if (!grepl("^[a-z]", slug)) slug <- paste0("project_", slug)
  check_slug(slug)
  slug
}

check_slug <- function(x) {
  scalar_text(x, "slug")
  if (!grepl("^[a-z][a-z0-9_]*$", x) || nchar(x) > 80L ||
      grepl("^(con|prn|aux|nul|com[0-9]|lpt[0-9])$", x)) {
    abort_cttir("Use a portable slug of at most 80 characters; device names are forbidden.")
  }
  invisible(x)
}

#' Validate a resolved project specification
#' @param spec A resolved specification list or local JSON/YAML file path.
#' @return Invisibly, the normalized specification. Also checks unique IDs,
#'   portable slugs and conservative scientific approval prerequisites.
#' @export
validate_spec <- function(spec) {
  if (is.character(spec)) spec <- read_document(spec)
  check_schema_version(spec)
  spec <- validate_document(spec, "project-spec")
  check_slug(spec$project$slug)
  for (key in c("publications", "data_sources", "packages")) {
    field <- if (key == "packages") "name" else "id"
    ids <- vapply(spec[[key]], function(x) x[[field]], character(1))
    if (anyDuplicated(tolower(ids))) abort_cttir(paste("Duplicate", key, "identities."), "cttir_schema_error")
  }
  slugs <- vapply(spec$publications, function(x) {
    check_slug(x$slug)
    x$slug
  }, character(1))
  if (anyDuplicated(slugs)) abort_cttir("Publication slugs must be unique.", "cttir_schema_error")
  dataset_ids <- vapply(spec$data_sources, function(x) x$id, character(1))
  for (pub in spec$publications) {
    unknown <- setdiff(unlist(pub$data_source_ids), dataset_ids)
    if (length(unknown)) {
      abort_cttir("Publications must reference registered data sources by ID.", "cttir_schema_error",
        "unknown_data_source", field = paste0("/publications/", pub$id, "/data_source_ids"))
    }
  }
  if (isTRUE(spec$analysis$approved) &&
      (spec$analysis$aim == "unknown" || spec$analysis$unit_structure == "unknown")) {
    abort_cttir("Analysis approval requires a known aim and unit structure.", "cttir_schema_error")
  }
  validate_analysis_mapping(spec)
  if (!is.null(spec$figures)) validate_figure_policy(spec$figures)
  invisible(spec)
}

current_template_version <- "0.3.0"

default_spec <- function(name, type, goal, slug, provenance = NULL) {
  research_class <- if (type %in% c("primary_research", "secondary_research", "methods", "software")) {
    type
  } else if (identical(type, "review")) {
    "secondary_research"
  } else {
    "unknown"
  }
  list(
    schema_version = 1L,
    project = list(
      id = uuid::UUIDgenerate(), name = name, slug = slug, type = type,
      goal = goal, language = "en", created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    research = list(
      design = NULL, domain = NULL, data_types = list(), ethics_status = "unknown",
      analysis_role = "unknown", data_origin = "unknown", notes = ""
    ),
    publications = list(list(
      id = "pub01", title = name, slug = "pub01_main",
      research_class = research_class, type = "other",
      analysis_role = "unknown", data_origin = "unknown"
    )),
    workflow = list(
      pipeline = "none", environment = "none", git = FALSE,
      prepare_environment = FALSE, network = "offline", reporting = "generic",
      readiness = "scaffold_ready", profile = "standard_reflowR",
      project_backend = "reflowR", table_backend = "DescrTab2"
    ),
    data_sources = list(), packages = list(), decisions = list(),
    provenance = list(
      catalog_id = if (is.null(provenance)) resolve_catalog()$content_id else provenance$catalog_id,
      template_version = current_template_version,
      prompt_version = "none", planner_mode = "deterministic", model_id = NULL, model_digest = NULL
    ),
    analysis = list(aim = "unknown", outcome_family = "unknown", unit_structure = "unknown", engine = NULL, approved = FALSE),
    figures = list(
      continuous_palette = "viridis", categorical_provider = "RColorBrewer",
      categorical_palette = "Dark2", diverging_palette = "BrBG", colourblind_friendly_only = TRUE,
      redundant_encoding_required = TRUE, panel_composer = "patchwork",
      checks = as.list(c("protanopia", "deuteranopia", "tritanopia", "grayscale")), na_colour = "#808080"
    ),
    ecosystem = list(seurat_for_relevant_gaps = TRUE, require_role_approval = TRUE, modality = "unknown")
  )
}

resolve_spec <- function(name, type, goal, config, options, identity = NULL, provenance = NULL) {
  scalar_text(name, "name")
  scalar_text(type, "type")
  scalar_text(goal, "goal")
  config <- validate_config(if (is.null(config)) list() else config)
  options <- validate_config(options)
  check_explicit_identity(config, options)
  slug <- safe_slug(name)
  defaults <- default_spec(name, type, goal, slug, provenance)
  combined <- merge_config(config, options)
  spec <- merge_config(defaults, combined)
  spec$project$name <- name
  spec$project$type <- type
  spec$project$goal <- goal
  if (!is.null(identity)) {
    spec$project$id <- identity$id
    spec$project$created_at <- identity$created_at
  }
  if (!is.null(provenance)) spec$provenance <- provenance
  spec$knowledge <- NULL
  record <- function(field, origin, reason, evidence = list()) {
    list(field = field, origin = origin, reason = reason, evidence_ids = as.list(evidence))
  }
  decisions <- list()
  # Goal keywords only fill fields that nobody supplied; they never approve anything.
  signals <- infer_goal(goal)
  for (field in c("aim", "outcome_family", "unit_structure")) {
    if (is.null(combined$analysis[[field]]) && identical(spec$analysis[[field]], "unknown") &&
        !identical(signals[[field]], "unknown")) {
      spec$analysis[[field]] <- signals[[field]]
      decisions[[length(decisions) + 1L]] <- record(paste0("/analysis/", field), "inferred",
        paste0("Goal keywords suggest '", signals[[field]], "'. Review before analysis; this is not an approval."),
        paste0("rule:", field, ":", signals[[field]]))
    }
  }
  if (is.null(combined$ecosystem$modality) && identical(spec$ecosystem$modality, "unknown") &&
      !identical(signals$modality, "unknown")) {
    spec$ecosystem$modality <- signals$modality
    decisions[[length(decisions) + 1L]] <- record("/ecosystem/modality", "inferred",
      paste0("Goal keywords suggest the '", signals$modality, "' data modality."), paste0("modality:", signals$modality))
  }
  check_supported_workflow(spec, combined)
  requested <- if (is.null(combined$workflow$profile)) "auto" else combined$workflow$profile
  route <- route_workflow(spec, requested, catalog_snapshot(spec$provenance$catalog_id))
  spec$workflow$profile <- route$profile
  decisions[[length(decisions) + 1L]] <- record("/workflow/profile",
    if (identical(requested, "auto")) "inferred" else "explicit", route$reason,
    paste0("capability:", vapply(route$stages, function(x) x$capability, character(1))))
  if (is.null(combined$workflow$table_backend)) {
    decisions[[length(decisions) + 1L]] <- record("/workflow/table_backend", "default",
      "DescrTab2 is the preferred descriptive table backend; it is used only at its reviewed pinned revision.",
      "capability:std.describe.descrtab2")
  }
  walk <- function(x, prefix, origin) {
    for (key in names(x)) {
      field <- paste0(prefix, "/", key)
      if (is.list(x[[key]]) && length(x[[key]]) && !is.null(names(x[[key]]))) {
        walk(x[[key]], field, origin)
      } else {
        decisions[[length(decisions) + 1L]] <<- record(field, origin, "Supplied customization.")
      }
    }
  }
  walk(config, "", "config")
  walk(options, "", "explicit")
  for (key in c("name", "type", "goal")) {
    decisions <- append(decisions, list(record(paste0("/project/", key), "explicit", "Required argument takes precedence.")))
  }
  spec$decisions <- decisions
  validate_spec(spec)
}

check_explicit_identity <- function(config, options) {
  if (any(c("name", "type", "goal") %in% names(options$project))) {
    abort_cttir("Use the required arguments to set project name, type and goal.")
  }
  for (x in list(config, options)) {
    if (any(c("id", "created_at", "slug") %in% names(x$project))) {
      abort_cttir("Project identity fields are assigned by the builder.")
    }
  }
  invisible(TRUE)
}

# Only integrations with reviewed adapters are accepted; others fail explicitly.
# targets, renv and Git are accepted for the current bundle when coherent.
check_supported_workflow <- function(spec, combined) {
  pending <- function(message, field) {
    abort_cttir(message, "cttir_api_mismatch", "integration_pending", field = field)
  }
  incoherent <- function(message, field, remediation) {
    abort_cttir(message, "cttir_input_error", "incoherent_workflow", field = field, remediation = remediation)
  }
  workflow <- spec$workflow
  if (!identical(workflow$readiness, "scaffold_ready")) {
    incoherent("Readiness is computed from evidence and cannot be requested.", "/workflow/readiness",
      "Remove workflow.readiness; project() and sync() report the verified level.")
  }
  if (isTRUE(workflow$prepare_environment) && !identical(workflow$environment, "renv")) {
    incoherent("Environment preparation requires workflow.environment = 'renv'.", "/workflow/prepare_environment",
      "Set workflow.environment to 'renv' or prepare_environment to FALSE.")
  }
  if ((workflow$git || workflow$pipeline != "none" || workflow$environment != "none") &&
      !identical(spec$provenance$template_version, current_template_version)) {
    pending("targets, renv and Git integration require the current standard template bundle.", "/workflow")
  }
  if (!identical(workflow$reporting, "generic")) pending("Only the reviewed reflowR-layout rendering is supported.", "/workflow/reporting")
  if (!workflow$table_backend %in% c("DescrTab2", "none")) {
    pending("This table backend has no reviewed adapter; use DescrTab2 or none.", "/workflow/table_backend")
  }
  if (!identical(workflow$project_backend, "reflowR")) pending("The standard profile requires the reflowR layout.", "/workflow/project_backend")
  if (length(spec$packages) || isTRUE(combined$knowledge$refresh)) {
    pending("Explicit package requests and knowledge refresh during creation are pending.", "/packages")
  }
  invisible(TRUE)
}

# Repeat creation compares explicit inputs with the accepted specification
# instead of re-deriving defaults, so older projects stay unchanged.
resolve_existing <- function(saved, name, type, goal, config, options) {
  scalar_text(name, "name")
  scalar_text(type, "type")
  scalar_text(goal, "goal")
  config <- validate_config(if (is.null(config)) list() else config)
  options <- validate_config(options)
  check_explicit_identity(config, options)
  different <- function() {
    abort_cttir("This project has different accepted inputs; use sync() to preview explicit changes.",
      "cttir_path_conflict", "different_spec",
      remediation = "Use sync() to preview changes or choose another project name.")
  }
  if (!identical(saved$project$name, name) || !identical(saved$project$type, type) ||
      !identical(saved$project$goal, goal)) {
    different()
  }
  combined <- merge_config(config, options)
  combined$knowledge <- NULL
  if (identical(combined$workflow$profile, "auto")) combined$workflow$profile <- NULL
  if (is.list(combined$workflow) && !length(combined$workflow)) combined$workflow <- NULL
  merged <- merge_config(saved, combined)
  if (!identical(json_text(merged), json_text(saved))) different()
  saved
}
