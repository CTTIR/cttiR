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
  if (isTRUE(spec$analysis$approved) &&
      (spec$analysis$aim == "unknown" || spec$analysis$unit_structure == "unknown")) {
    abort_cttir("Analysis approval requires a known aim and unit structure.", "cttir_schema_error")
  }
  validate_analysis_mapping(spec)
  invisible(spec)
}

default_spec <- function(name, type, goal, slug, provenance = NULL) {
  research_class <- if (type %in% c("primary_research", "secondary_research", "methods", "software")) type else "unknown"
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
      project_backend = "reflowR", table_backend = "none"
    ),
    data_sources = list(), packages = list(), decisions = list(),
    provenance = list(
      catalog_id = if (is.null(provenance)) resolve_catalog()$content_id else provenance$catalog_id, template_version = "0.2.0",
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
  if (any(c("name", "type", "goal") %in% names(options$project))) {
    abort_cttir("Use the required arguments to set project name, type and goal.")
  }
  for (x in list(config, options)) {
    if (any(c("id", "created_at", "slug") %in% names(x$project))) {
      abort_cttir("Project identity fields are assigned by the builder.")
    }
  }
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
  if (identical(spec$workflow$profile, "auto")) spec$workflow$profile <- "standard_reflowR"
  unsupported <- !identical(spec$workflow$profile, "standard_reflowR") ||
    spec$workflow$prepare_environment || spec$workflow$git ||
    spec$workflow$pipeline != "none" || spec$workflow$environment != "none" ||
    spec$workflow$reporting != "generic" || spec$workflow$table_backend != "none" ||
    spec$workflow$readiness != "scaffold_ready" || length(spec$packages) > 0L ||
    isTRUE(combined$knowledge$refresh)
  if (unsupported) {
    abort_cttir(
      "This foundation build supports offline scaffolding only; the requested integration is pending.",
      "cttir_api_mismatch", "integration_pending"
    )
  }
  spec$knowledge <- NULL
  record <- function(field, origin, reason) list(field = field, origin = origin, reason = reason, evidence_ids = list())
  decisions <- list(record("/workflow/profile", "default", "No approved specialist adapter is available; reflowR integration remains pending."))
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
  for (key in c("name", "type", "goal")) decisions <- append(decisions, list(record(paste0("/project/", key), "explicit", "Required argument takes precedence.")))
  spec$decisions <- decisions
  validate_spec(spec)
}
