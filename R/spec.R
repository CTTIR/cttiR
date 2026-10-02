reserved_device_names <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])$"

slug_hash <- function(x) substr(content_hash(enc2utf8(x)), 1L, 8L)

# TRUE when a letter or digit of the name has no ASCII transliteration (for
# example CJK, Cyrillic or Greek script on most platforms).
slug_loses_letters <- function(x) {
  chars <- strsplit(enc2utf8(x), "", fixed = TRUE)[[1]]
  wanted <- chars[grepl("^[\\p{L}\\p{N}]$", chars, perl = TRUE) & !grepl("^[A-Za-z0-9]$", chars)]
  if (!length(wanted)) return(FALSE)
  ascii <- iconv(wanted, to = "ASCII//TRANSLIT", sub = "")
  any(is.na(ascii) | !grepl("[A-Za-z0-9]", ascii))
}

# The directory slug is derived once from the display name, which is kept
# unchanged in the spec. Names whose letters cannot all be transliterated get a
# short hash of the full name, so that distinct names keep distinct slugs; long
# names are shortened the same way.
safe_slug <- function(x) {
  scalar_text(x, "name")
  if (grepl("[/\\\\]", x) || x %in% c(".", "..")) {
    abort_cttir("Names cannot contain path separators or traversal.", field = "name")
  }
  slug <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = "_"))
  slug <- gsub("[^a-z0-9]+", "_", slug)
  slug <- gsub("^_+|_+$", "", slug)
  if (slug_loses_letters(x)) slug <- paste0(if (nzchar(slug)) slug else "project", "_", slug_hash(x))
  if (!nzchar(slug)) {
    abort_cttir("The name needs at least one letter or digit to derive a directory name.", field = "name")
  }
  if (!grepl("^[a-z]", slug)) slug <- paste0("project_", slug)
  if (nchar(slug) > 80L) slug <- paste0(sub("_+$", "", substr(slug, 1L, 71L)), "_", slug_hash(x))
  if (grepl(reserved_device_names, slug)) {
    abort_cttir(sprintf("The name '%s' maps to the reserved Windows device name '%s'.", x, slug), field = "name",
      remediation = "Choose another project name, for example by adding a word.")
  }
  check_slug(slug)
  slug
}

check_slug <- function(x, field = "slug") {
  scalar_text(x, field)
  if (!grepl("^[a-z][a-z0-9_]*$", x)) {
    abort_cttir(sprintf("The slug '%s' must start with a lowercase ASCII letter and contain only lowercase letters, digits and underscores.", x),
      field = field)
  }
  if (nchar(x) > 80L) {
    abort_cttir(sprintf("The slug '%s...' has %d characters; at most 80 are allowed.", substr(x, 1L, 24L), nchar(x)), field = field)
  }
  if (grepl(reserved_device_names, x)) {
    abort_cttir(sprintf("The slug '%s' is a reserved Windows device name.", x), field = field,
      remediation = "Choose another slug, for example by adding a word.")
  }
  invisible(x)
}

# Readable summary of JSON Schema validation errors: the first offending JSON
# Pointer and one clause per error.
schema_error_detail <- function(errors) {
  if (!is.data.frame(errors) || !nrow(errors)) return(list(pointer = NULL, text = "the document does not match the schema"))
  path <- if ("instancePath" %in% names(errors)) errors$instancePath else rep("", nrow(errors))
  params <- if (is.data.frame(errors$params)) errors$params else data.frame(row.names = seq_len(nrow(errors)))
  pointers <- character()
  clauses <- character()
  for (i in seq_len(nrow(errors))) {
    pointer <- if (nzchar(path[[i]])) path[[i]] else "/"
    clause <- switch(as.character(errors$keyword[[i]]),
      additionalProperties = {
        pointer <- paste0(sub("/$", "", pointer), "/", params$additionalProperty[[i]])
        "is not a recognized field"
      },
      required = {
        pointer <- paste0(sub("/$", "", pointer), "/", params$missingProperty[[i]])
        "is required"
      },
      enum = paste("must be one of:", paste(unlist(params$allowedValues[[i]]), collapse = ", ")),
      as.character(errors$message[[i]]))
    pointers <- c(pointers, pointer)
    clauses <- c(clauses, paste(pointer, clause))
  }
  list(pointer = pointers[[1]], text = paste(unique(clauses), collapse = "; "))
}

# Empty strings are never answers (file 04): an unknown is null, and absence
# leaves a value unchanged. Leaves equal to their counterpart in `base` are not
# checked, so values stored by an older version never block unrelated edits.
check_empty_strings <- function(x, base = NULL, prefix = "") {
  if (!is.list(x)) return(invisible(x))
  keyed <- is.null(names(x))
  for (i in seq_along(x)) {
    value <- x[[i]]
    key <- if (keyed) as.character(i - 1L) else names(x)[[i]]
    pointer <- paste0(prefix, "/", key)
    if (pointer == "/research/notes" || startsWith(pointer, "/extensions")) next
    old <- NULL
    if (is.list(base) && !keyed) {
      old <- base[[key]]
    } else if (is.list(base) && is.list(value) && is.character(value$id)) {
      for (row in base) if (is.list(row) && identical(row$id, value$id)) old <- row
    }
    if (is.list(value)) {
      check_empty_strings(value, old, pointer)
    } else if (is.character(value) && length(value) == 1L && !is.na(value) && !nzchar(trimws(value)) &&
        !identical(value, old)) {
      abort_cttir(sprintf("%s is an empty string, which is not a valid answer.", pointer),
        "cttir_input_error", "empty_value", field = pointer,
        remediation = "Use null (R NULL, YAML ~) to record that the value is unknown, or leave the field out to keep it unchanged.")
    }
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
  check_slug(spec$project$slug, "/project/slug")
  for (key in c("publications", "data_sources", "packages")) {
    field <- if (key == "packages") "name" else "id"
    ids <- vapply(spec[[key]], function(x) x[[field]], character(1))
    if (anyDuplicated(tolower(ids))) abort_cttir(paste("Duplicate", key, "identities."), "cttir_schema_error")
  }
  slugs <- vapply(seq_along(spec$publications), function(i) {
    check_slug(spec$publications[[i]]$slug, paste0("/publications/", i - 1L, "/slug"))
    spec$publications[[i]]$slug
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
  check_empty_strings(config)
  check_empty_strings(options)
  # Required arguments win over conflicting config values (file 04); say so.
  required <- list(name = name, type = type, goal = goal)
  overridden <- Filter(function(key) !is.null(config$project[[key]]) && !identical(config$project[[key]], required[[key]]),
    names(required))
  for (key in overridden) {
    message <- sprintf("The configuration sets project.%s, which the required `%s` argument overrides.", key, key)
    warning(warningCondition(message, class = c("cttir_config_override", "cttir_warning")))
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
  spec$knowledge <- NULL
  record <- function(field, origin, reason, evidence = list()) {
    list(field = field, origin = origin, reason = reason, evidence_ids = as.list(evidence))
  }
  decisions <- list()
  # Goal keywords (or, when policy enables it, one bounded local planner
  # exchange; see plan_goal()) only fill fields that nobody supplied; they never
  # approve anything.
  unset <- c(vapply(c("aim", "outcome_family", "unit_structure"), function(field) {
    is.null(combined$analysis[[field]]) && identical(spec$analysis[[field]], "unknown")
  }, logical(1)), modality = is.null(combined$ecosystem$modality) && identical(spec$ecosystem$modality, "unknown"))
  planned <- planner_signals(name, type, goal, any(unset), replay = !is.null(provenance))
  signals <- planned$signals
  for (field in c("aim", "outcome_family", "unit_structure")) {
    if (unset[[field]] && !identical(signals[[field]], "unknown")) {
      spec$analysis[[field]] <- signals[[field]]
      decisions[[length(decisions) + 1L]] <- planner_record(planned, paste0("/analysis/", field), signals[[field]],
        paste0("Goal keywords suggest '", signals[[field]], "'. Review before analysis; this is not an approval."),
        paste0("rule:", field, ":", signals[[field]]))
    }
  }
  if (unset[["modality"]] && !identical(signals$modality, "unknown")) {
    spec$ecosystem$modality <- signals$modality
    decisions[[length(decisions) + 1L]] <- planner_record(planned, "/ecosystem/modality", signals$modality,
      paste0("Goal keywords suggest the '", signals$modality, "' data modality."), paste0("modality:", signals$modality))
  }
  applied <- planner_apply(planned, spec, decisions)
  spec <- applied$spec
  decisions <- applied$decisions
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
        reason <- if (identical(origin, "config") && field %in% paste0("/project/", overridden)) {
          "Configuration value overridden by the required argument."
        } else {
          "Supplied customization."
        }
        decisions[[length(decisions) + 1L]] <<- record(field, origin, reason)
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
  check_empty_strings(config, saved)
  check_empty_strings(options, saved)
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
