.capability_cache <- new.env(parent = emptyenv())

capability_fields <- c("id", "family", "stage", "title", "packages", "adapter", "applies", "keywords",
  "infrastructure", "specialist", "requirements", "status")

capability_stages <- c("project", "import", "check", "tidy", "describe", "figures", "model", "effects",
  "report", "pipeline", "environment", "preprocess", "analysis", "design", "interop")

# Reviewed capability records are static package resources, one file per module.
capability_registry <- function() {
  root <- resource_file("extdata", "capabilities")
  files <- sort(list.files(root, "\\.json$", full.names = TRUE), method = "radix")
  key <- content_hash(paste(vapply(files, function(f) digest::digest(file = f, algo = "sha256"), character(1)), collapse = ""))
  if (exists(key, envir = .capability_cache, inherits = FALSE)) {
    return(get(key, envir = .capability_cache, inherits = FALSE))
  }
  fail <- function(message) abort_cttir(message, "cttir_schema_error", "invalid_capability_registry")
  registry <- list(modalities = list(), rules = list(), capabilities = list())
  for (file in files) {
    doc <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    if (!identical(doc$schema_version, 1L) || !is.list(doc$capabilities)) fail("Unsupported capability registry file.")
    registry$modalities <- c(registry$modalities, doc$modalities)
    for (key_name in names(doc$rules)) registry$rules[[key_name]] <- c(registry$rules[[key_name]], doc$rules[[key_name]])
    for (cap in doc$capabilities) {
      if (!setequal(names(cap), capability_fields) || !is.character(cap$id) || length(cap$id) != 1L ||
          !grepl("^[a-z][a-z0-9_]*([.][a-z0-9_]+)+$", cap$id) || !cap$stage %in% capability_stages ||
          !cap$status %in% c("candidate", "adapter_tested") || !length(cap$packages) ||
          !is.logical(cap$infrastructure) || !is.logical(cap$specialist) ||
          (identical(cap$status, "adapter_tested") && is.null(cap$adapter))) {
        fail("A capability record is malformed.")
      }
      cap$packages <- unlist(cap$packages, use.names = FALSE)
      registry$capabilities[[cap$id]] <- cap
    }
  }
  ids <- vapply(registry$capabilities, function(x) x$id, character(1))
  if (anyDuplicated(ids)) fail("Capability IDs must be unique.")
  if (length(ls(.capability_cache)) >= 2L) rm(list = ls(.capability_cache), envir = .capability_cache)
  assign(key, registry, envir = .capability_cache)
  registry
}

keyword_hit <- function(text, keywords) {
  words <- unlist(c(keywords$en, keywords$de), use.names = FALSE)
  if (!length(words)) return(FALSE)
  any(vapply(words, function(k) grepl(tolower(k), text, fixed = TRUE), logical(1)))
}

# Conservative deterministic reading of the goal text. Ambiguous or absent
# signals stay unknown; nothing here approves an analysis.
infer_goal <- function(goal, registry = capability_registry()) {
  text <- tolower(enc2utf8(goal))
  pick <- function(rules, ordered) {
    hits <- unique(vapply(Filter(function(r) keyword_hit(text, r$keywords), rules), function(r) r$value, character(1)))
    if (!length(hits)) return("unknown")
    if (ordered || length(hits) == 1L) hits[[1]] else "unknown"
  }
  modalities <- vapply(Filter(function(m) keyword_hit(text, m$keywords), registry$modalities),
    function(m) m$id, character(1))
  modality <- if (length(modalities) == 1L) modalities else if (length(modalities) > 1L) "multiomics" else "unknown"
  matched <- Filter(function(cap) {
    !cap$infrastructure && (cap$specialist || cap$stage == "design") && keyword_hit(text, cap$keywords)
  }, registry$capabilities)
  list(
    aim = pick(registry$rules$aim, TRUE),
    outcome_family = pick(registry$rules$outcome_family, FALSE),
    unit_structure = pick(registry$rules$unit_structure, TRUE),
    modality = modality,
    keyword_capabilities = unname(vapply(matched, function(x) x$id, character(1)))
  )
}

# Approval lookup in a catalog snapshot: approved decisions bound to the exact
# cataloged revision and the adapter that would run.
capability_approval <- function(cap, catalog) {
  packages <- setdiff(cap$packages, "base")
  if (!length(packages)) return(list(status = "approved", approvals = list(), missing = character()))
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  approvals <- list()
  missing <- character()
  for (name in packages) {
    record <- index[[name]]
    hit <- NULL
    for (decision in record$approvals) {
      if (identical(decision$status, "approved") && identical(decision$source_hash, record$source_hash) &&
          identical(decision$adapter_id, cap$adapter$id) && identical(decision$adapter_version, cap$adapter$version)) {
        hit <- decision
      }
    }
    if (is.null(hit)) missing <- c(missing, name) else approvals[[name]] <- hit$approval_id
  }
  list(status = if (length(missing)) "approval_pending" else "approved", approvals = approvals, missing = missing)
}

route_stage <- function(registry, catalog, stage, id, enabled = TRUE, packages = NULL) {
  cap <- registry$capabilities[[id]]
  if (is.null(cap)) abort_cttir("A routed capability is not registered.", "cttir_api_mismatch", "unknown_capability")
  if (!is.null(packages)) cap$packages <- packages
  approval <- if (identical(cap$status, "candidate")) {
    list(status = "candidate_gap", approvals = list(), missing = cap$packages)
  } else {
    capability_approval(cap, catalog)
  }
  list(stage = stage, capability = id, family = cap$family,
    adapter = if (is.null(cap$adapter)) NULL else paste0(cap$adapter$id, "@", cap$adapter$version),
    packages = as.list(cap$packages), enabled = enabled, status = approval$status,
    approvals = approval$approvals, unapproved = as.list(approval$missing))
}

describe_capability <- function(backend) {
  switch(backend, DescrTab2 = "std.describe.descrtab2", gtsummary = "std.describe.gtsummary", "std.describe.base")
}

engine_capability <- c("stats::lm" = "std.model.lm", "stats::glm" = "std.model.glm_binomial",
  "nlme::lme" = "std.model.lme", "survival::coxph" = "std.model.coxph")

# Deterministic profile and stage routing. Specialist evidence requires an
# adapter-tested capability with approvals; infrastructure never counts.
route_workflow <- function(spec, requested = "auto", catalog = catalog_snapshot(spec$provenance$catalog_id)) {
  registry <- capability_registry()
  analysis <- spec$analysis
  modality <- if (is.null(spec$ecosystem$modality)) "unknown" else spec$ecosystem$modality
  signals <- infer_goal(spec$project$goal, registry)
  gaps <- character()
  stages <- list(
    route_stage(registry, catalog, "project", "std.project.reflowr_layout"),
    route_stage(registry, catalog, "import", "std.import.delimited"),
    route_stage(registry, catalog, "check", "std.check.mapped"),
    route_stage(registry, catalog, "tidy", "std.tidy.dplyr"),
    route_stage(registry, catalog, "describe", describe_capability(spec$workflow$table_backend)),
    route_stage(registry, catalog, "figures", "std.figures.accessible")
  )
  plan <- analysis_configuration(spec)
  engine <- plan$candidate_engine
  if (!is.null(engine)) {
    effects_package <- if (engine == "nlme::lme") "broom.mixed" else "broom"
    stages <- c(stages, list(
      route_stage(registry, catalog, "model", engine_capability[[engine]]),
      route_stage(registry, catalog, "effects", "std.effects.broom", packages = effects_package)
    ))
  } else if (identical(analysis$aim, "predictive")) {
    stages <- c(stages, list(route_stage(registry, catalog, "model", "std.prediction.tidymodels", enabled = FALSE)))
    gaps <- c(gaps, "prediction_adapter_pending")
  } else if (identical(analysis$aim, "causal")) {
    gaps <- c(gaps, "causal_design_adapter_unavailable")
  } else if (identical(analysis$aim, "explanatory")) {
    if ("unknown" %in% c(analysis$outcome_family, analysis$unit_structure)) {
      gaps <- c(gaps, "model_requires_outcome_family_and_unit_structure")
    } else {
      candidates <- Filter(function(cap) cap$stage == "model" && analysis$outcome_family %in% cap$applies$outcome_family,
        registry$capabilities)
      if (length(candidates)) {
        stages <- c(stages, list(route_stage(registry, catalog, "model", candidates[[1]]$id, enabled = FALSE)))
      }
      gaps <- c(gaps, "unsupported_outcome_or_unit_structure")
    }
  }
  stages <- c(stages, list(route_stage(registry, catalog, "report", "std.report.render")))
  ecosystem <- list()
  if (!modality %in% c("unknown", "tabular")) {
    allow_seurat <- isTRUE(spec$ecosystem$seurat_for_relevant_gaps)
    relevant <- Filter(function(cap) {
      modality %in% cap$applies$modality && !cap$specialist && !identical(cap$family, "standard") &&
        (allow_seurat || !identical(cap$family, "seurat"))
    }, registry$capabilities)
    ecosystem <- lapply(relevant, function(cap) route_stage(registry, catalog, cap$stage, cap$id, enabled = FALSE))
    gaps <- c(gaps, paste0("modality_workflow_requires_review:", modality))
  }
  is_specialist <- function(id) isTRUE(registry$capabilities[[id]]$specialist)
  by_keyword <- Filter(is_specialist, signals$keyword_capabilities)
  by_modality <- Filter(function(cap) cap$specialist && modality %in% cap$applies$modality, registry$capabilities)
  specialist_ids <- unique(c(unlist(by_keyword), vapply(by_modality, function(cap) cap$id, character(1))))
  specialist <- lapply(specialist_ids, function(id) route_stage(registry, catalog, registry$capabilities[[id]]$stage, id, enabled = FALSE))
  available <- Filter(function(x) identical(x$status, "approved"), specialist)
  design <- lapply(Filter(function(id) identical(registry$capabilities[[id]]$stage, "design"), signals$keyword_capabilities),
    function(id) route_stage(registry, catalog, "design", id, enabled = FALSE))
  automatic <- if (length(available)) "hybrid" else "standard_reflowR"
  if (!identical(requested, "auto") && !identical(requested, "standard_reflowR") && !length(available)) {
    abort_cttir("No approved CTTIR specialist adapter matches this project; the requested profile cannot be satisfied.",
      "cttir_api_mismatch", "specialist_adapter_unavailable", field = "/workflow/profile",
      remediation = "Use profile 'auto' or 'standard_reflowR'; specialist candidates are listed by route_workflow().")
  }
  profile <- if (identical(requested, "auto")) automatic else requested
  if (length(specialist) && !length(available)) gaps <- c(gaps, paste0("specialist_adapter_pending:", specialist_ids))
  for (stage in available) {
    stage$enabled <- TRUE
    stages <- c(stages, list(stage))
  }
  reason <- if (length(available)) {
    "Approved CTTIR specialist stages cover part of the workflow; standard stages cover the rest."
  } else if (length(specialist)) {
    "CTTIR specialist candidates match the goal but none has an approved adapter; the standard workflow is used and the gap is recorded."
  } else {
    "No CTTIR specialist capability matches; reflowR layout and standard R stages are used."
  }
  pending <- vapply(stages, function(x) isTRUE(x$enabled) && identical(x$status, "approval_pending"), logical(1))
  list(profile = profile, requested = requested, reason = reason, engine = engine, modality = modality,
    stages = stages, specialist = specialist, ecosystem = ecosystem, design = design,
    gaps = unique(gaps), approval_pending = vapply(stages[pending], function(x) x$capability, character(1)))
}

# Lean per-project dependency manifest derived from enabled stages and the
# pinned catalog; base R is pinned by the R version, not as a package install.
route_dependencies <- function(route, catalog) {
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  rows <- list()
  for (stage in route$stages) {
    if (!isTRUE(stage$enabled)) next
    for (package in unlist(stage$packages)) {
      if (package == "base") next
      record <- index[[package]]
      key <- package
      roles <- if (is.null(rows[[key]])) character() else unlist(rows[[key]]$stages)
      rows[[key]] <- list(package = package, family = stage$family,
        version = if (is.null(record)) NULL else record$version,
        source_hash = if (is.null(record)) NULL else record$source_hash,
        required = TRUE, stages = as.list(unique(c(roles, stage$stage))),
        approval = stage$approvals[[package]])
    }
  }
  unname(rows[sort(names(rows), method = "radix")])
}

route_summary <- function(route) {
  list(profile = route$profile, bundle = "standard-0.3.0", engine = route$engine, modality = route$modality,
    stages = lapply(route$stages, function(x) x[c("stage", "capability", "adapter", "enabled", "status")]),
    gaps = as.list(route$gaps))
}
