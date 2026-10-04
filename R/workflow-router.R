.capability_cache <- new.env(parent = emptyenv())

capability_fields <- c("id", "family", "stage", "title", "packages", "adapter", "applies", "keywords",
  "infrastructure", "specialist", "requirements", "status")

capability_stages <- c("project", "import", "check", "tidy", "describe", "figures", "model", "effects",
  "report", "pipeline", "environment", "preprocess", "analysis", "design", "interop", "container", "bridge",
  "aggregation", "normalization", "annotation", "interchange", "acceleration", "data_distribution", "demo")

# Reviewed capability records are static package resources, one file per module.
capability_registry <- function() {
  root <- resource_file("extdata", "capabilities")
  files <- sort(list.files(root, "\\.json$", full.names = TRUE), method = "radix")
  key <- content_hash(paste(vapply(files, function(f) digest::digest(file = f, algo = "sha256"), character(1)), collapse = ""))
  if (exists(key, envir = .capability_cache, inherits = FALSE)) {
    return(get(key, envir = .capability_cache, inherits = FALSE))
  }
  fail <- function(message) abort_cttir(message, "cttir_schema_error", "invalid_capability_registry")
  registry <- list(modalities = list(), rules = list(), capabilities = list(), required_callables = list())
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
    for (id in names(doc$required_callables)) {
      registry$required_callables[[id]] <- unlist(doc$required_callables[[id]], use.names = FALSE)
    }
  }
  ids <- vapply(registry$capabilities, function(x) x$id, character(1))
  if (anyDuplicated(ids)) fail("Capability IDs must be unique.")
  # Callables a capability needs are attached after the records are checked,
  # so capability_approval() can require each one to be approved.
  for (id in names(registry$required_callables)) {
    callables <- registry$required_callables[[id]]
    if (is.null(registry$capabilities[[id]]) || !is.character(callables) ||
        !all(grepl("^[A-Za-z][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$", callables)) ||
        !all(sub("::.*$", "", callables) %in% registry$capabilities[[id]]$packages)) {
      fail("A required-callable record is malformed.")
    }
    registry$capabilities[[id]]$callables <- callables
  }
  if (length(ls(.capability_cache)) >= 2L) rm(list = ls(.capability_cache), envir = .capability_cache)
  assign(key, registry, envir = .capability_cache)
  registry
}

# Specialist routing evidence is reserved for CTTIR-family capabilities.
is_cttir_specialist <- function(cap) isTRUE(cap$specialist) && identical(cap$family, "cttir")

# A keyword matches whole words: it starts at a word boundary and may carry a
# short inflectional ending ("figures", "logistische"). A trailing "*" marks a
# deliberate stem that matches any continuation ("vorhersag*", "trajector*");
# a leading "*" lets a German compound end in the keyword ("*zytometrie" in
# "Durchflusszytometrie").
keyword_pattern <- function(keyword) {
  keyword <- tolower(enc2utf8(keyword))
  stem <- endsWith(keyword, "*")
  compound <- startsWith(keyword, "*")
  body <- gsub("([][{}()^$.|*+?\\\\])", "\\\\\\1", gsub("^[*]|[*]$", "", keyword))
  ending <- if (stem) "" else "(?:s|es|e|en|er|em|n|ed|d|ing)?(?![\\p{L}\\p{N}])"
  paste0(if (compound) "" else "(?<![\\p{L}\\p{N}])", body, ending)
}

# A keyword directly negated ("not a survival study", "without repeated
# measures", "keine Messwiederholung") is not a signal.
negation_prefix <- paste0("(?:^|[^\\p{L}\\p{N}/-])(?:not|no|without|non|never|nor|neither|kein|keine|keinen|keinem|",
  "keiner|keines|ohne|nicht|nie)(?:\\s+|-)(?:(?:a|an|the|any|ein|eine|einen|einem|einer|der|die|das|den|dem)\\s+)?$")

# TRUE when `pattern` (PCRE) matches somewhere that is not directly negated.
affirmed_hit <- function(text, pattern) {
  text <- enc2utf8(text)
  starts <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (starts[[1]] < 0L) return(FALSE)
  any(vapply(starts, function(start) {
    !grepl(negation_prefix, substr(text, max(1L, start - 40L), start - 1L), perl = TRUE)
  }, logical(1)))
}

keyword_hit <- function(text, keywords) {
  words <- unlist(c(keywords$en, keywords$de), use.names = FALSE)
  if (!length(words)) return(FALSE)
  affirmed_hit(text, paste(vapply(words, keyword_pattern, character(1)), collapse = "|"))
}

# Event-type outcome words (death, complications, readmission, relapse) make a
# measured-value reading of the outcome unsafe, so they veto "continuous".
event_outcome_veto <- function(text, registry, outcome) {
  identical(outcome, "continuous") &&
    any(vapply(registry$rules$event_outcome, function(r) keyword_hit(text, r$keywords), logical(1)))
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
  outcome <- pick(registry$rules$outcome_family, FALSE)
  if (event_outcome_veto(text, registry, outcome)) outcome <- "unknown"
  # Registry order encodes specificity: "single-cell RNA-seq" is single-cell,
  # not bulk RNA; multi-omics needs its own explicit keywords.
  modalities <- vapply(Filter(function(m) keyword_hit(text, m$keywords), registry$modalities),
    function(m) m$id, character(1))
  modality <- if (length(modalities)) modalities[[1]] else "unknown"
  matched <- Filter(function(cap) {
    !cap$infrastructure && (is_cttir_specialist(cap) || cap$stage == "design") && keyword_hit(text, cap$keywords)
  }, registry$capabilities)
  list(
    aim = pick(registry$rules$aim, TRUE),
    outcome_family = outcome,
    unit_structure = pick(registry$rules$unit_structure, TRUE),
    modality = modality,
    keyword_capabilities = unname(vapply(matched, function(x) x$id, character(1)))
  )
}

# Approved decisions of one package record for one adapter at its exact revision.
adapter_decisions <- function(record, adapter) {
  Filter(function(decision) {
    identical(decision$status, "approved") && identical(decision$source_hash, record$source_hash) &&
      identical(decision$adapter_id, adapter$id) && identical(decision$adapter_version, adapter$version)
  }, record$approvals)
}

# Records from `package` to the owner of `name`, following at most three
# reexport hops; NULL when the chain does not end in an owned export.
callable_chain <- function(index, package, name) {
  record <- index[[package]]
  chain <- list()
  for (hop in 0:3) {
    entry <- if (is.null(record)) list() else Filter(function(x) identical(x$name, name), record$exports)
    if (!length(entry)) return(NULL)
    chain <- c(chain, list(record))
    if (!identical(entry[[1]]$kind, "reexport")) return(chain)
    record <- index[[if (is.null(entry[[1]]$owner_package)) "" else entry[[1]]$owner_package]]
  }
  NULL
}

callable_owner <- function(index, package, name) {
  chain <- callable_chain(index, package, name)
  if (is.null(chain)) NULL else chain[[length(chain)]]
}

# A required callable counts only when its owning revision carries a complete
# approval for the capability's adapter that lists it.
callable_approved <- function(index, callable, adapter) {
  parts <- strsplit(callable, "::", fixed = TRUE)[[1]]
  owner <- callable_owner(index, parts[[1]], parts[[2]])
  if (is.null(owner) || is.null(approved_export_index(owner)[[parts[[2]]]])) return(FALSE)
  any(vapply(adapter_decisions(owner, adapter), function(d) {
    parts[[2]] %in% c(approval_text(d$required_callables), approval_text(d$required_objects))
  }, logical(1)))
}

# Approval lookup in a catalog snapshot: approved decisions bound to the exact
# cataloged revision and the adapter that would run. A package-level decision
# is not enough when the capability declares callables (for example clustering
# functions): each of them must be approved for that adapter as well.
# Companion approval needs explicit text records tied to the indexed revision.
# Authors@R stays source text; only a literal grammar can establish cre roles.
companion_provenance_valid <- function(record) {
  if (is.null(record)) return(FALSE)
  evidence <- record$maintainer_evidence
  text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  if (is.null(evidence) || !text(evidence$source_hash) || !text(evidence$description_sha256) ||
      !text(evidence$description_file) ||
      !(text(evidence$author) || text(evidence$authors_r_literal))) return(FALSE)
  if (!text(evidence$maintainer) &&
      !description_has_maintainer(description_roles(evidence$authors_r_literal))) return(FALSE)
  identical(evidence$extraction, "dcf_text_no_execution") &&
    identical(evidence$source_hash, record$source_hash) &&
    identical(evidence$description_sha256, record$source_files[[evidence$description_file]])
}

capability_approval <- function(cap, catalog) {
  packages <- setdiff(cap$packages, "base")
  if (!length(packages)) return(list(status = "approved", approvals = list(), missing = character()))
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  approvals <- list()
  missing <- character()
  for (name in packages) {
    record <- index[[name]]
    hits <- if (is.null(record)) list() else adapter_decisions(record, cap$adapter)
    if (!length(hits)) missing <- c(missing, name) else approvals[[name]] <- hits[[length(hits)]]$approval_id
    if (name %in% c("SeuratDisk", "BPCells", "presto", "glmGamPoi") && !companion_provenance_valid(record)) {
      missing <- c(missing, paste0(name, ":maintainer_provenance"))
    }
  }
  # A stage may narrow the packages (broom or broom.mixed); only their callables apply.
  required <- Filter(function(x) sub("::.*$", "", x) %in% cap$packages, cap$callables)
  unapproved <- Filter(function(x) !callable_approved(index, x, cap$adapter), required)
  missing <- c(missing, unlist(unapproved))
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

# Time-to-event projects draw a Kaplan-Meier panel, which needs survival.
figure_packages <- function(registry, analysis) {
  packages <- registry$capabilities[["std.figures.accessible"]]$packages
  if (identical(analysis$outcome_family, "time_to_event")) packages <- c(packages, "survival")
  packages
}

describe_capability <- function(backend) {
  switch(backend, DescrTab2 = "std.describe.descrtab2", gtsummary = "std.describe.gtsummary", "std.describe.base")
}

engine_capability <- c("stats::lm" = "std.model.lm", "stats::glm" = "std.model.glm_binomial",
  "nlme::lme" = "std.model.lme", "survival::coxph" = "std.model.coxph")

# Ecosystem advice fills a requested gap only. Shared modality alone does not
# request every container, normalization method or annotation service.
ecosystem_gap_candidates <- function(registry, modality, goal, allow_seurat, stages,
  allowed_providers = c("bioconductor", "seurat")) {
  if (modality %in% c("unknown", "tabular")) return(list())
  covered <- vapply(Filter(function(stage) {
    cap <- registry$capabilities[[stage$capability]]
    isTRUE(stage$enabled) && identical(stage$status, "approved") && is_cttir_specialist(cap)
  }, stages), function(stage) stage$stage, character(1))
  text <- tolower(enc2utf8(goal))
  Filter(function(cap) {
    cap$family %in% allowed_providers && modality %in% cap$applies$modality &&
      (allow_seurat || !identical(cap$family, "seurat")) && !cap$stage %in% covered &&
      keyword_hit(text, cap$keywords)
  }, registry$capabilities)
}

# Deterministic profile and stage routing. Specialist evidence requires an
# adapter-tested capability with approvals; infrastructure never counts.
route_workflow <- function(spec, requested = "auto", catalog = catalog_snapshot(spec$provenance$catalog_id)) {
  registry <- capability_registry()
  analysis <- spec$analysis
  modality <- if (is.null(spec$ecosystem$modality)) "unknown" else spec$ecosystem$modality
  signals <- infer_goal(spec$project$goal, registry)
  screened <- any(spec$provenance$fallback_reason %in% c("goal_injection_suspected", "injection_suspected")) ||
    instruction_like(paste(spec$project$name, spec$project$type, spec$project$goal, sep = "\n"), hard_only = TRUE)
  if (screened) signals$keyword_capabilities <- character()
  gaps <- character()
  stages <- list(
    route_stage(registry, catalog, "project", "std.project.reflowr_layout"),
    route_stage(registry, catalog, "import", "std.import.delimited"),
    route_stage(registry, catalog, "check", "std.check.mapped"),
    route_stage(registry, catalog, "tidy", "std.tidy.dplyr"),
    route_stage(registry, catalog, "describe", describe_capability(spec$workflow$table_backend)),
    route_stage(registry, catalog, "figures", "std.figures.accessible",
      packages = figure_packages(registry, analysis))
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
  closing <- list(route_stage(registry, catalog, "demo", "std.demo.synthetic"),
    route_stage(registry, catalog, "report", "std.report.render"))
  stages <- c(stages, closing)
  # Orchestration is infrastructure: it adds targets to the lean manifest only when requested.
  if (identical(spec$workflow$pipeline, "targets")) {
    stages <- c(stages, list(route_stage(registry, catalog, "pipeline", "std.pipeline.targets")))
  }
  if (!modality %in% c("unknown", "tabular")) {
    gaps <- c(gaps, paste0("modality_workflow_requires_review:", modality))
  }
  by_keyword <- Filter(function(id) is_cttir_specialist(registry$capabilities[[id]]), signals$keyword_capabilities)
  by_modality <- Filter(function(cap) is_cttir_specialist(cap) && modality %in% cap$applies$modality, registry$capabilities)
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
  relevant <- ecosystem_gap_candidates(registry, modality, if (screened) "" else spec$project$goal,
    isTRUE(spec$ecosystem$seurat_for_relevant_gaps), stages,
    if (is.null(spec$ecosystem$allowed_providers)) c("bioconductor", "seurat") else unlist(spec$ecosystem$allowed_providers))
  ecosystem <- lapply(relevant, function(cap) route_stage(registry, catalog, cap$stage, cap$id, enabled = FALSE))
  reason <- if (length(available)) {
    "Approved CTTIR specialist stages cover part of the workflow; standard stages cover the rest."
  } else if (length(specialist)) {
    "CTTIR specialist candidates match the goal but none has an approved adapter; the standard workflow is used and the gap is recorded."
  } else {
    "No CTTIR specialist capability matches; reflowR layout and standard R stages are used."
  }
  pending <- vapply(stages, function(x) isTRUE(x$enabled) && identical(x$status, "approval_pending"), logical(1))
  optional <- interop_optional_packages(registry, modality)
  notes <- if (length(optional)) {
    paste0("code/R/cttir_interop.R calls ", paste(names(optional), collapse = ", "), "; they are pinned at their ",
      "catalog versions as optional (required = FALSE) dependencies and are not installed by default.")
  } else {
    character()
  }
  list(profile = profile, requested = requested, reason = reason, engine = engine, modality = modality,
    stages = stages, specialist = specialist, ecosystem = ecosystem, design = design,
    gaps = unique(gaps), approval_pending = vapply(stages[pending], function(x) x$capability, character(1)),
    optional_packages = optional, notes = notes)
}

# Packages called by the interoperability template, which modality projects
# receive: the packages of capabilities backed by the reviewed interop adapters.
interop_optional_packages <- function(registry, modality) {
  path <- sub("^[^/]+/", "", interop_adapters()$template[[1]])
  condition <- standard_bundle_manifest()$conditional[[path]]
  if (is.null(condition) || !bundle_condition_met(condition, list(ecosystem = list(modality = modality)))) {
    return(character())
  }
  out <- character()
  for (cap in registry$capabilities) {
    if (!isTRUE(cap$adapter$id %in% interop_adapters()$id)) next
    for (package in setdiff(cap$packages, c("base", names(out)))) out[[package]] <- cap$family
  }
  out[sort(names(out), method = "radix")]
}

# Owners (and intermediate reexporters) of approved callables that a package
# re-exports: broom::tidy is generics::tidy, so generics is pinned with broom.
reexport_owners <- function(index, record) {
  out <- list()
  for (entry in record$exports) {
    if (!identical(entry$kind, "reexport")) next
    chain <- callable_chain(index, record$name, entry$name)
    if (is.null(chain) || length(chain) < 2L) next
    owner <- chain[[length(chain)]]
    covering <- approved_export_index(owner)[[entry$name]]
    if (is.null(covering)) next
    for (link in chain[-1]) {
      if (is.null(out[[link$name]])) out[[link$name]] <- list(record = link, approval = covering[[1]]$approval_id)
    }
  }
  out
}

# Lean per-project dependency manifest derived from enabled stages and the
# pinned catalog; base R is pinned by the R version, not as a package install.
# One pinned dependency; `stage` is added to the stages already recorded.
dependency_row <- function(previous, package, family, record, stage, approval, required = TRUE) {
  roles <- if (is.null(previous)) character() else unlist(previous$stages)
  row <- list(package = package, family = family,
    version = if (is.null(record)) NULL else record$version,
    source_hash = if (is.null(record)) NULL else record$source_hash,
    required = required, stages = as.list(unique(c(roles, stage))), approval = approval)
  # Bioconductor revisions belong to one release, which fixes the compatible R.
  release <- bioc_release_of(record)
  if (!is.null(release)) row$bioc_release <- release
  row
}

bioc_release_of <- function(record) {
  revision <- if (is.null(record)) NULL else record$revision
  if (!is.character(revision) || length(revision) != 1L || !grepl("^bioc-[0-9]+[.][0-9]+:", revision)) return(NULL)
  sub("^bioc-([0-9]+[.][0-9]+):.*$", "\\1", revision)
}

route_dependencies <- function(route, catalog) {
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  rows <- list()
  for (stage in route$stages) {
    if (!isTRUE(stage$enabled)) next
    for (package in unlist(stage$packages)) {
      if (package == "base") next
      record <- index[[package]]
      rows[[package]] <- dependency_row(rows[[package]], package, stage$family, record, stage$stage,
        stage$approvals[[package]])
      if (is.null(record)) next
      for (owner in reexport_owners(index, record)) {
        name <- owner$record$name
        if (name %in% unlist(stage$packages)) next
        approval <- if (is.null(rows[[name]])) owner$approval else rows[[name]]$approval
        rows[[name]] <- dependency_row(rows[[name]], name, owner$record$family, owner$record, stage$stage, approval)
      }
    }
  }
  # The interop template is optional code: its packages are pinned so a user
  # who runs it gets the catalog revisions, but they are never required.
  for (package in names(route$optional_packages)) {
    if (!is.null(rows[[package]])) next
    rows[[package]] <- dependency_row(NULL, package, route$optional_packages[[package]], index[[package]], "interop",
      NULL, required = FALSE)
  }
  unname(rows[sort(names(rows), method = "radix")])
}

route_summary <- function(route) {
  summary <- list(profile = route$profile, bundle = "standard-0.3.0", engine = route$engine, modality = route$modality,
    stages = lapply(route$stages, function(x) x[c("stage", "capability", "adapter", "enabled", "status")]),
    gaps = as.list(route$gaps))
  if (length(route$notes)) summary$notes <- as.list(route$notes)
  summary
}
