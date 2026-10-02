validate_analysis_mapping <- function(spec) {
  mapping <- spec$analysis$mapping
  if (is.null(mapping)) return(invisible(spec))
  if (!is.null(mapping$data_source_id)) {
    ids <- vapply(spec$data_sources, function(x) x$id, character(1))
    if (!mapping$data_source_id %in% ids) {
      abort_cttir("Analysis mapping must reference a registered data source.", "cttir_schema_error",
        "unknown_data_source", field = "/analysis/mapping/data_source_id")
    }
  }
  roles <- unlist(mapping[c("outcome", "subject", "time", "event")], use.names = FALSE)
  if (anyDuplicated(roles)) {
    abort_cttir("Outcome, subject, time and event roles must use distinct columns.", "cttir_schema_error",
      "conflicting_roles", field = "/analysis/mapping")
  }
  forbidden <- unlist(mapping[c("outcome", "subject", "event")], use.names = FALSE)
  if (any(unlist(mapping$predictors, use.names = FALSE) %in% forbidden)) {
    abort_cttir("Outcome, event and subject identifiers cannot also be predictors in this adapter plan.",
      "cttir_schema_error", "conflicting_predictor", field = "/analysis/mapping/predictors")
  }
  if (!is.null(mapping$event_value) && !is.null(mapping$non_event_value) &&
      identical(as.character(mapping$event_value), as.character(mapping$non_event_value))) {
    abort_cttir("Event and non-event codes must differ.", "cttir_schema_error",
      "identical_event_codes", field = "/analysis/mapping/event_value")
  }
  invisible(spec)
}

analysis_configuration <- function(spec) {
  analysis <- spec$analysis
  mapping <- analysis$mapping
  missing <- character()
  gaps <- character()
  required <- function(fields) {
    for (field in fields) {
      value <- mapping[[field]]
      if (is.null(value) || (field == "missing_data" && identical(value, "unknown"))) {
        missing <<- c(missing, paste0("/analysis/mapping/", field))
      }
    }
  }
  for (field in c("aim", "outcome_family", "unit_structure")) {
    if (is.null(analysis[[field]]) || identical(analysis[[field]], "unknown")) {
      missing <- c(missing, paste0("/analysis/", field))
    }
  }
  engine <- NULL
  if (identical(analysis$aim, "descriptive")) {
    gaps <- c(gaps, "descriptive_adapter_pending")
  } else if (isTRUE(analysis$aim %in% c("predictive", "causal"))) {
    gaps <- c(gaps, "specialist_design_and_validation_required")
  } else if (identical(analysis$aim, "explanatory")) {
    if (identical(analysis$unit_structure, "independent")) {
      engine <- switch(analysis$outcome_family,
        continuous = "stats::lm", binary = "stats::glm",
        time_to_event = "survival::coxph", NULL)
    } else if (isTRUE(analysis$unit_structure %in% c("clustered", "longitudinal")) &&
        identical(analysis$outcome_family, "continuous")) {
      engine <- "nlme::lme"
    }
    if (is.null(engine)) gaps <- c(gaps, "unsupported_outcome_or_unit_structure")
  }
  if (!is.null(analysis$engine) && !identical(analysis$engine, engine)) {
    gaps <- c(gaps, "requested_engine_not_supported_for_this_configuration")
  }
  required(c("data_source_id", "estimand", "missing_data"))
  if (!is.null(engine)) {
    required("predictors")
    if (engine == "survival::coxph") {
      required(c("time", "event", "event_value", "non_event_value", "time_origin", "time_unit"))
    } else {
      required("outcome")
    }
    if (engine == "stats::glm") required(c("event_value", "non_event_value"))
    if (engine == "nlme::lme") {
      required("subject")
      if (identical(analysis$unit_structure, "longitudinal")) required(c("time", "time_origin", "time_unit"))
      gaps <- c(gaps, "random_effects_and_residual_structure_review_required")
    }
  }
  if (!isTRUE(analysis$approved)) missing <- c(missing, "/analysis/approved")
  list(
    state = if (length(missing) || length(gaps)) "incomplete" else "configuration_recorded",
    candidate_engine = engine, requested_engine = analysis$engine,
    missing_fields = as.list(unique(missing)), capability_gaps = as.list(unique(gaps)),
    executable = FALSE,
    blockers = as.list(c("adapter_and_revision_approval_pending", "data_checks_not_run")),
    limitations = as.list(c("Candidate routing is not methodological or workflow approval.",
        "No dataset is opened and no formula, expression or model is evaluated.",
        "Column types, levels, missingness, model assumptions and diagnostics require runtime checks."))
  )
}
