analysis_model_configuration <- function(analysis, engine) {
  model <- analysis$model
  required <- c(switch(if (is.null(engine)) "" else engine,
      "stats::lm" = "intercept", "stats::glm" = c("intercept", "binary_link"),
      "survival::coxph" = "survival_ties",
      "nlme::lme" = c("intercept", "random_effects", "residual_structure", "estimation")), "reviewed")
  missing <- required[vapply(required, function(field) is.null(model[[field]]), logical(1))]
  if (!isTRUE(model$reviewed)) missing <- union(missing, "reviewed")
  extra <- setdiff(names(model), required)
  gaps <- character()
  if (is.null(engine)) gaps <- c(gaps, "supported_engine_not_selected")
  if ("intercept" %in% required && identical(model$intercept, FALSE)) gaps <- c(gaps, "no_intercept_adapter_not_supported")
  if (length(extra)) gaps <- c(gaps, "settings_not_applicable_to_selected_engine")
  list(state = if (length(missing) || length(gaps)) "incomplete" else "settings_recorded",
    settings = model, missing_fields = if (length(missing)) as.list(paste0("/analysis/model/", missing)) else list(),
    inapplicable_fields = if (length(extra)) as.list(paste0("/analysis/model/", extra)) else list(),
    capability_gaps = as.list(gaps), executable = FALSE,
    limitation = "Recorded choices do not approve a package revision, model fit or scientific interpretation.")
}
