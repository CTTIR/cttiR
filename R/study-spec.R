# Closed study contracts for the initial, synthetic association recipe.
.study_recipe_id <- "clinical.independent_continuous_association.v1"

study_fail <- function(message, code = "invalid_study") {
  abort_cttir(message, "cttir_study_error", code)
}

study_keys <- function(x, keys, label) {
  if (!is.list(x) || is.null(names(x)) || anyNA(names(x)) || anyDuplicated(names(x)) ||
      !setequal(names(x), keys)) study_fail(paste0(label, " must have exactly: ", paste(keys, collapse = ", ")))
  invisible(x)
}

study_text <- function(x, label, limit = 2000L) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)) ||
      nchar(x, type = "bytes") > limit || grepl("[[:cntrl:]]", x))
    study_fail(paste0("Invalid ", label))
  invisible(x)
}

study_column <- function(x) {
  study_text(x, "column name", 80L)
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", x))
    study_fail("Column identifiers must use English letters, digits and underscores.")
  invisible(x)
}

study_variable <- function(x, categorical = FALSE) {
  if (!is.list(x) || is.null(x$type)) study_fail("A variable needs a typed declaration.")
  if (identical(x$type, "continuous")) {
    study_keys(x, c("name", "type", "unit"), "Continuous variable")
    study_text(x$unit, "measurement unit", 80L)
  } else if (categorical && identical(x$type, "categorical")) {
    study_keys(x, c("name", "type", "levels", "reference"), "Categorical variable")
    levels <- unlist(x$levels, use.names = FALSE)
    if (!is.character(levels) || length(levels) < 2L || length(levels) > 12L ||
        anyNA(levels) || anyDuplicated(levels)) study_fail("Declare two to twelve unique factor levels.")
    for (value in levels) study_text(value, "factor level", 80L)
    study_text(x$reference, "factor reference", 80L)
    if (!x$reference %in% levels) study_fail("Factor reference must be a declared level.")
  } else study_fail("This recipe requires continuous outcome/exposure and continuous or categorical covariates.")
  study_column(x$name)
  invisible(x)
}

#' Declare a Synthetic Independent Association Study
#'
#' Creates a closed, noncausal specification for the initial reviewed-code
#' development recipe. This version supports synthetic inputs only; it does not
#' approve a real study or infer scientific decisions from data.
#' @param study_id Stable identifier using letters, digits and underscores.
#' @param name Plain-text study name.
#' @param question Prespecified research question.
#' @param population Explicit synthetic population description.
#' @param observation_id Name of the unique observational-unit identifier.
#' @param outcome,exposure Lists with name, type (continuous) and unit.
#' @param covariates List of continuous or categorical declarations. Categorical
#'   declarations contain name, type, levels and reference.
#' @param research List with domain, data_origin and design.
#' @param confidence Confidence interval coverage, between 0.8 and 0.99.
#' @param evidence List of evidence declarations, retained without asserting support.
#' @return A validated specification. No data are opened or analyzed.
#' @export
study_spec <- function(study_id, name, question, population, observation_id, outcome,
                       exposure, covariates = list(),
                       research = list(domain = "clinical", data_origin = "secondary", design = "observational"),
                       confidence = 0.95, evidence = list()) {
  spec <- list(schema_version = 1L, recipe_id = .study_recipe_id, study_id = study_id,
    name = name, question = question, population = population, research = research,
    observation_id = observation_id, outcome = outcome, exposure = exposure,
    covariates = covariates, estimand = "adjusted_association", dependence = "independent",
    missingness = "reject", causal = FALSE, confidence = confidence,
    data_kind = "synthetic", evidence = evidence)
  study_validate(spec)
  spec
}

#' Validate a Study Contract and Optional Data
#' @param spec A closed specification from [study_spec()].
#' @param data Optional data frame. Every row must have a unique, nonmissing ID.
#' @param units Named character vector or list declaring the actual units of
#'   every continuous analysis column; these must equal the planned units.
#' @return Invisibly a list with valid, recipe_id, specification_sha256 and,
#'   when data are supplied, n. Invalid or unsupported input raises an error.
#' @export
study_validate <- function(spec, data = NULL, units = NULL) {
  keys <- c("schema_version", "recipe_id", "study_id", "name", "question", "population",
    "research", "observation_id", "outcome", "exposure", "covariates", "estimand",
    "dependence", "missingness", "causal", "confidence", "data_kind", "evidence")
  study_keys(spec, keys, "Study specification")
  if (!is.numeric(spec$schema_version) || length(spec$schema_version) != 1L ||
      is.na(spec$schema_version) || spec$schema_version != 1 ||
      !identical(spec$recipe_id, .study_recipe_id)) study_fail("Unsupported study schema or recipe.")
  study_column(spec$study_id)
  study_text(spec$name, "study name", 160L)
  for (field in c("question", "population")) study_text(spec[[field]], field)
  study_keys(spec$research, c("domain", "data_origin", "design"), "Research axes")
  if (!identical(spec$research$domain, "clinical") ||
      !isTRUE(spec$research$data_origin %in% c("primary", "secondary")) ||
      !identical(spec$research$design, "observational")) study_fail("Unsupported research axes for this recipe.")
  if (!identical(spec$estimand, "adjusted_association") ||
      !identical(spec$dependence, "independent") || !identical(spec$missingness, "reject") ||
      !identical(spec$causal, FALSE) || !identical(spec$data_kind, "synthetic"))
    study_fail("This development recipe supports only synthetic independent noncausal associations with no missing analysis values.", "unsupported_design")
  if (!is.numeric(spec$confidence) || length(spec$confidence) != 1L ||
      !is.finite(spec$confidence) || spec$confidence < 0.8 || spec$confidence > 0.99)
    study_fail("Confidence must be between 0.8 and 0.99.")
  if (!is.list(spec$covariates) || length(spec$covariates) > 8L) study_fail("At most eight declared covariates are supported.")
  if (!is.list(spec$evidence) || nchar(json_text(spec$evidence), type = "bytes") > 24000L)
    study_fail("Invalid bounded evidence declarations.")
  study_column(spec$observation_id)
  study_variable(spec$outcome); study_variable(spec$exposure)
  for (x in spec$covariates) study_variable(x, categorical = TRUE)
  variables <- c(list(spec$outcome, spec$exposure), spec$covariates)
  columns <- vapply(variables, `[[`, character(1), "name")
  if (anyDuplicated(c(spec$observation_id, columns))) study_fail("Observation ID and analysis roles must be distinct.")
  result <- list(valid = TRUE, recipe_id = .study_recipe_id,
    specification_sha256 = content_hash(json_text(spec)), scientific_approval = FALSE)
  if (is.null(data)) return(invisible(result))
  if (!is.data.frame(data) || anyDuplicated(names(data)) || anyNA(names(data)) ||
      !all(c(spec$observation_id, columns) %in% names(data))) study_fail("Data must contain unique declared columns.", "invalid_data")
  if (nrow(data) < 8L || nrow(data) > 100000L) study_fail("The bounded recipe accepts 8 to 100000 observations.")
  id <- data[[spec$observation_id]]
  if (!(is.character(id) || is.numeric(id)) || anyNA(id) || anyDuplicated(id) ||
      (is.numeric(id) && any(!is.finite(id))) || any(!nzchar(trimws(as.character(id)))))
    study_fail("Observation IDs must be complete and unique.", "invalid_observation_id")
  continuous <- Filter(function(x) identical(x$type, "continuous"), variables)
  expected_units <- stats::setNames(lapply(continuous, `[[`, "unit"), vapply(continuous, `[[`, "", "name"))
  if (is.character(units)) units <- as.list(units)
  study_keys(units, names(expected_units), "Data unit metadata")
  for (column in names(expected_units)) if (!identical(units[[column]], expected_units[[column]]))
    study_fail(paste0("Data unit differs from the plan: ", column), "unit_mismatch")
  for (x in variables) {
    value <- data[[x$name]]
    if (anyNA(value)) study_fail(paste0("Missing analysis values: ", x$name), "missing_data")
    if (identical(x$type, "continuous")) {
      if (!is.numeric(value) || any(!is.finite(value)) || length(unique(value)) < 2L)
        study_fail(paste0("Continuous column must contain finite, nonconstant numeric values: ", x$name))
    } else {
      if (!(is.character(value) || is.factor(value))) study_fail("Categorical columns must be strings or factors.")
      levels <- unlist(x$levels, use.names = FALSE)
      if (!setequal(unique(as.character(value)), levels))
        study_fail(paste0("Observed factor levels must equal the declared levels: ", x$name), "factor_levels")
    }
  }
  result$n <- nrow(data)
  invisible(result)
}

#' Execute the Synthetic Association Recipe
#' @inheritParams study_validate
#' @return A list with coefficient table, sample size, rank, residual degrees of
#'   freedom, residual standard deviation, diagnostics and explicit limitations.
#'   Passing numerical computation does not grant scientific approval.
#' @export
study_analyze <- function(spec, data, units) {
  admission <- study_validate(spec, data, units)
  frame <- data.frame(.outcome = data[[spec$outcome$name]], .exposure = data[[spec$exposure$name]])
  predictors <- ".exposure"; contrasts <- list()
  variables <- c(list(spec$exposure), spec$covariates)
  for (i in seq_along(spec$covariates)) {
    variable <- spec$covariates[[i]]; alias <- paste0(".cov", i)
    value <- data[[variable$name]]
    if (identical(variable$type, "categorical")) {
      levels <- unlist(variable$levels, use.names = FALSE)
      levels <- c(variable$reference, setdiff(levels, variable$reference))
      value <- factor(as.character(value), levels = levels)
      contrasts[[alias]] <- stats::contr.treatment(levels, base = 1L)
    }
    frame[[alias]] <- value; predictors <- c(predictors, alias)
  }
  formula <- stats::reformulate(predictors, response = ".outcome")
  fit <- stats::lm(formula, data = frame, contrasts = if (length(contrasts)) contrasts else NULL,
                   na.action = stats::na.fail, x = TRUE, y = TRUE, model = TRUE)
  if (fit$rank != ncol(fit$x) || fit$df.residual < 5L)
    study_fail("A full-rank design with at least five residual degrees of freedom is required.", "rank_or_information")
  summary <- summary(fit); table <- summary$coefficients
  if (any(!is.finite(table)) || !is.finite(summary$sigma) || summary$sigma <= 0)
    study_fail("The fit has degenerate or nonfinite uncertainty.", "degenerate_fit")
  assignments <- attr(fit$x, "assign")
  labels <- rownames(table)
  for (i in seq_along(predictors)) {
    selected <- which(assignments == i)
    variable <- variables[[i]]
    labels[selected] <- if (identical(variable$type, "continuous")) variable$name else
      paste0(variable$name, ":", sub(predictors[[i]], "", colnames(fit$x)[selected], fixed = TRUE))
  }
  interval <- stats::confint(fit, level = spec$confidence)
  coefficients <- data.frame(term = labels, estimate = table[, 1L], std_error = table[, 2L],
    statistic = table[, 3L], p_value = table[, 4L], conf_low = interval[, 1L], conf_high = interval[, 2L],
    row.names = NULL, stringsAsFactors = FALSE)
  residuals <- stats::residuals(fit); fitted <- stats::fitted(fit)
  leverage <- stats::hatvalues(fit); cooks <- stats::cooks.distance(fit)
  diagnostics <- data.frame(observation_id = as.character(data[[spec$observation_id]]),
    fitted = as.numeric(fitted), residual = as.numeric(residuals), leverage = as.numeric(leverage),
    cooks_distance = as.numeric(cooks), high_leverage = leverage > 2 * fit$rank / nrow(data),
    high_influence = cooks > 4 / nrow(data), row.names = NULL)
  list(schema_version = 1L, recipe_id = .study_recipe_id, specification_sha256 = admission$specification_sha256,
    coefficients = coefficients, n = nrow(data), rank = fit$rank, df_residual = fit$df.residual,
    sigma = unname(summary$sigma), residual_sum_squares = unname(sum(residuals^2)),
    confidence = spec$confidence, diagnostics = diagnostics, scientific_review = "pending",
    interpretation = "Conditional mean associations in synthetic observations; no causal effect or clinical approval is established.",
    limitations = c("Independence and design assumptions require study review.",
      "Classical model-based intervals require appropriate error assumptions.",
      "Influence flags are review prompts, not automated exclusions.",
      "No rows were excluded and no missing values were imputed.",
      "Covariate inference is descriptive; no multiplicity-controlled claim is made."))
}
