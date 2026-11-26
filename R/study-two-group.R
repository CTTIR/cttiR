.study_two_group_recipe_id <- "clinical.independent_two_group_mean_difference.v1"
study_is_two_group <- function(spec) is.list(spec) && identical(spec$recipe_id, .study_two_group_recipe_id)

#' Plan an Equal-Allocation Two-Group Continuous-Outcome Study
#'
#' Computes a two-sided two-sample t-test planning approximation, assuming a
#' common planning standard deviation and independent observations. Counts are
#' per group, rounded up before dropout inflation. This is not a calculation
#' for binary, clustered, survival, noninferiority or repeated-measures designs.
#' Analysis may use Welch inference; this planning approximation does not model
#' unequal group variances or covariate-adjustment gains.
#' @param delta Positive meaningful difference, in the outcome's units.
#' @param sd Positive common planning standard deviation, in the same units.
#' @param power Target power strictly between zero and one.
#' @param alpha Two-sided significance level strictly between zero and one.
#' @param dropout Expected unavailable-outcome fraction, from zero to below one.
#' @return Planning inputs, evaluable and randomized counts, and limitations.
#' @export
study_two_group_power <- function(delta, sd, power = 0.8, alpha = 0.05, dropout = 0) {
  positive <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  if (!positive(delta) || !positive(sd)) study_fail("delta and sd must be finite positive numbers.")
  if (!positive(power) || power >= 1 || !positive(alpha) || alpha >= 1)
    study_fail("power and alpha must be strictly between zero and one.")
  if (!is.numeric(dropout) || length(dropout) != 1L || !is.finite(dropout) || dropout < 0 || dropout >= 1)
    study_fail("dropout must be between zero and less than one.")
  fit <- tryCatch(stats::power.t.test(delta = delta, sd = sd, power = power,
    sig.level = alpha, type = "two.sample", alternative = "two.sided", strict = TRUE),
    error = function(e) study_fail(paste("Sample-size calculation unavailable:", conditionMessage(e))))
  evaluable <- ceiling(fit$n); randomized <- ceiling(evaluable / (1 - dropout))
  if (!is.finite(randomized) || randomized > 1e7) study_fail("Sample size exceeds the bounded planning range.")
  list(delta = delta, sd = sd, power = power, alpha = alpha, dropout = dropout,
    continuous_n_per_group = fit$n, evaluable_per_group = evaluable,
    randomized_per_group = randomized, total_randomized = 2 * randomized,
    allocation = "1:1", alternative = "two.sided", method = "stats::power.t.test",
    scientific_approval = FALSE,
    limitations = c("The meaningful difference and SD must be justified independently of the observed study data.",
      "Planning assumes equal allocation, independent observations and a common SD.",
      "Dropout inflation does not resolve missing-data bias; prespecify a missing-data strategy.",
      "This approximation does not include clustering, multiplicity or covariate-adjustment gains."))
}

#' Declare a Synthetic Two-Group Continuous-Outcome Study
#'
#' Creates a closed synthetic parallel-group example. Analysis uses a two-sided
#' Welch comparison, with treatment minus control as the contrast. No generated
#' code is executed, and no real-study or causal approval is inferred.
#' @inheritParams study_spec
#' @param group Column containing the two declared group labels.
#' @param outcome Numeric continuous-outcome column to compare between groups.
#' @param control,treatment Distinct character group labels.
#' @param independent_observations Explicit declaration, exactly `TRUE`.
#' @param planning List with delta, sd, power, alpha and dropout, as in
#'   [study_two_group_power()].
#' @return A validated synthetic specification.
#' @export
study_two_group_spec <- function(study_id, name, question, population, observation_id,
    group, outcome, control, treatment, independent_observations, planning,
    confidence = 0.95, evidence = list()) {
  spec <- list(schema_version = 1L, recipe_id = .study_two_group_recipe_id,
    study_id = study_id, name = name, question = question, population = population,
    research = list(domain = "clinical", data_origin = "primary", design = "experimental"),
    observation_id = observation_id, group = group, outcome = outcome,
    control = control, treatment = treatment, independent_observations = independent_observations,
    planning = planning, confidence = confidence, evidence = evidence, data_kind = "synthetic",
    estimand = "unadjusted_mean_difference", contrast = "treatment_minus_control", missingness = "reject", causal = FALSE)
  study_two_group_validate(spec)
  spec
}

study_two_group_validate <- function(spec, data = NULL, units = NULL) {
  study_keys(spec, c("schema_version", "recipe_id", "study_id", "name", "question", "population",
    "research", "observation_id", "group", "outcome", "control", "treatment", "independent_observations",
    "planning", "confidence", "evidence", "data_kind", "estimand", "contrast", "missingness", "causal"), "Two-group specification")
  if (!identical(spec$schema_version, 1L) && !identical(spec$schema_version, 1)) study_fail("Unsupported schema.")
  if (!study_is_two_group(spec) || !identical(spec$data_kind, "synthetic") || !identical(spec$causal, FALSE) ||
      !identical(spec$independent_observations, TRUE) || !identical(spec$missingness, "reject") ||
      !identical(spec$contrast, "treatment_minus_control") || !identical(spec$estimand, "unadjusted_mean_difference"))
    study_fail("Only synthetic independent two-group mean comparisons are supported.")
  study_keys(spec$research, c("domain", "data_origin", "design"), "Research axes")
  if (!identical(spec$research, list(domain = "clinical", data_origin = "primary", design = "experimental")))
    study_fail("Unsupported two-group research design.")
  study_column(spec$study_id); study_text(spec$name, "study name", 160L)
  for (key in c("question", "population", "control", "treatment")) study_text(spec[[key]], key)
  study_column(spec$observation_id); study_column(spec$group); study_variable(spec$outcome)
  if (identical(spec$control, spec$treatment)) study_fail("Group labels must differ.")
  columns <- c(spec$observation_id, spec$group, spec$outcome$name)
  if (anyDuplicated(columns)) study_fail("ID, group and outcome columns must differ.")
  if (!is.numeric(spec$confidence) || length(spec$confidence) != 1L || !is.finite(spec$confidence) ||
      spec$confidence < 0.8 || spec$confidence > 0.99) study_fail("Confidence must be between 0.8 and 0.99.")
  study_keys(spec$planning, c("delta", "sd", "power", "alpha", "dropout"), "Planning inputs")
  do.call(study_two_group_power, spec$planning)
  if (!is.list(spec$evidence) || nchar(json_text(spec$evidence), type = "bytes") > 24000L) study_fail("Invalid evidence declarations.")
  result <- list(valid = TRUE, recipe_id = spec$recipe_id, specification_sha256 = content_hash(json_text(spec)), scientific_approval = FALSE)
  if (is.null(data)) return(invisible(result))
  if (!is.data.frame(data) || anyDuplicated(names(data)) || anyNA(names(data)) || !setequal(names(data), columns))
    study_fail("Two-group data must contain exactly the declared columns.")
  if (nrow(data) > 200000L) study_fail("Too many observations.")
  if (is.character(units)) units <- as.list(units)
  study_keys(units, spec$outcome$name, "Data unit metadata")
  if (!identical(units[[spec$outcome$name]], spec$outcome$unit)) study_fail("Outcome unit differs from the plan.")
  ids <- data[[spec$observation_id]]; groups <- data[[spec$group]]; values <- data[[spec$outcome$name]]
  if (!is.character(ids) || !is.null(dim(ids)) || anyNA(ids) || any(!nzchar(trimws(ids))) ||
      anyDuplicated(ids) || any(grepl("[[:cntrl:]]", ids))) study_fail("Observation IDs must be unique complete character values.")
  if (!is.character(groups) || !is.null(dim(groups)) || anyNA(groups) ||
      !all(groups %in% c(spec$control, spec$treatment))) study_fail("Every observation needs a declared group.")
  if (!is.numeric(values) || is.object(values) || !is.null(dim(values)) || any(!is.finite(values)))
    study_fail("Outcomes must be finite plain numeric values.")
  if (any(vapply(c(spec$control, spec$treatment), function(g) sum(groups == g), integer(1)) < 4L))
    study_fail("At least four independent observations per group are required.")
  result$n <- nrow(data)
  invisible(result)
}

study_two_group_analyze <- function(spec, data, units) {
  admission <- study_two_group_validate(spec, data, units)
  data <- data[order(data[[spec$observation_id]], method = "radix"), , drop = FALSE]
  y <- data[[spec$outcome$name]]; group <- data[[spec$group]]
  a <- y[group == spec$treatment]; b <- y[group == spec$control]
  fit <- tryCatch(stats::t.test(a, b, paired = FALSE, var.equal = FALSE, alternative = "two.sided",
    conf.level = spec$confidence), error = function(e) study_fail(conditionMessage(e)))
  if (any(!is.finite(c(fit$estimate, fit$stderr, fit$statistic, fit$parameter, fit$p.value, fit$conf.int))) || fit$stderr <= 0)
    study_fail("Nonfinite or degenerate two-group uncertainty.")
  fitted <- ifelse(group == spec$treatment, mean(a), mean(b))
  list(schema_version = 1L, recipe_id = spec$recipe_id, specification_sha256 = admission$specification_sha256,
    coefficients = data.frame(term = "mean_treatment_minus_control", estimate = mean(a) - mean(b),
      std_error = unname(fit$stderr), statistic = unname(fit$statistic), p_value = fit$p.value,
      conf_low = unname(fit$conf.int[1L]), conf_high = unname(fit$conf.int[2L])), n = length(y),
    n_control = length(b), n_treatment = length(a), df_residual = unname(fit$parameter), confidence = spec$confidence,
    planning = do.call(study_two_group_power, spec$planning),
    diagnostics = data.frame(id = data[[spec$observation_id]], group = group, value = y, fitted = fitted, residual = y - fitted),
    scientific_review = "pending", interpretation = "Synthetic treatment-minus-control mean difference with Welch uncertainty; no clinical conclusion or causal approval.",
    limitations = c("Independent observations and a suitable sampling distribution require review.",
      "No exclusions, imputation, multiplicity correction or covariate adjustment were performed.",
      "Synthetic complete outcomes do not validate a real trial's missing-data strategy."))
}

study_two_group_stage <- function(stage, spec, recipe, result, directory, declaration, paragraph, table) {
  if (stage == "protocol") {
    paragraph(spec$question); paragraph(paste("Population:", spec$population))
    paragraph(paste("Contrast:", spec$treatment, "minus", spec$control, "[", spec$outcome$unit, "]; two-sided Welch comparison."))
    plan <- result$planning
    paragraph(paste("Planning: difference", plan$delta, "; SD", plan$sd, "; power", plan$power, "; two-sided alpha", plan$alpha,
      "; evaluable per group", plan$evaluable_per_group, "; randomized per group", plan$randomized_per_group, "; total", plan$total_randomized, "."))
    for (x in plan$limitations) paragraph(x)
  } else if (stage == "import") {
    paragraph(paste("Imported synthetic rows:", result$n, "; raw SHA-256:", recipe$input$sha256)); paragraph(recipe$input$projection)
  } else if (stage == "validation") {
    paragraph(paste("Unique complete IDs; declared groups and units; finite outcomes. Control n:", result$n_control, "; treatment n:", result$n_treatment))
  } else if (stage == "cleaning") {
    paragraph("No rows excluded, no imputation, no outcome-driven method switching. Raw observations remain read-only.")
  } else if (stage == "descriptive") {
    table(utils::read.csv(file.path(directory, "output/analysis/descriptives.csv")))
  } else if (stage %in% c("primary", "report")) {
    table(result$coefficients); paragraph(result$interpretation)
    for (x in result$limitations) paragraph(x)
  } else if (stage == "diagnostics") {
    path <- file.path(directory, "output/analysis/diagnostics.png")
    encoded <- jsonlite::base64_enc(readBin(path, "raw", n = file.info(path)$size))
    cat('<img alt="Synthetic residual and normal quantile diagnostics" style="max-width:100%" src="data:image/png;base64,', encoded, '"/>\n', sep = "")
    paragraph("Review group distributions and outliers; diagnostics do not establish independence or approve a real study.")
  } else if (stage == "reproducibility") {
    paragraph(paste("Recipe:", recipe$id, "; fixed seed:", declaration$structure$profile$seed))
    paragraph(paste("External analysis receipt:", declaration$run$sha256))
    paragraph("Verified recorded outputs are rendered without refitting or publishing.")
  }
  invisible(NULL)
}
