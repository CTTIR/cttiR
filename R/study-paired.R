.study_paired_recipe_id <- "clinical.paired_continuous_mean_change.v1"

study_is_paired <- function(spec) {
  is.list(spec) && identical(spec$recipe_id, .study_paired_recipe_id)
}

#' Declare a Synthetic Paired Mean-Change Study
#'
#' Declares two measurements per independent pair, matched by an explicit ID.
#' The contrast is after minus before, with a two-sided test against zero.
#' Only synthetic, noncausal clinical examples are supported. Declaring pair
#' independence does not establish it scientifically.
#' @inheritParams study_spec
#' @param pair_id Column containing complete character pair identifiers.
#' @param occasion Character column identifying the measurement occasion.
#' @param outcome A continuous variable declaration with `name`,
#'   `type = "continuous"` and `unit`.
#' @param before,after Distinct character labels in the occasion column.
#' @param independent_pairs Required explicit declaration, exactly `TRUE`.
#'   Clustered or otherwise dependent pairs require a different recipe.
#' @return A closed synthetic paired specification. No data are read or analyzed.
#' @export
study_paired_spec <- function(study_id, name, question, population, pair_id,
                              occasion, outcome, before, after, independent_pairs,
                              research = list(domain = "clinical", data_origin = "secondary", design = "observational"),
                              confidence = 0.95, evidence = list()) {
  if (missing(independent_pairs) || !identical(independent_pairs, TRUE))
    study_fail("Explicitly declare independent_pairs = TRUE; unknown or dependent pairs are unsupported.")
  spec <- list(schema_version = 1L, recipe_id = .study_paired_recipe_id,
    study_id = study_id, name = name, question = question, population = population,
    research = research, pair_id = pair_id, occasion = occasion, outcome = outcome,
    before = before, after = after, independent_pairs = independent_pairs,
    estimand = "mean_within_pair_change", contrast = "after_minus_before",
    alternative = "two.sided", null_mean_change = 0, missingness = "reject",
    causal = FALSE, confidence = confidence, data_kind = "synthetic", evidence = evidence)
  study_paired_validate(spec)
  spec
}

study_paired_validate <- function(spec, data = NULL, units = NULL) {
  study_keys(spec, c("schema_version", "recipe_id", "study_id", "name", "question",
    "population", "research", "pair_id", "occasion", "outcome", "before", "after",
    "independent_pairs", "estimand", "contrast", "alternative", "null_mean_change",
    "missingness", "causal", "confidence", "data_kind", "evidence"), "Paired specification")
  if (!is.numeric(spec$schema_version) || length(spec$schema_version) != 1L ||
      !isTRUE(spec$schema_version == 1) || !study_is_paired(spec))
    study_fail("Unsupported paired recipe or schema.")
  study_column(spec$study_id); study_text(spec$name, "study name", 160L)
  for (key in c("question", "population")) study_text(spec[[key]], key)
  study_keys(spec$research, c("domain", "data_origin", "design"), "Research axes")
  if (!identical(spec$research$domain, "clinical") ||
      !isTRUE(spec$research$data_origin %in% c("primary", "secondary")) ||
      !identical(spec$research$design, "observational")) study_fail("Unsupported research axes for the paired recipe.")
  if (!identical(spec$independent_pairs, TRUE) ||
      !identical(spec$estimand, "mean_within_pair_change") ||
      !identical(spec$contrast, "after_minus_before") ||
      !identical(spec$alternative, "two.sided") ||
      !is.numeric(spec$null_mean_change) || length(spec$null_mean_change) != 1L ||
      !isTRUE(spec$null_mean_change == 0) || !identical(spec$missingness, "reject") ||
      !identical(spec$causal, FALSE) || !identical(spec$data_kind, "synthetic"))
    study_fail("Only synthetic noncausal mean changes in explicitly independent pairs are supported.", "unsupported_design")
  if (!is.numeric(spec$confidence) || length(spec$confidence) != 1L ||
      !is.finite(spec$confidence) || spec$confidence < 0.8 || spec$confidence > 0.99)
    study_fail("Confidence must be between 0.8 and 0.99.")
  if (!is.list(spec$evidence) || nchar(json_text(spec$evidence), type = "bytes") > 24000L)
    study_fail("Invalid bounded evidence declarations.")
  study_column(spec$pair_id); study_column(spec$occasion); study_variable(spec$outcome)
  columns <- c(spec$pair_id, spec$occasion, spec$outcome$name)
  if (anyDuplicated(columns)) study_fail("Pair ID, occasion and outcome must be distinct.")
  study_text(spec$before, "before label", 160L); study_text(spec$after, "after label", 160L)
  if (identical(spec$before, spec$after)) study_fail("The before and after labels must differ.")
  result <- list(valid = TRUE, recipe_id = .study_paired_recipe_id,
    specification_sha256 = content_hash(json_text(spec)), scientific_approval = FALSE)
  if (is.null(data)) return(invisible(result))
  if (!is.data.frame(data) || anyDuplicated(names(data)) || anyNA(names(data)) ||
      !setequal(names(data), columns)) study_fail("Paired data must contain exactly the three declared columns.")
  if (nrow(data) < 16L || nrow(data) > 200000L)
    study_fail("The bounded paired recipe accepts 8 to 100000 complete pairs.")
  if (is.character(units)) units <- as.list(units)
  study_keys(units, spec$outcome$name, "Data unit metadata")
  if (!identical(units[[spec$outcome$name]], spec$outcome$unit)) study_fail("Paired outcome unit differs from the plan.", "unit_mismatch")
  ids <- data[[spec$pair_id]]; occasion <- data[[spec$occasion]]; value <- data[[spec$outcome$name]]
  if (!is.character(ids) || !is.null(dim(ids)) || anyNA(ids) ||
      any(!nzchar(trimws(ids))) || any(grepl("[[:cntrl:]]", ids)))
    study_fail("Pair identifiers must be complete character values.")
  if (!is.character(occasion) || !is.null(dim(occasion)) || anyNA(occasion) ||
      !all(occasion %in% c(spec$before, spec$after))) study_fail("Every row requires one declared occasion label.")
  if (!is.numeric(value) || is.object(value) || !is.null(dim(value)) || any(!is.finite(value)))
    study_fail("Paired measurements must be finite plain numeric values without missingness.")
  before <- which(occasion == spec$before); after <- which(occasion == spec$after)
  if (anyDuplicated(ids[before]) || anyDuplicated(ids[after]) || !setequal(ids[before], ids[after]))
    study_fail("Each pair ID requires exactly one measurement at each occasion.")
  result$n <- nrow(data); result$n_pairs <- length(before)
  invisible(result)
}

study_paired_values <- function(spec, data) {
  ids <- data[[spec$pair_id]]; occasion <- data[[spec$occasion]]; value <- data[[spec$outcome$name]]
  before <- which(occasion == spec$before); after <- which(occasion == spec$after)
  ordered <- sort(ids[before], method = "radix")
  baseline <- value[before[match(ordered, ids[before])]]
  followup <- value[after[match(ordered, ids[after])]]
  difference <- followup - baseline
  if (any(!is.finite(difference))) study_fail("Paired differences overflow.")
  data.frame(pair_id = ordered, before = baseline, after = followup,
    difference = difference, centered_difference = difference - mean(difference),
    stringsAsFactors = FALSE)
}

study_paired_analyze <- function(spec, data, units) {
  admission <- study_paired_validate(spec, data, units)
  values <- study_paired_values(spec, data)
  fit <- tryCatch(stats::t.test(values$difference, mu = 0, alternative = "two.sided",
    conf.level = spec$confidence), error = function(e) study_fail(paste("Paired uncertainty unavailable:", conditionMessage(e))))
  numbers <- c(fit$estimate, fit$stderr, fit$statistic, fit$parameter, fit$p.value, fit$conf.int)
  if (any(!is.finite(numbers)) || fit$stderr <= 0) study_fail("Degenerate or nonfinite paired uncertainty.")
  coefficients <- data.frame(term = "mean_after_minus_before", estimate = unname(fit$estimate),
    std_error = unname(fit$stderr), statistic = unname(fit$statistic), p_value = fit$p.value,
    conf_low = unname(fit$conf.int[1L]), conf_high = unname(fit$conf.int[2L]))
  list(schema_version = 1L, recipe_id = .study_paired_recipe_id,
    specification_sha256 = admission$specification_sha256, coefficients = coefficients,
    n = nrow(data), n_pairs = nrow(values), df_residual = unname(fit$parameter),
    confidence = spec$confidence, contrast = spec$contrast, diagnostics = values,
    scientific_review = "pending",
    interpretation = "Mean within-pair change in synthetic observations; no causal effect or clinical approval is established.",
    limitations = c("Pair independence is a caller declaration requiring scientific review.",
      "The distribution of within-pair differences must support t inference.",
      "A before-after difference does not identify a causal treatment effect.",
      "No rows were excluded and no missing values were imputed.",
      "No covariate adjustment, multiplicity procedure or automatic method switching is performed."))
}

study_data_layout <- function(spec) {
  if (study_is_paired(spec)) return(list(variables = list(spec$outcome),
    columns = c(spec$pair_id, spec$occasion, spec$outcome$name),
    classes = c("character", "character", "numeric"), id = spec$pair_id))
  variables <- c(list(spec$outcome, spec$exposure), spec$covariates)
  list(variables = variables,
    columns = c(spec$observation_id, vapply(variables, `[[`, "", "name")),
    classes = c("character", vapply(variables, function(x) if (x$type == "continuous") "numeric" else "character", "")),
    id = spec$observation_id)
}

study_paired_stage <- function(stage, spec, recipe, result, directory, declaration, paragraph, table) {
  if (stage == "protocol") {
    paragraph(spec$question); paragraph(paste("Population:", spec$population))
    paragraph(paste("Estimand: mean within-pair change;", spec$after, "minus", spec$before,
      "; independent pairs declared by the caller; no causal interpretation."))
    paragraph(paste("Outcome:", spec$outcome$name, "[", spec$outcome$unit, "]; pair ID:", spec$pair_id,
      "; occasion column:", spec$occasion))
    paragraph("Evidence declarations are retained as supplied; this compiler does not establish citation support.")
  } else if (stage == "import") {
    paragraph(paste("Imported rows:", result$n, "; complete pairs:", result$n_pairs, "; raw SHA-256:", recipe$input$sha256))
    paragraph(recipe$input$projection)
  } else if (stage == "validation") {
    paragraph(paste("Passed:", result$n_pairs, "complete pair IDs with exactly one measurement at each declared occasion; matching units; finite nonmissing values; residual degrees of freedom", result$df_residual))
    paragraph("Matching IDs establish correspondence only. Pair independence and distributional suitability require scientific review.")
  } else if (stage == "cleaning") {
    paragraph(paste("Measurements were matched by pair ID, not row position. Differences are", spec$after, "minus", spec$before, "."))
    paragraph("No exclusions, imputation, winsorization or significance-based method switching. Raw inputs remain read-only.")
  } else if (stage == "descriptive") {
    paragraph(paste("before =", spec$before, "; after =", spec$after, "; difference = after minus before."))
    table(utils::read.csv(file.path(directory, "output/analysis/descriptives.csv")))
  } else if (stage == "primary") {
    paragraph(paste("Contrast:", spec$after, "minus", spec$before, "[", spec$outcome$unit, "]; two-sided null mean change: 0."))
    table(result$coefficients)
    paragraph(paste("Complete pairs:", result$n_pairs, "; confidence coverage:", result$confidence,
      "; degrees of freedom:", result$df_residual))
    paragraph(result$interpretation)
  } else if (stage == "diagnostics") {
    path <- file.path(directory, "output/analysis/diagnostics.png")
    encoded <- jsonlite::base64_enc(readBin(path, "raw", n = file.info(path)$size))
    cat('<img alt="Recorded within-pair differences and their normal quantile plot" style="max-width:100%" src="data:image/png;base64,', encoded, '"/>\n', sep = "")
    paragraph("The pair index sorts identifiers and does not represent time. Plots prompt review; they do not establish independence, pass assumptions automatically or remove observations.")
  } else if (stage == "report") {
    paragraph(paste("Contrast:", spec$after, "minus", spec$before, "[", spec$outcome$unit, "]."))
    table(result$coefficients); paragraph(result$interpretation)
    for (limitation in result$limitations) paragraph(limitation)
  } else if (stage == "reproducibility") {
    paragraph(paste("Recipe:", recipe$id, "; fixed seed:", declaration$structure$profile$seed))
    paragraph(paste("External analysis receipt SHA-256:", declaration$run$sha256))
    paragraph("Current raw inputs, fixed compiler controls, complete installed writer inventory, R/package versions and recorded outputs were checked before this view. Rendering consumes recorded results and performs no statistical refit.")
  }
  invisible(NULL)
}
