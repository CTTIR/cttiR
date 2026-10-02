#' Check explicitly supplied analysis data
#'
#' Checks mapped columns in a plain data frame without opening data files, changing
#' data, evaluating column names, fitting models or approving a workflow. Only
#' the currently planned independent linear/binomial, right-censored survival,
#' and continuous grouped models have data checks. Complete-case exclusions are
#' reported only when explicitly configured; no imputation or deletion occurs.
#' @param data A plain base R data frame, at most one million rows and 200 columns.
#' @param spec A project specification list or local specification file.
#' @return A list with state, coded issues, aggregate missingness and row counts.
#'   A passed check is not model, diagnostic or scientific approval. Individual
#'   records, identifier values and excluded row numbers are never returned.
#' @export
check_analysis_data <- function(data, spec) {
  spec <- validate_spec(spec)
  if (!identical(class(data), "data.frame") || nrow(data) > 1000000L || ncol(data) > 200L) {
    abort_cttir("Supply a plain data frame within the row and column bounds.", "cttir_input_error")
  }
  if (is.null(names(data)) || anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) {
    abort_cttir("Data columns must have unique nonempty names.", "cttir_input_error")
  }
  plan <- analysis_configuration(spec)
  missing <- setdiff(unlist(plan$missing_fields), "/analysis/approved")
  checks <- new.env(parent = emptyenv())
  checks$issues <- list()
  add <- function(code, column = NULL) {
    checks$issues[[length(checks$issues) + 1L]] <- list(code = code, column = column)
  }
  result <- function(state, rows_used = 0L, missingness = list()) {
    list(state = state, candidate_engine = plan$candidate_engine, issues = checks$issues,
      missing_configuration = as.list(missing), rows_total = nrow(data),
      rows_complete = rows_used, missingness = missingness, executable = FALSE,
      limitations = as.list(c("No data were changed and no model was fitted.",
          "Revision approval, design, model assumptions and diagnostics remain separate requirements.",
          "Units, timestamps, joins, confounding and sampling independence are not established by these checks.")))
  }
  descriptive <- identical(spec$analysis$aim, "descriptive")
  if (length(missing) || (is.null(plan$candidate_engine) && !descriptive) ||
      any(!unlist(plan$capability_gaps) %in% "random_effects_and_residual_structure_review_required")) {
    return(result("configuration_incomplete"))
  }
  mapping <- spec$analysis$mapping
  roles <- unique(c(unlist(mapping[c("outcome", "subject", "time", "event")], use.names = FALSE),
      unlist(mapping[["predictors"]], use.names = FALSE)))
  absent <- setdiff(roles, names(data))
  for (column in absent) add("missing_column", column)
  if (length(absent)) return(result("failed"))
  if (!nrow(data)) {
    add("empty_data")
    return(result("failed"))
  }
  simple <- function(x) {
    is.null(dim(x)) && (identical(class(x), "factor") ||
        (!is.object(x) && typeof(x) %in% c("integer", "double", "character")))
  }
  for (column in roles) if (!simple(data[[column]])) add("unsupported_column_type", column)
  if (length(checks$issues)) return(result("failed"))
  missingness <- lapply(roles, function(column) list(column = column, missing = sum(is.na(data[[column]]))))
  complete <- rep(TRUE, nrow(data))
  for (column in roles) {
    x <- data[[column]]
    complete <- complete & !is.na(x)
    if (is.numeric(x) && any(!is.finite(x) & !is.na(x))) add("nonfinite_value", column)
  }
  if (any(!complete) && identical(mapping[["missing_data"]], "fail")) add("missing_values_forbidden")
  if (!any(complete)) {
    add("no_complete_rows")
    return(result("failed", sum(complete), missingness))
  }
  values <- function(column) data[[column]][complete]
  numeric_role <- function(column, positive = FALSE) {
    x <- data[[column]]
    if (!is.numeric(x) || is.object(x)) {
      add("numeric_column_required", column)
    } else if (positive && any(x <= 0, na.rm = TRUE)) {
      add("positive_time_required", column)
    }
  }
  engine <- if (descriptive) "descriptive" else plan$candidate_engine
  if (engine %in% c("stats::lm", "nlme::lme")) {
    numeric_role(mapping[["outcome"]])
    # A two-valued outcome under a continuous model is almost always a binary endpoint.
    if (length(unique(values(mapping[["outcome"]]))) <= 2L) add("two_valued_outcome_for_continuous_model", mapping[["outcome"]])
  }
  if (engine == "survival::coxph") numeric_role(mapping[["time"]], positive = TRUE)
  if (engine %in% c("stats::glm", "survival::coxph")) {
    column <- if (engine == "stats::glm") mapping[["outcome"]] else mapping[["event"]]
    x <- as.character(values(column))
    codes <- as.character(c(mapping[["event_value"]], mapping[["non_event_value"]]))
    if (any(!x %in% codes)) add("unmapped_event_code", column)
    if (!codes[[1]] %in% x) add("no_events", column)
    if (engine == "stats::glm" && !codes[[2]] %in% x) add("no_non_events", column)
  }
  if (!descriptive) for (column in unlist(mapping[["predictors"]], use.names = FALSE)) {
    if (length(unique(values(column))) < 2L) add("constant_predictor", column)
  }
  if (!is.null(mapping[["subject"]]) && any(!nzchar(trimws(as.character(values(mapping[["subject"]])))))) {
    add("empty_subject_identifier", mapping[["subject"]])
  }
  if (engine == "nlme::lme") {
    ids <- values(mapping[["subject"]])
    if (length(unique(ids)) < 2L) add("insufficient_groups", mapping[["subject"]])
    if (!anyDuplicated(ids)) add("no_repeated_units", mapping[["subject"]])
    if (identical(spec$analysis$unit_structure, "longitudinal")) {
      numeric_role(mapping[["time"]])
      if (anyDuplicated(data[complete, c(mapping[["subject"]], mapping[["time"]]), drop = FALSE])) {
        add("duplicate_subject_time")
      }
    }
  } else if (!descriptive && !is.null(mapping[["subject"]]) && anyDuplicated(values(mapping[["subject"]]))) {
    add("repeated_units_in_independent_plan", mapping[["subject"]])
  }
  result(if (length(checks$issues)) "failed" else "passed", sum(complete), missingness)
}
