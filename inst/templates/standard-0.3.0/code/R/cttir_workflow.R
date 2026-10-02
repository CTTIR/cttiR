# cttiR standard workflow stage library (template bundle standard-0.3.0).
#
# Reviewed static code copied into generated projects. Project text, dataset
# paths and column names are read from configuration files as data; they are
# never evaluated or pasted into formulas. Every non-base call is namespaced.
# Real-data runs happen only through code/run_workflow.R after explicit mapping,
# reviewed model settings, recorded analysis approval and a local data binding.

cw_engines <- c("stats::lm", "stats::glm", "nlme::lme", "survival::coxph")

cw_read_yaml <- function(path) {
  if (!file.exists(path)) stop("Missing configuration file: ", path, call. = FALSE)
  yaml::read_yaml(path, eval.expr = FALSE)
}

cw_config <- function(root = ".") {
  spec <- cw_read_yaml(file.path(root, "cttir-project.yml"))
  local_file <- file.path(root, ".cttir", "local.yml")
  local <- if (file.exists(local_file)) cw_read_yaml(local_file) else list(bindings = list())
  list(
    root = root, analysis = spec$analysis, figures = spec$figures,
    workflow = cw_read_yaml(file.path(root, "config", "workflow.yml")),
    datasets = cw_read_yaml(file.path(root, "metadata", "data-registry.yml"))$datasets,
    bindings = local$bindings
  )
}

# Mirrors the engine routing of the cttiR planner: unsupported designs return NULL.
cw_engine <- function(analysis) {
  if (!identical(analysis$aim, "explanatory")) return(NULL)
  family <- analysis$outcome_family
  unit <- analysis$unit_structure
  if (identical(unit, "independent")) {
    if (identical(family, "continuous")) return("stats::lm")
    if (identical(family, "binary")) return("stats::glm")
    if (identical(family, "time_to_event")) return("survival::coxph")
  }
  if (isTRUE(unit %in% c("clustered", "longitudinal")) && identical(family, "continuous")) return("nlme::lme")
  NULL
}

cw_required_mapping <- function(analysis, engine) {
  fields <- c("data_source_id", "estimand", "missing_data")
  if (identical(analysis$aim, "descriptive")) return(c("data_source_id", "missing_data"))
  if (is.null(engine)) return(fields)
  fields <- c(fields, "predictors")
  if (engine == "survival::coxph") {
    fields <- c(fields, "time", "event", "event_value", "non_event_value", "time_origin", "time_unit")
  } else {
    fields <- c(fields, "outcome")
  }
  if (engine == "stats::glm") fields <- c(fields, "event_value", "non_event_value")
  if (engine == "nlme::lme") {
    fields <- c(fields, "subject")
    if (identical(analysis$unit_structure, "longitudinal")) fields <- c(fields, "time", "time_origin", "time_unit")
  }
  unique(fields)
}

cw_required_settings <- function(engine) {
  if (is.null(engine)) return(character())
  c(switch(engine,
    "stats::lm" = "intercept", "stats::glm" = c("intercept", "binary_link"),
    "survival::coxph" = "survival_ties",
    "nlme::lme" = c("intercept", "random_effects", "residual_structure", "estimation")), "reviewed")
}

# Everything that must be true before real data may be read. Returns missing items.
cw_requirements <- function(config) {
  analysis <- config$analysis
  mapping <- analysis$mapping
  missing <- character()
  for (field in c("aim", "outcome_family", "unit_structure")) {
    if (is.null(analysis[[field]]) || identical(analysis[[field]], "unknown")) {
      if (!(identical(analysis$aim, "descriptive") && field != "aim")) missing <- c(missing, paste0("analysis.", field))
    }
  }
  engine <- cw_engine(analysis)
  if (!identical(analysis$aim, "descriptive") && is.null(engine)) missing <- c(missing, "supported_engine")
  for (field in cw_required_mapping(analysis, engine)) {
    value <- mapping[[field]]
    if (is.null(value) || (field == "missing_data" && identical(value, "unknown"))) {
      missing <- c(missing, paste0("analysis.mapping.", field))
    }
  }
  settings <- analysis$model
  for (field in cw_required_settings(engine)) {
    if (is.null(settings[[field]]) || (field == "reviewed" && !isTRUE(settings$reviewed))) {
      missing <- c(missing, paste0("analysis.model.", field))
    }
  }
  if (!isTRUE(analysis$approved)) missing <- c(missing, "analysis.approved")
  dataset <- cw_dataset(config)
  if (is.null(dataset$record)) {
    missing <- c(missing, "dataset.registry_entry")
  } else if (!isTRUE(dataset$format %in% c("csv", "tsv"))) {
    missing <- c(missing, "dataset.supported_format(csv|tsv)")
  }
  if (is.null(dataset$path) || !file.exists(dataset$path)) missing <- c(missing, "dataset.local_binding")
  for (stage in config$workflow$stages) {
    if (isTRUE(stage$enabled) && !identical(stage$status, "approved")) {
      missing <- c(missing, paste0("approval.", stage$stage, ":", stage$capability))
    }
  }
  versions <- cw_version_mismatches(config$workflow$dependencies)
  if (length(versions)) missing <- c(missing, paste0("installed_version.", versions))
  list(ready = !length(missing), missing = unique(missing), engine = engine)
}

cw_version_mismatches <- function(dependencies) {
  out <- character()
  for (dep in dependencies) {
    if (!isTRUE(dep$required) || is.null(dep$version) || dep$package %in% c("base", "stats", "utils")) next
    # package_version() treats "1.1-3" and "1.1.3" as the same version.
    installed <- tryCatch(utils::packageVersion(dep$package), error = function(e) NULL)
    pinned <- tryCatch(package_version(as.character(dep$version)), error = function(e) NULL)
    if (is.null(installed) || is.null(pinned) || installed != pinned) {
      out <- c(out, paste0(dep$package, "@", dep$version))
    }
  }
  out
}

cw_dataset <- function(config) {
  id <- config$analysis$mapping[["data_source_id"]]
  record <- NULL
  for (entry in config$datasets) if (identical(entry$id, id)) record <- entry
  path <- NULL
  for (binding in config$bindings) if (identical(binding$id, id)) path <- binding$path
  format <- if (is.null(record$format)) NULL else tolower(record$format)
  list(id = id, record = record, format = format, path = path)
}

# Explicit delimited import. Column types come from the data dictionary when it
# lists the dataset; otherwise readr guesses and the guess is reported.
cw_import <- function(config) {
  dataset <- cw_dataset(config)
  if (is.null(dataset$path) || !file.exists(dataset$path)) stop("The dataset has no local binding.", call. = FALSE)
  dictionary_file <- file.path(config$root, "metadata", "data-dictionary.csv")
  dictionary <- if (file.exists(dictionary_file)) {
    utils::read.csv(dictionary_file, colClasses = "character", na.strings = character(), check.names = FALSE)
  } else {
    data.frame()
  }
  rows <- if (nrow(dictionary)) dictionary[dictionary$dataset_id == dataset$id, , drop = FALSE] else dictionary
  readr_type <- function(type) {
    switch(tolower(type), numeric = readr::col_double(), double = readr::col_double(),
      integer = readr::col_integer(), logical = readr::col_logical(), date = readr::col_date(),
      readr::col_character())
  }
  types <- if (nrow(rows)) {
    spec <- lapply(rows$type, readr_type)
    names(spec) <- rows$variable
    do.call(readr::cols, c(spec, list(.default = readr::col_character())))
  } else {
    readr::cols(.default = readr::col_guess())
  }
  missing_codes <- if (nrow(rows) && "missing_codes" %in% names(rows)) {
    unique(c("", "NA", unlist(strsplit(rows$missing_codes[nzchar(rows$missing_codes)], ";", fixed = TRUE))))
  } else {
    c("", "NA")
  }
  parse_warnings <- character()
  data <- withCallingHandlers(
    readr::read_delim(dataset$path, delim = if (identical(dataset$format, "tsv")) "\t" else ",",
      col_types = types, na = missing_codes, locale = readr::locale(encoding = "UTF-8"),
      progress = FALSE, show_col_types = FALSE),
    warning = function(w) {
      parse_warnings <<- c(parse_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  attr(data, "cw_import") <- list(dataset_id = dataset$id, rows = nrow(data), columns = ncol(data),
    types_from_dictionary = nrow(rows) > 0L, parsing_warnings = parse_warnings)
  data
}

cw_roles <- function(mapping) {
  roles <- c(outcome = mapping[["outcome"]], time = mapping[["time"]], event = mapping[["event"]], subject = mapping[["subject"]])
  list(roles = roles[!vapply(roles, is.null, logical(1))], predictors = unlist(mapping[["predictors"]], use.names = FALSE))
}

# Structural checks mirroring cttiR::check_analysis_data(); no data are changed.
cw_check <- function(data, analysis) {
  mapping <- analysis$mapping
  engine <- cw_engine(analysis)
  issues <- list()
  add <- function(code, column = NULL) issues[[length(issues) + 1L]] <<- list(code = code, column = column)
  r <- cw_roles(mapping)
  columns <- unique(c(unname(unlist(r$roles)), r$predictors))
  for (column in setdiff(columns, names(data))) add("missing_column", column)
  if (length(issues)) return(list(state = "failed", issues = issues, rows_complete = 0L))
  if (!nrow(data)) {
    add("empty_data")
    return(list(state = "failed", issues = issues, rows_complete = 0L))
  }
  simple <- function(x) is.null(dim(x)) && (is.factor(x) || (!is.object(x) && typeof(x) %in% c("integer", "double", "character", "logical")))
  for (column in columns) if (!simple(data[[column]])) add("unsupported_column_type", column)
  if (length(issues)) return(list(state = "failed", issues = issues, rows_complete = 0L))
  complete <- rep(TRUE, nrow(data))
  for (column in columns) {
    x <- data[[column]]
    complete <- complete & !is.na(x)
    if (is.numeric(x) && any(!is.finite(x) & !is.na(x))) add("nonfinite_value", column)
  }
  if (any(!complete) && identical(mapping[["missing_data"]], "fail")) add("missing_values_forbidden")
  if (!any(complete)) {
    add("no_complete_rows")
    return(list(state = "failed", issues = issues, rows_complete = 0L))
  }
  values <- function(column) data[[column]][complete]
  numeric_role <- function(column, positive = FALSE) {
    x <- data[[column]]
    if (!is.numeric(x) || is.object(x)) add("numeric_column_required", column)
    else if (positive && any(x <= 0, na.rm = TRUE)) add("positive_time_required", column)
  }
  if (!is.null(engine)) {
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
    for (column in r$predictors) if (length(unique(values(column))) < 2L) add("constant_predictor", column)
  }
  if (!is.null(mapping[["subject"]]) && any(!nzchar(trimws(as.character(values(mapping[["subject"]])))))) {
    add("empty_subject_identifier", mapping[["subject"]])
  }
  if (identical(engine, "nlme::lme")) {
    ids <- values(mapping[["subject"]])
    if (length(unique(ids)) < 2L) add("insufficient_groups", mapping[["subject"]])
    if (!anyDuplicated(ids)) add("no_repeated_units", mapping[["subject"]])
    if (identical(analysis$unit_structure, "longitudinal")) {
      numeric_role(mapping[["time"]])
      if (anyDuplicated(data[complete, c(mapping[["subject"]], mapping[["time"]]), drop = FALSE])) add("duplicate_subject_time")
    }
  } else if (!is.null(engine) && !is.null(mapping[["subject"]]) && anyDuplicated(values(mapping[["subject"]]))) {
    add("repeated_units_in_independent_plan", mapping[["subject"]])
  }
  list(state = if (length(issues)) "failed" else "passed", issues = issues, rows_complete = sum(complete))
}

# Select mapped roles under fixed aliases (response, time, event, subject, x1..xk).
# Original names are kept in the alias map; the user's names never reach a formula.
cw_tidy <- function(data, analysis) {
  mapping <- analysis$mapping
  engine <- cw_engine(analysis)
  r <- cw_roles(mapping)
  source <- c(r$roles, stats::setNames(r$predictors, paste0("x", seq_along(r$predictors))))
  alias <- c(outcome = "response", time = "time", event = "event", subject = "subject")
  names(source)[names(source) %in% names(alias)] <- alias[names(source)[names(source) %in% names(alias)]]
  out <- data[unname(source)]
  names(out) <- names(source)
  complete <- stats::complete.cases(out)
  if (any(!complete) && !identical(mapping[["missing_data"]], "complete_case")) {
    stop("Mapped columns contain missing values and missing_data is not complete_case.", call. = FALSE)
  }
  out <- dplyr::filter(out, complete)
  binary_code <- as.character(mapping[["event_value"]])
  if (identical(engine, "stats::glm")) out <- dplyr::mutate(out, response = as.integer(as.character(response) == binary_code))
  if ("event" %in% names(out)) out <- dplyr::mutate(out, event = as.integer(as.character(event) == binary_code))
  binary <- if (length(binary_code) == 1L) intersect(c(if (identical(engine, "stats::glm")) "response", "event"), names(out)) else character()
  non_event <- mapping[["non_event_value"]]
  binary_labels <- c(if (is.null(non_event)) paste("not", binary_code) else as.character(non_event), binary_code)
  if ("subject" %in% names(out)) out <- dplyr::mutate(out, subject = factor(as.character(subject)))
  reference <- list()
  for (name in grep("^x[0-9]+$", names(out), value = TRUE)) {
    x <- out[[name]]
    if (is.character(x) || is.logical(x)) x <- factor(x, levels = sort(unique(x), method = "radix"))
    if (is.factor(x)) {
      x <- droplevels(x)
      reference[[name]] <- levels(x)[[1]]
    }
    out[[name]] <- x
  }
  if (all(c("subject", "time") %in% names(out))) out <- dplyr::arrange(out, subject, time)
  list(data = as.data.frame(out), alias = source, reference_levels = reference,
    binary = stats::setNames(rep(list(binary_labels), length(binary)), binary),
    rows_total = nrow(data), rows_used = nrow(out), rows_excluded = nrow(data) - nrow(out))
}

cw_label <- function(tidy, alias) {
  original <- tidy$alias[[alias]]
  if (is.null(original)) alias else original
}

cw_no_test <- function() {
  list(name = "Descriptive only - no inference", abbreviation = "none", p = function(...) NA_real_)
}

# Descriptive table without inferential tests; DescrTab2 when selected and
# installed, otherwise the documented base R descriptive-only fallback. Binary
# endpoints are shown as counts under their original codes; with repeated rows
# per subject only the first observation (earliest time when mapped) is
# described, so subjects are not counted once per visit.
cw_describe <- function(tidy, backend = "DescrTab2") {
  data <- tidy$data
  for (name in names(tidy$binary)) data[[name]] <- factor(data[[name]], levels = c(0L, 1L), labels = make.unique(tidy$binary[[name]]))
  repeated <- "subject" %in% names(data) && anyDuplicated(data$subject) > 0L
  if (repeated) data <- data[!duplicated(data$subject), , drop = FALSE]
  columns <- setdiff(names(data), "subject")
  labels <- stats::setNames(lapply(columns, function(x) cw_label(tidy, x)), columns)
  # A column mapped to two roles (e.g. time and predictor) is described once.
  columns <- columns[!duplicated(unlist(labels))]
  labels <- labels[columns]
  denominators <- list(rows = nrow(tidy$data),
    subjects = if ("subject" %in% names(data)) length(unique(data$subject)) else NA_integer_,
    rows_described = nrow(data),
    unit = if (repeated) "first observation per subject" else "row",
    rows_excluded = tidy$rows_excluded)
  if (identical(backend, "DescrTab2") && requireNamespace("DescrTab2", quietly = TRUE)) {
    described <- suppressWarnings(DescrTab2::descr(data[columns], var_labels = labels,
      test_options = list(test_override = cw_no_test()),
      format_options = list(print_p = FALSE, print_CI = FALSE)))
    printed <- print(described, print_format = "console", silent = TRUE)
    table <- as.data.frame(printed$tibble, stringsAsFactors = FALSE)
    return(list(table = table, backend = "DescrTab2", denominators = denominators,
      inference = "none: explicit no-test callback; p-values and confidence intervals suppressed"))
  }
  rows <- list()
  for (column in columns) {
    x <- data[[column]]
    label <- labels[[column]]
    if (is.numeric(x)) {
      q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(variable = label, level = "", n = sum(!is.na(x)),
        missing = sum(is.na(x)), summary = sprintf("mean %.3g (SD %.3g); median %.3g [%.3g, %.3g]",
          mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE), q[[2]], q[[1]], q[[3]]),
        stringsAsFactors = FALSE)
    } else {
      counts <- table(x, useNA = "ifany")
      for (level in names(counts)) {
        rows[[length(rows) + 1L]] <- data.frame(variable = label, level = if (is.na(level)) "(missing)" else level,
          n = as.integer(counts[[level]]), missing = sum(is.na(x)),
          summary = sprintf("%d (%.1f%%)", as.integer(counts[[level]]), 100 * counts[[level]] / length(x)),
          stringsAsFactors = FALSE)
      }
    }
  }
  list(table = do.call(rbind, rows), backend = "base_descriptive_only", denominators = denominators,
    inference = "none")
}

cw_formula <- function(tidy) {
  predictors <- grep("^x[0-9]+$", names(tidy$data), value = TRUE)
  rhs <- if (length(predictors)) Reduce(function(a, b) call("+", a, b), lapply(predictors, as.name)) else 1
  lhs <- if (all(c("time", "event") %in% names(tidy$data)) && !"response" %in% names(tidy$data)) {
    as.call(list(call("::", as.name("survival"), as.name("Surv")), as.name("time"), as.name("event")))
  } else {
    as.name("response")
  }
  stats::as.formula(as.call(list(as.name("~"), lhs, rhs)), env = baseenv())
}

# Fit one reviewed engine with explicit settings; warnings are recorded, not hidden.
cw_model <- function(tidy, analysis) {
  engine <- cw_engine(analysis)
  if (is.null(engine)) stop("No supported model engine for this analysis configuration.", call. = FALSE)
  settings <- analysis$model
  if (!isTRUE(settings$reviewed)) stop("Model settings must be reviewed before fitting.", call. = FALSE)
  if (identical(settings$intercept, FALSE)) stop("Models without an intercept are not supported by this adapter.", call. = FALSE)
  data <- tidy$data
  formula <- cw_formula(tidy)
  if (engine != "survival::coxph") {
    design <- stats::model.matrix(formula, data)
    if (qr(design)$rank < ncol(design) || nrow(design) <= ncol(design)) {
      stop("The design matrix is rank deficient or has no residual degrees of freedom.", call. = FALSE)
    }
  }
  warnings <- character()
  fit <- withCallingHandlers({
    switch(engine,
      "stats::lm" = stats::lm(formula, data = data, na.action = stats::na.fail, singular.ok = FALSE),
      "stats::glm" = stats::glm(formula, data = data, family = stats::binomial(link = settings$binary_link),
        na.action = stats::na.fail, singular.ok = FALSE),
      "nlme::lme" = {
        if (!identical(settings$random_effects, "random_intercept") ||
            !identical(settings$residual_structure, "independent_homoscedastic") ||
            !isTRUE(settings$estimation %in% c("ML", "REML"))) {
          stop("Only a reviewed random intercept with independent homoscedastic residuals is supported.", call. = FALSE)
        }
        nlme::lme(formula, data = data, random = ~ 1 | subject, method = settings$estimation,
          na.action = stats::na.fail)
      },
      "survival::coxph" = {
        if (!isTRUE(settings$survival_ties %in% c("efron", "breslow", "exact"))) {
          stop("An explicit tie method is required.", call. = FALSE)
        }
        survival::coxph(formula, data = data, ties = settings$survival_ties, na.action = stats::na.fail,
          singular.ok = FALSE, x = TRUE, model = TRUE)
      })
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  levels <- list()
  for (name in grep("^x[0-9]+$", names(data), value = TRUE)) if (is.factor(data[[name]])) levels[[name]] <- levels(data[[name]])
  list(fit = fit, engine = engine, formula = paste(deparse(formula), collapse = " "), warnings = warnings,
    rows_used = nrow(data), alias = tidy$alias, reference_levels = tidy$reference_levels, levels = levels)
}

# Computational diagnostics only; they never certify assumptions or approve inference.
cw_diagnose <- function(model, tidy) {
  fit <- model$fit
  engine <- model$engine
  findings <- list()
  add <- function(code, severity = "block", value = NULL) {
    findings[[length(findings) + 1L]] <<- list(code = code, severity = severity, value = value)
  }
  coefficients <- if (engine == "nlme::lme") nlme::fixef(fit) else stats::coef(fit)
  if (any(!is.finite(coefficients))) add("nonfinite_coefficients")
  if (length(model$warnings)) add("fit_warning_requires_review", "review")
  if (engine == "stats::lm") {
    if (fit$df.residual <= 0) add("no_residual_degrees_of_freedom")
    if (sum(stats::residuals(fit)^2) <= .Machine$double.eps * max(1, sum(tidy$data$response^2))) add("near_zero_residual_variation")
    add("max_cooks_distance", "info", max(stats::cooks.distance(fit)))
  }
  if (engine == "stats::glm") {
    if (!isTRUE(fit$converged)) add("iteration_did_not_converge")
    if (isTRUE(fit$boundary)) add("boundary_fit")
    fitted <- stats::fitted(fit)
    if (any(fitted < 1e-8 | fitted > 1 - 1e-8)) add("possible_separation_fitted_probabilities_0_or_1")
    add("multivariable_separation_not_proven_absent", "review")
  }
  if (engine == "nlme::lme") {
    sigma2 <- fit$sigma^2
    variance <- as.numeric(nlme::getVarCov(fit))[[1]]
    ratio <- variance / (variance + sigma2)
    if (!is.finite(ratio) || ratio <= sqrt(.Machine$double.eps)) add("numerical_random_variance_boundary")
    add("random_intercept_variance_fraction", "info", ratio)
    if (is.character(fit$apVar)) add("variance_parameter_covariance_unusable")
    add("residual_structure_and_influence_review_required", "review")
  }
  if (engine == "survival::coxph") {
    zph <- tryCatch(survival::cox.zph(fit), error = function(e) NULL)
    if (is.null(zph)) {
      add("proportional_hazards_check_failed")
    } else {
      add("proportional_hazards_global_test_p", "info", unname(zph$table[nrow(zph$table), "p"]))
    }
    add("proportional_hazards_and_influence_review_required", "review")
  }
  blocked <- any(vapply(findings, function(x) identical(x$severity, "block"), logical(1)))
  list(computational_state = if (blocked) "blocked" else "checks_completed", findings = findings,
    scientific_approval = FALSE,
    limitation = "Computational diagnostics are evidence for review, not proof that model assumptions hold.")
}

cw_effects <- function(model) {
  fit <- model$fit
  table <- if (model$engine == "nlme::lme") {
    broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
  } else {
    broom::tidy(fit, conf.int = TRUE)
  }
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  term <- table$term
  original <- term
  for (alias in names(model$alias)) {
    original[term == alias] <- model$alias[[alias]]
    for (level in model$levels[[alias]]) original[term == paste0(alias, level)] <- paste0(model$alias[[alias]], ": ", level)
  }
  table$term_original <- original
  if (model$engine %in% c("stats::glm", "survival::coxph")) {
    table$ratio <- exp(table$estimate)
    table$ratio_low <- exp(table$conf.low)
    table$ratio_high <- exp(table$conf.high)
    table$ratio_kind <- if (model$engine == "stats::glm") "odds ratio" else "hazard ratio"
  }
  table
}

cw_write_csv <- function(x, file) {
  utils::write.csv(x, file, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(file)
}

# Runs the configured stages on supplied in-memory data and writes outputs.
cw_run <- function(data, analysis, figures_policy, out_dir, backend = "DescrTab2", synthetic = FALSE,
  label = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  checked <- cw_check(data, analysis)
  result <- list(label = label, synthetic = synthetic, check = checked, outputs = character())
  if (!identical(checked$state, "passed")) {
    result$status <- "data_checks_failed"
    return(result)
  }
  tidy <- cw_tidy(data, analysis)
  described <- cw_describe(tidy, backend)
  result$outputs <- c(result$outputs, cw_write_csv(described$table, file.path(out_dir, "descriptives.csv")))
  result$describe <- described[c("backend", "denominators", "inference")]
  if (exists("cf_compose", mode = "function")) {
    result$figures <- cw_figures(tidy, analysis, figures_policy, out_dir)
    result$outputs <- c(result$outputs, result$figures$files)
  }
  engine <- cw_engine(analysis)
  result$status <- "completed"
  if (!is.null(engine)) {
    model <- cw_model(tidy, analysis)
    diagnostics <- cw_diagnose(model, tidy)
    result$model <- list(engine = engine, formula = model$formula, warnings = model$warnings,
      rows_used = model$rows_used, reference_levels = model$reference_levels, alias = as.list(model$alias))
    result$diagnostics <- diagnostics
    if (identical(diagnostics$computational_state, "blocked")) {
      # Estimates from a blocked fit are not reported as results.
      result$status <- "diagnostics_blocked"
    } else {
      effect_warnings <- character()
      effects <- withCallingHandlers(cw_effects(model), warning = function(w) {
        effect_warnings <<- c(effect_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
      result$model$effect_warnings <- effect_warnings
      result$outputs <- c(result$outputs, cw_write_csv(effects, file.path(out_dir, "effects.csv")))
      result$effects <- effects
    }
  }
  result
}

cw_write_json <- function(x, file) {
  jsonlite::write_json(x, file, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
  invisible(file)
}

# Hashes of the stage code that produced a receipt, normalised like the bundle
# manifest (UTF-8 lines joined with LF plus a final newline).
cw_code_hashes <- function(root = ".") {
  files <- c("code/R/cttir_workflow.R", "code/R/cttir_figures.R")
  hashes <- lapply(files, function(file) {
    path <- file.path(root, file)
    if (!file.exists(path)) return(NULL)
    # tools::sha256sum() exists from R 4.5; older R records the hash as unknown.
    if (!exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) return(NULL)
    text <- paste0(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n")
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    writeBin(charToRaw(enc2utf8(text)), tmp)
    unname(tools::sha256sum(tmp))
  })
  names(hashes) <- files
  hashes
}

cw_session <- function(packages) {
  versions <- lapply(packages, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_))
  names(versions) <- packages
  list(R = paste(R.version$major, R.version$minor, sep = "."), platform = R.version$platform, packages = versions)
}

# ---------------------------------------------------------------------------
# Synthetic demonstration. Every dataset below is constructed here with a fixed
# seed; nothing is read from the project. Each case is compared with an
# independent reference computation so the demo checks the adapters, not a
# scientific hypothesis.

cw_demo_cases <- function(seed = 20261002L) {
  set.seed(seed)
  n <- 60L
  dose <- round(stats::runif(n, 0, 10), 2)
  arm <- rep(c("control", "low", "high"), length.out = n)
  continuous <- data.frame(`Systolic BP (mmHg)` = 120 + 1.5 * dose + c(control = 0, low = -2, high = 3)[arm] +
      stats::rnorm(n, 0, 4), `Dose (mg)` = dose, Arm = arm, check.names = FALSE)
  linpred <- -1.5 + 0.35 * dose
  binary <- data.frame(Response = ifelse(stats::runif(n) < stats::plogis(linpred), "yes", "no"),
    `Dose (mg)` = dose, check.names = FALSE)
  subjects <- 24L
  visits <- 0:3
  id <- rep(sprintf("P%02d", seq_len(subjects)), each = length(visits))
  visit <- rep(visits, subjects)
  longitudinal <- data.frame(Patient = id, Visit = visit,
    Score = 10 + 0.8 * visit + rep(stats::rnorm(subjects, 0, 1.2), each = length(visits)) + stats::rnorm(length(id), 0, 0.7),
    check.names = FALSE)
  age <- round(stats::rnorm(80, 60, 8), 1)
  time <- stats::rexp(80, rate = 0.05 * exp(0.03 * (age - 60)))
  censor <- stats::runif(80, 5, 40)
  survival <- data.frame(Months = round(pmin(time, censor), 3) + 0.001,
    Status = ifelse(time <= censor, "died", "censored"), Age = age, check.names = FALSE)
  list(
    continuous = list(data = continuous, analysis = list(aim = "explanatory", outcome_family = "continuous",
      unit_structure = "independent", approved = TRUE, mapping = list(data_source_id = "synthetic",
        outcome = "Systolic BP (mmHg)", predictors = list("Dose (mg)", "Arm"), estimand = "synthetic adjusted mean difference",
        missing_data = "fail"), model = list(intercept = TRUE, reviewed = TRUE))),
    binary = list(data = binary, analysis = list(aim = "explanatory", outcome_family = "binary",
      unit_structure = "independent", approved = TRUE, mapping = list(data_source_id = "synthetic",
        outcome = "Response", predictors = list("Dose (mg)"), event_value = "yes", non_event_value = "no",
        estimand = "synthetic conditional odds ratio", missing_data = "fail"),
      model = list(intercept = TRUE, binary_link = "logit", reviewed = TRUE))),
    longitudinal = list(data = longitudinal, analysis = list(aim = "explanatory", outcome_family = "continuous",
      unit_structure = "longitudinal", approved = TRUE, mapping = list(data_source_id = "synthetic",
        outcome = "Score", subject = "Patient", time = "Visit", predictors = list("Visit"),
        time_origin = "baseline visit", time_unit = "visit", estimand = "synthetic mean change per visit",
        missing_data = "fail"), model = list(intercept = TRUE, random_effects = "random_intercept",
        residual_structure = "independent_homoscedastic", estimation = "REML", reviewed = TRUE))),
    survival = list(data = survival, analysis = list(aim = "explanatory", outcome_family = "time_to_event",
      unit_structure = "independent", approved = TRUE, mapping = list(data_source_id = "synthetic",
        time = "Months", event = "Status", event_value = "died", non_event_value = "censored",
        predictors = list("Age"), time_origin = "synthetic enrolment", time_unit = "months",
        estimand = "synthetic hazard ratio per year of age", missing_data = "fail"),
      model = list(survival_ties = "breslow", reviewed = TRUE)))
  )
}

# Independent reference computations for the four reviewed engines. Event
# coding is also recounted from the raw synthetic data, independently of cw_tidy().
cw_reference_check <- function(case, tidy, model, input = NULL) {
  data <- tidy$data
  fit <- model$fit
  check <- function(name, observed, expected, tolerance) {
    list(name = name, observed = unname(observed), expected = unname(expected), tolerance = tolerance,
      pass = length(observed) == length(expected) && all(abs(observed - expected) <= tolerance))
  }
  checks <- list()
  if (!is.null(input) && case %in% c("binary", "survival")) {
    mapping <- input$analysis$mapping
    column <- if (case == "binary") mapping[["outcome"]] else mapping[["event"]]
    coded <- if (case == "binary") data$response else data$event
    checks$coding <- check("event coding vs raw synthetic data", sum(coded),
      sum(as.character(input$data[[column]]) == as.character(mapping[["event_value"]])), 0)
  }
  if (case == "continuous") {
    x <- stats::model.matrix(~ x1 + x2, data)
    checks$model <- check("lm coefficients vs normal equations", stats::coef(fit),
      solve(crossprod(x), crossprod(x, data$response))[, 1], 1e-8)
  } else if (case == "binary") {
    x <- cbind(1, data$x1)
    beta <- c(0, 0)
    for (i in seq_len(50L)) {
      p <- stats::plogis(drop(x %*% beta))
      beta <- beta + solve(crossprod(x, x * (p * (1 - p))), crossprod(x, data$response - p))[, 1]
    }
    checks$model <- check("glm coefficients vs hand-written Newton-Raphson", stats::coef(fit), beta, 1e-6)
  } else if (case == "longitudinal") {
    # The adapter's own fit is checked: generalised least squares and the dense
    # Gaussian (restricted) likelihood at its variance estimates, and that
    # nearby variances do not improve the criterion it maximised.
    restricted <- identical(fit$method, "REML")
    x <- stats::model.matrix(~ x1, data)
    groups <- split(seq_len(nrow(data)), data$subject)
    criterion <- function(tau2, sigma2) {
      xvx <- 0
      xvy <- 0
      logdet <- 0
      for (rows in groups) {
        v <- diag(sigma2, length(rows)) + tau2
        xi <- x[rows, , drop = FALSE]
        xvx <- xvx + crossprod(xi, solve(v, xi))
        xvy <- xvy + crossprod(xi, solve(v, data$response[rows]))
        logdet <- logdet + as.numeric(determinant(v)$modulus)
      }
      beta <- solve(xvx, xvy)[, 1]
      quad <- 0
      for (rows in groups) {
        v <- diag(sigma2, length(rows)) + tau2
        r <- data$response[rows] - drop(x[rows, , drop = FALSE] %*% beta)
        quad <- quad + drop(crossprod(r, solve(v, r)))
      }
      n <- nrow(x) - if (restricted) ncol(x) else 0L
      extra <- if (restricted) as.numeric(determinant(xvx)$modulus) else 0
      list(beta = beta, loglik = -0.5 * (n * log(2 * pi) + logdet + extra + quad))
    }
    sigma2 <- fit$sigma^2
    tau2 <- as.numeric(nlme::getVarCov(fit))[[1]]
    at <- criterion(tau2, sigma2)
    nearby <- vapply(list(c(1.05, 1), c(0.95, 1), c(1, 1.05), c(1, 0.95)),
      function(f) criterion(tau2 * f[[1]], sigma2 * f[[2]])$loglik, numeric(1))
    checks$model <- check("lme fixed effects vs generalised least squares at the fitted variances",
      nlme::fixef(fit), at$beta, 1e-6)
    checks$likelihood <- check(paste(if (restricted) "REML" else "ML", "log-likelihood vs dense Gaussian computation"),
      as.numeric(stats::logLik(fit)), at$loglik, 1e-6)
    checks$optimum <- list(name = "criterion is not improved by variances 5% away from the fit", observed = at$loglik,
      expected = max(nearby), tolerance = 0, pass = all(at$loglik >= nearby))
  } else {
    ordered <- data[order(data$time), ]
    partial <- function(beta) {
      eta <- beta * ordered$x1
      sum(vapply(which(ordered$event == 1L), function(i) {
        risk <- ordered$time >= ordered$time[[i]]
        eta[[i]] - log(sum(exp(eta[risk])))
      }, numeric(1)))
    }
    reference <- stats::optimize(partial, c(-2, 2), maximum = TRUE, tol = 1e-12)$maximum
    checks$model <- check("Cox Breslow coefficient vs direct partial likelihood maximisation", stats::coef(fit), reference, 1e-5)
  }
  checks <- unname(checks)
  list(name = paste(vapply(checks, function(x) x$name, character(1)), collapse = "; "),
    pass = all(vapply(checks, function(x) isTRUE(x$pass), logical(1))), checks = checks)
}

cw_run_demo <- function(root = ".", out_dir = file.path(root, "demo", "outputs"), figures_policy = NULL,
  backend = "DescrTab2", write = TRUE) {
  label <- "SYNTHETIC DEMONSTRATION - constructed data, not study data or scientific results"
  cases <- cw_demo_cases()
  receipt <- list(label = label, synthetic = TRUE, seed = 20261002L, cases = list())
  for (name in names(cases)) {
    case <- cases[[name]]
    directory <- if (write) file.path(out_dir, name) else tempfile(paste0("cttir-demo-", name, "-"))
    result <- cw_run(case$data, case$analysis, figures_policy, directory, backend, synthetic = TRUE, label = label)
    tidy <- cw_tidy(case$data, case$analysis)
    reference <- if (identical(result$status, "completed")) cw_reference_check(name, tidy, cw_model(tidy, case$analysis), case) else NULL
    receipt$cases[[name]] <- list(status = result$status, engine = result$model$engine,
      describe_backend = result$describe$backend, reference = reference,
      diagnostics = vapply(result$diagnostics$findings, function(x) x$code, character(1)),
      outputs = if (write) basename(result$outputs) else character())
    if (!write) unlink(directory, recursive = TRUE)
  }
  receipt$status <- if (all(vapply(receipt$cases, function(x) identical(x$status, "completed") && isTRUE(x$reference$pass), logical(1)))) "passed" else "failed"
  receipt$session <- cw_session(c("readr", "dplyr", "DescrTab2", "ggplot2", "patchwork", "viridisLite",
    "RColorBrewer", "colorspace", "nlme", "survival", "broom", "broom.mixed", "yaml", "jsonlite"))
  receipt$code_sha256 <- cw_code_hashes(root)
  if (write) {
    dir.create(file.path(root, "demo"), showWarnings = FALSE)
    cw_write_json(receipt, file.path(root, "demo", "receipt.json"))
  }
  receipt
}

# Figure panels built only from aliased columns with the project figure policy.
cw_figures <- function(tidy, analysis, policy, out_dir) {
  policy <- cf_check_policy(if (is.null(policy)) cf_policy_default() else policy)
  data <- tidy$data
  panels <- list()
  outcome_label <- if ("response" %in% names(data)) cw_label(tidy, "response") else cw_label(tidy, "time")
  predictors <- grep("^x[0-9]+$", names(data), value = TRUE)
  group <- NULL
  numeric_x <- NULL
  for (name in predictors) {
    if (is.factor(data[[name]]) && is.null(group)) group <- name
    if (is.numeric(data[[name]]) && is.null(numeric_x)) numeric_x <- name
  }
  colours <- NULL
  shapes <- NULL
  if (!is.null(group)) {
    colours <- cf_palette_categorical(levels(data[[group]]), policy)
    shapes <- cf_shapes(levels(data[[group]]))
  }
  if ("response" %in% names(data)) {
    panels$distribution <- ggplot2::ggplot(data, ggplot2::aes(x = response)) +
      ggplot2::geom_histogram(bins = 15, fill = viridisLite::viridis(1, begin = 0.3), colour = "white") +
      ggplot2::labs(x = outcome_label, y = "Count") + ggplot2::theme_minimal(base_size = 11)
  } else {
    km <- survival::survfit(survival::Surv(time, event) ~ 1, data = data)
    curve <- data.frame(time = c(0, km$time), survival = c(1, km$surv))
    panels$distribution <- ggplot2::ggplot(curve, ggplot2::aes(x = time, y = survival)) +
      ggplot2::geom_step(colour = viridisLite::viridis(1, begin = 0.3), linewidth = 0.8) +
      ggplot2::labs(x = cw_label(tidy, "time"), y = "Event-free proportion") +
      ggplot2::coord_cartesian(ylim = c(0, 1)) + ggplot2::theme_minimal(base_size = 11)
  }
  mapping <- list(x = list(variable = outcome_label, type = "continuous"))
  if (all(c("subject", "time", "response") %in% names(data))) {
    trajectory <- viridisLite::viridis(2, begin = 0.25, end = 0.6)
    panels$relationship <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = response, group = subject)) +
      ggplot2::geom_line(alpha = 0.35, colour = trajectory[[2]]) +
      ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line", linewidth = 1,
        colour = trajectory[[1]], linetype = "dashed") +
      ggplot2::labs(x = cw_label(tidy, "time"), y = outcome_label,
        caption = "Thin lines: units; dashed line: mean") + ggplot2::theme_minimal(base_size = 11)
  } else if (!is.null(numeric_x) && "response" %in% names(data)) {
    aesthetics <- if (is.null(group)) {
      ggplot2::aes(x = .data[[numeric_x]], y = response)
    } else {
      ggplot2::aes(x = .data[[numeric_x]], y = response, colour = .data[[group]], shape = .data[[group]])
    }
    plot <- ggplot2::ggplot(data, aesthetics) + ggplot2::geom_point(size = 2, alpha = 0.85) +
      ggplot2::labs(x = cw_label(tidy, numeric_x), y = outcome_label) + ggplot2::theme_minimal(base_size = 11)
    if (!is.null(group)) {
      label <- cw_label(tidy, group)
      plot <- plot + ggplot2::scale_colour_manual(name = label, values = colours, drop = FALSE) +
        ggplot2::scale_shape_manual(name = label, values = shapes, drop = FALSE)
      mapping$colour <- list(variable = label, type = "categorical", levels = levels(data[[group]]))
    }
    panels$relationship <- plot
  }
  composed <- cf_compose(unname(panels), tags = TRUE, collect_guides = FALSE)
  file <- file.path(out_dir, "figure-overview.png")
  spec <- cf_figure_spec(id = "overview", mapping = mapping, policy = policy, colours = colours,
    encodings = if (is.null(group)) NULL else list(shape = shapes), width = 8, height = 3.6,
    units = "in", dpi = 200, background = "#FFFFFF")
  cf_save(composed, file, spec)
  list(files = c(file, paste0(file, ".json")), accessibility = spec$accessibility$status, panels = names(panels))
}
