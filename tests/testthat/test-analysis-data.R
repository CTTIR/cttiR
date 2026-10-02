data_spec <- function(engine = "stats::lm") {
  config <- list(
    data_sources = list(list(id = "source01", label = "Fixture", logical_uri = NULL, format = "csv",
        access_class = "unknown", checksum = NULL, checksum_status = "unknown", schema_ref = NULL)),
    analysis = list(aim = "explanatory", outcome_family = "continuous", unit_structure = "independent",
      engine = engine, approved = FALSE, mapping = list(data_source_id = "source01", outcome = "y",
        predictors = list("x"), estimand = "Conditional association", missing_data = "fail")))
  if (engine == "stats::glm") {
    config$analysis$outcome_family <- "binary"
    config$analysis$mapping$event_value <- "yes"
    config$analysis$mapping$non_event_value <- "no"
  }
  if (engine == "survival::coxph") {
    config$analysis$outcome_family <- "time_to_event"
    config$analysis$mapping$outcome <- NULL
    config$analysis$mapping <- c(config$analysis$mapping, list(time = "time", event = "event",
        event_value = 1L, non_event_value = 0L, time_origin = "Enrollment", time_unit = "days"))
  }
  if (engine == "nlme::lme") {
    config$analysis$unit_structure <- "longitudinal"
    config$analysis$mapping <- c(config$analysis$mapping, list(subject = "id", time = "time",
        time_origin = "Enrollment", time_unit = "days"))
  }
  project("Data checks", "methods", "Check explicit mappings", new_parent(), config, dry_run = TRUE)$spec
}

issue_codes <- function(x) vapply(x$issues, function(i) i$code, character(1))

test_that("explicit data checks preserve data and do not approve a model", {
  spec <- data_spec()
  data <- data.frame(y = 1:5, x = 2:6)
  original <- serialize(data, NULL)
  report <- check_analysis_data(data, spec)
  expect_equal(report$state, "passed")
  expect_equal(report$rows_complete, 5L)
  expect_false(report$executable)
  expect_identical(serialize(data, NULL), original)
  spec$analysis$mapping$outcome <- "stop('never evaluate')"
  names(data)[1] <- spec$analysis$mapping$outcome
  expect_equal(check_analysis_data(data, spec)$state, "passed")
  spec$analysis$mapping$estimand <- NULL
  expect_equal(check_analysis_data(data, spec)$state, "configuration_incomplete")
  expect_error(check_analysis_data("file.csv", spec), class = "cttir_input_error")
})

test_that("missingness follows the explicit mapped-column policy", {
  spec <- data_spec()
  data <- data.frame(y = c(1, NA, 3, 4), x = 1:4, ignored = NA)
  report <- check_analysis_data(data, spec)
  expect_contains(issue_codes(report), "missing_values_forbidden")
  expect_equal(report$rows_complete, 3L)
  spec$analysis$mapping$missing_data <- "complete_case"
  expect_equal(check_analysis_data(data, spec)$state, "passed")
  data$x <- NA_real_
  expect_contains(issue_codes(check_analysis_data(data, spec)), "no_complete_rows")
  expect_contains(issue_codes(check_analysis_data(data[FALSE, ], spec)), "empty_data")
  expect_contains(issue_codes(check_analysis_data(data[, "x", drop = FALSE], spec)), "missing_column")
})

test_that("unsupported classes and nonfinite numbers are refused", {
  spec <- data_spec()
  data <- data.frame(y = 1:4, x = c(1, 2, Inf, 4))
  expect_contains(issue_codes(check_analysis_data(data, spec)), "nonfinite_value")
  data$x <- rep(1, 4)
  expect_contains(issue_codes(check_analysis_data(data, spec)), "constant_predictor")
  data$x <- as.Date("2020-01-01") + 1:4
  expect_contains(issue_codes(check_analysis_data(data, spec)), "unsupported_column_type")
  data$x <- I(matrix(1:8, 4))
  expect_contains(issue_codes(check_analysis_data(data, spec)), "unsupported_column_type")
  data$x <- 1:4
  data$y <- letters[1:4]
  expect_contains(issue_codes(check_analysis_data(data, spec)), "numeric_column_required")
  names(data) <- c("y", "y")
  expect_error(check_analysis_data(data, spec), "unique nonempty")
})

test_that("binary event mapping is explicit and never inferred", {
  spec <- data_spec("stats::glm")
  data <- data.frame(y = factor(c("yes", "no", "no", "yes")), x = 1:4)
  expect_equal(check_analysis_data(data, spec)$state, "passed")
  data$y <- c("yes", "missing", "no", "yes")
  expect_contains(issue_codes(check_analysis_data(data, spec)), "unmapped_event_code")
  data$y <- "yes"
  expect_contains(issue_codes(check_analysis_data(data, spec)), "no_non_events")
  data$y <- "no"
  expect_contains(issue_codes(check_analysis_data(data, spec)), "no_events")
})

test_that("right-censored survival requires time and observed events", {
  spec <- data_spec("survival::coxph")
  data <- data.frame(time = 1:4, event = c(1, 0, 1, 0), x = 1:4)
  expect_equal(check_analysis_data(data, spec)$state, "passed")
  data$time[1] <- 0
  expect_contains(issue_codes(check_analysis_data(data, spec)), "positive_time_required")
  data$time <- 1:4
  data$event <- 0
  expect_contains(issue_codes(check_analysis_data(data, spec)), "no_events")
  data$event <- 1
  expect_equal(check_analysis_data(data, spec)$state, "passed")
})

test_that("unit structure checks preserve repeated-measure boundaries", {
  spec <- data_spec("nlme::lme")
  data <- data.frame(y = 1:6, x = 2:7, id = rep(c("a", "b"), each = 3), time = rep(1:3, 2))
  report <- check_analysis_data(data, spec)
  expect_equal(report$state, "passed")
  expect_false(report$executable)
  data$time[2] <- 1
  expect_contains(issue_codes(check_analysis_data(data, spec)), "duplicate_subject_time")
  data$id <- "private-person-id"
  report <- check_analysis_data(data, spec)
  expect_contains(issue_codes(report), "insufficient_groups")
  expect_false(grepl("private-person-id", json_text(report), fixed = TRUE))
  data$id <- " "
  expect_contains(issue_codes(check_analysis_data(data, spec)), "empty_subject_identifier")
  data$id <- as.character(1:6)
  expect_contains(issue_codes(check_analysis_data(data, spec)), "no_repeated_units")
  spec <- data_spec()
  spec$analysis$mapping$subject <- "id"
  data$id <- rep(c("a", "b"), each = 3)
  expect_contains(issue_codes(check_analysis_data(data, spec)), "repeated_units_in_independent_plan")
})
