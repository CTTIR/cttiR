mapped_analysis <- function() {
  list(
    data_sources = list(list(id = "source01", label = "Synthetic source", logical_uri = NULL,
        format = "csv", access_class = "unknown", checksum = NULL, checksum_status = "unknown", schema_ref = NULL)),
    analysis = list(aim = "explanatory", outcome_family = "continuous", unit_structure = "independent",
      engine = "stats::lm", approved = TRUE,
      mapping = list(data_source_id = "source01", outcome = "response", predictors = list("exposure"),
        estimand = "Specified conditional association", missing_data = "fail"))
  )
}

mapped_project <- function(config) {
  parent <- tempfile("mapping-plan-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE))
  project("Mapped fixture", "methods", "Explicit mapping", parent, config, dry_run = TRUE)
}

test_that("three-input setup reports missing analysis decisions without inference", {
  p <- project("Unspecified", "methods", "Fit a survival model to an unknown dataset", new_parent(), dry_run = TRUE)
  expect_null(p$readiness$analysis$candidate_engine)
  expect_false(p$readiness$analysis$executable)
  expect_contains(unlist(p$readiness$analysis$missing_fields), "/analysis/aim")
  expect_false(dir.exists(p$path))
  old <- p$spec
  old$analysis <- NULL
  expect_equal(analysis_configuration(validate_spec(old))$state, "incomplete")
})

test_that("explicit mappings survive create sync and configuration round trips", {
  config <- mapped_analysis()
  p <- project("Mapped", "methods", "Goal", new_parent(), config)
  expect_equal(p$readiness$analysis$state, "configuration_recorded")
  expect_equal(p$readiness$analysis$candidate_engine, "stats::lm")
  expect_false(p$readiness$analysis$executable)
  expect_length(p$readiness$analysis$missing_fields, 0L)
  expect_equal(read_document(file.path(p$path, "config/analysis.yml"))$mapping$outcome, "response")
  expect_equal(read_project(p$path)$spec$analysis$mapping, p$spec$analysis$mapping)
  result <- sync(p$path, config = list(analysis = list(mapping = list(outcome = "changed response"))), dry_run = FALSE)
  expect_equal(result$state, "applied")
  expect_equal(result$analysis$candidate_engine, "stats::lm")
  expect_false(result$analysis$executable)
  expect_equal(read_project(p$path)$spec$analysis$mapping$outcome, "changed response")
  config$analysis$mapping$predictors <- list()
  config$analysis$mapping$outcome <- "unknown"
  expect_length(mapped_project(config)$readiness$analysis$missing_fields, 0L)
})

test_that("mapping reference and role conflicts fail before creation", {
  config <- mapped_analysis()
  config$analysis$mapping$data_source_id <- "unregistered"
  expect_error(mapped_project(config), class = "cttir_schema_error")
  config <- mapped_analysis()
  config$analysis$mapping$subject <- "response"
  expect_error(mapped_project(config), "distinct columns")
  config <- mapped_analysis()
  config$analysis$mapping$predictors <- list("response")
  expect_error(mapped_project(config), "cannot also be predictors")
  config <- mapped_analysis()
  config$analysis$mapping$predictors <- list("exposure", "exposure")
  expect_error(mapped_project(config), class = "cttir_schema_error")
  config <- mapped_analysis()
  config$analysis$mapping$outcome <- "bad\ncolumn"
  expect_error(mapped_project(config), class = "cttir_schema_error")
  config <- mapped_analysis()
  config$analysis$mapping$missing_data <- "automatic_imputation"
  expect_error(mapped_project(config), class = "cttir_schema_error")
})

test_that("binary and survival plans require explicit coding and time meaning", {
  config <- mapped_analysis()
  config$analysis$outcome_family <- "binary"
  config$analysis$engine <- "stats::glm"
  plan <- mapped_project(config)$readiness$analysis
  expect_contains(unlist(plan$missing_fields), "/analysis/mapping/event_value")
  config$analysis$mapping$event_value <- 1L
  config$analysis$mapping$non_event_value <- 0L
  expect_length(mapped_project(config)$readiness$analysis$missing_fields, 0L)
  config$analysis$mapping$non_event_value <- "1"
  expect_error(mapped_project(config), "must differ")
  config$analysis$mapping$non_event_value <- 0L
  config$analysis$outcome_family <- "time_to_event"
  config$analysis$engine <- "survival::coxph"
  plan <- mapped_project(config)$readiness$analysis
  expect_contains(unlist(plan$missing_fields), "/analysis/mapping/time_origin")
  config$analysis$mapping$time <- "follow_up"
  config$analysis$mapping$event <- "status"
  config$analysis$mapping$time_origin <- "A specified start"
  config$analysis$mapping$time_unit <- "days"
  plan <- mapped_project(config)$readiness$analysis
  expect_equal(plan$candidate_engine, "survival::coxph")
  expect_length(plan$missing_fields, 0L)
  expect_false(plan$executable)
})

test_that("unsupported designs never silently use an independent model", {
  config <- mapped_analysis()
  config$analysis$unit_structure <- "longitudinal"
  config$analysis$engine <- "nlme::lme"
  plan <- mapped_project(config)$readiness$analysis
  expect_equal(plan$candidate_engine, "nlme::lme")
  expect_contains(unlist(plan$missing_fields), "/analysis/mapping/subject")
  expect_contains(unlist(plan$capability_gaps), "random_effects_and_residual_structure_review_required")
  config$analysis$unit_structure <- "paired"
  expect_null(mapped_project(config)$readiness$analysis$candidate_engine)
  for (aim in c("predictive", "causal")) {
    config$analysis$aim <- aim
    plan <- mapped_project(config)$readiness$analysis
    expect_null(plan$candidate_engine)
    expect_contains(unlist(plan$capability_gaps), "specialist_design_and_validation_required")
  }
  config <- mapped_analysis()
  config$analysis$engine <- "unreviewed::model"
  expect_contains(unlist(mapped_project(config)$readiness$analysis$capability_gaps), "requested_engine_not_supported_for_this_configuration")
})

test_that("variable names and logical locations remain unevaluated data", {
  config <- mapped_analysis()
  marker <- tempfile()
  literal <- paste0("system('touch ", marker, "')")
  config$analysis$mapping$outcome <- literal
  config$data_sources[[1]]$logical_uri <- "/unavailable/private/data.csv"
  p <- mapped_project(config)
  expect_equal(p$spec$analysis$mapping$outcome, literal)
  expect_false(file.exists(marker))
  expect_false(dir.exists(p$path))
})

test_that("project audits distinguish recorded configuration from workflow approval", {
  p <- project("Audit mapping", "methods", "Goal", new_parent(), mapped_analysis())
  report <- audit(p$path, scope = "project")
  status <- stats::setNames(report$checks$status, report$checks$id)
  expect_equal(unname(status[c("STD-001", "STD-002", "STD-003", "STD-004", "STD-005")]), rep("pass", 5))
  expect_false(analysis_configuration(read_project(p$path)$spec)$executable)
})
