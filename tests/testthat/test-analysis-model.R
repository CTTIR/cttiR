test_that("model choices stay unknown until explicitly recorded", {
  p <- project("Model draft", "methods", "Goal", new_parent(), dry_run = TRUE)
  model <- p$readiness$analysis$model
  expect_equal(model$state, "incomplete")
  expect_contains(unlist(model$missing_fields), "/analysis/model/reviewed")
  expect_contains(unlist(model$capability_gaps), "supported_engine_not_selected")
  expect_false(model$executable)
  expect_null(p$spec$analysis$model)
})

test_that("engine-specific model choices report missing and conflicting settings", {
  a <- list(model = list(intercept = TRUE, reviewed = TRUE))
  expect_equal(analysis_model_configuration(a, "stats::lm")$state, "settings_recorded")
  expect_length(analysis_model_configuration(a, "stats::lm")$missing_fields, 0L)
  expect_contains(unlist(analysis_model_configuration(a, "stats::glm")$missing_fields), "/analysis/model/binary_link")
  a$model$binary_link <- "logit"
  expect_equal(analysis_model_configuration(a, "stats::glm")$state, "settings_recorded")
  expect_contains(unlist(analysis_model_configuration(a, "stats::lm")$inapplicable_fields), "/analysis/model/binary_link")
  a$model$intercept <- FALSE
  expect_contains(unlist(analysis_model_configuration(a, "stats::glm")$capability_gaps), "no_intercept_adapter_not_supported")
  a$model$reviewed <- FALSE
  expect_contains(unlist(analysis_model_configuration(a, "stats::glm")$missing_fields), "/analysis/model/reviewed")
})

test_that("survival and mixed settings are explicit typed choices", {
  a <- list(model = list(reviewed = TRUE, survival_ties = "breslow"))
  expect_equal(analysis_model_configuration(a, "survival::coxph")$state, "settings_recorded")
  a$model <- list(intercept = TRUE, reviewed = TRUE, random_effects = "random_intercept",
    residual_structure = "independent_homoscedastic", estimation = "REML")
  expect_equal(analysis_model_configuration(a, "nlme::lme")$state, "settings_recorded")
  expect_false(analysis_model_configuration(a, "nlme::lme")$executable)
  a$aim <- "explanatory"
  a$outcome_family <- "continuous"
  a$unit_structure <- "longitudinal"
  planned <- analysis_configuration(list(analysis = a))
  expect_false("random_effects_and_residual_structure_review_required" %in% unlist(planned$capability_gaps))
  expect_false(planned$executable)
  expect_silent(validate_config(list(analysis = a)))
  a$model$residual_structure <- "guess_automatically"
  expect_error(validate_config(list(analysis = a)), class = "cttir_schema_error")
  expect_error(validate_config(list(analysis = list(model = list(formula = "y ~ x")))), class = "cttir_schema_error")
})

test_that("model settings survive managed configuration and synchronization", {
  config <- list(analysis = list(aim = "explanatory", outcome_family = "continuous",
      unit_structure = "independent", engine = "stats::lm",
      model = list(intercept = TRUE, reviewed = TRUE)))
  p <- project("Model settings", "methods", "Record choices", new_parent(), config)
  expect_equal(p$readiness$analysis$model$state, "settings_recorded")
  expect_equal(read_document(file.path(p$path, "config/analysis.yml"))$model, p$spec$analysis$model)
  result <- sync(p$path, config = list(analysis = list(model = list(reviewed = FALSE))), dry_run = FALSE)
  expect_contains(unlist(result$analysis$model$missing_fields), "/analysis/model/reviewed")
  expect_false(result$analysis$executable)
  expect_equal(read_project(p$path)$spec$analysis$model$reviewed, FALSE)
})
