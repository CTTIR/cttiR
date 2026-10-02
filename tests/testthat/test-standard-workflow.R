stage_library <- function(extra = list()) {
  env <- new.env(parent = globalenv())
  for (name in names(extra)) assign(name, extra[[name]], envir = env)
  root <- system.file("templates", "standard-0.3.0", "code", "R", package = "cttiR")
  for (file in sort(list.files(root, "\\.R$", full.names = TRUE))) sys.source(file, envir = env)
  env
}

workflow_spec <- function(analysis) {
  spec <- resolve_spec("Stage fixture", "methods", "Synthetic stage fixture", NULL,
    list(data_sources = list(list(id = "synthetic", label = "Synthetic", logical_uri = NULL, format = "csv",
      access_class = "public", checksum = NULL, checksum_status = "unknown", schema_ref = NULL)),
      analysis = analysis))
  spec
}

test_that("the standard bundle is pinned, integrity-checked and free of user text", {
  goal <- "<script>alert(1)</script> system('touch /tmp/pwned') {{ injection }}"
  p <- project("Standard bundle", "primary_research", goal, new_parent())
  manifest <- read_document(file.path(p$path, "metadata/workflow-template.json"))
  expect_equal(manifest$bundle, "standard-0.3.0")
  expect_false(manifest$initializer_invoked)
  for (path in names(manifest$files)) {
    if (!file.exists(file.path(p$path, path))) next
    expect_identical(file_hash(file.path(p$path, path)), manifest$files[[path]])
  }
  code <- list.files(file.path(p$path, "code"), recursive = TRUE, full.names = TRUE)
  for (file in code) expect_false(any(grepl("pwned|alert\\(1\\)|injection", readLines(file))))
  expect_equal(read_project(p$path)$lock$template_version, "0.3.0")
  expect_false(dir.exists(file.path(p$path, "demo/outputs")))
  expect_false(dir.exists(file.path(p$path, "reports/workflow")))
  ownership <- stats::setNames(vapply(p$manifest, function(x) x$ownership, character(1)),
    vapply(p$manifest, function(x) x$path, character(1)))
  expect_equal(unname(ownership[c("code/R/cttir_workflow.R", "config/workflow.yml", "analysis/02_eda.Rmd")]),
    c("managed", "managed", "user"))
  before <- tree_hashes(p$path)
  expect_true(all(sync(p$path)$actions$action == "skip"))
  expect_identical(tree_hashes(p$path), before)
})

test_that("stage checks agree with check_analysis_data on shared fixtures", {
  skip_if_not_installed("dplyr")
  env <- stage_library()
  analysis <- list(aim = "explanatory", outcome_family = "binary", unit_structure = "independent", approved = FALSE,
    mapping = list(data_source_id = "synthetic", outcome = "y", predictors = list("x", "z"), event_value = "1",
      non_event_value = "0", estimand = "odds ratio", missing_data = "complete_case"),
    model = list(intercept = TRUE, binary_link = "logit", reviewed = TRUE))
  spec <- workflow_spec(analysis)
  fixtures <- list(
    ok = data.frame(y = c(0, 1, 0, 1, 1), x = c(1, 2, 3, 4, 5), z = c("a", "b", "a", "b", "a")),
    unmapped = data.frame(y = c(0, 1, 2, 1), x = c(1, 2, 3, 4), z = c("a", "b", "a", "b")),
    constant = data.frame(y = c(0, 1, 0, 1), x = c(1, 1, 1, 1), z = c("a", "b", "a", "b")),
    missing = data.frame(y = c(0, 1), z = c("a", "b")),
    nonfinite = data.frame(y = c(0, 1, 0, 1), x = c(1, Inf, 2, 3), z = c("a", "b", "a", "b"))
  )
  codes <- function(issues) sort(vapply(issues, function(x) x$code, character(1)))
  for (name in names(fixtures)) {
    package <- check_analysis_data(fixtures[[name]], spec)
    stage <- env$cw_check(fixtures[[name]], analysis)
    expect_identical(stage$state, package$state, info = name)
    expect_identical(codes(stage$issues), codes(package$issues), info = name)
  }
})

test_that("tidy aliases keep column names out of formulas and record reference levels", {
  skip_if_not_installed("dplyr")
  env <- stage_library()
  data <- data.frame(`y ~ .` = c(1.2, 2.3, 2.9, 4.1, 5.2, 6.1), `system("x")` = c(1, 2, 3, 4, 5, 6),
    `arm` = c("b", "a", "b", "a", "c", "c"), check.names = FALSE)
  analysis <- list(aim = "explanatory", outcome_family = "continuous", unit_structure = "independent", approved = TRUE,
    mapping = list(data_source_id = "synthetic", outcome = "y ~ .", predictors = list('system("x")', "arm"),
      estimand = "difference", missing_data = "fail"), model = list(intercept = TRUE, reviewed = TRUE))
  tidy <- env$cw_tidy(data, analysis)
  expect_named(tidy$data, c("response", "x1", "x2"))
  expect_equal(tidy$reference_levels$x2, "a")
  expect_equal(unname(tidy$alias[["x1"]]), 'system("x")')
  model <- env$cw_model(tidy, analysis)
  expect_equal(model$formula, "response ~ x1 + x2")
  skip_if_not_installed("broom")
  effects <- env$cw_effects(model)
  expect_contains(effects$term_original, c('system("x")', "arm: b", "arm: c"))
})

test_that("descriptive tables never test and DescrTab2 absence uses the documented fallback", {
  skip_if_not_installed("dplyr")
  data <- data.frame(score = c(1, 2, 3, NA, 5), group = c("a", "b", "a", "b", "a"))
  analysis <- list(aim = "descriptive", outcome_family = "unknown", unit_structure = "unknown", approved = FALSE,
    mapping = list(data_source_id = "synthetic", outcome = "score", predictors = list("group"), missing_data = "complete_case"))
  base <- stage_library(list(requireNamespace = function(...) FALSE))
  described <- base$cw_describe(base$cw_tidy(data, analysis), "DescrTab2")
  expect_equal(described$backend, "base_descriptive_only")
  expect_equal(described$denominators$rows_excluded, 1L)
  expect_false(any(grepl("p-value|p =", unlist(described$table))))
  skip_if_not_installed("DescrTab2")
  env <- stage_library()
  described <- env$cw_describe(env$cw_tidy(data, analysis), "DescrTab2")
  expect_equal(described$backend, "DescrTab2")
  expect_match(described$inference, "no-test")
  expect_false(any(grepl("^p$|p-value", names(described$table))))
})

test_that("reviewed engines match independent references on synthetic data", {
  # DescrTab2 is optional here: without it the documented base fallback is used.
  for (package in c("dplyr", "nlme", "survival", "broom", "broom.mixed")) skip_if_not_installed(package)
  env <- stage_library()
  # The three-arm demo figure has a grayscale finding no eligible Brewer palette
  # avoids; it is recorded in the receipt with its non-colour encoding.
  receipt <- env$cw_run_demo(root = tempfile(), write = FALSE, figures_policy = default_spec("x", "methods", "x", "x")$figures)
  expect_equal(receipt$status, "passed")
  expect_equal(receipt$figure_accessibility$status, "findings_recorded")
  expect_match(unlist(receipt$figure_accessibility$findings), "grayscale", all = FALSE)
  expect_equal(receipt$cases$continuous$figure_accessibility$non_colour_encoding, "shape")
  expect_setequal(names(receipt$cases), c("continuous", "binary", "longitudinal", "survival"))
  for (case in receipt$cases) {
    expect_true(case$reference$pass)
    expect_equal(case$status, "completed")
  }
  expect_match(receipt$label, "SYNTHETIC")
})

test_that("blocked fits do not report estimates", {
  for (package in c("dplyr", "broom")) skip_if_not_installed(package)
  env <- stage_library()
  data <- data.frame(y = c(0, 0, 0, 1, 1, 1), x = c(1, 2, 3, 4, 5, 6))
  analysis <- list(aim = "explanatory", outcome_family = "binary", unit_structure = "independent", approved = TRUE,
    mapping = list(data_source_id = "synthetic", outcome = "y", predictors = list("x"), event_value = "1",
      non_event_value = "0", estimand = "odds ratio", missing_data = "fail"),
    model = list(intercept = TRUE, binary_link = "logit", reviewed = TRUE))
  result <- env$cw_run(data, analysis, NULL, tempfile(), backend = "none")
  expect_equal(result$status, "diagnostics_blocked")
  expect_null(result$effects)
  expect_contains(vapply(result$diagnostics$findings, function(x) x$code, character(1)),
    "possible_separation_fitted_probabilities_0_or_1")
})

test_that("the study-data runner refuses incomplete configuration with a precise list", {
  p <- project("Runner", "primary_research", "Describe outcomes", new_parent())
  result <- processx::run(file.path(R.home("bin"), "Rscript"), c("--vanilla", "code/run_workflow.R"),
    wd = p$path, error_on_status = FALSE, timeout = 120000)
  expect_false(result$status == 0L)
  expect_match(result$stderr, "not ready")
  expect_match(result$stderr, "analysis.mapping.data_source_id")
  expect_match(result$stderr, "analysis.approved")
  expect_false(dir.exists(file.path(p$path, "reports/workflow")))
})

test_that("the synthetic demo runs in a generated project and labels its outputs", {
  for (package in c("dplyr", "nlme", "survival", "broom", "broom.mixed", "ggplot2",
    "patchwork", "viridisLite", "RColorBrewer", "colorspace")) skip_if_not_installed(package)
  p <- project("Demo", "primary_research", "Describe outcomes", new_parent())
  skip_if_not(file.exists(file.path(p$path, "code/R/cttir_figures.R")))
  result <- processx::run(file.path(R.home("bin"), "Rscript"), c("--vanilla", "code/run_demo.R"),
    wd = p$path, error_on_status = FALSE, timeout = 300000)
  expect_equal(result$status, 0L, info = paste(result$stdout, result$stderr))
  receipt <- jsonlite::fromJSON(file.path(p$path, "demo/receipt.json"), simplifyVector = FALSE)
  expect_equal(receipt$status, "passed")
  expect_true(receipt$synthetic)
  expect_true(file.exists(file.path(p$path, "demo/outputs/continuous/figure-overview.png")))
  expect_true(file.exists(file.path(p$path, "demo/outputs/continuous/effects.csv")))
  readiness <- project_readiness(p$path)
  expect_true(readiness$checks[[3]]$passed)
  expect_false(readiness$checks[[4]]$passed)
})

test_that("delimited import honours dictionary types and missing codes without guessing", {
  skip_if_not_installed("readr")
  env <- stage_library()
  root <- new_parent()
  dir.create(file.path(root, "metadata"))
  csv <- file.path(root, "cohort.csv")
  writeLines(c("id,score,group,visit", "a,1.5,x,0", "b,-99,y,1", "c,2.25,,2", "d,abc,x,3"), csv)
  writeLines(c("dataset_id,variable,type,unit,allowed_values,missing_codes,description",
    "cohort,id,character,,,,", "cohort,score,numeric,,,-99,", "cohort,group,character,,,,", "cohort,visit,integer,,,,"),
    file.path(root, "metadata", "data-dictionary.csv"))
  config <- list(root = root, analysis = list(mapping = list(data_source_id = "cohort")),
    datasets = list(list(id = "cohort", format = "csv")), bindings = list(list(id = "cohort", path = csv)))
  data <- env$cw_import(config)
  expect_equal(nrow(data), 4L)
  expect_type(data$score, "double")
  expect_type(data$visit, "integer")
  expect_true(is.na(data$score[[2]]))
  expect_true(is.na(data$score[[4]]))
  info <- attr(data, "cw_import")
  expect_true(info$types_from_dictionary)
  expect_gt(length(info$parsing_warnings), 0L)
  config$bindings <- list()
  expect_error(env$cw_import(config), "no local binding")
})
