two_group_fixture <- function() {
  spec <- study_two_group_spec("therapy_demo", "Synthetic therapy comparison",
    "What is the therapy-minus-standard mean difference?", "Synthetic independent participants",
    "id", "arm", list(name = "score", type = "continuous", unit = "points"),
    "standard", "therapy", independent_observations = TRUE,
    planning = list(delta = 5, sd = 10, power = 0.8, alpha = 0.05, dropout = 0.1))
  list(spec = spec, data = data.frame(id = paste0("p", 1:10), arm = rep(c("standard", "therapy"), each = 5),
    score = c(1:5, 3:7)), units = c(score = "points"))
}

test_that("two-group planning reports evaluable and randomized counts separately", {
  plan <- study_two_group_power(delta = 5, sd = 10, power = 0.8, alpha = 0.05, dropout = 0.1)
  expect_equal(plan$evaluable_per_group, 64)
  expect_equal(plan$randomized_per_group, 72)
  expect_equal(plan$total_randomized, 144)
  expect_false(plan$scientific_approval)
  expect_error(study_two_group_power(delta = 0, sd = 10), "positive")
  expect_error(study_two_group_power(delta = 5, sd = 10, dropout = 1), "dropout")
})

test_that("independent-group analysis has the declared contrast and Welch uncertainty", {
  x <- two_group_fixture()
  result <- study_analyze(x$spec, x$data, x$units)
  expect_equal(result$coefficients$estimate, 2)
  expect_equal(result$coefficients$std_error, 1)
  expect_equal(result$df_residual, 8)
  expect_equal(result$coefficients$p_value, 0.080516237957, tolerance = 1e-10)
  expect_equal(result$coefficients$conf_low, -0.306004135204, tolerance = 1e-10)
  expect_equal(result$coefficients$conf_high, 4.306004135204, tolerance = 1e-10)
  expect_equal(study_analyze(x$spec, x$data[10:1, ], x$units)$coefficients, result$coefficients)
})

test_that("two-group contracts reject missing, duplicated or undeclared observations", {
  x <- two_group_fixture()
  bad <- x$data; bad$id[2] <- bad$id[1]
  expect_error(study_validate(x$spec, bad, x$units), "unique")
  bad <- x$data; bad$score[2] <- NA_real_
  expect_error(study_validate(x$spec, bad, x$units), "finite")
  bad <- x$data; bad$arm[2] <- "other"
  expect_error(study_validate(x$spec, bad, x$units), "group")
  bad <- x$spec; bad$independent_observations <- FALSE
  expect_error(study_validate(bad), "independent")
  expect_error(study_validate(x$spec, x$data, c(score = "mmHg")), "unit")
})

test_that("the app retains the supplied question and planning inputs in its two-group workflow", {
  request <- list(recipe = "two_group", directory = file.path(tempdir(), "new_trial"), author = "Study team",
    synthetic_acknowledged = TRUE, independent_pairs = FALSE,
    trial = list(question = "Does the new therapy reduce the continuous score compared with standard care?",
      independent_observations = TRUE, planning = list(delta = 5, sd = 10, power = 0.8, alpha = 0.05, dropout = 0.1)))
  request <- app_study_request(request)
  x <- app_study_fixture(request$recipe, request$trial)
  expect_identical(x$spec$question, request$trial$question)
  expect_equal(nrow(x$data), 128L)
  expect_equal(table(x$data$arm), c(standard = 64L, therapy = 64L), ignore_attr = TRUE)
  expect_identical(x$spec$planning, request$trial$planning)
  bad <- request; bad$trial$independent_observations <- FALSE
  expect_error(app_study_request(bad), "independent")
})

test_that("the two-group study uses a supported strict workflow profile", {
  skip_if_not_installed("reflowR")
  x <- two_group_fixture()
  expect_no_error(reflowR::reflow_profile(x$spec$research$domain, x$spec$research$data_origin,
    x$spec$research$design, profile_version = "strict-cttir-v1", data_access = "synthetic"))
})

test_that("Welch uncertainty accommodates unequal variances and sample sizes", {
  x <- two_group_fixture()
  a <- c(2, 4, 8, 15, 25, 40); b <- c(1, 2, 3, 4)
  data <- data.frame(id = paste0("id", seq_len(10)),
    arm = c(rep("therapy", 6), rep("standard", 4)), score = c(a, b))
  result <- study_analyze(x$spec, data, x$units)
  va <- var(a) / 6; vb <- var(b) / 4
  df <- (va + vb)^2 / (va^2 / 5 + vb^2 / 3)
  expect_equal(result$coefficients$std_error, sqrt(va + vb))
  expect_equal(result$df_residual, df)
  expect_equal(result$coefficients$p_value, 2 * stats::pt(-abs(mean(a) - mean(b)) / sqrt(va + vb), df))
  expect_gt(abs(df - 8), 1)
})

test_that("the workflow example preserves existing external continuation records", {
  source <- test_path("..", "..", "inst", "examples", "therapy-workflow.R")
  if (!file.exists(source)) source <- system.file("examples", "therapy-workflow.R", package = "cttiR")
  script <- readLines(source)
  prefix <- script[seq_len(grep("^planning <-", script)[[1]] - 1L)]
  parent <- tempfile("example-preserve-"); dir.create(parent)
  withr::defer(unlink(parent, recursive = TRUE))
  target <- file.path(parent, "trial")
  record <- paste0(target, "-declaration.rds")
  saveRDS("existing declaration", record)
  env <- new.env(parent = globalenv())
  env$commandArgs <- function(...) c(target, "Study author")
  expect_error(eval(parse(text = prefix), envir = env), "already exists")
  expect_identical(readRDS(record), "existing declaration")
  expect_false(dir.exists(target))
})


test_that("two-group analysis preserves typed missing-data and degenerate-uncertainty gates", {
  x <- two_group_fixture()
  expect_error(study_analyze(x$spec, NULL, x$units), "Supply the study data", class = "cttir_study_error")
  expect_error(study_analyze(x$spec, units = x$units), "Supply the study data", class = "cttir_study_error")
  constant <- x$data
  constant$score <- 10
  expect_error(study_analyze(x$spec, constant, x$units), class = "cttir_study_error")
  by_group <- x$data
  by_group$score <- ifelse(by_group$arm == "therapy", 20, 10)
  expect_error(study_analyze(x$spec, by_group, x$units), class = "cttir_study_error")
})
