association_integrity_fixture <- function() {
  list(spec = study_spec("integrity", "Synthetic example", "Association?",
      "Synthetic independent observations", "id",
      list(name = "y", type = "continuous", unit = "u"),
      list(name = "x", type = "continuous", unit = "u"),
      covariates = list(list(name = "z", type = "continuous", unit = "u"),
        list(name = "group", type = "categorical", levels = c("A", "B"), reference = "A"))),
    data = data.frame(id = sprintf("%03d", 1:12), x = 1:12,
      y = c(1, 3, 2, 5, 4, 8, 6, 9, 11, 9, 12, 14),
      z = c(2, 1, 4, 2, 3, 5, 1, 6, 7, 4, 8, 9), group = rep(c("A", "B"), 6)),
    units = c(y = "u", x = "u", z = "u"))
}

test_that("study paths resolve approved platform aliases without hiding user links", {
  parent <- withr::local_tempdir()
  requested <- file.path(parent, "new_study")
  physical <- file.path(normalizePath(parent, winslash = "/", mustWork = TRUE), "new_study")
  expect_identical(study_directory(requested), physical)
  request <- list(recipe = "paired", directory = requested, author = "Test group",
    synthetic_acknowledged = TRUE, independent_pairs = TRUE)
  expect_identical(app_study_request(request)$directory, physical)
  expect_false(dir.exists(requested))
  if (.Platform$OS.type != "windows") {
    link <- file.path(parent, "linked")
    expect_true(file.symlink(normalizePath(parent), link))
    expect_error(study_directory(file.path(link, "new_study")), "[Ss]ymbolic|symlink")
    expect_error(app_study_request(within(request, directory <- file.path(link, "new_study"))), "[Ss]ymbolic|symlink")
  }
})

test_that("association roles cannot expand into undeclared predictors or responses", {
  x <- association_integrity_fixture()
  for (column in c("x", "y", "z")) {
    for (make in list(function(v) matrix(v, ncol = 1), function(v) I(cbind(v, v^2)),
        function(v) structure(v, class = "measurement"))) {
      bad <- x$data
      bad[[column]] <- make(bad[[column]])
      expect_error(study_analyze(x$spec, bad, x$units), "plain numeric")
    }
  }
  bad <- x$data
  bad$id <- matrix(bad$id, ncol = 1)
  expect_error(study_validate(x$spec, bad, x$units), "plain vectors")
  bad <- x$data
  bad$group <- matrix(bad$group, ncol = 1)
  expect_error(study_validate(x$spec, bad, x$units), "string vectors or factors")
  original <- study_analyze(x$spec, x$data, x$units)
  x$data$group <- factor(x$data$group, levels = c("B", "A"))
  expect_identical(study_analyze(x$spec, x$data, x$units), original)
  expect_identical(original$coefficients$term, c("(Intercept)", "x", "z", "group:B"))
  reference <- stats::lm(y ~ x + z + factor(group, levels = c("A", "B")), x$data)
  expect_equal(original$coefficients$estimate, unname(stats::coef(reference)), tolerance = 1e-12)
})

test_that("observation IDs remain distinct when written as text", {
  x <- association_integrity_fixture()
  x$data$id <- c(1, 1 + .Machine$double.eps, 3:12)
  expect_error(study_validate(x$spec, x$data, x$units), "unique as text")
  x$data$id <- 1:12
  expect_true(study_validate(x$spec, x$data, x$units)$valid)
  x$data$id <- sprintf("%03d", 1:12)
  expect_identical(study_analyze(x$spec, x$data, x$units)$diagnostics$observation_id, x$data$id)
})

test_that("study decimal storage preserves IEEE measurements and literal identifiers", {
  withr::local_seed(107)
  values <- c(1, 1 + .Machine$double.eps, pi, 1e-300, .Machine$double.xmax,
    .Machine$double.xmin / 2, 0, -1e-100, stats::rnorm(1000))
  data <- data.frame(id = rep(c("001", "NA", "a,b", 'a"b', "M\u00fcller"), length.out = length(values)),
    value = values, category = factor(rep(c("A", "B"), length.out = length(values))))
  path <- withr::local_tempfile(fileext = ".csv")
  withr::local_options(list(digits = 3, OutDec = ","))
  study_write_csv(data, path)
  read <- study_read_csv(path, c("character", "numeric", "character"))
  expect_identical(read$value, values)
  expect_identical(read$id, data$id)
  expect_identical(read$category, as.character(data$category))
  json <- withr::local_tempfile(fileext = ".json")
  study_write_json(list(values = values), json)
  expect_identical(jsonlite::read_json(json, simplifyVector = TRUE)$values, values)
  writeLines(c('"id","value"', '"001","1e999"'), path)
  expect_error(study_read_csv(path, c("character", "numeric")), "finite")
  writeLines(c('"id","value"', '"001","1,2"'), path)
  expect_error(study_read_csv(path, c("character", "numeric")), "decimal")
  expect_error(study_read_csv(path, "numeric"), "declared classes")
})

test_that("paired differences and association results survive the input round trip", {
  paired <- list(spec = study_paired_spec("precise", "Synthetic pairs", "Mean change?",
      "Independent synthetic pairs", "id", "visit",
      list(name = "y", type = "continuous", unit = "u"), "pre", "post", independent_pairs = TRUE),
    data = data.frame(id = rep(sprintf("%03d", 1:8), 2),
      visit = rep(c("pre", "post"), each = 8),
      y = c(rep(1, 8), 1 + (1:8) * .Machine$double.eps)), units = c(y = "u"))
  for (x in list(association_integrity_fixture(), paired)) {
    layout <- study_data_layout(x$spec)
    expected <- study_analyze(x$spec, x$data, x$units)
    path <- withr::local_tempfile(fileext = ".csv")
    study_write_csv(x$data[, layout$columns], path)
    actual <- study_analyze(x$spec, study_read_csv(path, layout$classes), x$units)
    expect_identical(actual, expected)
    results <- withr::local_tempfile(fileext = ".csv")
    study_write_csv(actual$coefficients, results)
    recorded <- study_read_csv(results, c("character", rep("numeric", 6)))
    expect_identical(recorded, expected$coefficients)
  }
})
