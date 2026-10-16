paired_study_fixture <- function(differences = c(-4, -2, -1, 0, 1, 3, 5, 6)) {
  n <- length(differences)
  list(spec = study_paired_spec("paired_example", "Paired example", "What is the mean change?",
    "Synthetic independent pairs", "subject_id", "occasion",
    list(name = "measurement", type = "continuous", unit = "mmHg"),
    "baseline", "followup", independent_pairs = TRUE),
    data = data.frame(subject_id = rep(sprintf("pair_%06d", seq_len(n)), 2L),
      occasion = rep(c("baseline", "followup"), each = n),
      measurement = c(100 + seq_len(n), 100 + seq_len(n) + differences)),
    units = c(measurement = "mmHg"))
}

test_that("paired mean change uses ID matching and explicit direction", {
  x <- paired_study_fixture()
  result <- study_analyze(x$spec, x$data, x$units)
  expect_equal(result$n, 16L)
  expect_equal(result$n_pairs, 8L)
  expect_equal(result$df_residual, 7)
  expect_equal(result$coefficients$estimate, 1)
  expect_equal(result$coefficients$std_error, sqrt(1.5))
  expect_equal(result$coefficients$statistic, 1 / sqrt(1.5))
  expect_equal(result$coefficients$p_value, 0.441113158055499, tolerance = 1e-10)
  expect_equal(result$coefficients$conf_low, -1.896061425, tolerance = 1e-8)
  expect_equal(result$coefficients$conf_high, 3.896061425, tolerance = 1e-8)
  expect_identical(result$coefficients$term, "mean_after_minus_before")
  expect_identical(result$scientific_review, "pending")
  expect_identical(study_analyze(x$spec, x$data[c(16:9, 1:8), ], x$units), result)
  reversed <- x$spec
  reversed$before <- "followup"; reversed$after <- "baseline"
  opposite <- study_analyze(reversed, x$data, x$units)
  expect_equal(opposite$coefficients$estimate, -result$coefficients$estimate)
  expect_equal(opposite$coefficients$p_value, result$coefficients$p_value)
  expect_equal(opposite$coefficients$conf_low, -result$coefficients$conf_high)
  expect_false(study_validate(x$spec, x$data, x$units)$scientific_approval)
})

test_that("paired applicability needs an explicit caller declaration", {
  args <- list("paired", "Example", "Question", "Synthetic pairs", "id", "visit",
    list(name = "value", type = "continuous", unit = "unit"), "pre", "post")
  expect_error(do.call(study_paired_spec, args), "Explicitly declare")
  for (declaration in list(FALSE, NA, 1, "TRUE", c(TRUE, TRUE)))
    expect_error(do.call(study_paired_spec, c(args, list(independent_pairs = declaration))), "Explicitly declare")
  x <- paired_study_fixture()
  for (field in c("independent_pairs", "data_kind", "causal", "missingness", "contrast", "alternative")) {
    bad <- x$spec; bad[[field]] <- "unsupported"
    expect_error(study_validate(bad, x$data, x$units), "Only synthetic")
  }
  bad <- x$spec; bad$null_mean_change <- 1
  expect_error(study_validate(bad), "Only synthetic")
  bad <- x$spec; bad$before <- bad$after
  expect_error(study_validate(bad), "must differ")
  bad <- x$spec; bad$occasion <- bad$pair_id
  expect_error(study_validate(bad), "distinct")
})

test_that("paired observations reject missingness, duplicates and undeclared visits", {
  x <- paired_study_fixture(seq_len(10))
  bad <- x$data; bad$subject_id[2] <- bad$subject_id[1]
  expect_error(study_validate(x$spec, bad, x$units), "exactly one")
  expect_error(study_validate(x$spec, x$data[-1, ], x$units), "exactly one")
  bad <- x$data; bad$occasion[1] <- "third_visit"
  expect_error(study_validate(x$spec, bad, x$units), "declared occasion")
  bad <- x$data; bad$subject_id[1] <- "different_person"
  expect_error(study_validate(x$spec, bad, x$units), "exactly one")
  bad <- x$data; bad$subject_id[1] <- NA_character_
  expect_error(study_validate(x$spec, bad, x$units), "complete character")
  bad <- x$data; bad$cluster <- "one_cluster"
  expect_error(study_validate(x$spec, bad, x$units), "exactly the three")
  for (value in c(NA_real_, NaN, Inf, -Inf)) {
    bad <- x$data; bad$measurement[1] <- value
    expect_error(study_validate(x$spec, bad, x$units), "finite plain numeric")
  }
  bad <- x$data; bad$measurement <- matrix(bad$measurement, ncol = 1L)
  expect_error(study_validate(x$spec, bad, x$units), "finite plain numeric")
  expect_error(study_validate(x$spec, x$data, c(measurement = "kPa")), "unit differs")
  expect_error(study_validate(x$spec, x$data, "mmHg"), "exactly")
})

test_that("bounded pair count and numerical uncertainty are enforced", {
  x <- paired_study_fixture(seq_len(7))
  expect_error(study_validate(x$spec, x$data, x$units), "8 to 100000")
  x <- paired_study_fixture(rep(c(-1, 1), 50000))
  expect_equal(study_validate(x$spec, x$data, x$units)$n_pairs, 100000L)
  x <- paired_study_fixture(seq_len(100001))
  expect_error(study_validate(x$spec, x$data, x$units), "8 to 100000")
  for (differences in list(rep(0, 8), rep(2, 8))) {
    x <- paired_study_fixture(differences)
    expect_error(study_analyze(x$spec, x$data, x$units), "uncertainty")
  }
  x <- paired_study_fixture()
  x$data$measurement[1] <- -.Machine$double.xmax
  x$data$measurement[9] <- .Machine$double.xmax
  expect_error(study_analyze(x$spec, x$data, x$units), "overflow")
})

test_that("the existing association contract and layout remain distinct", {
  spec <- study_spec("association", "Example", "Association?", "Synthetic observations",
    "id", list(name = "outcome", type = "continuous", unit = "u"),
    list(name = "exposure", type = "continuous", unit = "v"))
  expect_identical(study_data_layout(spec)$columns, c("id", "outcome", "exposure"))
  expect_identical(study_data_layout(spec)$classes, c("character", "numeric", "numeric"))
  expect_identical(study_validate(spec)$recipe_id, "clinical.independent_continuous_association.v1")
  bad <- spec; bad$recipe_id <- "unregistered.recipe"
  expect_error(study_validate(bad), "Unsupported")
})
