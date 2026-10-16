test_that("absent bare calls do not fall through to unrelated lexical evidence", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain imaginary_marker_mapping() for source marker names", approved_only = FALSE)
  expect_length(context$references, 0L)
  expect_true(any(vapply(context$gaps, function(x)
    identical(x$symbol, "imaginary_marker_mapping") &&
      identical(x$reason, "export_absent_from_catalog_revision"), logical(1))))
})

test_that("known and absent bare calls retain both evidence and the missing-symbol gap", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain coxph() and imaginary_survival_method()", approved_only = FALSE)
  expect_true(any(vapply(context$references, function(x)
    any(vapply(x$callables, function(f) identical(f$symbol, "survival::coxph"), logical(1))), logical(1))))
  expect_true(any(vapply(context$gaps, function(x)
    identical(x$symbol, "imaginary_survival_method") &&
      identical(x$reason, "export_absent_from_catalog_revision"), logical(1))))
})

test_that("ambiguous bare names disclose their multiple catalog owners", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain filter()", approved_only = FALSE)
  expect_true(any(vapply(context$gaps, function(x)
    identical(x$symbol, "filter") && identical(x$reason, "ambiguous_unqualified_export"), logical(1))))
})

test_that("mixed namespaced and bare API requests retain every evidence boundary", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain survival::coxph and imaginary_survival_method()", approved_only = FALSE)
  expect_true(any(vapply(context$gaps, function(x)
    identical(x$symbol, "imaginary_survival_method") &&
      identical(x$reason, "export_absent_from_catalog_revision"), logical(1))))
  expect_true(any(vapply(context$references, function(x)
    any(vapply(x$callables, function(f) identical(f$symbol, "survival::coxph"), logical(1))), logical(1))))
  mixed <- assistant_context("Explain dplyr::mutate and coxph()", approved_only = FALSE)
  callables <- unlist(lapply(mixed$references, function(x)
    vapply(x$callables, function(f) f$symbol, "")), use.names = FALSE)
  expect_contains(callables, c("dplyr::mutate", "survival::coxph"))
  ambiguous <- assistant_context("Explain survival::coxph and filter()", approved_only = FALSE)
  expect_true(any(vapply(ambiguous$gaps, function(x)
    identical(x$symbol, "filter") && identical(x$reason, "ambiguous_unqualified_export"), logical(1))))
})

test_that("bare calls with arguments and spacing retain absent-export gaps", {
  for (call in c("imaginary_survival_method(x)", "imaginary_survival_method ()",
      "imaginary_survival_method (x)", ".imaginary_survival_method(x)")) {
    context <- assistant_context(paste("Explain survival::coxph and", call), approved_only = FALSE)
    expect_true(any(vapply(context$gaps, function(x)
      identical(x$reason, "export_absent_from_catalog_revision"), logical(1))), info = call)
  }
})

test_that("sentence punctuation does not change exact namespaced evidence", {
  plain <- assistant_context("Explain survival::coxph", approved_only = FALSE)
  for (punctuation in c(".", "...", ",", ";", ":", "!", "?")) {
    value <- assistant_context(paste0("Explain survival::coxph", punctuation), approved_only = FALSE)
    expect_identical(value$references, plain$references)
    expect_identical(value$gaps, plain$gaps)
  }
})
