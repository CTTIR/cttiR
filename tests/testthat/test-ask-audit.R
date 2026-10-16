test_that("known design conflicts cannot return a different model's code", {
  questions <- c(
    "Fit logistic regression to repeated binary outcomes per participant",
    "Fit a random-intercept binary model for repeated outcomes per participant",
    "Fit a Cox model for patients clustered in centres")
  for (question in questions) {
    answer <- ask(question)
    expect_identical(answer$code, "", info = question)
    expect_length(answer$approved_capabilities, 0L)
    expect_true(any(startsWith(unlist(answer$gaps), "model_configuration_mismatch:")), info = question)
    expect_match(answer$answer, "no model is substituted", fixed = TRUE)
  }
  positive <- ask("Fit logistic regression to an independent binary outcome")
  expect_contains(unlist(positive$approved_capabilities), "std.model.glm_binomial")
  expect_match(positive$code, "stats::glm", fixed = TRUE)
  mixed <- ask("Fit a random-intercept model to repeated continuous measurements per participant")
  expect_contains(unlist(mixed$approved_capabilities), "std.model.lme")
})

test_that("secondary capability phrases respect direct negation", {
  for (question in c("No plots please", "No CSV", "Ohne CSV", "Keine Graustufen")) {
    answer <- ask(question)
    expect_identical(answer$code, "", info = question)
    expect_length(answer$approved_capabilities, 0L)
  }
  positive <- ask("No plots please; fit a Cox regression")
  expect_false("std.figures.accessible" %in% unlist(positive$approved_capabilities))
  expect_contains(unlist(positive$approved_capabilities), "std.model.coxph")
})

test_that("exact bare callable questions retain successful pinned evidence", {
  bare <- ask("Explain coxph()")
  qualified <- ask("Explain survival::coxph")
  expect_identical(bare$answer, qualified$answer)
  expect_identical(bare$citations, qualified$citations)
  expect_identical(bare$evidence, qualified$evidence)
  expect_contains(vapply(bare$symbols, function(x) x$symbol, ""), "survival::coxph")
})

test_that("partial support is not presented as completing unsupported analysis", {
  mixed <- ask("Make a baseline table and use Seurat clustering")
  expect_match(mixed$answer, "Partially supported", fixed = TRUE)
  expect_contains(unlist(mixed$approved_capabilities), "std.describe.descrtab2")
  expect_true(any(grepl("seurat.single_cell.exploration", unlist(mixed$gaps), fixed = TRUE)))
  unreviewed <- ask("Draw a plot and fit a glmm")
  expect_identical(unreviewed$code, "")
  expect_length(unreviewed$approved_capabilities, 0L)
  expect_match(unreviewed$answer, "no compatible approved adapter", fixed = TRUE)
})
test_that("bare call recognition accepts arguments without treating prose as a call", {
  catalog <- resolve_catalog()
  for (call in c("imaginary_method(x)", "imaginary_method ()", "imaginary_method (x)", "fictional(x)")) {
    named <- ask_function_names(call, catalog)
    expect_length(named$absent, 1L)
    expect_length(named$symbols, 0L)
  }
  for (call in c("coxph(x)", "coxph ()", "coxph (x)")) {
    expect_contains(ask_function_names(call, catalog)$symbols, "survival::coxph")
  }
  expect_length(ask_function_names("Describe measurements (height, weight and age)", catalog)$absent, 0L)
  expect_length(ask_function_names("123", catalog)$absent, 0L)
  expect_length(ask_function_names("Describe measurements. (height and weight)", catalog)$absent, 0L)
  expect_length(ask_function_names("mean(x)", catalog)$absent, 0L)
})

test_that("sentence dots normalize only to a known namespaced export", {
  plain <- ask("Explain survival::coxph")
  expect_identical(ask("Explain survival::coxph.")$evidence, plain$evidence)
  pattern <- "[A-Za-z][A-Za-z0-9.]*:{2,3}[\\p{L}._][\\p{L}\\p{N}._]*"
  catalog <- list(content_id = "synthetic-dotted-exports", packages = list(list(name = "fixture",
    exports = list(list(name = "trailing.", kind = "function"), list(name = "trailing", kind = "function"),
      list(name = "borrowed", kind = "reexport")))))
  expect_identical(ask_namespaced_symbols("fixture::trailing.", catalog, pattern), "fixture::trailing.")
  expect_identical(ask_namespaced_symbols("fixture::missing.", catalog, pattern), "fixture::missing.")
  expect_identical(ask_namespaced_symbols("fixture:::trailing.", catalog, pattern), "fixture:::trailing.")
  expect_identical(ask_namespaced_symbols("fixture::borrowed.", catalog, pattern), "fixture::borrowed")
})
