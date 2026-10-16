test_that("deterministic advice is unchanged and does not contact a runtime", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  testthat::local_mocked_bindings(assistant_exchange = function(...) stop("must not run"))
  question <- "Explain dplyr::filter"
  result <- assistant_advice(question)
  expect_identical(result$deterministic, ask(question))
  expect_identical(result$reason, "model_not_requested")
  expect_null(result$proposal)
  expect_false(result$execution_authorized)
})

test_that("explanation can cite unapproved documentation without approving code", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  seen <- list()
  testthat::local_mocked_bindings(assistant_exchange = function(context, ...) {
    seen[[length(seen) + 1L]] <<- context
    list(state = "fallback", reason = "model_not_qualified", proposal = NULL)
  })
  explanation <- assistant_advice("Explain readr::read_csv", use_model = TRUE)
  expect_length(seen[[1]]$references, 1L)
  expect_false(seen[[1]]$references[[1]]$callables[[1]]$workflow_approved)
  expect_identical(seen[[1]]$gaps[[1]]$reason, "workflow_approval_missing_read_only_reference")
  expect_false(explanation$execution_authorized)
  suggestion <- assistant_advice("Explain readr::read_csv", use_model = TRUE, kind = "suggestion")
  expect_length(seen[[2]]$references, 0L)
  expect_identical(seen[[2]]$gaps[[1]]$reason, "workflow_approval_missing")
  expect_false(suggestion$execution_authorized)
  assistant_advice("Explain brainwritR::topic_for", use_model = TRUE)
  expect_length(seen[[3]]$references, 1L)
  expect_identical(seen[[3]]$gaps[[1]]$reason, "workflow_approval_missing_read_only_reference")
})

test_that("runtime failures preserve deterministic advice without leaking errors", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  question <- "Explain dplyr::filter"
  testthat::local_mocked_bindings(assistant_exchange = function(...) stop("PRIVATE_ERROR_SENTINEL"))
  result <- assistant_advice(question, use_model = TRUE, language = "de")
  expect_identical(result$state, "deterministic")
  expect_identical(result$deterministic, ask(question))
  expect_identical(result$reason, "runtime_unavailable")
  expect_identical(result$requested_language, "de")
  expect_false(any(grepl("PRIVATE_ERROR_SENTINEL", capture.output(str(result)), fixed = TRUE)))
})

test_that("model refusal is visible and drafts retain deterministic evidence", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  question <- "Explain dplyr::filter"
  reply <- list(state = "fallback", reason = "model_not_qualified", proposal = NULL)
  testthat::local_mocked_bindings(assistant_exchange = function(...) reply)
  result <- assistant_advice(question, use_model = TRUE)
  expect_identical(result$reason, "model_not_qualified")
  expect_null(result$proposal)
  reply <- list(state = "proposal", proposal = list(summary = "A draft"), execution_authorized = FALSE)
  draft <- assistant_advice(question, use_model = TRUE)
  expect_identical(draft$state, "proposal")
  expect_identical(draft$deterministic, ask(question))
  expect_false(draft$execution_authorized)
})

test_that("catalog changes during generation discard the stale draft", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  original <- resolve_catalog()
  changed <- FALSE
  testthat::local_mocked_bindings(
    resolve_catalog = function(...) {x <- original; if (changed) x$content_id <- strrep("a", 64); x},
    assistant_exchange = function(...) {
      changed <<- TRUE
      list(state = "proposal", proposal = list(summary = "Stale draft"))
    })
  result <- assistant_advice("Explain dplyr::filter", use_model = TRUE)
  expect_identical(result$reason, "catalog_changed")
  expect_null(result$proposal)
  expect_null(result$exchange)
  expect_false(result$execution_authorized)
})
