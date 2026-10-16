test_that("fallback explains runtime setup without claiming a draft exists", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  value <- assistant_advice("Explain dplyr::filter")
  value$reason <- "runtime_unverified"
  for (lang in c("en", "de")) {
    html <- as.character(app_assistance_ui(value, lang, shiny::NS("review")))
    expect_match(html, app_t("assistance.reason.runtime_unverified", lang), fixed = TRUE)
    expect_false(grepl("runtime_unverified", html, fixed = TRUE))
    expect_false(grepl(app_t("assistance.draft_note", lang), html, fixed = TRUE))
  }
})

test_that("assistance rendering escapes model text and retains evidence in both languages", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  value <- assistant_advice("Explain dplyr::filter")
  value$state <- "proposal"
  value$context <- assistant_context("Explain dplyr::filter")
  value$proposal <- list(summary = "<script>unsafe()</script>", limitations = list("Review required"),
    citation_ids = list(value$context$references[[1]]$citation_id),
    changes = list(list(path = "analysis/draft.R", reason = "<img src=x onerror=unsafe()>",
      content = "stop('DRAFT_NOT_EXECUTED')")))
  value$exchange <- list(qualification = "not_qualified_for_assistance", validation_level = "static_complete")
  for (lang in c("en", "de")) {
    html <- as.character(app_assistance_ui(value, lang, shiny::NS("review")))
    expect_false(grepl("<script>unsafe", html, fixed = TRUE))
    expect_false(grepl("<img src=x", html, fixed = TRUE))
    expect_match(html, "&lt;script&gt;", fixed = TRUE)
    expect_match(html, "DRAFT_NOT_EXECUTED", fixed = TRUE)
    expect_match(html, value$proposal$citation_ids[[1]], fixed = TRUE)
    expect_match(html, app_t("assistance.draft_note", lang), fixed = TRUE)
    expect_match(html, "not_qualified_for_assistance", fixed = TRUE)
  }
})

test_that("assistance cancellation kills the worker and changed input discards late results", {
  skip_if_not_installed("shiny")
  pending <- app_pending_worker()
  shiny::testServer(app_ask_server, args = app_test_args(pending$worker), {
    session$setInputs(action = "ask", question = "Explain dplyr::filter", local_assistance = TRUE,
      use_project = FALSE, run = 1)
    expect_false(slot$state$job$mutating)
    session$setInputs(cancel = 1)
    expect_identical(pending$control$killed, 1L)
    expect_null(state$result)
    session$setInputs(run = 2)
    completed <- slot$state$job$done
    session$setInputs(question = "A different question")
    completed(list(ok = TRUE, value = list(proposal = list(summary = "STALE_DRAFT"))))
    expect_null(state$result)
    expect_identical(state$status$key, "assistance.stale")
    session$setInputs(cancel = 2)
  })
})

test_that("Ask requests qualified assistance in a cancellable worker with fallback", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  calls <- list()
  worker <- function(operation, args) {
    calls[[length(calls) + 1L]] <<- list(operation = operation, args = args)
    list(is_alive = function() FALSE, kill = function() NULL,
      get_result = function() list(ok = TRUE, value = assistant_advice(args$question)))
  }
  shiny::testServer(app_ask_server, args = app_test_args(worker), {
    session$setInputs(action = "ask", question = "Explain dplyr::filter", local_assistance = TRUE,
      verified_only = FALSE, use_project = FALSE, run = 1)
    expect_identical(state$kind, "assistant_advice")
    expect_identical(calls[[1]]$operation, "assistant_advice")
    expect_true(calls[[1]]$args$use_model)
    expect_null(calls[[1]]$args$allow_unqualified)
    expect_match(as.character(output$result$html), "Catalog-based guidance", fixed = TRUE)
    expect_s3_class(state$result$deterministic, "cttir_answer")
  })
})
