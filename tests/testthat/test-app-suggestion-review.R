test_that("Ask suggestions require explicit preview and apply with real temporary files", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  p <- project("UI review", "methods", "Describe measurements", new_parent())
  question <- "Explain dplyr::filter"
  advice <- assistant_advice(question, path = p$path)
  advice$state <- "proposal"
  advice$context <- assistant_context(question, path = p$path)
  advice$proposal <- list(schema_version = 1L, policy_id = "cttir-code-1", catalog_id = advice$context$catalog_id,
    language = "en", operation = "suggest", summary = "Review this draft", limitations = list("Synthetic only"),
    citation_ids = list(advice$context$references[[1]]$citation_id), changes = list(list(
      path = "analysis/ui-draft.R", original_sha256 = NULL, content = "stop('NOT_EXECUTED')", reason = "Review")))
  advice$exchange <- list(qualification = "not_qualified_for_assistance", validation_level = "static_complete")
  testthat::local_mocked_bindings(assistant_advice = function(...) advice)
  target <- file.path(p$path, "analysis/ui-draft.R")
  shiny::testServer(app_ask_server, args = app_test_args(project = shiny::reactive(p$path)), {
    session$setInputs(action = "ask", question = question, local_assistance = TRUE,
      assistance_kind = "suggestion", use_project = TRUE, run = 1)
    expect_identical(state$kind, "assistant_advice")
    expect_false(file.exists(target))
    session$setInputs(apply_suggestion = 1)
    expect_false(file.exists(target))
    session$setInputs(preview_suggestion = 1)
    expect_false(state$preview$conflicts)
    expect_false(file.exists(target))
    expect_match(as.character(output$result$html), "Apply reviewed file changes", fixed = TRUE)
    session$setInputs(apply_suggestion = 2)
    expect_true(state$applied$applied)
    expect_false(state$applied$executed)
    expect_identical(readLines(target, warn = FALSE), "stop('NOT_EXECUTED')")
    expect_false(grepl(paste0('id="', session$ns("apply_suggestion"), '"'),
      as.character(output$result$html), fixed = TRUE))
  })
})

test_that("conflicts render without an apply control and escape before/after text", {
  skip_if_not_installed("shiny")
  preview <- list(conflicts = TRUE, changes = list(list(path = "analysis/test.R", role = "analysis",
    status = "stale_content", before = "<script>bad()</script>", after = "<img src=x>")))
  for (lang in c("en", "de")) {
    html <- as.character(app_suggestion_review_ui(preview, NULL, lang, shiny::NS("review")))
    expect_false(grepl('id="review-apply_suggestion"', html, fixed = TRUE))
    expect_false(grepl("<script>bad", html, fixed = TRUE))
    expect_false(grepl("<img src=x>", html, fixed = TRUE))
    expect_match(html, app_t("assistance.conflicts", lang), fixed = TRUE)
  }
})
