app_adviser_value <- function() {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  value <- advise("Explain dplyr::filter")
  context <- assistant_context("Explain dplyr::filter", approved_only = FALSE)
  value$evidence <- adviser_evidence(context)
  value
}

test_that("adviser fallback explains the reason without showing a plan", {
  skip_if_not_installed("shiny")
  value <- app_adviser_value()
  for (lang in c("en", "de")) {
    html <- as.character(app_adviser_ui(value, lang, shiny::NS("review")))
    expect_match(html, app_t("adviser.reason.acknowledgement_required", lang), fixed = TRUE)
    expect_match(html, app_t("adviser.experimental_note", lang), fixed = TRUE)
    expect_false(grepl(app_t("adviser.method", lang), html, fixed = TRUE))
  }
})

test_that("adviser plans render escaped, labelled and with their sources in both languages", {
  skip_if_not_installed("shiny")
  value <- app_adviser_value()
  id <- value$evidence[[1]]$source_id
  value$state <- "plan"; value$reason <- NULL
  value$proposal <- list(status = "ready", summary = "<script>unsafe()</script>", questions = list("Paired?"),
    limitations = list("Draft only"), source_ids = list(id),
    plan = list(method = "dplyr::filter(data, keep)", estimand = "Mean change", unit = "Patient",
      dependence = "paired", assumptions = list("Pairs are independent"),
      steps = list(list(id = "a", action = "Filter complete pairs")), unresolved = list()))
  value$checks <- list(unknown_apis = "madeup::fit")
  value$provenance <- list(model = "cttir-adviser:v1-q4", prompt_tokens = 3000L, generated_tokens = 400L, seconds = 12)
  for (lang in c("en", "de")) {
    html <- as.character(app_adviser_ui(value, lang, shiny::NS("review")))
    expect_false(grepl("<script>unsafe", html, fixed = TRUE))
    expect_match(html, "&lt;script&gt;", fixed = TRUE)
    expect_match(html, "dplyr::filter(data, keep)", fixed = TRUE)
    expect_match(html, id, fixed = TRUE)
    expect_match(html, "madeup::fit", fixed = TRUE)
    expect_match(html, app_t("adviser.status.ready", lang), fixed = TRUE)
    expect_match(html, app_t("adviser.experimental_note", lang), fixed = TRUE)
  }
})

test_that("Ask sends analysis-plan requests to advise() with the acknowledgement", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  calls <- list()
  worker <- function(operation, args) {
    calls[[length(calls) + 1L]] <<- list(operation = operation, args = args)
    list(is_alive = function() FALSE, kill = function() NULL,
      get_result = function() list(ok = TRUE, value = do.call(advise, args)))
  }
  expect_true("advise" %in% app_operations)
  shiny::testServer(app_ask_server, args = app_test_args(worker), {
    session$setInputs(action = "ask", question = "Explain dplyr::filter", local_assistance = TRUE,
      assistance_kind = "plan", acknowledge_experimental = FALSE, use_project = FALSE, run = 1)
    expect_identical(calls[[1]]$operation, "advise")
    expect_false(calls[[1]]$args$acknowledge_experimental)
    expect_identical(state$kind, "advise")
    expect_match(as.character(output$result$html), app_t("adviser.reason.acknowledgement_required", "en"), fixed = TRUE)
  })
})
