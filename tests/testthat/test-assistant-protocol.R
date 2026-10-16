assistant_fixture_proposal <- function(context) {
  list(schema_version = 1L, policy_id = "cttir-code-1", catalog_id = context$catalog_id,
    language = "en", operation = "explain", summary = "Review the supplied function reference.",
    citation_ids = list(context$references[[1]]$citation_id), limitations = list("No code was executed."))
}

test_that("requests bind schema, evidence pins, language and resource bounds", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  request <- assistant_request(context, "local:test", language = "de")
  expect_identical(request$format$properties$language$const, "de")
  expect_identical(request$format$properties$catalog_id$const, context$catalog_id)
  payload <- jsonlite::fromJSON(request$messages[[2]]$content, simplifyVector = FALSE)
  expect_identical(payload$response_schema, request$format)
  expect_identical(payload$prompt_version, "assistant-3")
  expect_false(request$stream); expect_false(request$think)
  expect_true(attr(request, "input_bytes") > context$reference_bytes)
  expect_error(assistant_request(context, "local:test", num_ctx = 2048L), class = "cttir_runtime_unavailable")
  empty <- context; empty$references <- list()
  expect_error(assistant_request(empty, "local:test"), class = "cttir_source_unavailable")
})

test_that("response validation rejects formatting, invented evidence and self-assigned claims", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  p <- assistant_fixture_proposal(context)
  check <- function(p) assistant_validate_response(json_text(p), context)
  expect_true(check(p)$ok); expect_false(check(p)$execution_authorized)
  expect_identical(check(p)$validation_level, "schema_valid")
  expect_false(assistant_validate_response(paste0("```json\n", json_text(p), "\n```"), context)$ok)
  expect_false(assistant_validate_response('{"summary":"a","summary":"b"}', context)$ok)
  expect_false(assistant_validate_response(strrep("x", 65537), context)$ok)
  wrong <- p; wrong$citation_ids <- list("invented"); expect_false(check(wrong)$ok)
  wrong <- p; wrong$language <- "de"; expect_false(check(wrong)$ok)
  wrong <- p; wrong$validation_level <- "fixture_tested"; expect_false(check(wrong)$ok)
})

test_that("suggestions require static checks and never execute their code", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  p <- assistant_fixture_proposal(context); p$operation <- "suggest"
  p$changes <- list(list(path = "analysis/review.R", original_sha256 = NULL,
    content = "result <- mean(c(1, 2))", reason = "Summarize the synthetic values."))
  check <- function(p) assistant_validate_response(json_text(p), context, kind = "suggestion")
  expect_true(check(p)$ok); expect_identical(check(p)$validation_level, "static_complete")
  expect_false(check(p)$execution_authorized)
  wrong <- p; wrong$changes[[1]]$content <- "mean()"
  expect_identical(check(wrong)$reason, "code_validation_failed")
  wrong$changes[[1]]$content <- "system('false')"
  expect_identical(check(wrong)$reason, "code_validation_failed")
  wrong <- p; wrong$changes[[1]]$path <- "data/raw/source.R"
  expect_false(check(wrong)$ok)
  wrong <- p; wrong$changes <- list(); expect_identical(check(wrong)$reason, "empty_suggestion")
})

test_that("document suggestions preserve prose and statically check every supported R entry point", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  proposal <- assistant_fixture_proposal(context); proposal$operation <- "suggest"
  proposal$changes <- list(list(path = "publications/report/manuscript/methods.md", original_sha256 = NULL,
    content = "# Methods\n\nThe measurements are summarized without inference.\n", reason = "Document methods."))
  check <- function(text, path = proposal$changes[[1]]$path) {
    proposal$changes[[1]]$content <- text; proposal$changes[[1]]$path <- path
    assistant_validate_response(json_text(proposal), context, kind = "suggestion")
  }
  prose <- check(proposal$changes[[1]]$content)
  expect_true(prose$ok); expect_false(prose$execution_authorized)
  expect_identical(prose$proposal$changes[[1]]$content, proposal$changes[[1]]$content)
  safe <- paste("---", "title: Synthetic analysis", "output: html_document", "---", "# Analysis", "",
    "Mean: `r mean(c(1, 2, 3))`.", "", "```{r summary, echo=FALSE, fig.width=6}",
    "result <- mean(c(1, 2, 3))", "```", sep = "\n")
  expect_true(check(safe, "analysis/review.Rmd")$ok)
  expect_true(check("Text only.\n", "analysis/review.Rmd")$ok)
  expect_true(check("```r\nresult <- mean(c(1, 2))\n```\n")$ok)
  for (bad in c("`r system('false')`", "```{r}\nsystem('false')\n```",
      "```{r, echo=system('false')}\n1\n```", "```r\nsystem('false')\n```",
      "```text\n`r system('false')`\n```")) {
    expect_identical(check(bad, "analysis/review.Rmd")$reason, "code_validation_failed")
  }
})

test_that("document suggestions reject unreviewed execution and ambiguous document syntax", {
  catalog <- resolve_catalog()
  reject <- function(text) {
    checked <- assistant_validate_change(list(path = "analysis/review.Rmd", content = text), catalog)
    expect_false(isTRUE(attr(checked, "static_complete")), info = text)
  }
  for (text in c(
      "```{python}\nprint('unreviewed')\n```", "```{bash}\necho unreviewed\n```",
      "```{r, engine='bash'}\nprint('unreviewed')\n```", "```{r, child='external.Rmd'}\n1\n```",
      "```{r, ref.label='external'}\n1\n```", "```{r}\n#| engine: python\n1\n```",
      "---\nknit: custom_knit\n---\nText", "\n---\nknit: custom_knit\n---\nText",
      "\ufeff---\nknit: custom_knit\n---\nText", "---\ntitle: &alias draft\nsubtitle: *alias\n---\nText",
      "---\ntitle: !expr system('false')\n---\nText",
      "---\noutput:\n  custom::format: default\n---\nText", "---\ntitle: draft",
      "```{r}\n1", "```{r, echo=}\n1\n```", "`r mean(1)",
      "`{python} system('false')`", "`r# comment`", "<script>execute()</script>",
      "````text\n```{r}\nsystem('false')\n```\n````")) reject(text)
  expect_true(isTRUE(attr(assistant_validate_change(list(path = "analysis/review.md",
    content = "A comparison of values < 5 with values > 5."), catalog), "static_complete")))
})


test_that("document drafts refuse literal TeX includes and external figure-device lookup", {
  catalog <- resolve_catalog()
  for (text in c("\\input{sentinel-do-not-open.tex}", "\\includegraphics{sentinel-do-not-open.pdf}",
      "---\ntitle: '\\input{sentinel-do-not-open.tex}'\n---\nText",
      '---\ntitle: "\\u005cinput{sentinel-do-not-open.tex}"\n---\nText',
      '---\ntitle: "\\u003cscript>sentinel()\\u003c/script>"\n---\nText',
      "```{r, dev='external_device'}\n1\n```", "```{r, dev='tikz'}\n1\n```")) {
    checked <- assistant_validate_change(list(path = "analysis/review.Rmd", content = text), catalog)
    expect_false(isTRUE(attr(checked, "static_complete")), info = text)
    expect_true(any(checked$reason %in% c("unsupported_document_tex", "unsupported_document_markup", "unsupported_document_options")))
  }
})
