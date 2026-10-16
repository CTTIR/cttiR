assistant_runtime_fixture <- function(context, alter = identity, env = parent.frame()) {
  log <- new.env(parent = emptyenv()); log$chat <- 0L; log$tags <- 0L
  digest <- strrep("b", 64)
  proposal <- list(schema_version = 1L, policy_id = "cttir-code-1", catalog_id = context$catalog_id,
    language = "en", operation = "explain", summary = "Review this pinned reference.",
    citation_ids = list(context$references[[1]]$citation_id), limitations = list("Not executed."))
  response <- list(model = "local:test", done = TRUE, done_reason = "stop", prompt_eval_count = 4000L,
    message = list(role = "assistant", content = json_text(proposal)))
  testthat::local_mocked_bindings(
    runtime_owner = function(...) list(model = "local:test", model_digest = digest),
    local_model = function(...) {log$tags <- log$tags + 1L; list(digest = digest)},
    runtime_request = function(...) {log$chat <- log$chat + 1L; alter(response)}, .env = env)
  log
}

test_that("unqualified models require a separate development opt-in", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  log <- assistant_runtime_fixture(context)
  blocked <- assistant_exchange(context, endpoint = "http://127.0.0.1:11434")
  expect_identical(blocked$reason, "model_not_qualified")
  expect_identical(log$chat, 0L); expect_identical(log$tags, 0L)
  reply <- assistant_exchange(context, endpoint = "http://127.0.0.1:11434", allow_unqualified = TRUE)
  expect_identical(reply$state, "proposal")
  expect_identical(reply$qualification, "not_qualified_for_assistance")
  expect_false(reply$execution_authorized)
  expect_identical(log$chat, 1L); expect_identical(log$tags, 2L)
  expect_identical(assistant_exchange(context, endpoint = "https://example.org")$reason, "endpoint_rejected")
})

test_that("transport rejects mismatches, truncation, reasoning and context overflow", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  mutations <- list(
    model_identity_mismatch = function(x) {x$model <- "other:model"; x},
    incomplete_response = function(x) {x$done_reason <- "length"; x},
    unexpected_reasoning = function(x) {x$message$thinking <- "not retained"; x},
    context_overflow = function(x) {x$prompt_eval_count <- 32768L; x},
    invalid_message = function(x) {x$message$tool_calls <- list(list(name = "run")); x})
  for (reason in names(mutations)) local({
    log <- assistant_runtime_fixture(context, mutations[[reason]], env = environment())
    reply <- assistant_exchange(context, endpoint = "http://127.0.0.1:11434", allow_unqualified = TRUE)
    expect_identical(reply$reason, reason); expect_null(reply$proposal)
    expect_identical(reply$attempts, 1L); expect_identical(log$chat, 1L)
    expect_false(grepl("not retained", json_text(reply), fixed = TRUE))
  })
})

test_that("model replacement during generation prevents proposal acceptance", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  log <- assistant_runtime_fixture(context)
  calls <- 0L
  testthat::local_mocked_bindings(local_model = function(...) {
    calls <<- calls + 1L
    list(digest = strrep(if (calls == 1L) "b" else "c", 64))
  })
  reply <- assistant_exchange(context, endpoint = "http://127.0.0.1:11434", allow_unqualified = TRUE)
  expect_identical(reply$reason, "model_digest_changed")
  expect_null(reply$proposal); expect_identical(log$chat, 1L)
})

test_that("an exhausted preflight deadline prevents network requests", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter")
  log <- assistant_runtime_fixture(context)
  testthat::local_mocked_bindings(runtime_owner = function(...) {
    Sys.sleep(1.05)
    list(model = "local:test", model_digest = strrep("b", 64))
  })
  reply <- assistant_exchange(context, endpoint = "http://127.0.0.1:11434", allow_unqualified = TRUE, timeout = 1)
  expect_identical(reply$reason, "deadline_exceeded")
  expect_identical(log$chat, 0L); expect_identical(log$tags, 0L)
})
