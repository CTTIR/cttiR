adviser_plan_fixture <- function(source_id, method = "stats::t.test(x, mu = 10)") {
  list(status = "ready", summary = "Use a one-sample t procedure.", questions = list(),
    limitations = list("Draft only."), source_ids = list(source_id),
    plan = list(analysis_id = "one-sample-mean",
      input_contract = list(list(name = "x", type = "numeric vector", required = TRUE)),
      steps = list(list(id = "compute", action = paste("Apply", method), input_names = list("x"),
        output_names = list("estimate"))),
      output_contract = list(list(name = "estimate", type = "numeric", required = TRUE)),
      estimand = "Mean minus 10", unit = "Independent unit", dependence = "independent",
      method = method, assumptions = list("Independent observations"), unresolved = list()))
}

adviser_context_fixture <- function(question = "Explain dplyr::filter") {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  assistant_context(question, approved_only = FALSE)
}

test_that("the system prompt is byte-identical to the training contract", {
  expect_identical(content_hash(adviser_system_prompt("en")),
    "acc39352f8d30fce53207083405fa6b8a0316c8e3f5bfadb44aba3653815f673")
  expect_identical(content_hash(adviser_system_prompt("de")),
    "f1ab854f246398adf8fb64c0e45d07b9acd17a0b438305ce891cff30af750bce")
})

test_that("evidence uses api identifiers, stays unique and respects its budget", {
  context <- adviser_context_fixture()
  evidence <- adviser_evidence(context)
  expect_gt(length(evidence), 0L)
  ids <- vapply(evidence, function(x) x$source_id, character(1))
  expect_true(all(grepl("^api:[A-Za-z0-9.]+:", ids)))
  expect_false(anyDuplicated(ids) > 0)
  small <- adviser_evidence(context, max_chars = 1500L)
  expect_lte(nchar(json_text(small)), 1500L + 400L)
  expect_identical(adviser_user_message("Q", list()), "Q\nEvidence: []")
  expect_match(adviser_user_message("Q", evidence), "^Q\nEvidence: \\[\\{")
})

test_that("replies must be exactly one schema-valid plan citing supplied sources", {
  context <- adviser_context_fixture()
  evidence <- adviser_evidence(context)
  id <- evidence[[1]]$source_id
  good <- json_text(adviser_plan_fixture(id))
  expect_true(adviser_validate(good, evidence)$ok)
  expect_identical(adviser_validate(paste("Here is the plan:", good), evidence)$reason, "invalid_json")
  expect_identical(adviser_validate(paste(good, good), evidence)$reason, "invalid_json")
  expect_identical(adviser_validate(substr(good, 1, 80), evidence)$reason, "invalid_json")
  missing <- adviser_plan_fixture(id); missing$status <- NULL
  expect_identical(adviser_validate(json_text(missing), evidence)$reason, "schema_mismatch")
  no_plan_key <- adviser_plan_fixture(id); no_plan_key$plan <- NULL
  expect_identical(adviser_validate(json_text(no_plan_key, digits = NA), evidence)$reason, "schema_mismatch")
  explanation <- sub('"plan":\\{.*\\}\\}$', '"plan":null}', good)
  expect_true(adviser_validate(explanation, evidence)$ok)
  invented <- adviser_plan_fixture("api:made:up")
  expect_identical(adviser_validate(json_text(invented), evidence)$reason, "unsupported_source")
})

test_that("plans naming functions outside the catalog are flagged, not hidden", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Explain dplyr::filter", approved_only = FALSE)
  evidence <- adviser_evidence(context)
  plan <- adviser_plan_fixture(evidence[[1]]$source_id, method = "dplyr::filter(x) then madeup::fit(x)")
  checked <- adviser_validate(json_text(plan), evidence, resolve_catalog())
  expect_true(checked$ok)
  expect_identical(checked$checks$unknown_apis, "madeup::fit")
})

adviser_runtime_fixture <- function(content, alter = identity, env = parent.frame()) {
  log <- new.env(parent = emptyenv()); log$chat <- 0L; log$request <- NULL
  digest <- strrep("d", 64)
  testthat::local_mocked_bindings(
    runtime_owner = function(...) list(model = "cttir-adviser:v1-q4", model_digest = digest),
    local_model = function(...) list(digest = digest),
    runtime_request = function(endpoint, route, body = NULL, ...) {
      log$chat <- log$chat + 1L; log$request <- body
      alter(list(model = "cttir-adviser:v1-q4", done = TRUE, done_reason = "stop",
        prompt_eval_count = 3000L, eval_count = 400L,
        message = list(role = "assistant", content = content(body))))
    }, .env = env)
  log
}

test_that("advise() needs an explicit acknowledgement before contacting the model", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  log <- adviser_runtime_fixture(function(body) "{}")
  blocked <- advise("Explain dplyr::filter", endpoint = "http://127.0.0.1:11434")
  expect_s3_class(blocked, "cttir_advice")
  expect_identical(blocked$reason, "acknowledgement_required")
  expect_identical(blocked$label, "experimental_not_qualified")
  expect_identical(log$chat, 0L)
})

test_that("advise() returns a checked plan with the training request shape", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  reply <- function(body) {
    evidence <- jsonlite::fromJSON(sub(".*\nEvidence: ", "", body$messages[[2]]$content), simplifyVector = FALSE)
    json_text(adviser_plan_fixture(evidence[[1]]$source_id, method = "dplyr::filter(data, keep)"))
  }
  log <- adviser_runtime_fixture(reply)
  advice <- advise("Explain dplyr::filter", language = "de", acknowledge_experimental = TRUE,
    endpoint = "http://127.0.0.1:11434")
  expect_identical(advice$state, "plan")
  expect_false(advice$execution_authorized)
  expect_identical(log$chat, 1L)
  body <- log$request
  expect_length(body$messages, 2L)
  expect_identical(vapply(body$messages, function(m) m$role, ""), c("system", "user"))
  expect_identical(body$messages[[1]]$content, adviser_system_prompt("de"))
  expect_false(body$think); expect_false(body$stream)
  expect_identical(body$options[c("temperature", "top_k", "seed", "num_ctx", "num_predict")],
    list(temperature = 0, top_k = 1L, seed = 42L, num_ctx = 8192L, num_predict = 1536L))
  expect_output(print(advice), "EXPERIMENTAL")
  expect_identical(names(body$format$properties), sort(names(body$format$properties)))
  expect_identical(body$format$properties$plan$type, list("object", "null"))
  advise("Explain dplyr::filter", acknowledge_experimental = TRUE, constrain = FALSE,
    endpoint = "http://127.0.0.1:11434")
  expect_null(log$request$format)
})

test_that("advise() falls back on runtime and reply failures", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  mutations <- list(
    context_overflow = function(x) {x$prompt_eval_count <- 7000L; x},
    incomplete_response = function(x) {x$done_reason <- "length"; x},
    model_identity_mismatch = function(x) {x$model <- "other:model"; x},
    invalid_json = function(x) {x$message$content <- "I think you should use a t-test."; x})
  for (reason in names(mutations)) local({
    adviser_runtime_fixture(function(body) "{}", mutations[[reason]], env = environment())
    advice <- advise("Explain dplyr::filter", acknowledge_experimental = TRUE, endpoint = "http://127.0.0.1:11434")
    expect_identical(advice$state, "fallback")
    expect_identical(advice$reason, reason)
    expect_null(advice$proposal)
    expect_output(print(advice), "No model plan")
  })
  testthat::local_mocked_bindings(runtime_owner = function(...) NULL)
  expect_identical(advise("Explain dplyr::filter", acknowledge_experimental = TRUE,
    endpoint = "http://127.0.0.1:11434")$reason, "runtime_unverified")
  testthat::local_mocked_bindings(runtime_owner = function(...) list(model = "x"), local_model = function(...) NULL)
  expect_identical(advise("Explain dplyr::filter", acknowledge_experimental = TRUE,
    endpoint = "http://127.0.0.1:11434")$reason, "model_missing")
})

test_that("installed help fills catalog entries without bundled text when versions match", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Use stats::t.test for paired measurements", approved_only = FALSE)
  skip_if(!any(vapply(context$gaps, function(g) identical(g$reason, "reference_text_not_stored"), logical(1))),
    "catalog already stores this help text")
  extra <- adviser_installed_evidence(context, resolve_catalog())
  catalog_version <- Filter(function(p) identical(p$name, "stats"), resolve_catalog()$packages)[[1]]$version
  if (identical(as.character(utils::packageVersion("stats")), catalog_version)) {
    expect_identical(vapply(extra, function(x) x$source_id, ""), "api:stats:t.test")
    expect_match(extra[[1]]$excerpt, "paired", fixed = TRUE)
    expect_identical(extra[[1]]$verification, "installed_documentation_matching_catalog_version")
  } else {
    expect_length(extra, 0L)
  }
  fitted <- adviser_fit(c(extra, extra), max_chars = 3000L)
  expect_lte(nchar(json_text(fitted)), 3000L)
})
