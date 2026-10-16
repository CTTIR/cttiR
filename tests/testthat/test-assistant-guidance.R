test_that("project guidance exposes literal docs without certifying project readiness", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  for (question in c("How do I migrate a cttiR project to workflowr?", "Wie migriere ich ein cttiR-Projekt zu workflowr?")) {
    context <- assistant_context(question, approved_only = FALSE)
    core <- Filter(function(ref) identical(ref$content_authority,
      "installed_package_documentation_not_project_readiness"), context$references)
    expect_length(core, 2L)
    expect_setequal(vapply(core, function(ref) ref$documented_symbol, ""), c("cttiR::project", "cttiR::sync"))
    expect_true(all(vapply(core, function(ref) identical(ref$reference_sha256, content_hash(ref$reference)), logical(1))))
    expect_true(all(vapply(core, function(ref) length(ref$callables) == 0L, logical(1))))
    expect_false(context$execution_authorized)
    expect_true(any(vapply(core, function(ref) grepl('dry_run = TRUE', ref$reference, fixed = TRUE), logical(1))))
  }
  expect_length(assistant_project_references("Explain dplyr::filter", "catalog"), 0L)
  limited <- assistant_context("Explain cttiR migration", max_reference_bytes = 10L)
  expect_length(limited$references, 0L)
  expect_true(length(limited$gaps) >= 2L)
})

test_that("bundled guidance matches shipped help sources when available", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  doc <- read_document(resource_file("extdata", "assistant-guidance.json"))
  for (entry in c(doc$documents, doc$policy_documents)) {
    source <- file.path(root, entry$source_path)
    if (file.exists(source)) expect_identical(file_hash(source), entry$source_sha256)
  }
  corrupt <- doc
  corrupt$documents[[1]]$reference <- "Tampered instructions"
  path <- tempfile(fileext = ".json")
  withr::defer(unlink(path))
  jsonlite::write_json(corrupt, path, auto_unbox = TRUE)
  testthat::local_mocked_bindings(resource_file = function(...) path)
  expect_error(assistant_project_references("Explain cttiR", "catalog"), class = "cttir_catalog_corrupt")
})

test_that("project planning guidance fits requests without unrelated lexical API expansion", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("Plan a new reflowR project with workflowr reporting and describe project status", approved_only = FALSE)
  expect_false(any(vapply(context$gaps, function(gap)
    identical(gap$reason, "lexical_reference_candidates_require_review"), logical(1))))
  request <- assistant_request(context, "local:test")
  expect_true(attr(request, "input_bytes") + 2048L <= request$options$num_ctx)
  explicit <- assistant_context("Explain dplyr::filter in a workflowr project", approved_only = FALSE)
  callables <- unlist(lapply(explicit$references, function(ref)
    vapply(ref$callables, function(callable) callable$symbol, "")))
  expect_true("dplyr::filter" %in% callables)
})

test_that("explicit external API questions do not inherit project preview semantics", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context('Is reflowR::reflow_preview("clinical") a headless migration check?', approved_only = FALSE)
  expect_false(any(vapply(context$references, function(ref)
    identical(ref$content_authority, "installed_package_documentation_not_project_readiness"), logical(1))))
  symbols <- unlist(lapply(context$references, function(ref)
    vapply(ref$callables, function(callable) callable$symbol, "")))
  expect_true("reflowR::reflow_preview" %in% symbols)
  expect_length(assistant_project_references("How does cttiR::sync preview a workflowr migration?", context$catalog_id), 2L)
})
