test_that("suggestion preview preserves files and detects stale content", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  p <- project("Preview", "methods", "Describe measurements", new_parent())
  context <- assistant_context("Explain dplyr::filter", path = p$path)
  proposal <- list(schema_version = 1L, policy_id = "cttir-code-1", catalog_id = context$catalog_id,
    language = "en", operation = "suggest", summary = "Draft summary", limitations = list("Synthetic only"),
    citation_ids = list(context$references[[1]]$citation_id), changes = list(list(
      path = "analysis/review-draft.R", original_sha256 = NULL, content = "result <- mean(c(1, 2, 3))", reason = "Review")))
  advice <- list(state = "proposal", context = context, proposal = proposal, requested_language = "en",
    exchange = list(qualification = "not_qualified_for_assistance", provenance = list(model = "test")))
  before <- tree_hashes(p$path)
  preview <- assistant_suggestion_preview(advice, p$path)
  expect_false(preview$conflicts)
  expect_false(preview$applied)
  expect_false(preview$execution_authorized)
  expect_identical(preview$changes[[1]]$action, "create")
  expect_identical(tree_hashes(p$path), before)
  target <- file.path(p$path, proposal$changes[[1]]$path)
  write_bytes("# Existing user content\n", target)
  stale <- assistant_suggestion_preview(advice, p$path)
  expect_true(stale$conflicts)
  expect_identical(stale$changes[[1]]$status, "stale_content")
  advice$proposal$changes[[1]]$original_sha256 <- file_hash(target)
  fresh <- assistant_suggestion_preview(advice, p$path)
  expect_false(fresh$conflicts)
  expect_identical(fresh$changes[[1]]$before, "# Existing user content\n")
  expect_identical(readLines(target), "# Existing user content")
  protected <- advice
  protected$proposal$changes[[1]]$path <- "data/raw/source.R"
  expect_error(assistant_suggestion_preview(protected, p$path), "no longer passes")
  mismatched <- advice
  mismatched$context$catalog_id <- strrep("b", 64)
  expect_error(assistant_suggestion_preview(mismatched, p$path), "no longer passes")
  local({
    saved <- read_project(p$path)
    saved$manifest$files <- append(saved$manifest$files, list(list(path = "analysis/review-draft.R",
      ownership = "managed", baseline_sha256 = file_hash(target))))
    testthat::local_mocked_bindings(read_project = function(...) saved)
    managed <- assistant_suggestion_preview(advice, p$path)
    expect_true(managed$conflicts)
    expect_identical(managed$changes[[1]]$status, "managed_file")
  })
  unlink(target)
  external <- tempfile(); writeLines("outside", external)
  withr::defer(unlink(external))
  if (file.symlink(external, target)) {
    expect_error(assistant_suggestion_preview(advice, p$path), class = "cttir_path_conflict")
    expect_identical(readLines(external), "outside")
  }
})
