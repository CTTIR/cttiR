suggestion_apply_fixture <- function(parent) {
  p <- project("Apply draft", "methods", "Describe measurements", parent)
  context <- assistant_context("Explain dplyr::filter", path = p$path)
  proposal <- list(schema_version = 1L, policy_id = "cttir-code-1", catalog_id = context$catalog_id,
    language = "en", operation = "suggest", summary = "Draft summary", limitations = list("Synthetic only"),
    citation_ids = list(context$references[[1]]$citation_id), changes = lapply(c("one", "two"), function(name) list(
      path = paste0("analysis/draft-", name, ".R"), original_sha256 = NULL,
      content = "stop('DRAFT_MUST_NOT_EXECUTE')", reason = "Review")))
  advice <- list(state = "proposal", context = context, proposal = proposal, requested_language = "en",
    exchange = list(qualification = "not_qualified_for_assistance", provenance = list(model = "test")))
  list(path = p$path, advice = advice)
}

test_that("explicit reviewed suggestions use transactions without executing code", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- suggestion_apply_fixture(new_parent())
  preview <- assistant_suggestion_preview(f$advice, f$path)
  expect_error(assistant_suggestion_apply(f$advice, f$path, strrep("a", 64)), "changed after review")
  expect_false(file.exists(file.path(f$path, "analysis/draft-one.R")))
  result <- assistant_suggestion_apply(f$advice, f$path, preview$preview_sha256)
  expect_true(result$applied)
  expect_false(result$executed)
  expect_identical(result$qualification, "not_qualified_for_assistance")
  expect_identical(read_document(file.path(result$journal, "journal.json"))$status, "committed")
  receipt <- read_document(file.path(f$path, result$receipt))
  expect_identical(receipt$reviewed_preview_sha256, preview$preview_sha256)
  expect_identical(receipt$proposal_sha256, preview$proposal_sha256)
  expect_identical(receipt$changes[[1]]$new_hash, file_hash(file.path(f$path, "analysis/draft-one.R")))
  expect_identical(receipt$references[[1]]$citation_id, f$advice$context$references[[1]]$citation_id)
  expect_false(receipt$executed)
  expect_identical(readLines(file.path(f$path, "analysis/draft-one.R"), warn = FALSE), "stop('DRAFT_MUST_NOT_EXECUTE')")
  expect_error(assistant_suggestion_apply(f$advice, f$path, preview$preview_sha256), "changed after review")
})

test_that("receipt write failure rolls back every suggested file", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- suggestion_apply_fixture(new_parent())
  preview <- assistant_suggestion_preview(f$advice, f$path)
  real <- replace_file
  testthat::local_mocked_bindings(replace_file = function(from, to) {
    if (grepl("/.cttir/suggestions/", to, fixed = TRUE)) stop("Injected receipt failure")
    real(from, to)
  })
  expect_error(assistant_suggestion_apply(f$advice, f$path, preview$preview_sha256), "receipt failure")
  expect_false(file.exists(file.path(f$path, "analysis/draft-one.R")))
  expect_false(file.exists(file.path(f$path, "analysis/draft-two.R")))
  expect_length(list.files(file.path(f$path, ".cttir/suggestions")), 0L)
})

test_that("failed suggestion transactions restore earlier targets", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- suggestion_apply_fixture(new_parent())
  first <- file.path(f$path, "analysis/draft-one.R")
  write_bytes("# User text\n", first)
  f$advice$proposal$changes[[1]]$original_sha256 <- file_hash(first)
  preview <- assistant_suggestion_preview(f$advice, f$path)
  real <- replace_file
  calls <- 0L
  testthat::local_mocked_bindings(replace_file = function(from, to) {
    calls <<- calls + 1L
    if (calls == 2L) stop("Injected second-file failure")
    real(from, to)
  })
  expect_error(assistant_suggestion_apply(f$advice, f$path, preview$preview_sha256), "Injected")
  expect_identical(readLines(first), "# User text")
  expect_false(file.exists(file.path(f$path, "analysis/draft-two.R")))
  journals <- list.files(file.path(f$path, ".cttir/transactions"), pattern = "journal.json", recursive = TRUE, full.names = TRUE)
  expect_true(any(vapply(journals, function(file) identical(read_document(file)$status, "rolled_back"), logical(1))))
})

test_that("project metadata changes at the write boundary refuse all draft writes", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- suggestion_apply_fixture(new_parent())
  preview <- assistant_suggestion_preview(f$advice, f$path)
  real <- transact_files
  testthat::local_mocked_bindings(transact_files = function(root, files, plan) {
    metadata <- file.path(root, ".cttir/managed-files.json")
    cat("\n", file = metadata, append = TRUE)
    real(root, files, plan)
  })
  expect_error(assistant_suggestion_apply(f$advice, f$path, preview$preview_sha256), "changed after planning")
  expect_false(file.exists(file.path(f$path, "analysis/draft-one.R")))
  expect_false(file.exists(file.path(f$path, "analysis/draft-two.R")))
})
