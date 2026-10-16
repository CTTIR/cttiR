test_that("proposal contracts cannot grant themselves execution or evidence", {
  proposal <- list(schema_version = 1L, policy_id = "cttir-code-1", catalog_id = "catalog-1",
    language = "en", summary = "Review the cited function contract.", citation_ids = list("topic-1"),
    limitations = list("Study inputs have not been supplied."), operation = "explain")
  check <- function(x, kind = "answer") validate_assistant_proposal(x, kind, "catalog-1", "topic-1")
  expect_identical(check(proposal), proposal)
  altered <- proposal
  altered$validation <- "fixture_tested"
  expect_error(check(altered), class = "cttir_schema_error")
  altered <- proposal
  altered$citation_ids <- list("invented")
  expect_error(check(altered), class = "cttir_schema_error")
  altered <- proposal
  altered$catalog_id <- "stale"
  expect_error(check(altered), class = "cttir_schema_error")
  proposal$operation <- "suggest"
  proposal$changes <- list(list(path = "analysis/review.R", original_sha256 = NULL,
    content = "stop('review before running')", reason = "Add an explicit review gate."))
  expect_identical(check(proposal, "suggestion"), proposal)
  for (path in c("data/raw/source.csv", "analysis/../raw.R", "/tmp/code.R", ".Rprofile", "code/setup.R")) {
    altered <- proposal
    altered$changes[[1]]$path <- path
    expect_error(check(altered, "suggestion"), class = "cttir_schema_error")
  }
  proposal$changes <- rep(proposal$changes, 2L)
  expect_error(check(proposal, "suggestion"), class = "cttir_schema_error")
})
