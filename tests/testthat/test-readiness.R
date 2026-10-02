test_that("readiness levels follow local evidence and stale receipts do not count", {
  p <- project("Readiness", "primary_research", "Describe outcomes", new_parent())
  r <- project_readiness(p$path)
  expect_equal(r$checks[[1]]$level, "scaffold_ready")
  expect_false(r$checks[[3]]$passed)
  receipt <- list(status = "passed", synthetic = TRUE, code_sha256 = list(
    "code/R/cttir_workflow.R" = file_hash(file.path(p$path, "code/R/cttir_workflow.R")),
    "code/R/cttir_figures.R" = file_hash(file.path(p$path, "code/R/cttir_figures.R"))))
  dir.create(file.path(p$path, "demo"), showWarnings = FALSE)
  write_bytes(json_text(receipt), file.path(p$path, "demo/receipt.json"))
  r <- project_readiness(p$path)
  expect_true(r$checks[[3]]$passed)
  receipt$code_sha256[["code/R/cttir_workflow.R"]] <- strrep("0", 64)
  write_bytes(json_text(receipt), file.path(p$path, "demo/receipt.json"))
  expect_false(project_readiness(p$path)$checks[[3]]$passed)
  expect_false(project_readiness(p$path)$checks[[5]]$passed)
  legacy <- resolve_spec("Legacy", "methods", "Goal", NULL, list())
  expect_true(readiness_levels[[1]] == "scaffold_ready")
})

test_that("environment readiness is derived consistently and never claimed by the lock", {
  p <- project("Derived env", "primary_research", "Describe outcomes", new_parent())
  lock <- read_project(p$path)$lock
  expect_false("environment_status" %in% names(lock))
  # Matching packages in a user library are not an environment_ready project.
  local_mocked_bindings(environment_status = function(dependencies, root = NULL, mode = "none") {
    list(state = "installed_versions_match", missing = list(), mismatched = list())
  })
  check <- project_readiness(p$path)$checks[[2]]
  expect_false(check$passed)
  expect_match(check$reason, "installed_versions_match")
  expect_match(check$reason, "workflow.environment = 'renv'")
  seen <- NULL
  local_mocked_bindings(environment_status = function(dependencies, root = NULL, mode = "none") {
    seen <<- list(root = root, mode = mode)
    list(state = "environment_ready", mode = "renv")
  })
  ready <- project_readiness(p$path)
  expect_true(ready$checks[[2]]$passed)
  expect_equal(ready$environment$state, "environment_ready")
  expect_equal(seen$root, p$path)
  expect_equal(seen$mode, "none")
})
