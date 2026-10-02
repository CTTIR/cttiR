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
