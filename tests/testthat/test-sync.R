test_that("sync previews, applies and preserves user content and pins", {
  parent <- new_parent()
  p <- project("Sync", "methods", "Original goal", parent)
  manuscript <- file.path(p$path, "publications/pub01_main/manuscript/README.md")
  writeLines("Reviewed manuscript", manuscript)
  before <- tree_hashes(p$path)
  preview <- sync(p$path, options = list(project = list(goal = "Updated goal")))
  expect_identical(tree_hashes(p$path), before)
  expect_equal(preview$state, "planned")
  expect_length(preview$conflicts, 0)
  result <- sync(p$path, options = list(project = list(goal = "Updated goal")), dry_run = FALSE)
  expect_equal(result$state, "applied")
  accepted <- validate_spec(file.path(p$path, "cttir-project.yml"))
  expect_equal(accepted$project$goal, "Updated goal")
  expect_equal(accepted$project$id, p$spec$project$id)
  expect_equal(readLines(manuscript), "Reviewed manuscript")
  expect_equal(read_project(p$path)$lock$resource_snapshot,
               jsonlite::fromJSON(system.file("extdata/resource-manifest.json", package = "cttiR"))$content_id)
  after <- tree_hashes(p$path)
  expect_length(sync(p$path, dry_run = FALSE)$changed_files, 0)
  expect_identical(tree_hashes(p$path), after)
})

test_that("sync conflicts and path attacks cause no writes", {
  parent <- new_parent()
  p <- project("Conflict", "methods", "Goal", parent)
  writeLines("aim: reviewed", file.path(p$path, "config/analysis.yml"))
  before <- tree_hashes(p$path)
  s <- sync(p$path, options = list(analysis = list(aim = "descriptive")), dry_run = FALSE)
  expect_equal(s$state, "conflict")
  expect_contains(s$conflicts, "config/analysis.yml")
  expect_identical(tree_hashes(p$path), before)
  expect_error(sync(p$path, options = list(project = list(slug = "elsewhere"))), class = "cttir_input_error")
  expect_error(sync(p$path, options = list(publications = list())), class = "cttir_path_conflict")
  expect_error(sync(p$path, options = list(publications = list(list(id = "pub01", slug = "renamed")))), class = "cttir_path_conflict")
  if (.Platform$OS.type != "windows") {
    unlink(file.path(p$path, "config/analysis.yml"))
    external <- file.path(parent, "external")
    writeLines("Keep external", external)
    expect_true(file.link(external, file.path(p$path, "config/analysis.yml")))
    expect_error(sync(p$path, dry_run = FALSE), class = "cttir_path_conflict")
    expect_equal(readLines(external), "Keep external")
  }
})

test_that("a failed replacement rolls back completed writes and retains evidence", {
  parent <- new_parent()
  p <- project("Rollback", "methods", "Goal", parent)
  before <- tree_hashes(p$path)
  calls <- 0L
  real <- replace_file
  testthat::local_mocked_bindings(replace_file = function(from, to) {
    calls <<- calls + 1L
    if (calls == 2L) stop("injected I/O failure")
    real(from, to)
  })
  expect_error(sync(p$path, options = list(analysis = list(aim = "descriptive")), dry_run = FALSE), "injected")
  after <- tree_hashes(p$path)
  expect_identical(after[names(before)], before)
  journals <- list.files(file.path(p$path, ".cttir/transactions"), "journal.json", recursive = TRUE, full.names = TRUE)
  expect_length(journals, 1)
  expect_equal(read_document(journals[[1]])$status, "rolled_back")
  expect_false(dir.exists(file.path(p$path, ".cttir/write-lock")))
})
