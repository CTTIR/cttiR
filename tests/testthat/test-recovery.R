interrupted_fixture <- function(root, pid) {
  relative <- "code/validate_project.R"
  dest <- file.path(root, relative)
  before <- file_hash(dest)
  dir <- file.path(root, ".cttir/transactions/interrupted-test")
  dir.create(file.path(dir, "backup"), recursive = TRUE)
  file.copy(dest, file.path(dir, "backup/1"))
  write_bytes("# Interrupted replacement\n", dest)
  actions <- data.frame(path = relative, action = "update", old_hash = before, new_hash = file_hash(dest))
  write_bytes(json_text(list(schema_version = 1L, status = "applying", actions = actions)), file.path(dir, "journal.json"))
  write_bytes(json_text(list(pid = pid, host = Sys.info()[["nodename"]])), file.path(root, ".cttir/write-lock/owner.json"))
  before
}

test_that("interrupted transactions restore only matching preimages", {
  skip_if_not_installed("callr")
  parent <- new_parent()
  p <- project("Interrupted", "methods", "Goal", parent)
  dead_pid <- callr::r(function() Sys.getpid())
  baseline <- interrupted_fixture(p$path, dead_pid)
  before <- tree_hashes(p$path)
  expect_equal(audit(p$path, scope = "project")$overall_status, "fail")
  expect_identical(tree_hashes(p$path), before)
  result <- audit(p$path, scope = "project", repair = TRUE)
  expect_equal(result$repairs[[1]]$id, "recover_interrupted_transaction")
  expect_equal(file_hash(file.path(p$path, "code/validate_project.R")), baseline)
  expect_false(dir.exists(file.path(p$path, ".cttir/write-lock")))
  expect_length(pending_transactions(p$path), 0L)
})

test_that("recovery refuses active writers and post-interruption edits", {
  skip_if_not_installed("callr")
  parent <- new_parent()
  p <- project("Active", "methods", "Goal", parent)
  interrupted_fixture(p$path, Sys.getpid())
  expect_error(recover_transactions(p$path), "still running", class = "cttir_transaction_conflict")
  owner <- file.path(p$path, ".cttir/write-lock/owner.json")
  write_bytes(json_text(list(pid = callr::r(function() Sys.getpid()), host = Sys.info()[["nodename"]])), owner)
  target <- file.path(p$path, "code/validate_project.R")
  writeLines("# User edits after interruption", target)
  before <- tree_hashes(p$path)
  expect_error(recover_transactions(p$path), "changed after interruption", class = "cttir_transaction_conflict")
  expect_identical(tree_hashes(p$path), before)
})
