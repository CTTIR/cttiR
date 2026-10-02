test_that("remote update verifies blobs and publishes a complete pinned revision", {
  f <- remote_fixture()
  local_repository_mock()
  local_mocked_bindings(github_json = f$json, github_download = f$download)
  withr::local_options(cttiR.sources = list(f$record))
  expect_equal(update(dry_run = TRUE)$status, "planned")
  expect_error(update(sources = "remote", dry_run = TRUE), class = "cttir_source_unavailable")
  plan <- update(mode = "remote", dry_run = TRUE)
  expect_false(file.exists(f$f$store))
  result <- update(mode = "remote")
  expect_equal(result$new_id, plan$new_id)
  hit <- search("cttirFixtureA::keep")
  expect_equal(hit$revision, f$sha)
  expect_match(hit$evidence, f$sha, fixed = TRUE)
  expect_equal(update(mode = "remote")$status, "unchanged")
  expect_equal(packages()$freshness[packages()$package == "cttirFixtureA"], "public_commit_fetched")
})

test_that("incomplete remote trees and mismatched blobs do not activate", {
  f <- remote_fixture()
  local_repository_mock()
  withr::local_options(cttiR.sources = list(f$record))
  local_mocked_bindings(github_json = f$json, github_download = f$download)
  update(mode = "remote")
  pointer <- read_document(file.path(f$f$store, "active.json"))
  local_mocked_bindings(github_download = function(url, path, ...) writeLines("wrong", path))
  expect_error(update(mode = "remote"), class = "cttir_source_unavailable")
  expect_equal(read_document(file.path(f$f$store, "active.json")), pointer)
  local_mocked_bindings(github_json = function(url, ...) {
    if (grepl("/commits/", url, fixed = TRUE)) return(list(sha = f$sha))
    list(truncated = TRUE, tree = f$tree)
  })
  expect_error(update(mode = "remote"), class = "cttir_source_unavailable")
  expect_equal(read_document(file.path(f$f$store, "active.json")), pointer)
})

test_that("remote registry and transport reject untrusted destinations", {
  expect_error(github_source(list(github = "https://evil.invalid/repo")), class = "cttir_input_error")
  expect_error(github_source(list(github = "CTTIR/reflowR", ref = "../main")), class = "cttir_input_error")
  expect_error(github_download("http://localhost/private", tempfile(), 100L), class = "cttir_source_unavailable")
  f <- remote_fixture()
  f$tree[[1]]$mode <- "120000"
  local_mocked_bindings(github_json = function(url, ...) {
    if (grepl("/commits/", url, fixed = TRUE)) return(list(sha = f$sha))
    list(truncated = FALSE, tree = f$tree)
  })
  expect_error(github_source(f$record), class = "cttir_source_unavailable")
})

test_that("nested source paths, malformed metadata and identity mismatch are checked", {
  f <- remote_fixture()
  local_repository_mock()
  record <- f$record
  record$subdir <- "packages/nested"
  tree <- lapply(f$tree, function(x) {
    x$path <- paste0("packages/nested/", x$path)
    x
  })
  local_mocked_bindings(github_json = function(url, ...) {
    if (grepl("/commits/", url, fixed = TRUE)) return(list(sha = f$sha))
    list(truncated = FALSE, tree = tree)
  }, github_download = function(url, path, max_bytes) f$download(sub("packages/nested/", "", url, fixed = TRUE), path, max_bytes))
  entry <- github_source(record)
  expect_equal(entry$source_subdir, "packages/nested/")
  record$package <- "wrongPackage"
  withr::local_options(cttiR.sources = list(record))
  expect_error(update(mode = "remote", dry_run = TRUE), class = "cttir_source_unavailable")
  tree[[1]]$type <- NULL
  expect_error(github_source(record), class = "cttir_source_unavailable")
})

test_that("explicit package selection does not fetch unrelated registered sources", {
  f <- local_update_fixture()
  withr::local_options(cttiR.sources = list(list(id = "local", package = "cttirFixtureA", path = f$source),
      list(id = "unrelated", github = "CTTIR/missing", package = "missing")))
  local_mocked_bindings(github_source = function(...) stop("Unselected source fetched"))
  mock <- local_repository_mock()
  expect_equal(update(packages = "cttirFixtureA", mode = "remote", dry_run = TRUE)$status, "planned")
  expect_length(mock$calls, 0L)
})
