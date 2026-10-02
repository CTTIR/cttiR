repository_source_files <- function(version = "1.0.0", code = c("old <- function(x) x", "keep <- function(x = 1) x"),
  exports = c("old", "keep")) {
  bytes <- tarball_bytes(package_members(version = version, code = code, exports = exports))
  stanza <- c(Package = "cttirFixtureA", Version = version, License = "MIT",
    MD5sum = digest::digest(bytes, algo = "md5", serialize = FALSE))
  files <- repository_files(cran = c(bundled_stanzas("CRAN"), list(cttirFixtureA = stanza)))
  files[[paste0("https://cloud.r-project.org/src/contrib/cttirFixtureA_", version, ".tar.gz")]] <- bytes
  files
}

test_that("required source failures abort while optional failures are partial and stale", {
  f <- local_update_fixture()
  mock <- local_repository_mock(repository_source_files())
  withr::local_options(cttiR.sources = list(list(id = "cran-a", cran = "cttirFixtureA")))
  first <- update(mode = "remote", catalogs = "knowledge")
  expect_equal(first$status, "succeeded")
  pointer <- read_document(file.path(f$store, "active.json"))
  tarball <- "https://cloud.r-project.org/src/contrib/cttirFixtureA_1.0.0.tar.gz"
  mock$files[[tarball]] <- NULL
  err <- expect_error(update(mode = "remote", catalogs = "knowledge"), class = "cttir_source_unavailable")
  expect_equal(err$report$status, "failed")
  expect_equal(err$report$sources[[1]]$id, "cran-a")
  expect_equal(err$report$sources[[1]]$status, "failed_required")
  expect_equal(read_document(file.path(f$store, "active.json")), pointer)
  optional <- list(list(id = "cran-a", cran = "cttirFixtureA", optional = TRUE),
    list(id = "new-optional", cran = "neverListedFixture", optional = TRUE))
  withr::local_options(cttiR.sources = optional)
  plan <- update(mode = "remote", catalogs = "knowledge", dry_run = TRUE)
  expect_equal(plan$status, "planned")
  expect_true(plan$partial)
  result <- update(mode = "remote", catalogs = "knowledge")
  expect_equal(result$status, "partial")
  expect_true(result$activation)
  expect_setequal(vapply(result$sources, function(x) x$status, character(1)), "unavailable_optional")
  expect_true(any(result$api_diff$change == "freshness_changed" & !result$api_diff$breaking))
  row <- packages()[packages()$package == "cttirFixtureA", ]
  expect_equal(row$freshness, "source_unavailable")
  expect_equal(nrow(search("cttirFixtureA::old")), 1L)
  expect_false("neverListedFixture" %in% packages()$package)
  again <- update(mode = "remote", catalogs = "knowledge")
  expect_equal(again$status, "partial")
  expect_false(again$activation)
  withr::local_options(cttiR.sources = list(list(id = "x", cran = "a", optional = "yes")))
  expect_error(update(mode = "remote", dry_run = TRUE), class = "cttir_input_error")
})

test_that("pruning requires evidence and writes tombstones without touching history", {
  f <- local_update_fixture()
  first <- update()
  pinned <- project("Prune pinned", "methods", "Goal", f$parent)
  pinned_files <- tree_hashes(pinned$path)
  bundled <- digest::digest(file = system.file("extdata", "api-catalog.json.gz", package = "cttiR"), algo = "sha256")
  snapshots <- list.files(file.path(f$store, "snapshots"))
  pointer <- read_document(file.path(f$store, "active.json"))
  withr::local_options(cttiR.sources = list(list(id = "fixture", package = "cttirFixtureA", path = f$source, retired = TRUE)))
  err <- expect_error(update(prune = TRUE), class = "cttir_input_error")
  expect_equal(err$code, "missing_retirement_evidence")
  expect_equal(read_document(file.path(f$store, "active.json")), pointer)
  kept <- update(prune = FALSE, dry_run = TRUE)
  expect_true(any(grepl("prune = FALSE", kept$warnings, fixed = TRUE)))
  expect_equal(kept$sources[[1]]$status, "retired_not_pruned")
  expect_true(any(kept$api_diff$change == "freshness_changed"))
  retired <- list(id = "fixture", package = "cttirFixtureA", path = f$source, retired = TRUE,
    retirement_evidence = "Synthetic upstream archival notice")
  withr::local_options(cttiR.sources = list(retired))
  result <- update(prune = TRUE)
  expect_equal(result$status, "succeeded")
  expect_true(any(result$api_diff$change == "package_removed" & result$api_diff$package == "cttirFixtureA"))
  tomb <- resolve_catalog()$tombstones
  expect_length(tomb, 1L)
  expect_equal(tomb[[1]]$package, "cttirFixtureA")
  expect_equal(tomb[[1]]$last_revision, first$sources[[1]]$revision)
  expect_equal(tomb[[1]]$evidence, "Synthetic upstream archival notice")
  expect_equal(tomb[[1]]$reason, "retired_by_registry")
  expect_true(nzchar(tomb[[1]]$removed_at))
  expect_equal(result$tombstones[[1]]$package, "cttirFixtureA")
  expect_false("cttirFixtureA" %in% packages()$package)
  expect_equal(nrow(search("cttirFixtureA::old")), 0L)
  expect_equal(nrow(search("cttirFixtureA::old", path = pinned$path)), 1L)
  expect_equal(tree_hashes(pinned$path), pinned_files)
  expect_true(all(snapshots %in% list.files(file.path(f$store, "snapshots"))))
  expect_equal(digest::digest(file = system.file("extdata", "api-catalog.json.gz", package = "cttiR"), algo = "sha256"), bundled)
  expect_equal(update(prune = TRUE)$status, "unchanged")
  expect_true(rollback_knowledge(first$new_id, dry_run = FALSE)$activation)
  expect_equal(nrow(search("cttirFixtureA::old")), 1L)
  conflicting <- list(list(id = "fixture", path = f$source),
    list(id = "retired", package = "cttirFixtureA", retired = TRUE, retirement_evidence = "x"),
    list(id = "active", package = "cttirFixtureA", path = f$source))
  withr::local_options(cttiR.sources = conflicting)
  expect_error(update(prune = TRUE, dry_run = TRUE), class = "cttir_input_error")
})

test_that("local mode never calls any network adapter", {
  f <- local_update_fixture()
  calls <- character()
  local_mocked_bindings(
    repository_download = function(url, ...) {
      calls <<- c(calls, url)
      stop("network forbidden")
    },
    github_download = function(url, ...) {
      calls <<- c(calls, url)
      stop("network forbidden")
    },
    github_json = function(url, ...) {
      calls <<- c(calls, url)
      stop("network forbidden")
    }
  )
  registry <- list(list(id = "fixture", path = f$source),
    list(id = "cran-x", cran = "patchwork"), list(id = "bioc-x", bioc = "SummarizedExperiment"),
    list(id = "gh-x", github = "CTTIR/reflowR", package = "reflowR"))
  withr::local_options(cttiR.sources = registry)
  expect_equal(update(dry_run = TRUE)$status, "planned")
  result <- update()
  expect_equal(result$status, "succeeded")
  expect_length(result$resource_sources, 0L)
  expect_equal(nrow(result$resource_changes), 0L)
  for (id in c("cran-x", "bioc-x", "gh-x")) {
    expect_error(update(sources = id, dry_run = TRUE), class = "cttir_source_unavailable")
  }
  expect_error(update(packages = "patchwork", dry_run = TRUE), class = "cttir_source_unavailable")
  expect_length(calls, 0L)
})

test_that("Bioconductor release policy is pinned, derived and validated", {
  table <- bioc_release_table()
  expect_true(all(c("3.19", "3.20", "3.21", "3.22", "3.23") %in% table$release))
  expect_equal(table$r_minor[match(c("3.19", "3.20", "3.21", "3.22", "3.23"), table$release)], c("4.4", "4.4", "4.5", "4.5", "4.6"))
  expect_false(any(c("release", "devel", "3.24") %in% table$release))
  store <- tempfile("cttir-policy-")
  withr::local_options(cttiR.catalog_dir = store)
  local_mocked_bindings(running_r_minor = function() "4.5")
  expect_equal(bioc_release_policy()$release, "3.22")
  expect_true(bioc_release_policy()$compatible)
  expect_equal(bioc_release_policy("3.21")$source, "explicit")
  expect_true(bioc_release_policy("3.21")$changes_policy)
  local_mocked_bindings(running_r_minor = function() "9.1")
  unresolved <- bioc_release_policy()
  expect_true(is.na(unresolved$release))
  expect_equal(unresolved$source, "unresolved")
  expect_error(require_bioc_release(unresolved), class = "cttir_source_unavailable")
  for (bad in list("devel", "release", "9.99", "", NA_character_, c("3.22", "3.23"), 3.22)) {
    expect_error(bioc_release_policy(bad), class = "cttir_input_error")
  }
  dir.create(store)
  write_bioc_policy("3.20", store)
  expect_equal(bioc_release_policy()$release, "3.20")
  expect_equal(bioc_release_policy()$source, "store_policy")
  expect_false(bioc_release_policy("3.20")$changes_policy)
  expect_false(file.exists(file.path(store, "write-lock")))
  unlink(store, recursive = TRUE)
})

test_that("resources-only runs observe repository registrations without fetching tarballs", {
  f <- local_update_fixture()
  mock <- local_repository_mock(repository_source_files())
  withr::local_options(cttiR.sources = list(list(id = "cran-a", cran = "cttirFixtureA"), list(id = "cran-seurat", cran = "Seurat")))
  result <- update(sources = "cran-seurat", mode = "remote", catalogs = "resources", dry_run = TRUE)
  expect_equal(mock$calls, cran_index_url)
  expect_equal(unique(result$resource_changes$package), "Seurat")
  mock$calls <- character()
  result <- update(packages = "Seurat", mode = "remote", catalogs = "resources", dry_run = TRUE)
  expect_equal(mock$calls, cran_index_url)
  expect_equal(unique(result$resource_changes$package), "Seurat")
  expect_equal(result$catalogs$knowledge$current, result$catalogs$knowledge$previous)
  withr::local_options(cttiR.sources = list(list(id = "cran-a", cran = "bad name")))
  expect_error(update(mode = "remote", catalogs = "resources", dry_run = TRUE), class = "cttir_input_error")
})
