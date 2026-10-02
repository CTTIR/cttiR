cran_files <- function(members, version = "1.0.0", md5 = NULL, package = "cttirFixtureA", archive = FALSE) {
  bytes <- tarball_bytes(members)
  stanza <- c(Package = package, Version = version, License = "MIT",
    MD5sum = if (is.null(md5)) digest::digest(bytes, algo = "md5", serialize = FALSE) else md5, NeedsCompilation = "no")
  files <- repository_files(cran = c(bundled_stanzas("CRAN"), if (!archive) stats::setNames(list(stanza), package)))
  url <- if (archive) {
    paste0("https://cloud.r-project.org/src/contrib/Archive/", package, "/", package, "_", version, ".tar.gz")
  } else {
    paste0("https://cloud.r-project.org/src/contrib/", package, "_", version, ".tar.gz")
  }
  files[[url]] <- bytes
  attr(files, "md5") <- digest::digest(bytes, algo = "md5", serialize = FALSE)
  files
}

cran_record <- function(...) list(id = "cran-fixture", cran = "cttirFixtureA", documentation_rights = "Synthetic test fixture", ...)

test_that("a CRAN tarball is verified, extracted statically and pinned to its MD5 revision", {
  f <- local_update_fixture()
  files <- cran_files(package_members())
  mock <- local_repository_mock(files)
  withr::local_options(cttiR.sources = list(cran_record()))
  plan <- update(mode = "remote", dry_run = TRUE)
  expect_equal(plan$status, "planned")
  expect_false(file.exists(f$store))
  expect_equal(sum(mock$calls == cran_index_url), 1L)
  expect_true(all(mock$calls %in% names(files)))
  mock$calls <- character()
  result <- update(mode = "remote")
  expect_equal(sum(mock$calls == cran_index_url), 1L)
  expect_equal(result$status, "succeeded")
  expect_equal(result$new_id, plan$new_id)
  info <- packages()
  row <- info[info$package == "cttirFixtureA", ]
  expect_equal(row$revision, paste0("cran:cttirFixtureA@1.0.0:", attr(files, "md5")))
  expect_equal(row$provider, "configured_cran")
  expect_equal(row$repository, "https://cran.r-project.org/package=cttirFixtureA")
  expect_equal(row$freshness, "repository_current_fetched")
  expect_equal(nrow(search("cttirFixtureA::keep")), 1L)
  entry <- Filter(function(x) x$name == "cttirFixtureA", resolve_catalog()$packages)[[1]]
  expect_equal(entry$archive$checksum_source, "repository_index")
  expect_match(entry$archive$checksum_meaning, "not publisher authenticity")
  expect_equal(entry$coverage$documented, 1L)
  expect_equal(update(mode = "remote")$status, "unchanged")
})

test_that("an MD5 mismatch fails closed with a failed report and keeps the pointer", {
  f <- local_update_fixture()
  update()
  pointer <- read_document(file.path(f$store, "active.json"))
  local_repository_mock(cran_files(package_members(), md5 = paste(rep("0", 32), collapse = "")))
  withr::local_options(cttiR.sources = list(cran_record()))
  err <- expect_error(update(mode = "remote", catalogs = "knowledge"), class = "cttir_source_unavailable")
  expect_equal(err$code, "checksum_mismatch")
  expect_s3_class(err$report, "cttir_update")
  expect_equal(err$report$status, "failed")
  expect_false(err$report$activation)
  expect_equal(err$report$sources[[1]]$status, "failed_required")
  expect_equal(read_document(file.path(f$store, "active.json")), pointer)
  expect_false(dir.exists(file.path(f$store, "write-lock")))
})

test_that("archived versions use the archive endpoint and label checksum provenance", {
  f <- local_update_fixture()
  files <- cran_files(package_members(version = "0.9.0"), version = "0.9.0", archive = TRUE)
  mock <- local_repository_mock(files)
  withr::local_options(cttiR.sources = list(cran_record(version = "0.9.0")))
  result <- update(mode = "remote", catalogs = "knowledge", dry_run = TRUE)
  expect_true(any(grepl("/src/contrib/Archive/cttirFixtureA/cttirFixtureA_0.9.0.tar.gz", mock$calls, fixed = TRUE)))
  expect_true(any(grepl("No repository checksum is published", result$warnings, fixed = TRUE)))
  withr::local_options(cttiR.sources = list(cran_record(version = "0.9.0", md5 = attr(files, "md5"))))
  result <- update(mode = "remote", catalogs = "knowledge")
  entry <- Filter(function(x) x$name == "cttirFixtureA", resolve_catalog()$packages)[[1]]
  expect_equal(entry$archive$checksum_source, "registered_record")
  expect_equal(entry$freshness, "repository_archive_fetched")
  withr::local_options(cttiR.sources = list(cran_record(version = "0.9.0", md5 = paste(rep("f", 32), collapse = ""))))
  expect_error(update(mode = "remote", catalogs = "knowledge", dry_run = TRUE), class = "cttir_source_unavailable")
  withr::local_options(cttiR.sources = list(cran_record()))
  err <- expect_error(update(mode = "remote", catalogs = "knowledge", dry_run = TRUE), class = "cttir_source_unavailable")
  expect_equal(err$code, "missing_from_selected_index")
})

test_that("malicious archive members are refused before anything is written", {
  base <- package_members()
  pax_header <- list(name = "cttirFixtureA/PaxHeader", type = "x", data = "30 path=cttirFixtureA/../evil\n")
  cases <- list(
    absolute = c(base, list(list(name = "/etc/cttir-escape", data = "x"))),
    parent = c(base, list(list(name = "cttirFixtureA/../escape", data = "x"))),
    symlink = c(base, list(list(name = "cttirFixtureA/R/link.R", type = "2", linkname = "/etc/passwd"))),
    hardlink = c(base, list(list(name = "cttirFixtureA/R/hard.R", type = "1", linkname = "cttirFixtureA/R/api.R"))),
    device = c(base, list(list(name = "cttirFixtureA/dev", type = "3"))),
    outside = c(base, list(list(name = "otherPackage/R/api.R", data = "x"))),
    pax_parent = c(base, list(pax_header, list(name = "cttirFixtureA/plain", data = "x"))),
    duplicate = c(base, list(base[[3]])),
    case = c(base, list(list(name = "cttirFixtureA/r/API.R", data = "x"), list(name = "cttirFixtureA/R/API.r", data = "y")))
  )
  for (name in names(cases)) {
    tarball <- make_tarball(tempfile(fileext = ".tar.gz"), cases[[name]])
    stage <- tempfile("stage-")
    expect_error(stage_package_archive(tarball, "cttirFixtureA", stage), class = "cttir_source_unavailable", info = name)
    expect_false(dir.exists(stage))
  }
  good <- make_tarball(tempfile(fileext = ".tar.gz"), base)
  expect_error(tar_members(good, "cttirFixtureA", max_bytes = 1024L), class = "cttir_source_unavailable")
  con <- gzfile(good, "rb")
  raw_tar <- readBin(con, "raw", n = 1000000L)
  close(con)
  raw_tar[200] <- as.raw(65L)
  corrupt <- tempfile(fileext = ".tar")
  writeBin(raw_tar, corrupt)
  expect_error(tar_members(corrupt, "cttirFixtureA"), class = "cttir_source_unavailable")
  f <- local_update_fixture()
  update()
  pointer <- read_document(file.path(f$store, "active.json"))
  local_repository_mock(cran_files(cases$symlink))
  withr::local_options(cttiR.sources = list(cran_record()))
  expect_error(update(mode = "remote", catalogs = "knowledge"), class = "cttir_source_unavailable")
  expect_equal(read_document(file.path(f$store, "active.json")), pointer)
})

test_that("oversize documentation is omitted with evidence while oversize API files fail", {
  big <- strrep("x", 1100000)
  members <- c(package_members(), list(list(name = "cttirFixtureA/man/figures/logo.svg", data = big)))
  staged <- stage_package_archive(make_tarball(tempfile(fileext = ".tar.gz"), members), "cttirFixtureA", tempfile())
  expect_equal(staged$omitted[[1]]$path, "man/figures/logo.svg")
  expect_equal(staged$omitted[[1]]$sha256, digest::digest(big, algo = "sha256", serialize = FALSE))
  expect_false(file.exists(file.path(staged$root, "man/figures/logo.svg")))
  members <- package_members(code = c("keep <- function(x = 1) x", paste0("# ", big)))
  expect_error(stage_package_archive(make_tarball(tempfile(fileext = ".tar.gz"), members), "cttirFixtureA", tempfile()),
    class = "cttir_source_unavailable")
})

test_that("missing documentation is reported as absent, not invented", {
  f <- local_update_fixture()
  local_repository_mock(cran_files(package_members(man = FALSE)))
  withr::local_options(cttiR.sources = list(cran_record()))
  update(mode = "remote", catalogs = "knowledge")
  entry <- Filter(function(x) x$name == "cttirFixtureA", resolve_catalog()$packages)[[1]]
  expect_equal(entry$coverage$documented, 0L)
  expect_equal(entry$documentation_corpus$coverage$vignette_status, "not_present_in_source")
  expect_null(entry$exports[[1]]$documentation)
})

test_that("redirects, foreign endpoints, aliases and oversize responses are refused", {
  refused <- c("http://cloud.r-project.org/src/contrib/PACKAGES", "https://cran.r-project.org/src/contrib/PACKAGES",
    "https://cloud.r-project.org/src/contrib/../../PACKAGES", "https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES",
    "https://bioconductor.org/packages/devel/bioc/src/contrib/PACKAGES", "https://cloud.r-project.org/src/contrib/Archive/a/b_1.0.tar.gz",
    "https://cloud.r-project.org/src/contrib/PACKAGES?x=1", "https://evil.example/src/contrib/PACKAGES")
  for (url in refused) {
    expect_false(repository_url_allowed(url), info = url)
    expect_error(repository_download(url, tempfile(), 100L), class = "cttir_source_unavailable")
  }
  expect_true(repository_url_allowed("https://bioconductor.org/packages/3.23/bioc/src/contrib/PACKAGES"))
  httr2::local_mocked_responses(function(req) httr2::response(302L, headers = list(Location = "https://evil.example/PACKAGES")))
  err <- expect_error(repository_download(cran_index_url, tempfile(), 100L), class = "cttir_source_unavailable")
  expect_equal(err$code, "redirect_refused")
  httr2::local_mocked_responses(function(req) httr2::response(404L))
  err <- expect_error(repository_download(cran_index_url, tempfile(), 100L), class = "cttir_source_unavailable")
  expect_equal(err$code, "repository_http_error")
  context <- new_fetch_context(tempfile())
  local_mocked_bindings(repository_download = function(url, path, max_bytes) {
    writeBin(raw(max_bytes + 1L), path)
    list(status = 200L)
  })
  err <- expect_error(repository_fetch(cran_index_url, tempfile(), 64L, context), class = "cttir_source_unavailable")
  expect_equal(err$code, "response_bound")
})

test_that("Bioconductor sources are fetched from the selected frozen release", {
  f <- local_update_fixture()
  bytes <- tarball_bytes(package_members(version = "1.2.0"))
  md5 <- digest::digest(bytes, algo = "md5", serialize = FALSE)
  stanza <- list(cttirFixtureA = c(Package = "cttirFixtureA", Version = "1.2.0", License = "MIT", MD5sum = md5))
  files <- repository_files(bioc = c(bundled_stanzas("Bioconductor"), stanza), release = "3.22")
  files[["https://bioconductor.org/packages/3.22/bioc/src/contrib/cttirFixtureA_1.2.0.tar.gz"]] <- bytes
  local_repository_mock(files)
  withr::local_options(cttiR.sources = list(list(id = "bioc-fixture", bioc = "cttirFixtureA", bioc_version = "3.22")))
  update(mode = "remote", catalogs = "knowledge")
  row <- packages()[packages()$package == "cttirFixtureA", ]
  expect_equal(row$revision, paste0("bioc-3.22:cttirFixtureA@1.2.0:", md5))
  expect_equal(row$repository, "https://bioconductor.org/packages/3.22/bioc/html/cttirFixtureA.html")
  expect_equal(row$provider, "configured_bioconductor")
  withr::local_options(cttiR.sources = list(list(id = "bioc-fixture", bioc = "cttirFixtureA", bioc_version = "3.22", version = "1.0.0")))
  expect_error(update(mode = "remote", catalogs = "knowledge", dry_run = TRUE), class = "cttir_source_unavailable")
  withr::local_options(cttiR.sources = list(list(id = "bioc-fixture", bioc = "cttirFixtureA", bioc_version = "devel")))
  expect_error(update(mode = "remote", catalogs = "knowledge", dry_run = TRUE), class = "cttir_input_error")
})

test_that("a CRAN v2 removing an export cannot resurrect it while pins keep history", {
  f <- local_update_fixture()
  v1 <- cran_files(package_members(code = c("old <- function(x) x", "keep <- function(x = 1) x"), exports = c("old", "keep")))
  mock <- local_repository_mock(v1)
  withr::local_options(cttiR.sources = list(cran_record()))
  first <- update(mode = "remote", catalogs = "knowledge")
  expect_equal(nrow(search("cttirFixtureA::old")), 1L)
  pinned <- project("Pinned CRAN project", "methods", "Goal", f$parent)
  pinned_files <- tree_hashes(pinned$path)
  v2_members <- package_members(version = "2.0.0", code = c("keep <- function(x = 2) x", "new <- function(y) y"), exports = c("keep", "new"))
  v2 <- cran_files(v2_members, version = "2.0.0")
  mock$files <- v2
  second <- update(mode = "remote", catalogs = "knowledge")
  expect_true(any(second$api_diff$change == "export_removed" & second$api_diff$symbol == "old"))
  expect_true(any(second$api_diff$change == "export_changed" & second$api_diff$symbol == "keep"))
  expect_equal(nrow(search("cttirFixtureA::old")), 0L)
  expect_equal(nrow(search("cttirFixtureA::new")), 1L)
  expect_equal(nrow(search("cttirFixtureA::old", path = pinned$path)), 1L)
  expect_equal(tree_hashes(pinned$path), pinned_files)
  expect_true(rollback_knowledge(first$new_id, dry_run = FALSE)$activation)
  expect_equal(nrow(search("cttirFixtureA::old")), 1L)
})
