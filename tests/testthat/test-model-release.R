test_that("blob upload accepts an empty success response before model creation", {
  f <- release_fixture(); routes <- character(); create <- NULL
  httr2::local_mocked_responses(function(req) {
    routes <<- c(routes, req$url)
    if (grepl("/api/blobs/sha256:", req$url, fixed = TRUE)) return(httr2::response(201L))
    create <<- req$body$data
    httr2::response(200L, headers = list(`content-type` = "application/json"),
      body = charToRaw('{"status":"success"}'))
  })
  expect_true(release_import_model("http://127.0.0.1:11434", f$manifest$model, f$model))
  expect_identical(routes, paste0("http://127.0.0.1:11434/api/",
    c(paste0("blobs/sha256:", f$manifest$model$sha256), "create")))
  expect_identical(create$template, f$manifest$model$import_template)
  native <- f$manifest$model; native$import_template <- NULL
  expect_true(release_import_model("http://127.0.0.1:11434", native, f$model))
  expect_false("template" %in% names(create))
})

test_that("release previews and missing consent perform no writes or network", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture(); root <- file.path(f$parent, "absent-runtime")
  withr::local_options(cttiR.runtime_dir = root)
  local_mocked_bindings(runtime_resources = release_test_resources,
    runtime_request = function(...) stop("unexpected HTTP"), release_start_runtime = function(...) stop("unexpected start"))
  before <- tree_hashes(f$parent)
  result <- setup_release(f$path, f$hash, model_file = f$model)
  expect_identical(result$state, "planned")
  expect_identical(result$model$validation, "not_qualified_for_assistance")
  expect_equal(result$storage$model_bytes, file.info(f$model)$size)
  expect_gt(result$storage$required_free_bytes, f$manifest$resources$required_disk_bytes)
  expect_false(dir.exists(root)); expect_equal(tree_hashes(f$parent), before)
  result <- setup_release(f$path, f$hash, model_file = f$model, dry_run = FALSE)
  expect_identical(result$state, "blocked")
  expect_contains(result$blockers, "explicit_setup_consent_required")
  expect_equal(tree_hashes(f$parent), before)
})

test_that("the release view previews locally and carries explicit consent to the worker", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture(); root <- file.path(f$parent, "absent-runtime")
  withr::local_options(cttiR.runtime_dir = root)
  local_mocked_bindings(runtime_resources = release_test_resources,
    runtime_request = function(...) stop("unexpected HTTP"))
  shiny::testServer(app_runtime_server, args = app_test_args(), {
    session$setInputs(use_release = TRUE, release_manifest = f$path,
      release_checksum = f$hash, release_file = f$model, offline = TRUE)
    session$setInputs(plan = 1)
    expect_identical(state$result$state, "planned")
    expect_false(dir.exists(root))
    session$setInputs(run = 1)
    expect_identical(state$result$state, "blocked")
    expect_contains(state$result$blockers, "explicit_setup_consent_required")
    expect_false(dir.exists(root))
  })
  pending <- app_pending_worker()
  shiny::testServer(app_runtime_server, args = app_test_args(worker = pending$worker), {
    session$setInputs(use_release = TRUE, release_manifest = f$path,
      release_checksum = f$hash, release_file = f$model, release_consent = TRUE, offline = TRUE)
    session$setInputs(run = 1)
    call <- pending$control$calls[[1L]]
    expect_identical(call$operation, "setup_release")
    expect_true(call$args$consent); expect_false(call$args$dry_run)
    expect_identical(call$args$model_file, f$model)
    session$setInputs(cancel = 1)
    expect_equal(pending$control$killed, 0L)
  })
})

test_that("release manifests bind checksums, schemas, templates and immutable URLs", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture()
  expect_equal(read_model_release(f$path, f$hash)$release_id, f$manifest$release_id)
  expect_error(read_model_release(f$path, strrep("0", 64L)), class = "cttir_catalog_corrupt")
  reject <- function(change) {
    doc <- f$manifest; doc <- change(doc)
    path <- file.path(f$parent, "bad.json"); write_bytes(json_text(doc), path)
    expect_error(read_model_release(path, file_hash(path)), class = "cttir_error")
  }
  reject(function(x) {x$model$template <- "changed"; x})
  reject(function(x) {x$model$file <- "../outside.gguf"; x})
  reject(function(x) {x$model$url <- "https://huggingface.co/owner/repo/resolve/main/model.gguf"; x})
  reject(function(x) {x$model$url <- "http://127.0.0.1/model.gguf"; x})
  reject(function(x) {x$model$tag <- "remote-cloud:latest"; x})
  reject(function(x) {x$qualification <- "qualified_for_assistance"; x})
  reject(function(x) {x$model$bytes <- 4e9; x})
})

test_that("compatibility and resources block before any side effects", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture(); root <- file.path(f$parent, "absent")
  withr::local_options(cttiR.runtime_dir = root)
  local_mocked_bindings(runtime_resources = function(...) list(platform = "Windows", arch = "x86_64",
    available_memory_bytes = NA_real_, available_disk_bytes = 0), runtime_request = function(...) stop("HTTP"))
  result <- setup_release(f$path, f$hash, dry_run = FALSE, consent = TRUE)
  expect_contains(result$blockers, c("platform_unverified", "memory_headroom_unavailable", "disk_headroom_unavailable"))
  expect_false(dir.exists(root))
  local_mocked_bindings(runtime_resources = release_test_resources)
  f$manifest$catalog_id <- strrep("d", 64L)
  write_bytes(json_text(f$manifest), f$path)
  result <- setup_release(f$path, file_hash(f$path), dry_run = FALSE, consent = TRUE)
  expect_contains(result$blockers, "release_compatibility_mismatch")
  expect_false(dir.exists(root))
})

test_that("model acquisition verifies bytes and preserves failed or interrupted input", {
  f <- release_fixture(); root <- file.path(f$parent, "runtime")
  retained <- release_acquire_model(root, f$manifest$model, f$model, offline = TRUE)
  expect_identical(file_hash(retained), f$manifest$model$sha256)
  expect_identical(file_hash(f$model), f$manifest$model$sha256)
  expect_identical(release_acquire_model(root, f$manifest$model, offline = TRUE), retained)
  write_bytes("corruption", retained)
  expect_error(release_acquire_model(root, f$manifest$model, offline = TRUE), class = "cttir_catalog_corrupt")
  expect_identical(readLines(retained, warn = FALSE), "corruption")
  other <- file.path(f$parent, "other")
  expect_error(release_acquire_model(other, f$manifest$model, offline = TRUE), class = "cttir_runtime_unavailable")
  part <- file.path(other, "release-downloads", paste0(f$manifest$model$sha256, ".gguf.part"))
  write_bytes("GGUFpartial", part)
  expect_error(release_acquire_model(other, f$manifest$model, f$model, offline = TRUE), class = "cttir_path_conflict")
  expect_identical(readLines(part, warn = FALSE), "GGUFpartial")
  bad <- f$manifest$model; bad$sha256 <- strrep("0", 64L)
  expect_false(release_file_verified(f$model, bad))
  if (.Platform$OS.type != "windows") {
    link <- file.path(f$parent, "link.gguf"); expect_true(file.symlink(f$model, link))
    expect_error(release_file_verified(link, f$manifest$model), class = "cttir_path_conflict")
  }
})

test_that("release install refuses tag replacement and never stops a reused daemon", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture(); root <- file.path(f$parent, "runtime")
  withr::local_options(cttiR.runtime_dir = root)
  owner <- list(pid = Sys.getpid(), locality = "managed_cloud_disabled", host = Sys.info()[["nodename"]])
  local_mocked_bindings(runtime_resources = release_test_resources,
    release_start_runtime = function(...) list(owner = owner, process = NULL),
    runtime_state_verified = function(...) TRUE,
    runtime_request = function(...) list(version = f$manifest$runtime_version),
    local_model = function(...) list(digest = strrep("d", 64L)),
    release_import_model = function(...) stop("must not overwrite"))
  result <- setup_release(f$path, f$hash, f$model, dry_run = FALSE, consent = TRUE, offline = TRUE)
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "will not be overwritten", fixed = TRUE)
  expect_false(file.exists(file.path(root, "current-release.json")))
  expect_false(dir.exists(file.path(root, "setup-lock")))
  expect_equal(length(list.files(file.path(root, "release-attempts"), recursive = TRUE)), 1L)
})

test_that("successful transport checks do not grant model qualification", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture(); root <- file.path(f$parent, "runtime")
  withr::local_options(cttiR.runtime_dir = root)
  owner <- list(pid = Sys.getpid(), locality = "managed_cloud_disabled", host = Sys.info()[["nodename"]])
  present <- FALSE; imported <- 0L
  local_mocked_bindings(runtime_resources = release_test_resources,
    release_start_runtime = function(...) list(owner = owner, process = NULL),
    runtime_state_verified = function(...) TRUE,
    runtime_request = function(endpoint, route, ...) if (route == "version") list(version = f$manifest$runtime_version) else list(template = f$manifest$model$template),
    local_model = function(...) if (present) list(digest = f$manifest$model$digest) else NULL,
    release_import_model = function(endpoint, model, path) {expect_true(release_file_verified(path, model)); present <<- TRUE; imported <<- imported + 1L},
    runtime_probe = function(...) list(state = "pass"))
  result <- setup_release(f$path, f$hash, f$model, dry_run = FALSE, consent = TRUE, offline = TRUE)
  expect_identical(result$state, "model_ready")
  expect_identical(result$model$validation, "not_qualified_for_assistance")
  expect_identical(result$blockers, "model_independent_qualification_pending")
  expect_identical(imported, 1L)
  expect_equal(read_document(file.path(root, "runtime-state.json"))$model_digest, f$manifest$model$digest)
  expect_equal(read_document(result$receipt)$state, "model_ready")
  again <- setup_release(f$path, f$hash, f$model, dry_run = FALSE, consent = TRUE, offline = TRUE)
  expect_identical(again$state, "model_ready"); expect_identical(imported, 1L)
  expect_identical(file_hash(file.path(root, "releases", f$hash, "manifest.json")), f$hash)
  expect_equal(length(list.files(file.path(root, "release-attempts"))), 2L)
})

test_that("release setup preserves another writer and checks supplied files first", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture(); root <- file.path(f$parent, "runtime")
  withr::local_options(cttiR.runtime_dir = root)
  local_mocked_bindings(runtime_resources = release_test_resources,
    release_start_runtime = function(...) stop("must not start"))
  dir.create(file.path(root, "setup-lock"), recursive = TRUE)
  write_bytes("other writer", file.path(root, "setup-lock", "owner.txt"))
  before <- tree_hashes(root)
  result <- setup_release(f$path, f$hash, f$model, dry_run = FALSE, consent = TRUE)
  expect_identical(result$state, "blocked"); expect_equal(tree_hashes(root), before)
  unlink(file.path(root, "setup-lock"), recursive = TRUE)
  write_bytes("bad bytes", f$model)
  result <- setup_release(f$path, f$hash, f$model, dry_run = FALSE, consent = TRUE)
  expect_match(result$blockers, "integrity checks", fixed = TRUE)
})

test_that("release import stops when ownership proof expires", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  f <- release_fixture(); root <- file.path(f$parent, "runtime")
  withr::local_options(cttiR.runtime_dir = root)
  checks <- 0L; imports <- 0L; killed <- 0L
  process <- list(is_alive = function() TRUE, kill = function() {killed <<- killed + 1L})
  local_mocked_bindings(runtime_resources = release_test_resources,
    release_start_runtime = function(...) list(owner = list(pid = 1L), process = process),
    runtime_state_verified = function(...) {checks <<- checks + 1L; checks == 1L},
    runtime_request = function(...) list(version = f$manifest$runtime_version),
    local_model = function(...) NULL,
    release_import_model = function(...) {imports <<- imports + 1L})
  result <- setup_release(f$path, f$hash, f$model, dry_run = FALSE, consent = TRUE, offline = TRUE)
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "identity or model storage changed", fixed = TRUE)
  expect_equal(imports, 0L); expect_equal(killed, 1L)
  expect_false(file.exists(file.path(root, "current-release.json")))
})
