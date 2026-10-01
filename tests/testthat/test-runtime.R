test_that("runtime previews are side effect free and reject unsafe endpoints", {
  root <- file.path(new_parent(), "absent")
  withr::local_options(cttiR.runtime_dir = root)
  local_mocked_bindings(runtime_request = function(...) stop("unexpected HTTP"))
  expect_equal(setup(dry_run = TRUE)$state, "planned")
  expect_false(file.exists(root))
  for (endpoint in c("https://example.org:443", "http://127.0.0.1:0", "http://127.0.0.1:65536", "http://127.0.0.1:11434/path")) {
    withr::local_options(cttiR.ollama_endpoint = endpoint)
    expect_error(setup(dry_run = TRUE), class = "cttir_input_error")
  }
  expect_error(setup(model = "example-cloud", dry_run = TRUE), class = "cttir_input_error")
  expect_error(setup(offline = NA), class = "cttir_input_error")
})

test_that("offline setup never acquires and preserves another setup lock", {
  root <- new_parent()
  withr::local_options(cttiR.runtime_dir = root)
  local_mocked_bindings(
    runtime_request = function(...) stop("unavailable"),
    acquire_runtime = function(...) stop("unexpected acquisition")
  )
  result <- setup(offline = TRUE)
  expect_equal(result$state, "blocked")
  expect_match(result$blockers, "acquisition is disabled", fixed = TRUE)
  expect_false(dir.exists(file.path(root, "setup-lock")))
  dir.create(file.path(root, "setup-lock"))
  writeLines("another writer", file.path(root, "setup-lock", "marker"))
  before <- tree_hashes(root)
  expect_match(setup(offline = TRUE)$blockers, "already locked", fixed = TRUE)
  expect_equal(tree_hashes(root), before)
})

test_that("setup refuses an unmanaged daemon without model requests", {
  withr::local_options(cttiR.runtime_dir = new_parent())
  local_mocked_bindings(runtime_request = function(endpoint, route, ...) {
    expect_equal(route, "version")
    list(version = "0.34.4")
  })
  expect_match(setup(offline = TRUE)$blockers, "unmanaged daemon", fixed = TRUE)
})

test_that("local model metadata refuses cloud backed execution", {
  entry <- list(name = "local:small", size = 100, digest = paste(rep("a", 64), collapse = ""), details = list(format = "gguf"))
  local_mocked_bindings(runtime_request = function(...) list(models = list(entry)))
  expect_equal(local_model("unused", "local:small")$digest, entry$digest)
  expect_null(local_model("unused", "missing"))
  entry$remote_host <- "cloud.example"
  expect_error(local_model("unused", "local:small"), class = "cttir_runtime_unavailable")
})

test_that("structured probe validates output and never returns thinking", {
  response <- list(done = TRUE, model = "local:small", message = list(content = '{"status":"ready"}', thinking = "discard"))
  local_mocked_bindings(runtime_request = function(endpoint, route, body, timeout) {
    expect_equal(route, "chat")
    expect_equal(body$options$num_gpu, 0L)
    expect_equal(body$options$num_ctx, 2048L)
    expect_null(body$tools)
    response
  })
  probe <- runtime_probe("unused", "local:small")
  expect_equal(probe$state, "pass")
  expect_false(any(grepl("discard", unlist(probe))))
  response$message$content <- '{"status":"ready","extra":true}'
  expect_error(runtime_probe("unused", "local:small"), class = "cttir_runtime_unavailable")
  response$message$content <- '{"status":"ready"}'
  response$done <- FALSE
  expect_error(runtime_probe("unused", "local:small"), class = "cttir_runtime_unavailable")
})

test_that("owned reuse probes locally and failed model verification preserves state", {
  root <- new_parent()
  withr::local_options(cttiR.runtime_dir = root)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  owner <- list(pid = Sys.getpid(), host = Sys.info()[["nodename"]], locality = "managed_cloud_disabled")
  entry <- list(digest = manifest$model_digest)
  probes <- new.env(parent = emptyenv())
  probes$count <- 0L
  local_mocked_bindings(runtime_owner = function(...) owner,
    local_model = function(...) entry,
    runtime_request = function(...) stop("unexpected download"),
    runtime_probe = function(...) {
      probes$count <- probes$count + 1L
      list(state = "pass")
    })
  result <- setup(offline = TRUE)
  expect_equal(result$state, "runtime_ready")
  expect_equal(probes$count, 1L)
  expect_equal(read_document(file.path(root, "runtime-state.json"))$model_digest, manifest$model_digest)
  before <- tree_hashes(root)
  entry$digest <- paste(rep("a", 64), collapse = "")
  expect_match(setup(offline = TRUE)$blockers, "changed digest", fixed = TRUE)
  expect_equal(probes$count, 1L)
  expect_equal(tree_hashes(root), before)
  entry <- NULL
  expect_match(setup(offline = TRUE)$blockers, "absent in offline", fixed = TRUE)
  expect_equal(tree_hashes(root), before)
})

test_that("unverified runtime archives are rejected without download", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  skip_if_not(nzchar(Sys.which("zstd")) && nzchar(Sys.which("tar")))
  root <- new_parent()
  archive <- file.path(root, "ollama-linux-amd64.tar.zst")
  writeLines("invalid archive", archive)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  expect_error(acquire_runtime(root, manifest), class = "cttir_path_conflict")
  expect_equal(readLines(archive), "invalid archive")
  manifest$platform <- "unsupported"
  expect_error(acquire_runtime(root, manifest), class = "cttir_runtime_unavailable")
})
