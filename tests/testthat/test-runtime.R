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

test_that("model tags are plain local name[:tag] values", {
  withr::local_options(cttiR.runtime_dir = file.path(new_parent(), "absent"))
  local_mocked_bindings(runtime_request = function(...) stop("unexpected HTTP"))
  refused <- c("https://ollama.com/library/qwen", "hf.co/x/y:latest", "library/qwen2.5:1.5b", "qwen:1b:extra",
    ":latest", "qwen2.5 1.5b")
  for (model in refused) {
    expect_error(setup(model = model, dry_run = TRUE), class = "cttir_input_error")
  }
  expect_equal(setup(model = "qwen2.5-coder:1.5b", dry_run = TRUE)$model$name, "qwen2.5-coder:1.5b")
  expect_true(valid_model_tag("qwen3:4b-instruct-2507-q4_K_M"))
})

test_that("runtime ownership needs the owned binary as the process image, not just in the arguments", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux") && dir.exists("/proc/self"))
  skip_if_not(nzchar(Sys.which("sleep")) && nzchar(Sys.which("sh")))
  sleep <- normalizePath(Sys.which("sleep"))
  real <- processx::process$new(sleep, "30")
  on.exit(real$kill(), add = TRUE)
  expect_true(runtime_executable_matches(real$get_pid(), sleep))
  # A shell whose command line merely names the binary path and "serve".
  fake <- file.path(new_parent(), "ollama")
  file.copy(sleep, fake)
  spoof <- processx::process$new(Sys.which("sh"), c("-c", "sleep 30", fake, "serve"))
  on.exit(spoof$kill(), add = TRUE)
  expect_true(fake %in% ps::ps_cmdline(ps::ps_handle(spoof$get_pid())))
  expect_false(runtime_executable_matches(spoof$get_pid(), fake))
})

test_that("offline setup never acquires and preserves another setup lock", {
  local_mocked_bindings(runtime_select_model = function(manifest, root)
    list(model = manifest$model, digest = manifest$model_digest, blockers = character(),
      requirements = list(available_memory_bytes = 1)))
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
  local_mocked_bindings(runtime_select_model = function(manifest, root)
    list(model = manifest$model, digest = manifest$model_digest, blockers = character(),
      requirements = list(available_memory_bytes = 1)))
  withr::local_options(cttiR.runtime_dir = new_parent())
  local_mocked_bindings(runtime_request = function(endpoint, route, ...) {
    expect_equal(route, "version")
    list(version = "0.34.4")
  }, acquire_runtime = function(...) stop("unexpected acquisition"))
  # The port probe precedes acquisition; offline setup without a runtime never probes.
  expect_match(setup()$blockers, "unmanaged daemon", fixed = TRUE)
  requests <- 0L
  local_mocked_bindings(runtime_request = function(...) {
    requests <<- requests + 1L
    stop("unexpected request")
  })
  expect_match(setup(offline = TRUE)$blockers, "acquisition is disabled", fixed = TRUE)
  expect_equal(requests, 0L)
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
  local_mocked_bindings(runtime_select_model = function(manifest, root)
    list(model = manifest$model, digest = manifest$model_digest, blockers = character(),
      requirements = list(available_memory_bytes = 1)))
  root <- new_parent()
  withr::local_options(cttiR.runtime_dir = root)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  owner <- list(pid = Sys.getpid(), host = Sys.info()[["nodename"]], locality = "managed_cloud_disabled")
  entry <- list(digest = manifest$model_digest)
  probes <- new.env(parent = emptyenv())
  probes$count <- 0L
  local_mocked_bindings(runtime_owner = function(...) owner,
    local_model = function(...) entry,
    runtime_state_verified = function(...) TRUE,
    runtime_request = function(endpoint, route, ...) {
      expect_identical(route, "version"); list(version = manifest$runtime_version)
    },
    runtime_probe = function(...) {
      probes$count <- probes$count + 1L
      list(state = "pass")
    })
  result <- setup(offline = TRUE)
  expect_equal(result$state, "runtime_ready")
  expect_equal(result$download_estimate$total_bytes, 0)
  expect_identical(result$download_estimate$model_presence, "present")
  expect_equal(probes$count, 1L)
  expect_equal(read_document(file.path(root, "runtime-state.json"))$model_digest, manifest$model_digest)
  before <- tree_hashes(root)
  entry$digest <- paste(rep("a", 64), collapse = "")
  expect_match(setup(offline = TRUE)$blockers, "changed digest", fixed = TRUE)
  expect_equal(probes$count, 1L)
  expect_equal(tree_hashes(root), before)
  entry <- NULL
  missing <- setup(offline = TRUE)
  expect_match(missing$blockers, "absent in offline", fixed = TRUE)
  expect_identical(missing$download_estimate$model_presence, "absent")
  expect_equal(missing$download_estimate$total_bytes, manifest$model_bytes)
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

test_that("completed unverified runtime parts remain available for review", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  skip_if_not(nzchar(Sys.which("zstd")) && nzchar(Sys.which("tar")))
  root <- new_parent()
  part <- file.path(root, "ollama-linux-amd64.tar.zst.part")
  write_bytes("unverified", part)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  manifest$archive_bytes <- file.info(part)$size
  before <- file_hash(part)
  expect_error(acquire_runtime(root, manifest), class = "cttir_path_conflict")
  expect_identical(file_hash(part), before)
  inventory <- runtime_storage_files(root)
  expect_identical(inventory$role, "runtime_archive")
  expect_true(inventory$retained)
})

test_that("legacy setup rechecks runtime ownership and model identity before activating state", {
  root <- new_parent(); withr::local_options(cttiR.runtime_dir = root)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  state <- file.path(root, "runtime-state.json"); write_bytes('{"previous":"retained"}', state)
  before <- file_hash(state)
  owner <- list(pid = Sys.getpid(), host = Sys.info()[["nodename"]], locality = "managed_cloud_disabled")
  original <- strrep("a", 64); changed <- strrep("b", 64)
  actual <- original; owned <- TRUE; pulled <- 0L; mutate <- "digest"
  local_mocked_bindings(runtime_owner = function(...) owner,
    runtime_state_verified = function(...) owned,
    runtime_request = function(endpoint, route, ...) {
      if (route == "pull") pulled <<- pulled + 1L
      list(version = manifest$runtime_version)
    },
    local_model = function(...) list(digest = actual),
    runtime_probe = function(...) {
      if (mutate == "digest") actual <<- changed else owned <<- FALSE
      list(state = "pass")
    })
  result <- setup(model = "local:small", offline = TRUE)
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "Model identity changed", fixed = TRUE)
  expect_identical(file_hash(state), before)
  actual <- original; mutate <- "owner"
  result <- setup(model = "local:small", offline = TRUE)
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "identity or model storage changed", fixed = TRUE)
  expect_identical(file_hash(state), before)
  expect_equal(pulled, 0L)
})

test_that("legacy setup never pulls after ownership expires or the runtime version changes", {
  root <- new_parent(); withr::local_options(cttiR.runtime_dir = root)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  owned <- TRUE; version <- manifest$runtime_version; pulled <- 0L
  local_mocked_bindings(runtime_owner = function(...) list(pid = Sys.getpid()),
    runtime_state_verified = function(...) owned,
    runtime_request = function(endpoint, route, ...) {
      if (route == "pull") pulled <<- pulled + 1L
      list(version = version)
    },
    local_model = function(...) {owned <<- FALSE; NULL})
  result <- setup(model = "local:small")
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "identity or model storage changed", fixed = TRUE)
  expect_equal(pulled, 0L)
  owned <- TRUE; version <- "different"
  result <- setup(model = "local:small")
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "runtime version differs", fixed = TRUE)
  expect_equal(pulled, 0L)
  expect_false(file.exists(file.path(root, "runtime-state.json")))
})

test_that("failed fresh startup preserves the previous activation record", {
  root <- new_parent(); withr::local_options(cttiR.runtime_dir = root)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  executable <- file.path(root, paste0("ollama-", manifest$runtime_version), "bin", "ollama")
  write_bytes("synthetic preflight fixture", executable)
  state <- file.path(root, "runtime-state.json"); write_bytes('{"previous":"retained"}', state)
  before <- file_hash(state); killed <- 0L
  child <- list(is_alive = function() TRUE, kill = function() {killed <<- killed + 1L})
  local_mocked_bindings(runtime_owner = function(...) NULL,
    release_start_runtime = function(...) list(owner = list(pid = 123L), process = child),
    runtime_state_verified = function(...) TRUE,
    runtime_request = function(...) list(version = "different"))
  result <- setup(model = "local:small", offline = TRUE)
  expect_identical(result$state, "blocked")
  expect_identical(killed, 1L)
  expect_identical(file_hash(state), before)
  expect_false(dir.exists(file.path(root, "setup-lock")))
})
