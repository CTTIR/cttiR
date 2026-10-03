test_that("automatic selection requires qualified settings and conservative available resources", {
  withr::local_options(cttiR.planner_processor = "cpu", cttiR.planner_threads = 16L)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  resources <- list(platform = "Linux", arch = "x86_64", physical_cores = 16,
    available_memory_bytes = 16 * 1024^3, available_disk_bytes = 16 * 1024^3)
  idx <- which(vapply(manifest$tested_models, function(x) identical(x$tag, "qwen2.5:7b"), logical(1)))
  selected <- runtime_select_model(manifest, tempdir(), resources)
  expect_identical(selected$model, "qwen2.5:7b")
  expect_length(selected$blockers, 0L)
  expect_identical(selected$digest, manifest$tested_models[[idx]]$digest)
  for (field in c("physical_cores", "available_memory_bytes", "available_disk_bytes")) {
    for (value in c(resources[[field]] - 1, NA_real_, Inf, 0)) {
      changed <- resources
      changed[[field]] <- value
      blocked <- runtime_select_model(manifest, tempdir(), changed)
      expect_identical(blocked$model, "auto", info = field)
      expect_length(blocked$blockers, 1L)
    }
  }
  changed <- resources
  changed$platform <- "Darwin"
  expect_match(runtime_select_model(manifest, tempdir(), changed)$blockers, "platform")
  manifest$tested_models[[idx]]$qualification_context$prompt_version <- "stale"
  expect_length(runtime_select_model(manifest, tempdir(), resources)$blockers, 1L)
})

test_that("blocked automatic setup performs no network or filesystem mutations", {
  root <- file.path(new_parent(), "absent")
  withr::local_options(cttiR.runtime_dir = root, cttiR.planner_processor = "cpu", cttiR.planner_threads = 16L)
  local_mocked_bindings(runtime_resources = function(...) list(platform = "Linux", arch = "x86_64",
    physical_cores = 16, available_memory_bytes = 0, available_disk_bytes = 1e12),
    runtime_request = function(...) stop("unexpected HTTP"),
    acquire_runtime = function(...) stop("unexpected acquisition"))
  preview <- setup(dry_run = TRUE)
  expect_identical(preview$state, "planned")
  expect_length(preview$actions, 0L)
  expect_length(preview$blockers, 1L)
  expect_identical(setup()$state, "blocked")
  expect_false(file.exists(root))
  explicit <- setup(model = "qwen2.5:7b", dry_run = TRUE)
  expect_null(explicit$selection)
  expect_identical(explicit$model$name, "qwen2.5:7b")
})

test_that("automatic setup rechecks resources before acquisition", {
  root <- file.path(new_parent(), "absent")
  withr::local_options(cttiR.runtime_dir = root, cttiR.planner_processor = "cpu", cttiR.planner_threads = 16L)
  reads <- 0L
  local_mocked_bindings(runtime_resources = function(...) {
    reads <<- reads + 1L
    list(platform = "Linux", arch = "x86_64", physical_cores = 16,
      available_memory_bytes = if (reads == 1L) 1e12 else 0, available_disk_bytes = 1e12)
  }, runtime_request = function(...) stop("unexpected HTTP"))
  result <- setup()
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "headroom changed")
  expect_false(file.exists(root))
})

test_that("automatic selection orders qualified candidates by model bytes", {
  withr::local_options(cttiR.planner_processor = "cpu", cttiR.planner_threads = 16L)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  candidate <- Filter(function(x) identical(x$tag, "qwen2.5:7b"), manifest$tested_models)[[1]]
  candidate$tag <- "fixture:small"
  candidate$size_bytes <- candidate$size_bytes - 1
  manifest$tested_models <- c(manifest$tested_models, list(candidate))
  resources <- list(platform = "Linux", arch = "x86_64", physical_cores = 16,
    available_memory_bytes = 1e12, available_disk_bytes = 1e12)
  expect_identical(runtime_select_model(manifest, tempdir(), resources)$model, "fixture:small")
})

test_that("memory pressure before inference preserves an existing runtime state", {
  root <- new_parent()
  withr::local_options(cttiR.runtime_dir = root, cttiR.planner_processor = "cpu", cttiR.planner_threads = 16L)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  candidate <- Filter(function(x) identical(x$tag, "qwen2.5:7b"), manifest$tested_models)[[1]]
  writeLines("preserved owner", file.path(root, "runtime-state.json"))
  before <- tree_hashes(root)
  reads <- 0L
  local_mocked_bindings(runtime_resources = function(...) {
    reads <<- reads + 1L
    list(platform = "Linux", arch = "x86_64", physical_cores = 16,
      available_memory_bytes = if (reads < 3L) 1e12 else 0, available_disk_bytes = 1e12)
  }, runtime_owner = function(...) list(pid = Sys.getpid()),
    local_model = function(...) list(digest = candidate$digest),
    runtime_probe = function(...) stop("unexpected inference"),
    runtime_request = function(...) stop("unexpected HTTP"))
  result <- setup(offline = TRUE)
  expect_identical(result$state, "blocked")
  expect_match(result$blockers, "memory headroom changed")
  expect_equal(tree_hashes(root), before)
})
