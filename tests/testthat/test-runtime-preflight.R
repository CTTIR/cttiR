test_that("offline setup previews expose known blockers and size provenance", {
  root <- file.path(new_parent(), "absent")
  withr::local_options(cttiR.runtime_dir = root)
  local_mocked_bindings(runtime_request = function(...) stop("unexpected HTTP"))
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  resources <- list(platform = "unsupported", arch = "unknown", available_disk_bytes = 100)
  preflight <- runtime_preflight(root, runtime_endpoint(), manifest, manifest$model,
    "not_qualified_for_planning", TRUE, FALSE, resources, c(tar = "", zstd = ""))
  expect_setequal(preflight$acquisition_blockers, c("platform_unverified", "archive_tools_missing"))
  expect_identical(preflight$planner_blockers, "workflow_model_not_qualified")
  expect_equal(preflight$downloads$model_bytes, manifest$model_bytes)
  expect_equal(preflight$downloads$runtime_archive_bytes, manifest$archive_bytes)
  expect_equal(preflight$disk$available_bytes, 100)
  expect_null(preflight$owner)
  unknown <- runtime_preflight(root, runtime_endpoint(), manifest, "unknown:1b",
    "not_qualified_for_planning", FALSE, TRUE, resources)
  expect_null(unknown$downloads$model_bytes)
  expect_null(unknown$disk$admission_required_bytes)
  expect_identical(unknown$acquisition_blockers, "runtime_absent_acquisition_disabled")
  preview <- setup(model = manifest$model, offline = TRUE, dry_run = TRUE)
  expect_true(all(c("workflow_model_not_qualified", "runtime_absent_acquisition_disabled") %in% preview$blockers))
  expect_length(preview$actions, 0)
  expect_output(print(preview), "Full download sizes")
  expect_output(print(preview), "admission screen")
  expect_false(file.exists(root))
  expect_identical(preview$download_estimate$model_presence, "not_queried")
  expect_null(preview$download_estimate$model_bytes)
  expect_null(preview$download_estimate$total_bytes)
  expect_equal(preview$download_estimate$runtime_bytes, manifest$archive_bytes)
  expect_output(print(preview), "Estimated acquisition")
})

test_that("offline ownership and disk screens do not imply qualification", {
  root <- new_parent()
  local_mocked_bindings(runtime_owner = function(...) list(pid = 123L, host = "fixture", endpoint = "loopback"))
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  result <- runtime_preflight(root, runtime_endpoint(), manifest, "qwen2.5:7b",
    "not_qualified_for_planning", TRUE, FALSE,
    resources = list(platform = "Linux", arch = "x86_64", available_disk_bytes = 100))
  expect_identical(result$disk$admission_state, "insufficient")
  expect_equal(result$owner$pid, 123L)
  expect_equal(result$owner$host, "fixture")
  expect_equal(result$disk$admission_required_bytes, 17179869184)
  expect_identical(result$planner_blockers, "workflow_model_not_qualified")
})

test_that("download estimates distinguish missing, reusable and unrecorded models", {
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  preflight <- runtime_preflight(new_parent(), runtime_endpoint(), manifest, manifest$model,
    "not_qualified_for_planning", TRUE, FALSE)
  absent <- runtime_download_estimate(preflight, FALSE)
  expect_equal(absent$total_bytes, as.double(manifest$archive_bytes) + as.double(manifest$model_bytes))
  preflight$executable_present <- TRUE
  expect_equal(runtime_download_estimate(preflight, TRUE)$total_bytes, 0)
  preflight$downloads$model_bytes <- NULL
  expect_null(runtime_download_estimate(preflight, FALSE)$total_bytes)
  expect_equal(runtime_download_estimate(preflight, TRUE)$total_bytes, 0)
  expect_match(absent$scope, "not_measured_transfer", fixed = TRUE)
})

test_that("successful acquisition retains its before-pull estimate", {
  root <- new_parent()
  withr::local_options(cttiR.runtime_dir = root)
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  queries <- 0L
  pulls <- 0L
  local_mocked_bindings(
    runtime_owner = function(...) list(pid = Sys.getpid(), host = "fixture"),
    local_model = function(...) {
      queries <<- queries + 1L
      if (queries == 1L) NULL else list(digest = manifest$model_digest)
    },
    runtime_request = function(endpoint, route, body, ...) {
      expect_identical(route, "pull")
      expect_identical(body$model, manifest$model)
      pulls <<- pulls + 1L
      list(status = "success")
    },
    runtime_probe = function(...) list(state = "pass")
  )
  result <- setup(model = manifest$model)
  expect_identical(result$state, "runtime_ready")
  expect_equal(pulls, 1L)
  expect_equal(queries, 2L)
  expect_identical(result$download_estimate$model_presence, "absent")
  expect_equal(result$download_estimate$total_bytes, as.double(manifest$model_bytes))
})
