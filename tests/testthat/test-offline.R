# Gate G16: ordinary workflows make no network attempts. Every request primitive
# reachable from the package is replaced by a sentinel that records the attempt
# and fails, so a hidden network call cannot silently succeed or go unnoticed.

local_network_sentinels <- function(env = parent.frame()) {
  attempts <- new.env(parent = emptyenv())
  attempts$calls <- character()
  sentinel <- function(name) {
    force(name)
    function(...) {
      attempts$calls <- c(attempts$calls, name)
      condition <- list(message = paste("Network primitive called:", name), call = NULL)
      stop(structure(condition, class = c("cttir_test_network_attempt", "error", "condition")))
    }
  }
  mock <- function(package, functions) {
    functions <- functions[functions %in% getNamespaceExports(package)]
    bindings <- stats::setNames(lapply(paste0(package, "::", functions), sentinel), functions)
    do.call(testthat::local_mocked_bindings, c(bindings, list(.package = package, .env = env)))
  }
  mock("httr2", c(
    "req_perform", "req_perform_stream", "req_perform_parallel", "req_perform_sequential",
    "req_perform_connection", "req_perform_iterative", "req_perform_promise"
  ))
  mock("curl", c(
    "curl_fetch_memory", "curl_fetch_disk", "curl_fetch_stream", "curl_fetch_multi",
    "curl_download", "curl", "multi_run"
  ))
  mock("utils", c("download.file", "url.show"))
  mock("processx", "run")
  testthat::local_mocked_bindings(process = list(new = sentinel("processx::process$new")),
    .package = "processx", .env = env)
  attempts
}

test_that("the network sentinels intercept the package's request paths", {
  attempts <- local_network_sentinels()
  expect_error(runtime_request("http://127.0.0.1:9", "version"), class = "cttir_runtime_unavailable")
  expect_error(github_download("https://api.github.com/repos/a/b", tempfile(), 10L), class = "cttir_source_unavailable")
  expect_error(utils::download.file("https://example.org", tempfile(), quiet = TRUE), class = "cttir_test_network_attempt")
  expect_identical(attempts$calls, c("httr2::req_perform", "httr2::req_perform", "utils::download.file"))
})

test_that("core workflows complete offline with zero network attempts", {
  attempts <- local_network_sentinels()
  f <- local_update_fixture()
  runtime <- file.path(f$parent, "empty-runtime")
  dir.create(runtime)
  withr::local_options(cttiR.runtime_dir = runtime)
  p <- project("Offline study", "primary_research", "Work without a network", f$parent)
  expect_true(dir.exists(p$path))
  again <- project("Offline study", "primary_research", "Work without a network", f$parent)
  expect_true(all(again$plan$action == "skip"))
  preview <- sync(p$path, options = list(project = list(goal = "Updated offline goal")))
  expect_equal(preview$state, "planned")
  applied <- sync(p$path, options = list(project = list(goal = "Updated offline goal")), dry_run = FALSE)
  expect_equal(applied$state, "applied")
  updated <- update(mode = "local")
  expect_equal(updated$status, "succeeded")
  expect_gt(nrow(search("keep")), 0L)
  expect_s3_class(ask("cttirFixtureA::keep"), "cttir_answer")
  expect_s3_class(packages(), "data.frame")
  expect_gt(nrow(resources(limit = 5L)), 0L)
  report <- audit(p$path)
  expect_false(report$overall_status %in% c("fail", "not_tested"))
  expect_s3_class(doctor(p$path), "cttir_audit")
  expect_equal(setup(dry_run = TRUE)$state, "planned")
  blocked <- setup(offline = TRUE)
  expect_equal(blocked$state, "blocked")
  expect_match(blocked$blockers, "acquisition is disabled", fixed = TRUE)
  expect_identical(attempts$calls, character())
})
