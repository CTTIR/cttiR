# In-process stand-in for the background worker: same dispatch and error
# payloads as the callr worker, without a child process.
app_test_worker <- function(operation, args) {
  if (isFALSE(args$dry_run) && operation == "project") expect_type(attr(args, "cttir_catalog_fingerprint"), "character")
  if (isFALSE(args$dry_run) && operation == "sync") expect_type(attr(args, "cttir_sync_plan"), "character")
  result <- app_worker_run(operation, args)
  list(is_alive = function() FALSE, get_result = function() result, kill = function() TRUE)
}

# A worker that stays alive until released, recording calls and kills.
app_pending_worker <- function(control = new.env(parent = emptyenv())) {
  control$count <- 0L
  control$alive <- TRUE
  control$killed <- 0L
  control$calls <- list()
  worker <- function(operation, args) {
    control$count <- control$count + 1L
    control$calls[[length(control$calls) + 1L]] <- list(operation = operation, args = args)
    control$alive <- TRUE
    list(is_alive = function() control$alive,
      kill = function() {
        control$killed <- control$killed + 1L
        control$alive <- FALSE
      },
      get_result = function() control$result %||% list(ok = TRUE, value = NULL))
  }
  list(worker = worker, control = control)
}

app_test_args <- function(worker = app_test_worker, ...) {
  list(pool = app_job_pool(worker), lang = shiny::reactive("en"), ...)
}

app_status_message <- function(status) app_status_text(status, "en")

# Plan hash over paths and content hashes, independent of row order.
app_plan_hash <- function(plan) {
  plan <- plan[order(plan$path), c("path", "sha256")]
  content_hash(json_text(plan))
}

# Rebuild the file plan of an accepted spec through the R API builder.
app_api_plan <- function(spec) {
  files <- project_bundle(spec)$files
  data.frame(path = names(files), sha256 = vapply(files, content_hash, character(1)), stringsAsFactors = FALSE, row.names = NULL)
}
