# Bounded background execution for the local application. Every operation is
# an exported domain function run in a separate R process with serialized
# immutable inputs; the Shiny session only polls, cancels read-only work and
# renders the returned values.

app_operations <- c("project", "sync", "ask", "search", "resources", "packages", "update",
  "rollback_knowledge", "audit", "setup")

app_error_field <- function(field) {
  if (is.character(field) && length(field) >= 1L && !is.na(field[[1]])) return(field[[1]])
  if (is.data.frame(field) && nrow(field) && "instancePath" %in% names(field)) {
    return(as.character(field$instancePath[[1]]))
  }
  NULL
}

# Safe, serializable failure description. Unexpected errors never expose their
# message, call or stack in the browser.
app_condition_payload <- function(e) {
  if (!inherits(e, "cttir_error")) {
    return(list(ok = FALSE, message = NULL, code = "unexpected_error", field = NULL, remediation = NULL, class = "error"))
  }
  text <- function(x) if (is.character(x) && length(x) >= 1L && !is.na(x[[1]])) x[[1]] else NULL
  list(ok = FALSE, message = conditionMessage(e), code = if (is.null(text(e$code))) "invalid_input" else text(e$code),
    field = app_error_field(e$field), remediation = text(e$remediation), class = class(e)[[1]])
}

# Runs inside the worker process (or in-process in tests).
app_worker_run <- function(operation, args) {
  if (!is.character(operation) || length(operation) != 1L || !operation %in% app_operations) {
    return(list(ok = FALSE, message = "Unknown application operation.", code = "invalid_input", class = "cttir_input_error"))
  }
  expected <- attr(args, "cttir_catalog_fingerprint")
  attr(args, "cttir_catalog_fingerprint") <- NULL
  expected_plan <- attr(args, "cttir_sync_plan")
  attr(args, "cttir_sync_plan") <- NULL
  target <- getExportedValue("cttiR", operation)
  if (operation == "project" && !is.null(expected)) {
    target <- project_impl
    args$expected_catalog <- expected
  }
  if (operation == "sync" && !is.null(expected_plan)) {
    target <- sync_impl
    args$expected_plan <- expected_plan
  }
  tryCatch(list(ok = TRUE, value = do.call(target, args)), error = app_condition_payload)
}

app_worker <- function(operation, args) {
  if (!operation %in% app_operations) abort_cttir("Unknown application operation.")
  keys <- c("cttiR.catalog_dir", "cttiR.sources", "cttiR.runtime_dir", "cttiR.ollama_endpoint")
  settings <- options()[intersect(names(options()), keys)]
  callr::r_bg(
    function(operation, args, settings, version) {
      options(settings)
      if (!identical(as.character(utils::packageVersion("cttiR")), version)) {
        message <- "The installed cttiR version differs from the running application. Reinstall the current package."
        return(list(ok = FALSE, message = message, code = "version_mismatch", class = "cttir_api_mismatch"))
      }
      utils::getFromNamespace("app_worker_run", "cttiR")(operation, args)
    },
    args = list(operation, args, settings, as.character(utils::packageVersion("cttiR"))),
    libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE, supervise = FALSE
  )
}

# Session-wide cap on concurrent worker processes.
app_job_pool <- function(worker = app_worker, max_jobs = 3L) {
  pool <- new.env(parent = emptyenv())
  pool$worker <- worker
  pool$max_jobs <- max_jobs
  pool$active <- 0L
  pool
}

# One job slot per view: duplicate submissions are refused, read-only work can
# be cancelled and a write is never interrupted by the interface.
app_job_slot <- function(pool, session = shiny::getDefaultReactiveDomain(), timeout = 900) {
  state <- shiny::reactiveValues(job = NULL)
  finish <- function() {
    pool$active <- max(0L, pool$active - 1L)
    state$job <- NULL
  }
  start <- function(operation, args, label, done, mutating = FALSE) {
    if (!is.null(shiny::isolate(state$job))) return("busy")
    if (pool$active >= pool$max_jobs) return("capacity")
    process <- pool$worker(operation, args)
    pool$active <- pool$active + 1L
    state$job <- list(process = process, operation = operation, label = label, mutating = mutating,
      started = Sys.time(), done = done)
    "started"
  }
  cancel <- function() {
    job <- shiny::isolate(state$job)
    if (is.null(job)) return("idle")
    if (job$mutating) return("mutating")
    try(job$process$kill(), silent = TRUE)
    finish()
    "cancelled"
  }
  shiny::observe({
    job <- state$job
    if (is.null(job)) return()
    alive <- tryCatch(isTRUE(job$process$is_alive()), error = function(e) FALSE)
    if (alive) {
      waited <- as.numeric(difftime(Sys.time(), job$started, units = "secs"))
      if (!job$mutating && waited > timeout) {
        try(job$process$kill(), silent = TRUE)
        shiny::isolate(finish())
        shiny::isolate(job$done(list(ok = FALSE, code = "timeout")))
        return()
      }
      shiny::invalidateLater(150, session)
      return()
    }
    result <- tryCatch(job$process$get_result(), error = function(e) list(ok = FALSE, code = "worker_stopped"))
    if (!is.list(result) || is.null(result$ok)) result <- list(ok = FALSE, code = "worker_stopped")
    shiny::isolate(finish())
    shiny::isolate(job$done(result))
  })
  if (!is.null(session)) {
    session$onSessionEnded(function() {
      job <- shiny::isolate(state$job)
      if (!is.null(job) && !job$mutating) try(job$process$kill(), silent = TRUE)
    })
  }
  list(state = state, start = start, cancel = cancel, busy = function() !is.null(shiny::isolate(state$job)))
}

# Status records keep message keys so a language switch re-renders them.
app_status <- function(kind = "info", key = "status.ready", detail = NULL, remediation = NULL, ...) {
  list(kind = kind, key = key, args = list(...), detail = detail, remediation = remediation)
}

app_status_error <- function(result) {
  key <- switch(result$code %||% "",
    timeout = "error.timeout",
    worker_stopped = "error.worker_stopped",
    unexpected_error = "error.unexpected",
    version_mismatch = "error.version_mismatch",
    "error.operation"
  )
  app_status("error", key, detail = result$message, remediation = result$remediation)
}

app_condition_status <- function(e) app_status_error(app_condition_payload(e))

`%||%` <- function(x, y) if (is.null(x)) y else x

# Start a job and translate the slot outcome into a status record.
app_start_job <- function(slot, operation, args, label, done, mutating = FALSE) {
  outcome <- tryCatch(slot$start(operation, args, label, done, mutating),
    error = function(e) structure("failed", condition = e))
  switch(as.character(outcome),
    started = app_status("busy", label),
    busy = app_status("warning", "status.duplicate"),
    capacity = app_status("warning", "status.capacity"),
    app_condition_status(attr(outcome, "condition"))
  )
}

app_cancel_status <- function(slot) {
  switch(slot$cancel(),
    cancelled = app_status("info", "status.cancelled"),
    mutating = app_status("warning", "status.cannot_cancel"),
    app_status("info", "status.nothing_running")
  )
}
