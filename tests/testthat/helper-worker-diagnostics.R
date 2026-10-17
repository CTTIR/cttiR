# Test-only diagnostics; original worker function, arguments and result are retained.
with_original_worker_diagnostics <- function(code) {
  original_r_bg <- callr::r_bg
  wrapped_r_bg <- function(func, args = list(), ...) {
    dots <- list(...)
    if (!is.null(dots$stderr)) stop("This diagnostic wrapper owns its stderr destination")
    directory <- tempfile("cttir-worker-diagnostic-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
    if (!dir.create(directory, mode = "0700")) stop("Cannot create owned diagnostic directory")
    error_file <- file.path(directory, "stderr.log")
    if (!file.create(error_file)) stop("Cannot create owned diagnostic file")
    Sys.chmod(error_file, mode = "0600")
    cleanup <- function() {
      unlink(error_file)
      if (dir.exists(directory) && !length(list.files(directory, all.files = TRUE, no.. = TRUE)))
        unlink(directory, recursive = TRUE)
    }
    diagnostic_child <- function(original_function, original_args) {
      ns <- asNamespace("cttiR")
      tracer <- quote({
        tryCatch({
          cat("CTTIR_TEST_RENDER_CONDITION_BEGIN\n", file = stderr())
          current <- e
          for (depth in seq_len(8L)) {
            if (!inherits(current, "condition")) break
            cat("depth=", depth, " class=", substr(paste(class(current), collapse = ","), 1L, 1024L), "\n", sep = "", file = stderr())
            cat(substr(conditionMessage(current), 1L, 16384L), "\n", file = stderr())
            call <- conditionCall(current)
            if (!is.null(call)) cat(substr(paste(deparse(call, nlines = 32L), collapse = " "), 1L, 4096L), "\n", file = stderr())
            for (field in c("stdout", "stderr")) {
              text <- tryCatch(current[[field]], error = function(...) NULL)
              if (is.character(text)) cat(field, ": ", substr(paste(text, collapse = "\n"), 1L, 32768L), "\n", sep = "", file = stderr())
            }
            current <- tryCatch(current$parent, error = function(...) NULL)
          }
          cat("CTTIR_TEST_RENDER_CONDITION_END\n", file = stderr())
        }, error = function(trace_error) cat("CTTIR_TEST_DIAGNOSTIC_CAPTURE_FAILED\n", file = stderr()))
      })
      trace("app_condition_payload", tracer = tracer, print = FALSE, where = ns)
      on.exit(untrace("app_condition_payload", where = ns), add = TRUE)
      do.call(original_function, original_args)
    }
    dots$stderr <- error_file
    process <- tryCatch(do.call(original_r_bg, c(list(func = diagnostic_child,
      args = list(original_function = func, original_args = args)), dots)),
      error = function(e) { cleanup(); stop(e) })
    proxy <- new.env(parent = emptyenv())
    for (name in ls(process, all.names = TRUE)) proxy[[name]] <- process[[name]]
    attr(proxy, "cttir_test_diagnostic_path") <- error_file
    forward <- function() {
      if (!file.exists(error_file)) return(invisible(NULL))
      size <- file.info(error_file)$size
      bytes <- readBin(error_file, what = "raw", n = 65536L)
      attr(proxy, "cttir_test_diagnostic_bytes") <- bytes
      attr(proxy, "cttir_test_diagnostic_original_size") <- size
      if (length(bytes)) cat(rawToChar(bytes), "\n", file = stderr())
      if (size > length(bytes)) cat("CTTIR_TEST_DIAGNOSTIC_TRUNCATED\n", file = stderr())
    }
    proxy$get_result <- function(...) {
      failed <- TRUE
      on.exit({
        if (!process$is_alive()) {
          tryCatch(if (failed) forward(), error = function(e) NULL)
          cleanup()
        }
      }, add = TRUE)
      result <- process$get_result(...)
      failed <- is.list(result) && identical(result$ok, FALSE)
      result
    }
    proxy$kill <- function(...) {
      value <- process$kill(...)
      if (!process$is_alive()) cleanup()
      value
    }
    # Never kill in a finalizer; only remove our file after the owned child ended.
    reg.finalizer(proxy, function(e) { if (!process$is_alive()) cleanup() }, onexit = TRUE)
    proxy
  }
  testthat::with_mocked_bindings(code, r_bg = wrapped_r_bg, .package = "callr")
}
