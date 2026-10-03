.owned_runtimes <- new.env(parent = emptyenv())

runtime_directory <- function() {
  root <- getOption("cttiR.runtime_dir", file.path(tools::R_user_dir("cttiR", "data"), "runtime"))
  scalar_text(root, "runtime directory")
  assert_plain_path(root)
  root
}

runtime_endpoint <- function() {
  endpoint <- getOption("cttiR.ollama_endpoint", "http://127.0.0.1:11434")
  scalar_text(endpoint, "runtime endpoint")
  if (!grepl("^http://127\\.0\\.0\\.1:[0-9]{1,5}$", endpoint)) {
    abort_cttir("Runtime endpoints must use explicit IPv4 loopback HTTP and a port.")
  }
  port <- as.integer(sub(".*:", "", endpoint))
  if (port < 1L || port > 65535L) abort_cttir("Invalid runtime port.")
  endpoint
}

runtime_request <- function(endpoint, route, body = NULL, timeout = 10) {
  if (!grepl("^http://127\\.0\\.0\\.1:[0-9]{1,5}$", endpoint) ||
      !route %in% c("version", "tags", "show", "pull", "chat")) {
    abort_cttir("Invalid local runtime request.")
  }
  request <- httr2::request(paste0(endpoint, "/api/", route))
  request <- httr2::req_options(request, proxy = "", followlocation = FALSE, connecttimeout = 3, maxfilesize = 1048576)
  request <- httr2::req_timeout(request, timeout)
  if (!is.null(body)) request <- httr2::req_body_json(request, body, auto_unbox = TRUE)
  tryCatch(
    {
      response <- httr2::req_perform(request)
      if (length(httr2::resp_body_raw(response)) > 1048576L) {
        abort_cttir("Runtime response exceeded the size limit.", "cttir_runtime_unavailable")
      }
      httr2::resp_body_json(response, simplifyVector = FALSE)
    },
    error = function(e) abort_cttir("The local runtime request failed.", "cttir_runtime_unavailable", "runtime_request")
  )
}

runtime_owner <- function(root, endpoint) {
  file <- file.path(root, "runtime-state.json")
  assert_plain_path(file)
  if (!file.exists(file)) {
    return(NULL)
  }
  state <- read_document(file)
  if (!identical(state$endpoint, endpoint) || !identical(state$host, Sys.info()[["nodename"]]) ||
      !identical(state$locality, "managed_cloud_disabled")) {
    return(NULL)
  }
  valid <- tryCatch(
    {
      handle <- ps::ps_handle(state$pid)
      args <- ps::ps_cmdline(handle)
      environment <- ps::ps_environ(handle)
      manifest <- read_document(resource_file("runtime", "manifest.json"))
      isTRUE(ps::ps_is_running(handle)) && state$executable %in% args &&
        runtime_executable_matches(state$pid, state$executable) &&
        "serve" %in% args && identical(unname(environment[["OLLAMA_NO_CLOUD"]]), "1") &&
        identical(unname(environment[["OLLAMA_HOST"]]), endpoint) &&
        identical(state$binary_sha256, manifest$binary_sha256) &&
        identical(digest::digest(file = state$executable, algo = "sha256"), state$binary_sha256)
    },
    error = function(e) FALSE
  )
  if (valid) state else NULL
}

acquire_runtime <- function(root, manifest) {
  if (!identical(Sys.info()[["sysname"]], manifest$platform) ||
      !identical(Sys.info()[["machine"]], manifest$arch)) {
    abort_cttir("Portable acquisition is currently verified only for Linux x86_64.", "cttir_runtime_unavailable", "platform_unverified")
  }
  if (!nzchar(Sys.which("tar")) || !nzchar(Sys.which("zstd"))) {
    abort_cttir("Portable acquisition requires tar with zstd support.", "cttir_runtime_unavailable", "archive_tools_missing")
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(root, "ollama-linux-amd64.tar.zst")
  assert_plain_path(archive)
  intact <- file.exists(archive) && identical(digest::digest(file = archive, algo = "sha256"), manifest$archive_sha256)
  if (file.exists(archive) && !intact) {
    abort_cttir("An existing unverified archive requires manual review.", "cttir_path_conflict")
  }
  if (!intact) {
    staged <- tempfile("ollama-download-", tmpdir = root)
    on.exit(unlink(staged), add = TRUE)
    request <- httr2::request(manifest$archive_url)
    request <- httr2::req_timeout(request, 900)
    request <- httr2::req_options(request, maxfilesize = manifest$archive_bytes)
    httr2::req_perform(request, path = staged)
    if (!identical(digest::digest(file = staged, algo = "sha256"), manifest$archive_sha256)) {
      abort_cttir("Publisher archive checksum mismatch.", "cttir_runtime_unavailable", "checksum_mismatch")
    }
    if (file.exists(archive)) abort_cttir("An existing unverified archive requires manual review.", "cttir_path_conflict")
    if (!file.rename(staged, archive)) abort_cttir("Could not retain the verified runtime archive.", "cttir_path_conflict")
  }
  # The pinned publisher digest and audited member list bind this extraction to
  # the reviewed release, including its internal library symlinks.
  members <- utils::untar(archive, list = TRUE, tar = Sys.which("tar"))
  if (!identical(sort(sub("/+$", "", members)), sort(sub("/+$", "", unlist(manifest$archive_members))))) {
    abort_cttir("Runtime archive members differ from the audited release.", "cttir_runtime_unavailable")
  }
  stage <- tempfile("ollama-stage-", tmpdir = root)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  status <- utils::untar(archive, exdir = stage, tar = Sys.which("tar"))
  executable <- file.path(stage, "bin", "ollama")
  if (!identical(as.integer(status), 0L) || !file.exists(executable) ||
      !identical(digest::digest(file = executable, algo = "sha256"), manifest$binary_sha256)) {
    abort_cttir("Extracted runtime failed verification.", "cttir_runtime_unavailable")
  }
  target <- file.path(root, paste0("ollama-", manifest$runtime_version))
  if (file.exists(target) || !file.rename(stage, target)) {
    abort_cttir("Runtime installation destination already exists or cannot be created.", "cttir_path_conflict")
  }
  file.path(target, "bin", "ollama")
}

# Plain local model tags only: name[:tag]. Registry hosts, namespaces, URLs and
# cloud-backed tags are refused before any request.
valid_model_tag <- function(model) {
  is.character(model) && length(model) == 1L && !is.na(model) &&
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}(:[A-Za-z0-9][A-Za-z0-9._-]{0,127})?$", model) &&
    !grepl("cloud", model, ignore.case = TRUE)
}

# On Linux the kernel's view of the process image must be the owned binary; a
# process that merely names that path among its arguments does not qualify.
runtime_executable_matches <- function(pid, executable) {
  if (!identical(Sys.info()[["sysname"]], "Linux")) return(TRUE)
  image <- Sys.readlink(file.path("/proc", as.integer(pid), "exe"))
  !is.na(image) && nzchar(image) &&
    identical(normalizePath(image, mustWork = FALSE), normalizePath(executable, mustWork = FALSE))
}

local_model <- function(endpoint, model) {
  tags <- runtime_request(endpoint, "tags")$models
  names <- vapply(tags, function(x) x$name, character(1))
  index <- match(model, names)
  if (is.na(index)) {
    return(NULL)
  }
  entry <- tags[[index]]
  if (!identical(entry$details$format, "gguf") || !is.numeric(entry$size) || entry$size <= 0 ||
      !is.character(entry$digest) || !grepl("^[a-f0-9]{64}$", entry$digest) ||
      !is.null(entry$remote_host) || !is.null(entry$remote_model) || grepl("cloud", entry$name, ignore.case = TRUE)) {
    abort_cttir("Model metadata does not establish a local downloaded model.", "cttir_runtime_unavailable", "locality_unverified")
  }
  entry
}

#' Explicitly prepare an owned local runtime
#'
#' Acquisition uses a pinned publisher archive and checksum, currently for Linux
#' x86_64 only. No system services, existing daemons or package libraries are
#' modified. The runtime uses a retained user data directory and disables cloud
#' execution. Unsupported platforms return blockers; they are not claimed tested.
#' Calling this function explicitly permits its reported downloads and startup.
#' Dry runs never create directories or contact a daemon. Offline mode reuses
#' verified local artifacts and never pulls a model.
#' @param model Plain local model tag (`name` or `name:tag`, no registry host,
#'   namespace or URL), or `auto` for the smallest qualified model whose tested
#'   settings and recorded CPU, available RAM and disk budgets fit. Unknown or
#'   insufficient resources block automatic acquisition. Budgets are conservative
#'   admission checks, not reserved resources or process memory limits.
#' @param install_ollama Allow portable acquisition when the runtime is absent.
#' @param offline Forbid acquisition and model downloads.
#' @param dry_run Return the plan without persistent changes or HTTP requests.
#' @return A `cttir_setup` with steps, runtime/model identity, actions and blockers.
#'   The offline `preflight` reports acquisition and planning blockers separately,
#'   published full download sizes (unknown for unrecorded models), available disk
#'   space, recorded admission budgets and verified local process ownership when
#'   available. File presence alone does not establish integrity or readiness.
#'   Explicit unqualified model preparation remains possible; it does not qualify
#'   that model for planning.
#' @details Machine preferences may set `options(cttiR.runtime_dir = path)` and
#'   `options(cttiR.ollama_endpoint = "http://127.0.0.1:11434")`. Other endpoints
#'   are rejected. The local HTTP client disables proxies and redirects.
#' @export
setup <- function(model = "auto", install_ollama = TRUE, offline = FALSE, dry_run = FALSE) {
  scalar_text(model, "model")
  for (key in c("install_ollama", "offline", "dry_run")) scalar_flag(get(key), key)
  if (!valid_model_tag(model)) {
    abort_cttir("Supply a plain local model tag (name or name:tag) without cloud execution.")
  }
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  automatic <- model == "auto"
  root <- runtime_directory()
  selection <- if (automatic) runtime_select_model(manifest, root) else NULL
  if (automatic) model <- selection$model
  # Planner qualification comes from the recorded benchmark; overrides that
  # were never benchmarked are labelled unvalidated.
  tested <- Filter(function(x) identical(x$tag, model), manifest$tested_models)
  expected_digest <- if (automatic) selection$digest else if (length(tested)) tested[[1]]$digest else NULL
  validation <- planner_qualification(model, expected_digest, manifest)
  endpoint <- runtime_endpoint()
  result <- structure(list(
    state = "planned", steps = list(),
    runtime = list(version = manifest$runtime_version, endpoint = endpoint),
    model = list(name = model, validation = validation),
    actions = c("verify runtime", "start owned local daemon if absent", "verify or acquire local model"),
    blockers = if (automatic) selection$blockers else character(),
    selection = selection
  ), class = "cttir_setup")
  if (length(result$blockers)) result$actions <- character()
  result$preflight <- runtime_preflight(root, endpoint, manifest, model, validation, install_ollama, offline,
    resources = if (!is.null(selection$resources)) selection$resources else runtime_resources(root))
  if (dry_run) {
    result$blockers <- unique(c(result$blockers, result$preflight$acquisition_blockers,
        result$preflight$planner_blockers))
    if (length(result$preflight$acquisition_blockers)) result$actions <- character()
    return(result)
  }
  if (length(result$blockers)) {
    result$state <- "blocked"
    return(result)
  }
  process <- NULL
  tryCatch(
    {
      if (automatic) {
        fresh <- runtime_select_model(manifest, root)
        if (length(fresh$blockers) || !identical(fresh$model, model)) {
          abort_cttir("Automatic model resource headroom changed; retry setup after reviewing available resources.", "cttir_runtime_unavailable")
        }
        result$selection <- fresh
      }
      dir.create(root, recursive = TRUE, showWarnings = FALSE)
      lock <- file.path(root, "setup-lock")
      assert_plain_path(lock)
      if (!dir.create(lock, showWarnings = FALSE)) {
        abort_cttir("Runtime setup is already locked; inspect any interrupted setup before removing the lock.", "cttir_transaction_conflict")
      }
      on.exit(unlink(lock, recursive = TRUE), add = TRUE)
      write_bytes(paste0(json_text(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]])), "\n"), file.path(lock, "owner.json"))
      assert_plain_path(file.path(root, "server.log"))
      assert_plain_path(file.path(root, "models"))
      owner <- runtime_owner(root, endpoint)
      if (is.null(owner)) {
        executable <- file.path(root, paste0("ollama-", manifest$runtime_version), "bin", "ollama")
        assert_plain_path(executable)
        # Nothing could be started without a verified runtime, so block before any
        # request, including the loopback port probe.
        if (!file.exists(executable) && (offline || !install_ollama)) {
          abort_cttir("The verified runtime is absent and acquisition is disabled.", "cttir_runtime_unavailable")
        }
        reachable <- tryCatch(
          {
            runtime_request(endpoint, "version", timeout = 2)
            TRUE
          },
          error = function(e) FALSE
        )
        if (reachable) abort_cttir("The port belongs to an unmanaged daemon; choose an unused loopback port.", "cttir_runtime_unavailable", "unmanaged_daemon")
        if (!file.exists(executable)) executable <- acquire_runtime(root, manifest)
        if (!identical(digest::digest(file = executable, algo = "sha256"), manifest$binary_sha256)) {
          abort_cttir("Runtime binary checksum mismatch.", "cttir_runtime_unavailable")
        }
        executable <- normalizePath(executable, mustWork = TRUE)
        dir.create(file.path(root, "models"), recursive = TRUE, showWarnings = FALSE)
        process <- processx::process$new(executable, "serve",
          env = c("current",
            OLLAMA_NO_CLOUD = "1", OLLAMA_HOST = endpoint,
            OLLAMA_MODELS = normalizePath(file.path(root, "models")),
            OLLAMA_MAX_LOADED_MODELS = "1", OLLAMA_NUM_PARALLEL = "1"
          ),
          stdout = file.path(root, "server.log"), stderr = "2>&1", cleanup = FALSE
        )
        owner <- list(
          pid = process$get_pid(), host = Sys.info()[["nodename"]], executable = executable,
          binary_sha256 = manifest$binary_sha256, endpoint = endpoint, locality = "managed_cloud_disabled"
        )
        assign(as.character(owner$pid), process, envir = .owned_runtimes)
        write_bytes(paste0(json_text(owner, TRUE), "\n"), file.path(root, "runtime-state.json"))
        ready <- FALSE
        for (i in seq_len(40L)) {
          ready <- tryCatch(
            {
              runtime_request(endpoint, "version", timeout = 1)
              TRUE
            },
            error = function(e) FALSE
          )
          if (ready || !process$is_alive()) break
          Sys.sleep(0.25)
        }
        if (!ready) abort_cttir("Owned runtime did not become ready.", "cttir_runtime_unavailable")
      }
      entry <- local_model(endpoint, model)
      if (is.null(entry)) {
        if (offline) abort_cttir("The requested model is absent in offline mode.", "cttir_runtime_unavailable")
        runtime_request(endpoint, "pull", list(model = model, stream = FALSE), timeout = 900)
        entry <- local_model(endpoint, model)
      }
      if (is.null(entry)) abort_cttir("The downloaded model could not be verified.", "cttir_runtime_unavailable")
      if (automatic && !identical(entry$digest, expected_digest)) {
        abort_cttir("The automatic model tag changed digest; review is required.", "cttir_runtime_unavailable", "model_digest_mismatch")
      }
      if (automatic) {
        available <- runtime_resources(root)$available_memory_bytes
        required <- result$selection$requirements$available_memory_bytes
        if (!is.numeric(available) || length(available) != 1L || !is.finite(available) ||
            !is.numeric(required) || length(required) != 1L || !is.finite(required) || available < required) {
          abort_cttir("Automatic model memory headroom changed before inference; retry after reviewing available resources.",
            "cttir_runtime_unavailable")
        }
      }
      probe <- runtime_probe(endpoint, model)
      owner$selection <- result$selection
      owner$model <- model
      owner$model_digest <- entry$digest
      owner$probe <- probe
      write_bytes(paste0(json_text(owner, TRUE), "\n"), file.path(root, "runtime-state.json"))
      result$state <- "runtime_ready"
      # Re-evaluate the actual acquired digest; a mutable explicit tag must not
      # inherit an older digest's positive qualification.
      validation <- planner_qualification(model, entry$digest, manifest)
      result$model$validation <- validation
      result$model$digest <- entry$digest
      result$runtime$pid <- owner$pid
      result$runtime$host <- owner$host
      result$runtime$locality <- owner$locality
      result$steps <- list(
        list(id = "local_runtime", state = "verified"), list(id = "local_model", state = "verified"),
        list(id = "structured_inference", state = "verified", evidence = probe)
      )
      result$blockers <- switch(validation, qualified_for_planning = character(),
        candidate_pending_benchmark = "workflow_model_benchmark_pending", "workflow_model_not_qualified")
      result
    },
    error = function(e) {
      if (!is.null(process) && process$is_alive()) process$kill()
      result$state <- "blocked"
      result$blockers <- if (inherits(e, "cttir_error")) conditionMessage(e) else "Runtime setup failed; inspect local prerequisites and retained artifacts."
      result
    }
  )
}

runtime_probe <- function(endpoint, model) {
  schema <- list(
    type = "object", properties = list(status = list(type = "string", enum = list("ready"))),
    required = list("status"), additionalProperties = FALSE
  )
  started <- proc.time()[["elapsed"]]
  response <- runtime_request(endpoint, "chat", list(
    model = model, stream = FALSE,
    messages = list(list(role = "user", content = 'Return exactly {"status":"ready"}.')),
    format = schema, keep_alive = 0,
    options = list(temperature = 0, seed = 1L, num_ctx = 2048L, num_predict = 32L, num_gpu = 0L, num_thread = 2L)
  ), timeout = 120)
  content <- response$message$content
  valid <- isTRUE(response$done) && identical(response$model, model) && is.character(content) &&
    length(content) == 1L && is.null(response$message$tool_calls) &&
    isTRUE(json_schema_validate(content, json_text(schema), engine = "ajv"))
  if (!valid) abort_cttir("The model did not pass the structured-output probe.", "cttir_runtime_unavailable", "probe_failed")
  list(
    state = "pass", elapsed_seconds = unname(proc.time()[["elapsed"]] - started),
    processor = "CPU", context = 2048L, max_output_tokens = 32L,
    scope = "structured_output_smoke_only"
  )
}

#' @export
print.cttir_setup <- function(x, ...) {
  cat("Local runtime: ", x$state, "\n", sep = "")
  if (!is.null(x$preflight)) {
    bytes <- function(value) if (is.null(value) || length(value) != 1L || is.na(value)) "unknown" else format(value, scientific = FALSE)
    cat("Full download sizes (bytes): runtime ", bytes(x$preflight$downloads$runtime_archive_bytes),
      "; model ", bytes(x$preflight$downloads$model_bytes), "\n", sep = "")
    cat("Disk (bytes): available ", bytes(x$preflight$disk$available_bytes),
      "; admission screen ", bytes(x$preflight$disk$admission_required_bytes), "\n", sep = "")
    owner <- if (!is.null(x$runtime$pid)) x$runtime else x$preflight$owner
    if (!is.null(owner)) cat("Owned runtime: PID ", owner$pid, " on ", owner$host, "\n", sep = "")
  }
  if (length(x$blockers)) cat(paste(x$blockers, collapse = "\n"), "\n")
  invisible(x)
}
