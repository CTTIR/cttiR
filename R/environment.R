# Dependency environment (renv) and optional Git initialization for generated
# projects. Both run only on explicit request, after the scaffold exists, and
# never inside the caller's R session: renv runs in a fresh callr process and Git
# through processx. Readiness is computed from files on disk, never assumed.

base_r_packages <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
  "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")

# Packages shipped with R are pinned by the R version, not installed per project.
environment_requirements <- function(dependencies) {
  rows <- list()
  for (dep in dependencies) {
    if (!is.character(dep$package) || length(dep$package) != 1L || dep$package %in% base_r_packages) next
    rows[[dep$package]] <- list(package = dep$package, pinned = dep$version)
  }
  rows[sort(names(rows), method = "radix")]
}

environment_recovery <- function(root, reason, missing = character()) {
  sync_call <- function(options = NULL) {
    sprintf("cttiR::sync(%s,%s dry_run = FALSE)", deparse(root),
      if (is.null(options)) "" else paste0(" options = ", options, ","))
  }
  prepare <- 'list(workflow = list(environment = "renv", prepare_environment = TRUE))'
  install <- if (length(missing)) sprintf("install.packages(%s); ", deparse(as.character(missing))) else ""
  switch(reason,
    not_materialized = "Create the project first; environment preparation runs after the scaffold exists.",
    callr_unavailable = paste0('install.packages(c("callr", "renv")); ', sync_call(prepare)),
    renv_unavailable = paste0('install.packages("renv"); ', sync_call(prepare)),
    preparation_disabled = sync_call(prepare),
    packages_unavailable_offline = paste0(install, sync_call(prepare), "  # or allow downloads: ",
      sync_call('list(workflow = list(network = "allowed"))')),
    project_library_missing = ,
    library_out_of_sync = sprintf("renv::restore(project = %s, prompt = FALSE); %s", deparse(root), sync_call()),
    existing_rprofile = paste0("Review the existing .Rprofile (renv would append to it), add ",
      "source(\"renv/activate.R\") yourself or move the file, then run ", sync_call()),
    paste0(install, sync_call(prepare)))
}

read_renv_lock <- function(path) {
  assert_plain_path(path)
  if (!file.exists(path) || dir.exists(path) || file.info(path)$size > 33554432) {
    abort_cttir("renv.lock is missing, a directory or larger than 32 MiB.", "cttir_schema_error", "invalid_lockfile")
  }
  doc <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(doc) || !is.list(doc$Packages) || (length(doc$Packages) && is.null(names(doc$Packages)))) {
    abort_cttir("renv.lock is not a readable renv lockfile.", "cttir_schema_error", "invalid_lockfile")
  }
  versions <- vapply(doc$Packages, function(x) {
    if (is.list(x) && is.character(x$Version) && length(x$Version) == 1L) x$Version else NA_character_
  }, character(1))
  list(sha256 = digest::digest(file = path, algo = "sha256"), r_version = doc$R$Version,
    versions = versions[!is.na(versions)])
}

environment_record_path <- function(root) file.path(root, ".cttir", "environment.json")

read_environment_record <- function(root) {
  path <- environment_record_path(root)
  if (!file.exists(path)) return(NULL)
  tryCatch(read_document(path), error = function(e) NULL)
}

# Machine-local outcome of the last preparation; gitignored and outside the
# managed baseline, so sync never plans it.
write_environment_record <- function(root, result) {
  path <- environment_record_path(root)
  assert_plain_path(path)
  record <- c(list(schema_version = 1L), unclass(result))
  temporary <- tempfile(".environment-", tmpdir = dirname(path), fileext = ".json")
  write_bytes(paste0(json_text(record, TRUE), "\n"), temporary)
  if (!file.rename(temporary, path)) {
    unlink(temporary)
    abort_cttir("Could not record the environment outcome.", "cttir_transaction_conflict", "write_failed")
  }
  invisible(path)
}

# The renv project library for this R version: <root>/renv/library/<os>/R-x.y/<platform>.
project_renv_library <- function(root, recorded = NULL) {
  if (is.character(recorded) && length(recorded) == 1L && dir.exists(recorded)) return(recorded)
  base <- file.path(root, "renv", "library")
  if (!dir.exists(base)) return(NULL)
  version <- paste0("R-", R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][[1]])
  for (dir in sort(list.dirs(base, recursive = FALSE), method = "radix")) {
    candidate <- file.path(dir, version, R.version$platform)
    if (dir.exists(candidate)) return(candidate)
  }
  NULL
}

library_version <- function(library, package) {
  file <- file.path(library, package, "DESCRIPTION")
  if (is.null(library) || !file.exists(file)) return(NA_character_)
  version <- tryCatch(read.dcf(file, fields = "Version")[1, 1], error = function(e) NA_character_)
  unname(version)
}

# Ready only when renv.lock records every required package at its pinned version
# (unpinned packages take the recorded version) and the project library on this
# machine holds exactly the recorded versions. Reads metadata only.
renv_environment_status <- function(dependencies, root = NULL, library = NULL) {
  required <- environment_requirements(dependencies)
  status <- list(state = "environment_pending", mode = "renv", reason = NULL, lockfile = "renv.lock",
    lockfile_sha256 = NULL, library = NULL, library_in_sync = FALSE, dependencies = list(),
    missing = list(), mismatched = list(), unpinned = list(), last_preparation = NULL, recovery = NULL)
  pending <- function(reason, missing = character()) {
    status$reason <- reason
    status$recovery <- environment_recovery(if (is.null(root)) "." else root, reason, missing)
    status
  }
  if (is.null(root)) return(pending("not_materialized"))
  record <- read_environment_record(root)
  if (!is.null(record)) {
    status$last_preparation <- record[intersect(c("state", "reason", "network", "prepared_at", "lockfile_sha256"), names(record))]
  }
  lockfile <- file.path(root, "renv.lock")
  if (!file.exists(lockfile)) return(pending("lockfile_missing"))
  lock <- tryCatch(read_renv_lock(lockfile), error = function(e) NULL)
  if (is.null(lock)) return(pending("lockfile_unreadable"))
  status$lockfile_sha256 <- lock$sha256
  library <- project_renv_library(root, if (is.null(library)) record[["library"]] else library)
  status["library"] <- list(library)
  missing <- character()
  mismatched <- character()
  unsynced <- character()
  for (dep in required) {
    recorded <- unname(lock$versions[dep$package])
    installed <- library_version(library, dep$package)
    status$dependencies[[length(status$dependencies) + 1L]] <- list(package = dep$package,
      pinned = dep$pinned, recorded = if (is.na(recorded)) NULL else recorded,
      installed = if (is.na(installed)) NULL else installed)
    if (is.na(recorded)) {
      missing <- c(missing, dep$package)
      next
    }
    if (is.null(dep$pinned)) status$unpinned[[length(status$unpinned) + 1L]] <- dep$package
    if (!is.null(dep$pinned) && !identical(recorded, dep$pinned)) {
      mismatched <- c(mismatched, paste0(dep$package, " ", recorded, " != ", dep$pinned))
    }
    if (!identical(installed, recorded)) unsynced <- c(unsynced, dep$package)
  }
  status$missing <- as.list(missing)
  status$mismatched <- as.list(mismatched)
  status$library_in_sync <- !is.null(library) && !length(unsynced)
  if (length(missing)) return(pending("lockfile_incomplete", missing))
  if (length(mismatched)) return(pending("pin_mismatch"))
  if (is.null(library)) return(pending("project_library_missing"))
  if (length(unsynced)) return(pending("library_out_of_sync"))
  status$state <- "environment_ready"
  status
}

environment_footprint <- function(root) {
  top <- list.files(root, all.files = TRUE, no.. = TRUE)
  inner <- list.files(file.path(root, "renv"), all.files = TRUE, no.. = TRUE)
  sort(c(top, if (length(inner)) file.path("renv", inner)), method = "radix")
}

# Files renv may append to; their hashes show whether preparation edited them.
environment_touchable <- function(root) {
  files <- c(".Rprofile", ".Rbuildignore", ".gitignore")
  stats::setNames(vapply(files, function(f) {
    path <- file.path(root, f)
    if (file.exists(path)) digest::digest(file = path, algo = "sha256") else NA_character_
  }, character(1)), files)
}

# renv routes every download through `renv.download.override` when it is set.
# Offline, this refuses and logs each attempt before any connection is opened.
# Its environment only holds the log path, so it serializes to a fresh process.
renv_offline_override <- function(guard) {
  override <- function(url, destfile, ...) {
    cat(url, "\n", sep = "", file = guard, append = TRUE)
    stop("Network access is disabled (workflow.network = 'offline'); refused: ", url, call. = FALSE)
  }
  environment(override) <- list2env(list(guard = guard), parent = baseenv())
  override
}

# Runs in a fresh R process (callr); it must only use base R and namespaced calls.
environment_worker <- function(root, packages, pins, sources, repos, override) {
  offline <- is.function(override)
  options(renv.consent = TRUE, renv.config.ppm.enabled = FALSE, renv.config.pak.enabled = FALSE,
    renv.config.updates.check = FALSE, renv.config.synchronized.check = FALSE, repos = repos,
    renv.download.override = override)
  installed <- utils::installed.packages(lib.loc = sources)
  installed <- installed[!duplicated(installed[, "Package"]), , drop = FALSE]
  base <- rownames(utils::installed.packages(lib.loc = .Library, priority = "base"))
  closure <- tools::package_dependencies(packages, db = installed, which = c("Depends", "Imports", "LinkingTo"),
    recursive = TRUE)
  needed <- setdiff(unique(c(packages, unlist(closure, use.names = FALSE))), c(base, "R"))
  missing <- setdiff(needed, installed[, "Package"])
  version <- function(p) unname(installed[match(p, installed[, "Package"]), "Version"])
  wrong <- character()
  for (p in names(pins)) {
    if (!is.null(pins[[p]]) && p %in% installed[, "Package"] && !identical(version(p), pins[[p]])) wrong <- c(wrong, p)
  }
  incomplete <- packages[vapply(packages, function(p) {
    p %in% c(missing, wrong) || any(closure[[p]] %in% missing)
  }, logical(1))]
  if (offline && length(incomplete)) {
    return(list(status = "unavailable_offline", missing = missing, wrong_version = wrong))
  }
  renv::init(project = root, bare = TRUE, restart = FALSE, load = FALSE, repos = repos,
    settings = list(snapshot.type = "implicit"))
  library <- renv::paths$library(project = root)
  local <- setdiff(packages, incomplete)
  if (length(local)) {
    renv::hydrate(packages = local, library = library, sources = sources, update = FALSE,
      prompt = FALSE, report = FALSE, project = root)
  }
  requested <- character()
  if (length(incomplete)) {
    requested <- vapply(incomplete, function(p) if (is.null(pins[[p]])) p else paste0(p, "@", pins[[p]]), character(1))
    renv::install(packages = unname(requested), library = library, repos = repos, prompt = FALSE, project = root)
  }
  present <- utils::installed.packages(lib.loc = c(library, .Library))[, "Package"]
  absent <- setdiff(packages, present)
  if (length(absent)) {
    return(list(status = "install_incomplete", missing = absent, wrong_version = character(), library = library))
  }
  renv::snapshot(project = root, library = library, packages = packages, repos = repos, prompt = FALSE)
  list(status = "prepared", library = library, renv_version = as.character(utils::packageVersion("renv")),
    hydrated = local, installed = unname(requested), missing = character(), wrong_version = character())
}

offline_process_env <- function() {
  blackhole <- "http://127.0.0.1:9"
  c(callr::rcmd_safe_env(), http_proxy = blackhole, https_proxy = blackhole, HTTP_PROXY = blackhole,
    HTTPS_PROXY = blackhole, ftp_proxy = blackhole, all_proxy = blackhole, ALL_PROXY = blackhole,
    no_proxy = "", NO_PROXY = "")
}

process_tail <- function(file, n = 20L) {
  if (!file.exists(file)) return(list())
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  as.list(utils::tail(lines[nzchar(trimws(lines))], n))
}

#' Prepare a project's renv environment in an isolated R process
#'
#' Initializes renv in `root` with `renv::init(bare = TRUE)` and populates the
#' project library with only `dependencies` and their recursive hard
#' dependencies. Offline, packages are hydrated (linked or copied) from
#' `libraries` and every renv download is refused; anything unavailable leaves
#' the environment pending without creating renv files. With
#' `network = "allowed"`, packages missing locally are installed from `repos`.
#' `renv::snapshot()` then writes `renv.lock` from the actually installed
#' versions. The caller's working directory, library paths, options and random
#' state are untouched. The outcome is recorded in `.cttir/environment.json`.
#' @param root Existing project root.
#' @param dependencies Lock dependency records (`package`, optional `version`).
#' @param network `"offline"` or `"allowed"`.
#' @param libraries Libraries to hydrate from; renv must be installed in one.
#' @param repos Repositories recorded in the lockfile and used when allowed.
#' @param timeout Seconds before the preparation process is stopped.
#' @return A `cttir_environment` list with `state` (`environment_ready` or
#'   `environment_pending`), `reason`, lockfile hash, pinned versus recorded
#'   versions, mismatches, created files, refused downloads and `recovery`.
#' @noRd
prepare_project_environment <- function(root, dependencies, network = "offline", libraries = .libPaths(),
  repos = getOption("repos"), timeout = 3600) {
  scalar_text(root, "root")
  assert_plain_path(root)
  if (!dir.exists(root)) abort_cttir("The project root must already exist.", "cttir_path_conflict")
  if (!identical(network, "offline") && !identical(network, "allowed")) {
    abort_cttir("network must be 'offline' or 'allowed'.", field = "/workflow/network")
  }
  if (!is.character(libraries) || !length(libraries) || anyNA(libraries)) abort_cttir("libraries must be library paths.")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  required <- environment_requirements(dependencies)
  started <- Sys.time()
  before <- environment_footprint(root)
  touched <- environment_touchable(root)
  outcome <- list(worker = NULL, error = NULL, attempts = character(), log = list())
  finish <- function(reason = NULL, missing = character(), library = NULL) {
    status <- renv_environment_status(dependencies, root, library)
    if (!is.null(reason)) {
      status$state <- "environment_pending"
      status$reason <- reason
      status$recovery <- environment_recovery(root, reason, missing)
      if (length(missing)) status$unavailable <- as.list(missing)
    }
    after <- environment_touchable(root)
    modified <- !is.na(touched) & (is.na(after) | after != touched)
    result <- c(status[setdiff(names(status), "last_preparation")], list(
      network = network, prepared_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      elapsed_seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 3),
      renv_version = outcome$worker$renv_version,
      requested = as.list(names(required)),
      hydrated = as.list(outcome$worker$hydrated), installed = as.list(outcome$worker$installed),
      files_created = as.list(setdiff(environment_footprint(root), before)),
      files_modified = as.list(names(after)[modified %in% TRUE]),
      refused_downloads = as.list(outcome$attempts),
      error = outcome$error, process_log = if (identical(status$state, "environment_ready")) list() else outcome$log))
    result <- structure(result, class = "cttir_environment")
    write_environment_record(root, result)
    result
  }
  if (!length(required)) return(finish("no_dependencies_recorded"))
  if (!requireNamespace("callr", quietly = TRUE)) return(finish("callr_unavailable"))
  if (!length(find.package("renv", lib.loc = libraries, quiet = TRUE))) return(finish("renv_unavailable"))
  rprofile <- file.path(root, ".Rprofile")
  if (file.exists(rprofile) && !any(grepl("source(\"renv/activate.R\")", readLines(rprofile, warn = FALSE), fixed = TRUE))) {
    return(finish("existing_rprofile"))
  }
  guard <- tempfile("cttir-network-guard-", fileext = ".log")
  log <- tempfile("cttir-environment-", fileext = ".log")
  on.exit(unlink(c(guard, log)), add = TRUE)
  pins <- lapply(required, function(x) x$pinned)
  outcome$worker <- tryCatch(
    callr::r(environment_worker,
      args = list(root = root, packages = names(required), pins = pins, sources = libraries, repos = repos,
        override = if (identical(network, "offline")) renv_offline_override(guard) else NULL),
      libpath = libraries, repos = repos, stdout = log, stderr = "2>&1", user_profile = FALSE,
      system_profile = FALSE, package = FALSE, timeout = timeout, wd = tempdir(),
      env = if (identical(network, "offline")) offline_process_env() else callr::rcmd_safe_env()),
    error = function(e) {
      outcome$error <<- conditionMessage(e)
      NULL
    })
  if (file.exists(guard)) outcome$attempts <- unique(trimws(readLines(guard, warn = FALSE)))
  outcome$log <- process_tail(log)
  worker <- outcome$worker
  if (is.null(worker)) return(finish("preparation_failed"))
  if (identical(worker$status, "unavailable_offline")) {
    return(finish("packages_unavailable_offline", c(worker$missing, worker$wrong_version)))
  }
  if (identical(worker$status, "install_incomplete")) return(finish("install_incomplete", worker$missing, worker$library))
  finish(library = worker$library)
}

restore_worker <- function(project, library, override) {
  options(renv.consent = TRUE, renv.config.ppm.enabled = FALSE, renv.config.pak.enabled = FALSE,
    renv.config.updates.check = FALSE, renv.download.override = override)
  renv::restore(project = project, library = library, prompt = FALSE, clean = FALSE)
  found <- list()
  for (lib in unique(c(library, .Library.site, .Library))) {
    ip <- utils::installed.packages(lib.loc = lib, noCache = TRUE)
    for (i in seq_len(nrow(ip))) {
      if (is.null(found[[ip[i, "Package"]]])) {
        found[[ip[i, "Package"]]] <- list(version = unname(ip[i, "Version"]), restored = identical(lib, library))
      }
    }
  }
  found
}

#' Offline restore smoke test of a prepared project
#'
#' Copies only the environment definition (`renv.lock`, `.Rprofile` and renv
#' settings/activation; never data) to a temporary directory and runs
#' `renv::restore()` into `library` with every download refused, so packages come
#' from the renv cache. Restored versions are compared with `renv.lock`.
#' Packages already provided by R's site or system library at the locked version
#' are reported separately.
#' @param root Prepared project root.
#' @param library Empty destination library for the restore.
#' @param libraries Library paths for the isolated process (must contain renv).
#' @param timeout Seconds before the restore process is stopped.
#' @return A list with `state` (`restore_verified`, `restore_failed` or
#'   `not_tested`), lockfile hash, restored versions, mismatches and refused
#'   download attempts.
#' @noRd
restore_smoke <- function(root, library = tempfile("cttir-restore-library-"), libraries = .libPaths(), timeout = 3600) {
  scalar_text(root, "root")
  assert_plain_path(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  lockfile <- file.path(root, "renv.lock")
  if (!file.exists(lockfile)) return(list(state = "not_tested", reason = "lockfile_missing"))
  if (!requireNamespace("callr", quietly = TRUE) || !length(find.package("renv", lib.loc = libraries, quiet = TRUE))) {
    return(list(state = "not_tested", reason = "renv_or_callr_unavailable"))
  }
  lock <- read_renv_lock(lockfile)
  copy <- tempfile("cttir-restore-project-")
  dir.create(file.path(copy, "renv"), recursive = TRUE)
  on.exit(unlink(copy, recursive = TRUE), add = TRUE)
  for (file in c("renv.lock", ".Rprofile", "renv/activate.R", "renv/settings.json", "renv/.gitignore")) {
    if (file.exists(file.path(root, file))) file.copy(file.path(root, file), file.path(copy, file))
  }
  dir.create(library, recursive = TRUE, showWarnings = FALSE)
  guard <- tempfile("cttir-restore-guard-", fileext = ".log")
  log <- tempfile("cttir-restore-", fileext = ".log")
  on.exit(unlink(c(guard, log)), add = TRUE)
  started <- Sys.time()
  error <- NULL
  restore_args <- list(project = copy, library = normalizePath(library, winslash = "/"),
    override = renv_offline_override(guard))
  found <- tryCatch(
    callr::r(restore_worker, args = restore_args,
      libpath = libraries, stdout = log, stderr = "2>&1", user_profile = FALSE, system_profile = FALSE,
      package = FALSE, timeout = timeout, wd = tempdir(), env = offline_process_env()),
    error = function(e) {
      error <<- conditionMessage(e)
      NULL
    })
  restored <- list()
  provided <- list()
  mismatches <- character()
  missing <- character()
  for (package in names(lock$versions)) {
    hit <- found[[package]]
    if (is.null(hit)) {
      missing <- c(missing, package)
    } else if (!identical(hit$version, lock$versions[[package]])) {
      mismatches <- c(mismatches, paste0(package, " ", hit$version, " != ", lock$versions[[package]]))
    } else if (isTRUE(hit$restored)) {
      restored[[package]] <- hit$version
    } else {
      provided[[package]] <- hit$version
    }
  }
  attempts <- if (file.exists(guard)) unique(trimws(readLines(guard, warn = FALSE))) else character()
  verified <- is.null(error) && !length(missing) && !length(mismatches)
  list(state = if (verified) "restore_verified" else "restore_failed", lockfile_sha256 = lock$sha256,
    library = library, restored = restored, provided_by_r_library = provided,
    missing = as.list(missing), mismatches = as.list(mismatches), refused_downloads = as.list(attempts),
    elapsed_seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 3),
    error = error, process_log = if (verified) list() else process_tail(log))
}

# Explicit environment step after the scaffold exists (creation or applied sync).
# Never overwrites a lockfile whose library is absent here: restoring it is the
# reproducible action. Preparation failures become a pending status.
environment_step <- function(root, spec, dependencies, libraries = .libPaths()) {
  workflow <- spec$workflow
  status <- environment_status(dependencies, root, workflow$environment)
  if (!identical(workflow$environment, "renv") || identical(status$state, "environment_ready")) return(status)
  if (!isTRUE(workflow$prepare_environment)) {
    status$reason <- if (identical(status$reason, "lockfile_missing")) "preparation_disabled" else status$reason
    status$recovery <- environment_recovery(root, status$reason)
    return(status)
  }
  if (file.exists(file.path(root, "renv.lock")) && !isTRUE(status$library_in_sync) &&
      !identical(status$reason, "lockfile_unreadable")) {
    status$reason <- if (is.null(status[["library"]])) "project_library_missing" else "library_out_of_sync"
    status$recovery <- environment_recovery(root, status$reason)
    return(status)
  }
  tryCatch(prepare_project_environment(root, dependencies, workflow$network, libraries),
    error = function(e) {
      status$reason <- "preparation_failed"
      status$error <- conditionMessage(e)
      status$recovery <- environment_recovery(root, "preparation_failed")
      status
    })
}

# --- Git ---------------------------------------------------------------------

git_binary <- function() unname(Sys.which("git"))

# GIT_* variables could redirect commands to another repository; drop them.
git_env <- function() {
  env <- Sys.getenv()
  keep <- !grepl("^GIT_", names(env))
  stats::setNames(as.character(env)[keep], names(env)[keep])
}

git_toplevel <- function(git, dir) {
  query <- function() {
    processx::run(git, c("rev-parse", "--show-toplevel"), wd = dir, env = git_env(), error_on_status = FALSE, timeout = 60)
  }
  out <- tryCatch(query(), error = function(e) NULL)
  if (is.null(out) || out$status != 0L || !nzchar(trimws(out$stdout))) return(NULL)
  normalizePath(trimws(out$stdout), winslash = "/", mustWork = FALSE)
}

git_blockers <- function(git) {
  switch(git$state, git_unavailable = "git_unavailable", refused_parent_worktree = ,
    would_refuse_parent_worktree = "git_parent_worktree", failed = "git_init_failed",
    pending = "git_pending", character())
}

# Read-only Git state; a dry run only checks whether a parent work tree exists.
git_status <- function(root, enabled, parent = NULL) {
  if (!isTRUE(enabled)) return(list(state = "not_requested"))
  if (is.null(root)) {
    git <- git_binary()
    if (!nzchar(git)) return(list(state = "git_unavailable"))
    worktree <- if (is.null(parent)) NULL else git_toplevel(git, parent)
    if (!is.null(worktree)) return(list(state = "would_refuse_parent_worktree", worktree = worktree))
    return(list(state = "planned"))
  }
  if (file.exists(file.path(root, ".git"))) return(list(state = "initialized"))
  list(state = "pending", recovery = sprintf("cttiR::sync(%s, dry_run = FALSE)", deparse(root)))
}

# `git init` in the project root only. Refuses when the root already sits inside
# another work tree; never stages, commits or pushes.
git_initialize <- function(root) {
  git <- git_binary()
  if (!nzchar(git)) return(list(state = "git_unavailable"))
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  own <- git_toplevel(git, root)
  if (identical(own, root)) return(list(state = "initialized", detail = "existing repository at the project root"))
  worktree <- if (is.null(own)) git_toplevel(git, dirname(root)) else own
  if (!is.null(worktree)) {
    detail <- "The project lies inside another Git work tree; no nested repository was created."
    return(list(state = "refused_parent_worktree", worktree = worktree, detail = detail))
  }
  init <- function() {
    processx::run(git, c("init", "--quiet"), wd = root, env = git_env(), error_on_status = FALSE, timeout = 60)
  }
  out <- tryCatch(init(), error = function(e) list(status = -1L, stderr = conditionMessage(e)))
  if (out$status != 0L || !identical(git_toplevel(git, root), root)) {
    return(list(state = "failed", detail = trimws(out$stderr)))
  }
  list(state = "initialized", detail = "git init ran in the project root; nothing was staged or committed")
}

# Readiness rises only on verified evidence: environment_ready needs a matching
# renv.lock and project library.
readiness_with <- function(readiness, spec, bundle, environment, git) {
  readiness$environment <- environment
  readiness$git <- git
  readiness$blockers <- unique(c(project_blockers(spec, bundle, environment), git_blockers(git)))
  readiness$level <- if (identical(environment$state, "environment_ready")) "environment_ready" else "scaffold_ready"
  readiness
}
