# Dependency environment (renv), targets pipeline and Git for generated projects.

live <- function() identical(Sys.getenv("CTTIR_LIVE_TESTS"), "true")

# renv must never touch the user's real cache or root during tests.
local_renv_paths <- function(env = parent.frame()) {
  root <- tempfile("cttir-renv-paths-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  withr::local_envvar(c(RENV_PATHS_ROOT = file.path(root, "root"), RENV_PATHS_CACHE = file.path(root, "cache"),
      RENV_PATHS_LIBRARY = NA, RENV_PATHS_LIBRARY_ROOT = NA, RENV_PROJECT = NA), .local_envir = env)
  root
}

caller_state <- function() {
  list(wd = getwd(), libpaths = .libPaths(), options = options(),
    seed = if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL)
}

stage_functions <- function() {
  env <- new.env(parent = globalenv())
  root <- system.file("templates", "standard-0.3.0", "code", "R", package = "cttiR")
  for (file in sort(list.files(root, "\\.R$", full.names = TRUE))) sys.source(file, envir = env)
  env
}

pipeline_template <- function() {
  system.file("templates", "standard-0.3.0", "_targets.R", package = "cttiR")
}

synthetic_source <- list(id = "cohort", label = "Synthetic cohort", logical_uri = NULL, format = "csv",
  access_class = "restricted", checksum = NULL, checksum_status = "unknown", schema_ref = NULL)

renv_files <- c(".Rprofile", "renv", "renv.lock", "renv/.gitignore", "renv/activate.R", "renv/library", "renv/settings.json")

test_that("workflow integrations are accepted only in coherent combinations", {
  parent <- new_parent()
  plan <- function(workflow) {
    project("Options", "primary_research", "Describe outcomes", parent, options = list(workflow = workflow), dry_run = TRUE)
  }
  pipeline <- plan(list(pipeline = "targets"))
  stages <- vapply(pipeline$readiness$workflow$stages, function(x) x$capability, character(1))
  expect_contains(stages, "std.pipeline.targets")
  expect_contains(pipeline$plan$path, "_targets.R")
  expect_contains(vapply(project_bundle(pipeline$spec)$lock$dependencies, function(x) x$package, character(1)), "targets")
  expect_equal(pipeline$spec$workflow$profile, "standard_reflowR")
  plain <- project("Options", "primary_research", "Describe outcomes", parent, dry_run = TRUE)
  expect_false("_targets.R" %in% plain$plan$path)
  expect_false("targets" %in% vapply(project_bundle(plain$spec)$lock$dependencies, function(x) x$package, character(1)))
  renv <- plan(list(environment = "renv", prepare_environment = TRUE, network = "offline"))
  expect_equal(renv$readiness$environment$state, "environment_pending")
  expect_equal(renv$readiness$environment$reason, "not_materialized")
  expect_contains(renv$readiness$blockers, "environment_pending")
  expect_equal(renv$readiness$level, "scaffold_ready")
  expect_equal(plan(list(environment = "renv", prepare_environment = FALSE))$spec$workflow$environment, "renv")
  expect_equal(plan(list(network = "allowed", environment = "renv", prepare_environment = TRUE))$spec$workflow$network, "allowed")
  expect_true(plan(list(git = TRUE))$spec$workflow$git)
  expect_error(plan(list(prepare_environment = TRUE)), class = "cttir_input_error")
  expect_error(plan(list(prepare_environment = TRUE, environment = "none")), class = "cttir_input_error")
  expect_error(plan(list(readiness = "environment_ready")), class = "cttir_input_error")
  expect_error(plan(list(reporting = "quarto")), class = "cttir_api_mismatch")
  expect_error(plan(list(network = "sometimes")), class = "cttir_schema_error")
  expect_length(list.files(parent, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("the pipeline capability is infrastructure and never specialist evidence", {
  cap <- capability_registry()$capabilities[["std.pipeline.targets"]]
  expect_equal(cap$stage, "pipeline")
  expect_equal(cap$family, "standard")
  expect_equal(cap$packages, "targets")
  expect_equal(paste0(cap$adapter$id, "@", cap$adapter$version), "standard.pipeline_targets@0.3.0")
  expect_true(cap$infrastructure)
  expect_false(cap$specialist)
  expect_equal(cap$status, "adapter_tested")
  expect_false("std.pipeline.targets" %in% infer_goal("Run a targets pipeline with renv and git")$keyword_capabilities)
})

test_that("_targets.R is a pinned static file with only reviewed calls", {
  file <- pipeline_template()
  manifest <- standard_bundle_manifest()
  text <- paste0(paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n")
  expect_identical(content_hash(text), manifest$files[["_targets.R"]])
  expect_equal(manifest$conditional[["_targets.R"]]$pipeline, "targets")
  data <- utils::getParseData(parse(file, keep.source = TRUE))
  expect_setequal(unique(data$text[data$token == "SYMBOL_PACKAGE"]), "targets")
  calls <- unique(data$text[data$token == "SYMBOL_FUNCTION_CALL"])
  expect_false(any(c("library", "require", "attach", "system", "system2", "shell", "eval", "evalq", "parse",
        "install.packages", "download.file", "setwd", "Sys.setenv", "options", "unlink", "file.remove") %in% calls))
  expect_equal(sum(data$token == "SYMBOL_FUNCTION_CALL" & data$text == "source"), 2L)
  strings <- data$text[data$token == "STR_CONST"]
  expect_contains(strings, c('"code/R/cttir_figures.R"', '"code/R/cttir_workflow.R"'))
  expect_false(any(grepl("run_demo|run_workflow|render_report", strings)))
  targets <- data$text[which(data$token == "SYMBOL_FUNCTION_CALL" & data$text == "tar_target")]
  expect_length(targets, 7L)
})

test_that("the study target reads data only when every prerequisite is ready", {
  for (package in c("dplyr", "broom", "jsonlite", "yaml")) skip_if_not_installed(package)
  env <- stage_functions()
  # Only the reviewed template's own `ct_*` definitions are evaluated (as the
  # stage library itself is sourced above); the target list and its source()
  # calls are skipped.
  exprs <- parse(pipeline_template(), keep.source = FALSE)
  for (expr in exprs) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) && grepl("^ct_", as.character(expr[[2]]))) {
      eval(expr, envir = env)
    }
  }
  root <- new_parent()
  withr::local_dir(root)
  config <- list(root = ".", figures = NULL, workflow = list(table_backend = "none", dependencies = list()),
    analysis = list(aim = "explanatory", outcome_family = "continuous", unit_structure = "independent",
      approved = TRUE, mapping = list(data_source_id = "cohort", outcome = "y", predictors = list("x"),
        estimand = "difference", missing_data = "fail"), model = list(intercept = TRUE, reviewed = TRUE)),
    datasets = list(list(id = "cohort", format = "csv")), bindings = list(list(id = "cohort", path = "cohort.csv")))
  utils::write.csv(data.frame(y = c(1.1, 2.3, 2.8, 4.2, 5.1, 5.9), x = 1:6), "cohort.csv", row.names = FALSE)
  reads <- 0L
  real_import <- env$cw_import
  env$cw_import <- function(...) {
    reads <<- reads + 1L
    real_import(...)
  }
  expect_message(blocked <- env$ct_study(config, list(ready = FALSE, missing = c("analysis.approved", "dataset.local_binding"))),
    "2 prerequisites missing")
  expect_s3_class(blocked, "cttir_missing_input")
  expect_equal(blocked$status, "blocked")
  expect_false(blocked$data_read)
  expect_equal(blocked$missing, c("analysis.approved", "dataset.local_binding"))
  expect_equal(reads, 0L)
  expect_false(dir.exists("reports"))
  done <- env$ct_study(config, list(ready = TRUE, missing = character()))
  expect_equal(reads, 1L)
  expect_equal(done$status, "completed")
  expect_true(done$data_read)
  receipt <- jsonlite::fromJSON("reports/workflow/receipt.json", simplifyVector = FALSE)
  expect_equal(receipt$scheduler, "targets")
  expect_equal(receipt$model$engine, "stats::lm")
  expect_true(file.exists("reports/workflow/effects.csv"))
})

test_that("sync adds the pipeline as managed infrastructure and keeps the profile fixed", {
  p <- project("Pipeline sync", "primary_research", "Describe outcomes", new_parent())
  before <- tree_hashes(p$path)
  preview <- sync(p$path, options = list(workflow = list(pipeline = "targets")))
  expect_identical(tree_hashes(p$path), before)
  expect_equal(preview$actions$action[preview$actions$path == "_targets.R"], "create")
  applied <- sync(p$path, options = list(workflow = list(pipeline = "targets")), dry_run = FALSE)
  expect_equal(applied$state, "applied")
  expect_true(file.exists(file.path(p$path, "_targets.R")))
  project <- read_project(p$path)
  expect_contains(vapply(project$lock$dependencies, function(x) x$package, character(1)), "targets")
  expect_equal(yaml::read_yaml(file.path(p$path, "config/workflow.yml"))$pipeline, "targets")
  ownership <- vapply(project$manifest$files, function(x) x$ownership, character(1))
  expect_equal(unname(ownership[vapply(project$manifest$files, function(x) x$path, character(1)) == "_targets.R"]), "managed")
  expect_true(all(sync(p$path)$actions$action == "skip"))
  expect_error(sync(p$path, options = list(workflow = list(profile = "hybrid"))), class = "cttir_api_mismatch")
  expect_error(sync(p$path, options = list(workflow = list(prepare_environment = TRUE))), class = "cttir_input_error")
})

test_that("a default tar_make validates, runs the demo and blocks the study target", {
  skip_on_cran()
  for (package in c("targets", "callr", "dplyr", "nlme", "survival", "broom", "broom.mixed", "jsonlite", "yaml",
      "ggplot2", "patchwork", "viridisLite", "RColorBrewer", "colorspace")) {
    skip_if_not_installed(package)
  }
  parent <- new_parent()
  p <- project("Pipeline run", "primary_research", "Describe outcomes", parent,
    options = list(workflow = list(pipeline = "targets"), data_sources = list(synthetic_source)))
  skip_if_not(file.exists(file.path(p$path, "code/R/cttir_figures.R")), "The figure stage library is not bundled yet.")
  data <- file.path(parent, "restricted.csv")
  writeLines(c("y,x", "1,2"), data)
  Sys.chmod(data, "0000")
  withr::defer(Sys.chmod(data, "0600"))
  writeLines(c("bindings:", "- id: cohort", paste0("  path: ", data)), file.path(p$path, ".cttir/local.yml"))
  run <- function() {
    callr::r(function(root) {
      setwd(root)
      targets::tar_make(reporter = "silent", callr_function = NULL)
      list(progress = as.data.frame(targets::tar_progress()), validation = targets::tar_read(validation),
        demo = targets::tar_read(demo), demo_files = targets::tar_read(demo_files), study = targets::tar_read(study))
    }, args = list(root = p$path), package = FALSE, user_profile = FALSE, timeout = 600)
  }
  first <- run()
  expect_true(all(first$progress$progress == "completed"))
  expect_equal(first$validation$status, "passed")
  expect_false(first$validation$study_data_opened)
  expect_equal(first$demo$status, "passed")
  expect_true(first$demo$synthetic)
  expect_true(file.exists(file.path(p$path, "demo/receipt.json")))
  expect_contains(first$demo_files, "demo/receipt.json")
  expect_s3_class(first$study, "cttir_missing_input")
  expect_false(first$study$data_read)
  expect_contains(first$study$missing, "analysis.approved")
  expect_false(dir.exists(file.path(p$path, "reports/workflow")))
  expect_equal(file.info(data)$size, 8)
  second <- run()
  progress <- stats::setNames(second$progress$progress, second$progress$name)
  expect_equal(unname(progress[c("demo", "study", "validation")]), rep("skipped", 3))
})

test_that("renv readiness requires a lockfile and project library that match", {
  root <- new_parent()
  dir.create(file.path(root, ".cttir"))
  deps <- list(list(package = "alpha", version = NULL), list(package = "beta", version = "2.0"),
    list(package = "stats", version = NULL))
  status <- function(d = deps) environment_status(d, root, "renv")
  expect_equal(status()$reason, "lockfile_missing")
  expect_match(status()$recovery, "cttiR::sync", fixed = TRUE)
  lock <- function(versions) {
    packages <- lapply(names(versions), function(n) list(Package = n, Version = versions[[n]], Source = "Repository"))
    jsonlite::write_json(list(R = list(Version = "4.6.1"), Packages = stats::setNames(packages, names(versions))),
      file.path(root, "renv.lock"), auto_unbox = TRUE, pretty = TRUE)
  }
  install <- function(package, version) {
    library <- file.path(root, "renv", "library", "test-os", paste0("R-", R.version$major, ".",
        strsplit(R.version$minor, ".", fixed = TRUE)[[1]][[1]]), R.version$platform)
    dir.create(file.path(library, package), recursive = TRUE, showWarnings = FALSE)
    writeLines(c(paste("Package:", package), paste("Version:", version)), file.path(library, package, "DESCRIPTION"))
  }
  lock(list(alpha = "1.0"))
  expect_equal(status()$reason, "lockfile_incomplete")
  expect_equal(unlist(status()$missing), "beta")
  lock(list(alpha = "1.0", beta = "1.9"))
  expect_equal(status()$reason, "pin_mismatch")
  lock(list(alpha = "1.0", beta = "2.0"))
  expect_equal(status()$reason, "project_library_missing")
  expect_match(status()$recovery, "renv::restore", fixed = TRUE)
  install("alpha", "1.0")
  install("beta", "1.0")
  expect_equal(status()$reason, "library_out_of_sync")
  install("beta", "2.0")
  ready <- status()
  expect_equal(ready$state, "environment_ready")
  expect_equal(unlist(ready$unpinned), "alpha")
  expect_false("stats" %in% vapply(ready$dependencies, function(x) x$package, character(1)))
  spec <- list(workflow = list(environment = "renv", prepare_environment = TRUE, network = "offline"))
  testthat::local_mocked_bindings(prepare_project_environment = function(...) stop("must not prepare"))
  expect_equal(environment_step(root, spec, deps)$state, "environment_ready")
  unlink(file.path(root, "renv", "library"), recursive = TRUE)
  before <- tree_hashes(root)
  kept <- environment_step(root, spec, deps)
  expect_equal(kept$reason, "project_library_missing")
  expect_identical(tree_hashes(root), before)
})

test_that("offline preparation hydrates only the selected packages in an isolated process", {
  skip_on_cran()
  skip_if_not_installed("renv", "1.0.0")
  skip_if_not_installed("callr")
  paths <- local_renv_paths()
  p <- project("Renv offline", "primary_research", "Describe outcomes", new_parent())
  before <- tree_hashes(p$path)
  yaml_version <- as.character(utils::packageVersion("yaml"))
  deps <- list(list(package = "yaml", version = yaml_version), list(package = "jsonlite", version = NULL),
    list(package = "stats", version = NULL))
  requireNamespace("callr", quietly = TRUE)
  set.seed(20261002L)
  state <- caller_state()
  result <- prepare_project_environment(p$path, deps, "offline")
  expect_identical(caller_state(), state)
  expect_s3_class(result, "cttir_environment")
  expect_equal(result$state, "environment_ready", info = paste(unlist(result$process_log), collapse = "\n"))
  expect_null(result$reason)
  expect_length(result$refused_downloads, 0L)
  expect_length(result$files_modified, 0L)
  expect_setequal(unlist(result$files_created), renv_files)
  after <- tree_hashes(p$path)
  expect_identical(after[names(before)], before)
  added <- setdiff(names(after), names(before))
  expect_true(all(grepl("^(renv/|renv\\.lock$|\\.Rprofile$|\\.cttir/environment\\.json$)", added)))
  lock <- jsonlite::fromJSON(file.path(p$path, "renv.lock"), simplifyVector = FALSE)
  expect_setequal(names(lock$Packages), c("yaml", "jsonlite"))
  expect_equal(lock$Packages$yaml$Version, yaml_version)
  expect_equal(lock$Packages$jsonlite$Version, as.character(utils::packageVersion("jsonlite")))
  expect_identical(result$lockfile_sha256, digest::digest(file = file.path(p$path, "renv.lock"), algo = "sha256"))
  expect_contains(readLines(file.path(p$path, ".Rprofile")), 'source("renv/activate.R")')
  expect_true(length(list.files(file.path(paths, "cache"), recursive = TRUE)) > 0L)
  record <- read_document(file.path(p$path, ".cttir/environment.json"))
  expect_equal(record$state, "environment_ready")
  expect_equal(record$network, "offline")
  status <- environment_status(deps, p$path, "renv")
  expect_equal(status$state, "environment_ready")
  expect_equal(status$last_preparation$lockfile_sha256, result$lockfile_sha256)
  expect_equal(environment_status(list(list(package = "yaml", version = "0.0.1")), p$path, "renv")$reason, "pin_mismatch")
  ignored <- readLines(file.path(p$path, ".gitignore"))
  expect_contains(ignored, c("renv/library/", ".cttir/environment.json", ".cttir/local.yml", "data/raw/",
      "administration/private/", "_targets/", "demo/outputs/", "reports/workflow/"))
  expect_true(all(sync(p$path)$actions$action == "skip"))
  expect_false("renv.lock" %in% sync(p$path)$actions$path)
})

test_that("offline preparation reports unavailable packages without downloading or writing renv files", {
  skip_on_cran()
  skip_if_not_installed("renv", "1.0.0")
  skip_if_not_installed("callr")
  local_renv_paths()
  root <- new_parent()
  dir.create(file.path(root, ".cttir"))
  result <- prepare_project_environment(root, list(list(package = "yaml"), list(package = "cttirAbsentPackage")), "offline")
  expect_equal(result$state, "environment_pending")
  expect_equal(result$reason, "packages_unavailable_offline")
  expect_equal(unlist(result$unavailable), "cttirAbsentPackage")
  expect_match(result$recovery, 'install.packages("cttirAbsentPackage")', fixed = TRUE)
  expect_match(result$recovery, 'network = "allowed"', fixed = TRUE)
  expect_length(result$refused_downloads, 0L)
  expect_false(any(file.exists(file.path(root, c("renv", "renv.lock", ".Rprofile")))))
  expect_equal(read_document(file.path(root, ".cttir/environment.json"))$reason, "packages_unavailable_offline")
  missing_renv <- prepare_project_environment(root, list(list(package = "yaml")), "offline", libraries = new_parent())
  expect_equal(missing_renv$reason, "renv_unavailable")
  expect_match(missing_renv$recovery, 'install.packages("renv")', fixed = TRUE)
})

test_that("the offline guard refuses every renv download loudly", {
  skip_on_cran()
  skip_if_not_installed("renv", "1.0.0")
  skip_if_not_installed("callr")
  local_renv_paths()
  guard <- tempfile("cttir-guard-")
  withr::defer(unlink(guard))
  work <- new_parent()
  dir.create(file.path(work, "library"))
  outcome <- callr::r(function(override, library) {
    options(renv.consent = TRUE, renv.config.pak.enabled = FALSE, renv.config.ppm.enabled = FALSE,
      renv.download.override = override)
    tryCatch({
      renv::install("cttirAbsentPackage", library = library, repos = c(CRAN = "https://cloud.r-project.org"),
        prompt = FALSE)
      "installed"
    }, error = function(e) conditionMessage(e))
  }, args = list(override = renv_offline_override(guard), library = file.path(work, "library")),
  package = FALSE, user_profile = FALSE, wd = work, env = offline_process_env())
  expect_false(identical(outcome, "installed"))
  expect_true(file.exists(guard), info = outcome)
  attempts <- readLines(guard)
  expect_true(length(attempts) > 0L)
  expect_true(all(grepl("^https://", attempts)))
  expect_error(renv_offline_override(guard)("https://example.org/x", tempfile()), "Network access is disabled")
})

test_that("a failed preparation keeps the scaffold and reports a pending environment", {
  skip_if_not_installed("callr")
  skip_if_not(nzchar(system.file(package = "renv")))
  local_renv_paths()
  testthat::local_mocked_bindings(r = function(...) stop("injected process failure"), .package = "callr")
  p <- project("Renv failure", "primary_research", "Describe outcomes", new_parent(),
    options = list(workflow = list(environment = "renv", prepare_environment = TRUE)))
  for (path in vapply(p$manifest, function(x) x$path, character(1))) {
    expect_true(file.exists(file.path(p$path, path)), info = path)
  }
  environment <- p$readiness$environment
  expect_equal(environment$state, "environment_pending")
  expect_equal(environment$reason, "preparation_failed")
  expect_match(environment$error, "injected process failure")
  expect_match(environment$recovery, "cttiR::sync", fixed = TRUE)
  expect_equal(p$readiness$level, "scaffold_ready")
  expect_contains(p$readiness$blockers, "environment_pending")
  expect_false(file.exists(file.path(p$path, "renv.lock")))
  expect_equal(read_document(file.path(p$path, ".cttir/environment.json"))$state, "environment_pending")
  expect_equal(read_project(p$path)$spec$workflow$environment, "renv")
})

test_that("sync prepares the environment only when applied", {
  calls <- list()
  testthat::local_mocked_bindings(prepare_project_environment = function(root, dependencies, network, ...) {
    calls[[length(calls) + 1L]] <<- list(root = root, packages = vapply(dependencies, function(x) x$package, character(1)),
      network = network)
    structure(list(state = "environment_pending", reason = "preparation_failed"), class = "cttir_environment")
  })
  p <- project("Renv sync", "primary_research", "Describe outcomes", new_parent())
  options <- list(workflow = list(environment = "renv", prepare_environment = TRUE, network = "offline"))
  before <- tree_hashes(p$path)
  preview <- sync(p$path, options = options)
  expect_identical(tree_hashes(p$path), before)
  expect_length(calls, 0L)
  expect_equal(preview$environment$reason, "lockfile_missing")
  applied <- sync(p$path, options = options, dry_run = FALSE)
  expect_equal(applied$state, "applied")
  expect_length(calls, 1L)
  expect_equal(calls[[1]]$root, p$path)
  expect_equal(calls[[1]]$network, "offline")
  expect_setequal(calls[[1]]$packages, vapply(read_project(p$path)$lock$dependencies, function(x) x$package, character(1)))
  expect_equal(applied$readiness, "scaffold_ready")
  expect_contains(applied$blockers, "environment_pending")
  expect_equal(read_project(p$path)$spec$workflow$environment, "renv")
  expect_false(any(file.exists(file.path(p$path, c("renv", "renv.lock", ".Rprofile")))))
})

test_that("git initializes only the generated project and never commits", {
  git <- unname(Sys.which("git"))
  skip_if_not(nzchar(git))
  parent <- new_parent()
  skip_if_not(is.null(git_toplevel(git, parent)), "The temporary directory is inside a Git work tree.")
  p <- project("Git project", "primary_research", "Describe outcomes", parent, options = list(workflow = list(git = TRUE)))
  expect_equal(p$readiness$git$state, "initialized")
  expect_false(any(grepl("^git_", p$readiness$blockers)))
  expect_true(dir.exists(file.path(p$path, ".git")))
  expect_identical(git_toplevel(git, p$path), normalizePath(p$path, winslash = "/"))
  expect_null(git_toplevel(git, parent))
  head <- processx::run(git, c("rev-parse", "--verify", "HEAD"), wd = p$path, error_on_status = FALSE, env = git_env())
  expect_false(head$status == 0L)
  staged <- processx::run(git, c("diff", "--cached", "--name-only"), wd = p$path, env = git_env())
  expect_equal(trimws(staged$stdout), "")
  ignored <- processx::run(git, c("check-ignore", "data/raw/x.csv", ".cttir/local.yml", "renv/library/x"),
    wd = p$path, env = git_env(), error_on_status = FALSE)
  expect_length(strsplit(trimws(ignored$stdout), "\n")[[1]], 3L)
  later <- project("Later git", "primary_research", "Describe outcomes", parent)
  expect_equal(later$readiness$git$state, "not_requested")
  enabled <- sync(later$path, options = list(workflow = list(git = TRUE)), dry_run = FALSE)
  expect_equal(enabled$git$state, "initialized")
  expect_true(dir.exists(file.path(later$path, ".git")))
})

test_that("git refuses nested work trees and reports an unavailable binary", {
  git <- unname(Sys.which("git"))
  skip_if_not(nzchar(git))
  outer <- new_parent()
  processx::run(git, c("init", "--quiet"), wd = outer, env = git_env())
  preview <- project("Nested", "primary_research", "Describe outcomes", outer, options = list(workflow = list(git = TRUE)),
    dry_run = TRUE)
  expect_equal(preview$readiness$git$state, "would_refuse_parent_worktree")
  expect_contains(preview$readiness$blockers, "git_parent_worktree")
  p <- project("Nested", "primary_research", "Describe outcomes", outer, options = list(workflow = list(git = TRUE)))
  expect_equal(p$readiness$git$state, "refused_parent_worktree")
  expect_contains(p$readiness$blockers, "git_parent_worktree")
  expect_false(file.exists(file.path(p$path, ".git")))
  expect_true(file.exists(file.path(p$path, "cttir-project.yml")))
  testthat::local_mocked_bindings(git_binary = function() "")
  missing <- project("No git", "primary_research", "Describe outcomes", new_parent(), options = list(workflow = list(git = TRUE)))
  expect_equal(missing$readiness$git$state, "git_unavailable")
  expect_contains(missing$readiness$blockers, "git_unavailable")
  expect_true(file.exists(file.path(missing$path, "cttir-project.yml")))
})

test_that("an offline restore smoke reproduces the recorded versions from the cache", {
  skip_if_not(live(), "Set CTTIR_LIVE_TESTS=true to run renv restore smoke tests.")
  skip_if_not_installed("renv", "1.0.0")
  skip_if_not_installed("callr")
  local_renv_paths()
  root <- new_parent()
  dir.create(file.path(root, ".cttir"))
  prepared <- prepare_project_environment(root, list(list(package = "yaml"), list(package = "jsonlite")), "offline")
  expect_equal(prepared$state, "environment_ready")
  library <- tempfile("cttir-restore-library-")
  withr::defer(unlink(library, recursive = TRUE))
  smoke <- restore_smoke(root, library)
  expect_equal(smoke$state, "restore_verified", info = paste(unlist(smoke$process_log), collapse = "\n"))
  expect_equal(smoke$lockfile_sha256, prepared$lockfile_sha256)
  lock <- jsonlite::fromJSON(file.path(root, "renv.lock"), simplifyVector = FALSE)
  restored <- c(smoke$restored, smoke$provided_by_r_library)
  expect_setequal(names(restored), names(lock$Packages))
  for (package in names(lock$Packages)) expect_equal(restored[[package]], lock$Packages[[package]]$Version)
  expect_true(all(file.exists(file.path(library, names(smoke$restored), "DESCRIPTION"))))
  expect_true(all(grepl("^https://", unlist(smoke$refused_downloads))))
})

test_that("allowed network installs only the packages missing locally", {
  skip_if_not(live(), "Set CTTIR_LIVE_TESTS=true to install a local fixture package.")
  skip_if_not_installed("renv", "1.0.0")
  skip_if_not_installed("callr")
  local_renv_paths()
  blackhole <- "http://127.0.0.1:9"
  withr::local_envvar(c(http_proxy = blackhole, https_proxy = blackhole, HTTP_PROXY = blackhole,
      HTTPS_PROXY = blackhole, no_proxy = "", NO_PROXY = "", RENV_CONFIG_CRANDB_ENABLED = "FALSE"))
  repo <- new_parent()
  source <- file.path(repo, "cttirEnvFixture")
  dir.create(file.path(source, "R"), recursive = TRUE)
  writeLines(c("Package: cttirEnvFixture", "Version: 0.0.1", "Title: Synthetic Environment Fixture",
      "Description: Synthetic fixture.", "License: MIT", "Authors@R: person('A', 'B', role = c('aut', 'cre'), email = 'a@b.c')",
      "Encoding: UTF-8"), file.path(source, "DESCRIPTION"))
  writeLines("export(fixture_value)", file.path(source, "NAMESPACE"))
  writeLines("fixture_value <- function() 1", file.path(source, "R", "fixture.R"))
  contrib <- file.path(repo, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  withr::with_dir(repo, utils::tar(file.path(contrib, "cttirEnvFixture_0.0.1.tar.gz"), "cttirEnvFixture",
      compression = "gzip", tar = "internal"))
  tools::write_PACKAGES(contrib, type = "source")
  root <- new_parent()
  dir.create(file.path(root, ".cttir"))
  result <- prepare_project_environment(root, list(list(package = "yaml"), list(package = "cttirEnvFixture", version = "0.0.1")),
    "allowed", repos = c(LOCAL = paste0("file://", normalizePath(repo, winslash = "/"))))
  expect_equal(result$state, "environment_ready", info = paste(unlist(result$process_log), collapse = "\n"))
  expect_equal(unlist(result$hydrated), "yaml")
  expect_equal(unlist(result$installed), "cttirEnvFixture@0.0.1")
  lock <- jsonlite::fromJSON(file.path(root, "renv.lock"), simplifyVector = FALSE)
  expect_equal(lock$Packages$cttirEnvFixture$Version, "0.0.1")
  expect_setequal(names(lock$Packages), c("yaml", "cttirEnvFixture"))
})

test_that("recovery commands stay one parseable line however many packages are missing", {
  packages <- c("DescrTab2", "RColorBrewer", "broom", "broom.mixed", "colorspace", "dplyr", "ggplot2", "jsonlite",
    "knitr", "patchwork", "readr", "rmarkdown", "viridisLite", "yaml")
  root <- file.path(new_parent(), "a project directory with a rather long name to force wrapping")
  for (reason in c("packages_unavailable_offline", "install_incomplete", "library_out_of_sync", "renv_unavailable")) {
    recovery <- environment_recovery(root, reason, packages)
    expect_length(recovery, 1L)
    expect_type(parse(text = recovery), "expression")
  }
  expect_match(environment_recovery(root, "packages_unavailable_offline", packages), '"viridisLite", "yaml")', fixed = TRUE)
  dir.create(file.path(root, ".cttir"), recursive = TRUE)
  recovery <- environment_recovery(root, "packages_unavailable_offline", packages)
  write_environment_record(root, list(state = "environment_pending", recovery = recovery))
  stored <- read_environment_record(root)$recovery
  expect_length(stored, 1L)
  expect_type(parse(text = stored), "expression")
})
