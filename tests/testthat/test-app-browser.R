# Live browser tests of the local application (opt-in). They install this
# source tree into a private temporary library for the application process and
# its background workers, use only temporary catalog, runtime and project
# directories, and never contact a model runtime or the network. Screenshots go
# to CTTIR_SCREENSHOT_DIR when set, otherwise to a temporary directory.

browser_env <- new.env(parent = emptyenv())

browser_skip <- function() {
  skip_if_not(identical(Sys.getenv("CTTIR_LIVE_TESTS"), "true"))
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(file.exists(file.path(root, "DESCRIPTION")) && dir.exists(file.path(root, "R")), "source tree unavailable")
  if (file.exists("/usr/bin/chromium") && !nzchar(Sys.getenv("CHROMOTE_CHROME"))) Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium")
  root
}

browser_library <- function(root) {
  if (!is.null(browser_env$lib)) return(browser_env$lib)
  lib <- tempfile("cttir-browser-lib-")
  dir.create(lib)
  result <- processx::run(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-docs", "--no-html", "--no-multiarch", "-l", lib, root),
    error_on_status = FALSE, timeout = 600)
  if (result$status != 0L) stop("Private installation failed: ", result$stderr)
  browser_env$lib <- lib
  lib
}

browser_fixture_source <- function(parent) {
  source <- file.path(parent, "fixture-source")
  dir.create(file.path(source, "R"), recursive = TRUE)
  writeLines(c("Package: cttirFixtureA", "Version: 1.0.0", "Title: Synthetic Test Package", "License: MIT"), file.path(source, "DESCRIPTION"))
  writeLines(c("export(keep)", "export(old)"), file.path(source, "NAMESPACE"))
  writeLines(c("keep <- function(x = 1) x", "old <- function(x) x"), file.path(source, "R", "api.R"))
  source
}

# Start the installed application on loopback with isolated stores.
browser_app <- function(name, call, env = parent.frame()) {
  root <- browser_skip()
  lib <- browser_library(root)
  parent <- tempfile("cttir-browser-")
  dir.create(parent)
  withr::defer(unlink(parent, recursive = TRUE), envir = env)
  projects <- file.path(parent, "projects")
  dir.create(projects)
  source <- browser_fixture_source(parent)
  app_dir <- file.path(parent, "app")
  dir.create(app_dir)
  writeLines(c(
    sprintf(".libPaths(c(%s, .libPaths()))", deparse(lib)),
    sprintf("options(cttiR.catalog_dir = %s, cttiR.runtime_dir = %s,", deparse(file.path(parent, "store")), deparse(file.path(parent, "runtime"))),
    "  cttiR.ollama_endpoint = 'http://127.0.0.1:59997',",
    sprintf("  cttiR.sources = list(list(id = 'fixture', path = %s)))", deparse(source)),
    sprintf("setwd(%s)", deparse(projects)),
    call
  ), file.path(app_dir, "app.R"))
  app <- shinytest2::AppDriver$new(app_dir, name = name, width = 1280, height = 900, seed = 1,
    load_timeout = 120000, timeout = 60000)
  withr::defer(app$stop(), envir = env)
  list(app = app, parent = parent, projects = projects)
}

screenshot_dir <- function() {
  dir <- Sys.getenv("CTTIR_SCREENSHOT_DIR", file.path(tempdir(), "cttir-screenshots"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

no_horizontal_overflow <- function(app) {
  isTRUE(app$get_js("document.documentElement.scrollWidth <= window.innerWidth + 1"))
}

# Capture normal and narrow layouts; the narrow one must not scroll sideways.
# A narrow close-up of the key region keeps long pages reviewable.
capture <- function(app, stem, detail = NULL) {
  dir <- screenshot_dir()
  unlink(file.path(dir, paste0(stem, c("-1280.png", "-390.png", "-390-detail.png"))))
  app$set_window_size(1280, 900)
  Sys.sleep(0.5)
  app$get_screenshot(file.path(dir, paste0(stem, "-1280.png")))
  expect_true(no_horizontal_overflow(app), info = paste(stem, "1280"))
  app$set_window_size(390, 844)
  Sys.sleep(0.5)
  app$get_screenshot(file.path(dir, paste0(stem, "-390.png")))
  expect_true(no_horizontal_overflow(app), info = paste(stem, "390"))
  if (!is.null(detail)) app$get_screenshot(file.path(dir, paste0(stem, "-390-detail.png")), selector = detail)
  app$set_window_size(1280, 900)
  Sys.sleep(0.3)
}

wait_text <- function(app, selector, pattern, timeout = 90000) {
  script <- sprintf("(function(){var e=document.querySelector(%s);return !!e && e.textContent.indexOf(%s) >= 0;})()",
    jsonlite::toJSON(selector, auto_unbox = TRUE), jsonlite::toJSON(pattern, auto_unbox = TRUE))
  app$wait_for_js(script, timeout = timeout)
}

test_that("Fast creation previews the routing and creates the project in a browser", {
  live <- browser_app("fast", "cttiR::setup_app(launch.browser = FALSE)")
  app <- live$app
  expect_match(app$get_text(".tab-content .tab-pane.active h1"), "cttir project builder", fixed = TRUE)
  capture(app, "home")
  app$set_inputs(lang = "de")
  wait_text(app, ".navbar-nav", "Projekt anlegen")
  expect_equal(app$get_js("document.documentElement.lang"), "de")
  capture(app, "home-de")
  app$set_inputs(lang = "en")
  wait_text(app, ".navbar-nav", "Create project")
  app$click("home-go_create")
  app$wait_for_js("document.querySelector('#create-name') && document.querySelector('#create-name').offsetParent !== null")
  app$set_inputs(`create-name` = "Browser fast", `create-type` = "primary_research",
    `create-goal` = "Explain blood pressure with a linear model in independent patients with a continuous outcome",
    `create-parent` = live$projects)
  app$click("create-preview", wait_ = FALSE)
  wait_text(app, "#create-review", "Standard R with reflowR")
  review <- app$get_text("#create-review")
  expect_match(review, "Routing reason", fixed = TRUE)
  expect_match(review, "DescrTab2", fixed = TRUE)
  expect_false(dir.exists(file.path(live$projects, "browser_fast")))
  capture(app, "create-fast", "#create-review")
  app$click("create-apply", wait_ = FALSE)
  wait_text(app, "#create-status", "Project created")
  expect_true(file.exists(file.path(live$projects, "browser_fast", "cttir-project.yml")))
})

test_that("Detailed creation applies questionnaire answers through the same builder", {
  live <- browser_app("detailed", "cttiR::setup_app('detailed', launch.browser = FALSE)")
  app <- live$app
  app$set_inputs(`create-name` = "Browser detailed", `create-type` = "methods", `create-goal` = "Compare two groups",
    `create-parent` = live$projects)
  app$wait_for_js("document.querySelector('#create-goto_analysis') !== null")
  app$click("create-goto_analysis")
  app$wait_for_js("document.querySelector('#create-q_analysis_aim') !== null")
  app$set_inputs(`create-q_analysis_aim` = "explanatory")
  app$wait_for_js("document.querySelector('#create-q_analysis_outcome_family') !== null")
  app$set_inputs(`create-q_analysis_outcome_family` = "continuous", `create-q_analysis_unit_structure` = "longitudinal")
  app$wait_for_js("document.querySelector('#create-q_mapping_subject') !== null")
  app$set_inputs(`create-q_mapping_subject` = "patient_id")
  app$click("create-q_next")
  app$wait_for_js("document.querySelector('#create-q_model_estimation') !== null")
  app$set_inputs(`create-q_model_estimation` = "REML")
  app$click("create-q_back")
  wait_text(app, "#create-questionnaire", "Section 3 of 7")
  capture(app, "detailed", "#create-questionnaire")
  app$set_inputs(`create-q_analysis_aim` = "descriptive")
  wait_text(app, "#create-questionnaire", "Answers that no longer apply")
  app$set_inputs(`create-q_analysis_aim` = "explanatory")
  for (id in c("analysis_outcome_family", "analysis_unit_structure", "mapping_subject")) {
    app$wait_for_js(sprintf("document.querySelector('#create-keep_%s.shiny-bound-input') !== null", id))
    app$click(paste0("create-keep_", id))
  }
  expect_match(app$get_text("#create-questionnaire"), "Needs review", fixed = TRUE)
  app$click("create-preview", wait_ = FALSE)
  wait_text(app, "#create-review", "nlme::lme")
  app$click("create-apply", wait_ = FALSE)
  wait_text(app, "#create-status", "Project created")
  spec <- yaml::read_yaml(file.path(live$projects, "browser_detailed", "cttir-project.yml"))
  expect_equal(spec$analysis$aim, "explanatory")
  expect_equal(spec$analysis$unit_structure, "longitudinal")
  expect_equal(spec$analysis$mapping$subject, "patient_id")
  expect_null(spec$analysis$model$estimation)
})

test_that("Configure previews field changes and applies them explicitly", {
  root <- browser_skip()
  lib <- browser_library(root)
  target <- tempfile("cttir-configure-")
  dir.create(target)
  withr::defer(unlink(target, recursive = TRUE))
  created <- callr::r(function(lib, target) {
    .libPaths(c(lib, .libPaths()))
    cttiR::project("Configure browser", "methods", "Goal", target)$path
  }, args = list(lib, target), libpath = c(lib, .libPaths()))
  live <- browser_app("configure", sprintf("cttiR::configure(%s, launch.browser = FALSE)", deparse(created)))
  app <- live$app
  wait_text(app, "#open-configure-project_summary", "Configure browser")
  app$wait_for_js("document.querySelector('#open-configure-q_project_language') !== null")
  app$set_inputs(`open-configure-q_project_language` = "de")
  app$click("open-configure-preview", wait_ = FALSE)
  wait_text(app, "#open-configure-review", "/project/language")
  app$click("open-configure-apply", wait_ = FALSE)
  wait_text(app, "#open-configure-status", "Changes applied")
  expect_equal(yaml::read_yaml(file.path(created, "cttir-project.yml"))$project$language, "de")
})

test_that("Knowledge previews an update, audit repairs, cancellation and an unavailable runtime work", {
  live <- browser_app("views", "cttiR::setup_app(launch.browser = FALSE)")
  app <- live$app
  app$click("home-go_knowledge")
  app$wait_for_js("document.querySelector('#knowledge-preview_update').offsetParent !== null")
  app$click("knowledge-load_packages", wait_ = FALSE)
  wait_text(app, "#knowledge-packages", "Exports")
  app$click("knowledge-preview_update", wait_ = FALSE)
  wait_text(app, "#knowledge-update_result", "package_added")
  expect_false(dir.exists(file.path(live$parent, "store", "snapshots")))
  capture(app, "knowledge", "#knowledge-update_result")
  # Hidden optional columns must hide header and cells together at every width.
  aligned <- paste0("(function(){var t=document.querySelector('#knowledge-packages table');",
    "var vis=function(e){return getComputedStyle(e).display!=='none';};",
    "var h=Array.from(t.querySelectorAll('thead th')).filter(vis).length;",
    "var c=Array.from(t.querySelector('tbody tr').children).filter(vis).length;return h===c;})()")
  for (width in c(1280, 390)) {
    app$set_window_size(width, 900)
    Sys.sleep(0.3)
    expect_true(app$get_js(aligned), info = width)
  }
  app$set_window_size(1280, 900)

  app$click("home-go_create")
  app$set_inputs(`create-name` = "Audit target", `create-goal` = "Goal", `create-type` = "methods", `create-parent` = live$projects)
  app$click("create-preview", wait_ = FALSE)
  wait_text(app, "#create-review", "Routing reason")
  app$click("create-apply", wait_ = FALSE)
  wait_text(app, "#create-status", "Project created")
  project <- file.path(live$projects, "audit_target")
  unlink(file.path(project, "code", "validate_project.R"))
  app$set_inputs(nav = "audit")
  app$set_inputs(`audit-path` = project, `audit-scope` = c("installation", "project"))
  app$click("audit-run", wait_ = FALSE)
  wait_text(app, "#audit-repair_preview", "code/validate_project.R")
  expect_match(app$get_text("#audit-repair_preview"), "restore_missing_managed", fixed = TRUE)
  capture(app, "audit", "#audit-checks")
  # A file removed after the preview is restored only once a new preview shows it.
  expect_true(file.exists(file.path(project, "code", "run_demo.R")))
  unlink(file.path(project, "code", "run_demo.R"))
  app$click("audit-repair", wait_ = FALSE)
  wait_text(app, "#audit-status", "changed since the repair preview")
  wait_text(app, "#audit-repair_preview", "code/run_demo.R")
  expect_false(file.exists(file.path(project, "code", "validate_project.R")))
  capture(app, "audit-repair", "#audit-repair_preview")
  app$click("audit-repair", wait_ = FALSE)
  wait_text(app, "#audit-status", "Repair finished")
  expect_true(all(file.exists(file.path(project, "code", c("validate_project.R", "run_demo.R")))))
  app$set_inputs(`audit-output` = file.path(live$parent, "reports"))
  app$click("audit-export", wait_ = FALSE)
  wait_text(app, "#audit-reports", "cttir-audit-")
  expect_length(list.files(file.path(live$parent, "reports")), 2L)

  app$set_inputs(`audit-scope` = c("installation", "knowledge", "project", "integration"))
  app$click("audit-run", wait_ = FALSE)
  app$click("audit-cancel", wait_ = FALSE)
  wait_text(app, "#audit-status", "cancelled")

  app$set_inputs(nav = "runtime")
  app$set_inputs(`runtime-offline` = TRUE)
  app$click("runtime-plan", wait_ = FALSE)
  wait_text(app, "#runtime-result", "Planned")
  expect_false(dir.exists(file.path(live$parent, "runtime")))
  capture(app, "runtime", "#runtime-result")
  app$click("runtime-run", wait_ = FALSE)
  wait_text(app, "#runtime-status", "blocked")
  wait_text(app, "#runtime-result", "No qualified model fits")
  expect_false(dir.exists(file.path(live$parent, "runtime")))

  # An explicit model bypasses automatic selection, but offline acquisition
  # must still be refused before a runtime can be created or started.
  app$set_inputs(`runtime-model` = "qwen2.5-coder:1.5b")
  app$click("runtime-run", wait_ = FALSE)
  wait_text(app, "#runtime-result", "acquisition is disabled")
  expect_match(app$get_text("#runtime-result"), "acquisition is disabled", fixed = TRUE)
})
