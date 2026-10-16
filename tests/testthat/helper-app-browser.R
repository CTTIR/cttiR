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
