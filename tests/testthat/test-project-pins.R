test_that("pin inventory includes optional packages and rejects unresolved versions", {
  version <- as.character(utils::packageVersion("yaml"))
  deps <- list(list(package = "yaml", version = version, required = FALSE),
    list(package = "cttirMissingFixturePackage", version = "1.0", required = FALSE),
    list(package = "jsonlite", version = "999.0", required = TRUE),
    list(package = "digest", version = NULL, required = FALSE))
  result <- project_pin_versions(deps, new_parent(), "none")
  expect_equal(vapply(result$pins, function(x) x$status, character(1)),
    c("match", "not_installed", "version_mismatch", "invalid_pin"))
  expect_false(result$pins[[1]]$required)
  expect_equal(length(result$pins), length(deps))
})

test_that("renv pin inventory uses the project library for optional CRAN packages", {
  root <- new_parent()
  version <- as.character(utils::packageVersion("yaml"))
  deps <- list(list(package = "yaml", version = version, required = FALSE))
  expect_equal(project_pin_versions(deps, root, "renv")$pins[[1]]$status, "not_installed")
  library <- file.path(root, "renv", "library", "test-os", paste0("R-", running_r_minor()), R.version$platform)
  dir.create(file.path(library, "yaml"), recursive = TRUE)
  description <- file.path(library, "yaml", "DESCRIPTION")
  writeLines(c("Package: yaml", "Version: 999.0"), description)
  expect_equal(project_pin_versions(deps, root, "renv")$pins[[1]]$status, "version_mismatch")
  writeLines(c("Package: yaml", paste("Version:", version)), description)
  expect_equal(project_pin_versions(deps, root, "renv")$pins[[1]]$status, "match")
})

test_that("all-pin audit warns at scaffold level and fails an analysis-ready claim", {
  for (mode in c("none", "renv")) {
    p <- project("Audit pins", "methods", "Describe a cohort", new_parent())
    project <- read_project(p$path)
    project$spec$workflow$environment <- mode
    project$lock$dependencies <- list(list(package = "yaml", version = "999.0", required = FALSE))
    local_mocked_bindings(audit_project = function(context) project, audit_spec = function(context) project$spec)
    context <- audit_context(p$path, "project", FALSE, FALSE)
    before <- tree_state(p$path)
    expect_equal(audit_res_all_pins(context)$status, "warning")
    project$spec$workflow$readiness <- "analysis_ready"
    expect_equal(audit_res_all_pins(context)$status, "fail")
    expect_identical(tree_state(p$path), before)
  }
})

test_that("invalid package names never become metadata paths", {
  local_mocked_bindings(library_version = function(...) stop("unexpected read"))
  result <- project_pin_versions(list(list(package = "../outside", version = "1.0")), new_parent(), "renv")
  expect_equal(result$pins[[1]]$status, "invalid_pin")
})

test_that("all-pin audit is registered with readiness-dependent enforcement", {
  check <- audit_checks()[["RES-008"]]
  expect_true(is.function(check$required))
  p <- project("Registered pins", "methods", "Describe outcomes", new_parent())
  report <- audit(p$path, scope = "project")
  expect_true("RES-008" %in% report$checks$id)
  row <- report$checks[report$checks$id == "RES-008", ]
  expect_true(row$status %in% c("pass", "warning"))
})
