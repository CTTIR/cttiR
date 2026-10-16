test_that("public workflowr creation is explicit, pinned and idempotent", {
  skip_if(Sys.which("git") == "")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  parent <- new_parent()
  for (preset in c("minimal", "standard")) {
    options <- list(workflow = list(reporting = "workflowr", reporting_preset = preset, git = TRUE))
    p <- project(paste("Report", preset), "methods", "Describe measurements", parent, options = options)
    saved <- read_project(p$path)
    expect_true(file.exists(file.path(p$path, "_workflowr.yml")))
    expect_identical(saved$lock$workflow$bundle, "workflowr-0.4.0")
    expect_true(dir.exists(file.path(p$path, ".git")))
    expect_identical(processx::run("git", c("rev-parse", "--verify", "HEAD"), wd = p$path,
      error_on_status = FALSE)$status, 128L)
    before <- tree_hashes(p$path)
    project(paste("Report", preset), "methods", "Describe measurements", parent, options = options)
    expect_identical(tree_hashes(p$path), before)
    expect_true(all(sync(p$path)$actions$action == "skip"))
    report <- audit(p$path, scope = "project")
    expect_false(report$overall_status %in% c("fail", "not_tested"))
    pins <- vapply(saved$lock$dependencies, function(x) x$package, "")
    expect_contains(pins, c("workflowr", "digest"))
  }
})

test_that("workflowr requires its root and supports the reviewed scheduler", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  expect_error(project("No Git", "methods", "Describe measurements", new_parent(), dry_run = TRUE,
    options = list(workflow = list(reporting = "workflowr"))), "explicit root Git")
  expect_no_error(project("Targets", "methods", "Describe measurements", new_parent(), dry_run = TRUE,
    options = list(workflow = list(reporting = "workflowr", git = TRUE, pipeline = "targets"))))
})

test_that("the prior catalog remains usable and cannot silently enable workflowr", {
  old_id <- "92627577cce4722d0673e6c5a71773b0a8b35d6dbd9e38f6bd7b959972a9e67c"
  catalog <- catalog_snapshot(old_id)
  expect_identical(catalog$content_id, old_id)
  spec <- default_spec("Historical", "methods", "Describe measurements", "historical")
  spec$provenance$catalog_id <- old_id
  route <- project_route(spec)
  expect_identical(route_summary(route)$bundle, "standard-0.3.0")
  expect_false("workflowr" %in% vapply(route_dependencies(route, catalog), function(x) x$package, ""))
  spec$workflow$reporting <- "workflowr"
  spec$workflow$git <- TRUE
  expect_error(check_supported_workflow(spec, list()), "lacks complete workflowr")
})
