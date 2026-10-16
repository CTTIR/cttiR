test_that("workflowr migration previews without writes and preserves edited manuscripts", {
  skip_if(Sys.which("git") == "")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  p <- project("Migration", "methods", "Describe measurements", new_parent())
  publication <- file.path(p$path, "publications", p$spec$publications[[1L]]$slug, "manuscript/README.md")
  write_bytes("# Reviewed manuscript\n", publication)
  before <- tree_state(p$path)
  options <- list(workflow = list(reporting = "workflowr", reporting_preset = "minimal", git = TRUE))
  preview <- sync(p$path, options = options)
  expect_identical(preview$state, "planned")
  expect_identical(preview$reporting_migration$from, "generic")
  expect_identical(preview$reporting_migration$to, "workflowr")
  expect_identical(tree_state(p$path), before)
  expect_contains(preview$actions$path, c("_workflowr.yml", "research.Rproj"))
  applied <- sync(p$path, options = options, dry_run = FALSE)
  expect_identical(applied$state, "applied")
  saved <- read_project(p$path)
  expect_identical(saved$spec$project$id, p$spec$project$id)
  expect_identical(saved$spec$publications, p$spec$publications)
  expect_identical(saved$lock$workflow$bundle, "workflowr-0.4.0")
  replay_options <- options
  replay_options$knowledge <- list(refresh = TRUE)
  expect_identical(sync(p$path, options = replay_options)$state, "planned")
  expect_identical(readLines(publication), "# Reviewed manuscript")
  expect_false(audit(p$path, scope = "project")$overall_status %in% c("fail", "not_tested"))
  expect_true(all(sync(p$path)$actions$action == "skip" | sync(p$path)$actions$action == "preserve"))
})

test_that("migration cannot overwrite edited site configuration", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  p <- project("Conflict", "methods", "Describe measurements", new_parent())
  write_bytes("# User site settings\n", file.path(p$path, "analysis/_site.yml"))
  before <- tree_state(p$path)
  result <- sync(p$path, options = list(workflow = list(reporting = "workflowr", git = TRUE)), dry_run = FALSE)
  expect_identical(result$state, "conflict")
  expect_contains(result$conflicts, "analysis/_site.yml")
  expect_identical(tree_state(p$path), before)
})

test_that("older catalog pins require explicit local refresh for workflowr migration", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  spec <- default_spec("Older", "methods", "Describe measurements", "older")
  spec$provenance$catalog_id <- "92627577cce4722d0673e6c5a71773b0a8b35d6dbd9e38f6bd7b959972a9e67c"
  options <- list(workflow = list(reporting = "workflowr", git = TRUE))
  expect_error(sync_spec(spec, NULL, options), "lacks complete workflowr")
  options$knowledge <- list(refresh = TRUE)
  migrated <- sync_spec(spec, NULL, options)
  expect_identical(migrated$provenance$catalog_id, resolve_catalog()$content_id)
  expect_identical(project_bundle(migrated, project_bundle(spec)$lock)$lock$catalog_id,
    migrated$provenance$catalog_id)
  expect_error(sync_spec(spec, NULL, list(knowledge = list(refresh = TRUE))), "only for explicit migration")
})


test_that("catalog refresh refuses changed existing dependency sources", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  spec <- default_spec("Changed", "methods", "Describe measurements", "changed")
  candidate <- resolve_catalog()
  dependency <- route_dependencies(project_route(spec), candidate)[[1L]]$package
  at <- which(vapply(candidate$packages, function(p) identical(p$name, dependency), logical(1)))
  candidate$packages[[at]]$source_hash <- strrep("0", 64L)
  testthat::local_mocked_bindings(resolve_catalog = function(...) candidate)
  expect_error(sync_spec(spec, NULL, list(workflow = list(reporting = "workflowr", git = TRUE),
    knowledge = list(refresh = TRUE))), class = "cttir_api_mismatch")
})
