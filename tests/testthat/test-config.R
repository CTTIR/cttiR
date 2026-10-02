test_that("unknown fields, malformed values and unsafe YAML are rejected", {
  expect_error(validate_config(list(unknown = TRUE)), class = "cttir_schema_error")
  expect_error(validate_config(list(workflow = list(git = NA))), class = "cttir_input_error")
  expect_error(validate_config(list(workflow = list(network = "sometimes"))), class = "cttir_schema_error")
  expect_error(validate_config(list(x = 1, x = 2)), class = "cttir_input_error")
  expect_error(validate_config(list(figures = list(panel_composer = "other"))), class = "cttir_schema_error")
  p <- tempfile(fileext = ".yml")
  on.exit(unlink(p))
  writeLines("research: !expr stop('should never run')", p)
  expect_error(validate_config(p), class = "cttir_input_error")
  writeLines("research: [unclosed", p)
  expect_error(validate_config(p), class = "cttir_schema_error")
  for (x in list(NA, character(), c("a", "b"), "", 1)) {
    expect_error(project(x, "methods", "Goal", tempdir(), dry_run = TRUE), class = "cttir_input_error")
  }
  expect_error(project("Valid", "invalid_type", "Goal", tempdir(), dry_run = TRUE), class = "cttir_schema_error")
  expect_error(project("Valid", "methods", "Goal", tempdir(), dry_run = NA), class = "cttir_input_error")
})

test_that("config precedence, keyed publications and explicit nulls work", {
  parent <- new_parent()
  config <- list(
    project = list(name = "Ignored name"), research = list(design = "cohort", data_origin = "existing_dataset"),
    publications = list(
      list(id = "pub01", title = "Main paper"),
      list(
        id = "pub02", title = "Review", slug = "pub02_review", type = "systematic_review",
        research_class = "secondary_research", analysis_role = "unknown", data_origin = "literature"
      )
    )
  )
  options <- list(
    research = list(design = NULL, analysis_role = "secondary_analysis"),
    publications = list(list(id = "pub02", title = "Updated review"))
  )
  expect_warning(p <- project("Actual name", "primary_research", "Goal", parent, config, options),
    "configuration sets project.name", class = "cttir_config_override")
  expect_identical(p$spec$project$name, "Actual name")
  overridden <- Filter(function(d) identical(d$field, "/project/name") && identical(d$origin, "config"), p$spec$decisions)
  expect_match(overridden[[1]]$reason, "overridden by the required argument")
  expect_null(p$spec$research$design)
  expect_identical(p$spec$research$data_origin, "existing_dataset")
  expect_identical(p$spec$research$analysis_role, "secondary_analysis")
  expect_identical(p$spec$publications[[2]]$title, "Updated review")
  expect_true(dir.exists(file.path(p$path, "publications/pub02_review/analysis")))
  expect_identical(validate_spec(file.path(p$path, "cttir-project.yml")), p$spec)
  expect_error(project("Actual name", "methods", "Goal", parent,
      options = list(project = list(name = "Override"))
    ), class = "cttir_input_error")
})

test_that("unimplemented integrations are never silently claimed", {
  for (workflow in list(list(profile = "cttir_specialist"), list(table_backend = "gtsummary"))) {
    expect_error(project("Pending", "methods", "Goal", tempdir(),
        options = list(workflow = workflow), dry_run = TRUE
      ), class = "cttir_api_mismatch")
  }
  expect_error(project("Pending", "methods", "Goal", tempdir(),
      options = list(workflow = list(prepare_environment = TRUE)), dry_run = TRUE
    ), class = "cttir_input_error")
  p <- project("Standard", "methods", "Goal", tempdir(), dry_run = TRUE)
  expect_equal(p$spec$workflow$profile, "standard_reflowR")
  expect_false("workflow_approval_pending" %in% p$readiness$blockers)
  expect_true(all(vapply(p$readiness$workflow$stages, function(x) identical(x$status, "approved"), logical(1))))
  expect_equal(p$spec$workflow$table_backend, "DescrTab2")
  bad <- p$spec
  bad$schema_version <- 2L
  expect_error(validate_spec(bad), class = "cttir_schema_error")
  bad <- p$spec
  bad$publications <- rep(bad$publications, 2)
  expect_error(validate_spec(bad), class = "cttir_schema_error")
  bad <- p$spec
  bad$workflow$project_backend <- "cttir"
  expect_error(validate_spec(bad), class = "cttir_schema_error")
})

test_that("multiline goals and literal tag text round trip without evaluation", {
  parent <- new_parent()
  goal <- "Document the literal !expr tag.\nCompare two safe parser inputs."
  p <- project("Text", "software", goal, parent)
  expect_equal(validate_spec(file.path(p$path, "cttir-project.yml"))$project$goal, goal)
  expect_error(validate_config(structure(list(), class = "custom")), class = "cttir_input_error")
  expect_error(validate_config(list(research = structure(list(notes = "text"), class = "custom"))), class = "cttir_input_error")
})

test_that("schema versions other than the supported one get a precise typed error", {
  code <- function(expr) tryCatch(expr, error = function(e) c(class(e)[[1]], e$code, paste(e$field, collapse = "")))
  expect_equal(code(validate_config(list(schema_version = 0L))), c("cttir_schema_error", "unsupported_schema_version", "/schema_version"))
  expect_match(tryCatch(validate_config(list(schema_version = 0L)), error = conditionMessage), "older than the supported version 1")
  expect_equal(code(validate_config(list(schema_version = 1.5)))[1:2], c("cttir_schema_error", "schema_validation"))
  p <- project("Versioned", "methods", "Goal", new_parent())
  file <- file.path(p$path, "cttir-project.yml")
  writeLines(sub("^schema_version: 1$", "schema_version: 0", readLines(file)), file)
  before <- tree_hashes(p$path)
  expect_equal(code(sync(p$path))[1:2], c("cttir_schema_error", "unsupported_schema_version"))
  expect_match(tryCatch(read_project(p$path), error = conditionMessage), "no migration from version 0 is defined")
  expect_identical(tree_hashes(p$path), before)
  expect_identical(migrate_spec(p$spec)$steps, list())
})
