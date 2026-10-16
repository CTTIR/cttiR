test_that("candidate workflowr presets have one root and explicit page lists", {
  for (preset in c("minimal", "standard")) {
    files <- workflowr_profile_files(preset)
    expect_length(grep("[.]Rproj$", names(files)), 1L)
    expect_false(".Rprofile" %in% names(files))
    config <- jsonlite::fromJSON(files[["config/report-pages.json"]])
    expect_length(config$pages, if (preset == "minimal") 4L else 9L)
    expect_true(all(config$pages %in% names(files)))
    site <- yaml::yaml.load(files[["analysis/_site.yml"]], eval.expr = FALSE)
    expect_named(site$output, "workflowr::wflow_html")
    expect_identical(site$output_dir, "../docs")
    settings <- yaml::yaml.load(files[["_workflowr.yml"]], eval.expr = FALSE)
    expect_identical(settings$knit_root_dir, ".")
    expect_identical(settings$seed, 12345L)
    expect_false(any(grepl("install.packages|pacman::p_load|wflow_publish", unlist(files))))
  }
})

test_that("workflowr descriptive stage accepts an outcome without predictors", {
  skip_if_not_installed("dplyr")
  env <- new.env(parent = baseenv())
  sys.source(system.file("templates", "workflowr-0.4.0", "code/R/cttir_workflow.R", package = "cttiR"), env)
  analysis <- list(aim = "descriptive", mapping = list(outcome = "score", missing_data = "fail"))
  tidy <- env$cw_tidy(data.frame(score = seq_len(20)), analysis)
  expect_identical(names(tidy$data), "response")
  expect_identical(tidy$data$response, seq_len(20))
  summary <- env$cw_describe(tidy, backend = "none")
  expect_equal(summary$table$n, 20)
  expect_match(summary$table$summary, "mean 10.5", fixed = TRUE)
  expect_identical(summary$inference, "none")
})

test_that("project generation overlays workflowr without duplicate file paths", {
  spec <- default_spec("Report", "primary_research", "Describe measurements", "report")
  original <- render_project(spec)
  expect_false("_workflowr.yml" %in% names(original))
  spec$workflow$reporting <- "workflowr"
  for (preset in c("minimal", "standard")) {
    spec$workflow$reporting_preset <- preset
    route <- route_workflow(spec, "standard_reflowR")
    files <- render_project(spec, route)
    expect_identical(anyDuplicated(names(files)), 0L)
    expect_identical(files[["code/render_report.R"]], workflowr_profile_files(preset)[["code/render_report.R"]])
    config <- yaml::yaml.load(files[["config/workflow.yml"]], eval.expr = FALSE)
    expect_identical(config$bundle, "workflowr-0.4.0")
    expect_identical(jsonlite::fromJSON(files[["config/report-pages.json"]])$preset, preset)
    expect_match(files[["README.md"]], "root site uses workflowr", fixed = TRUE)
    expect_false(grepl("workflowr are not invoked", files[["README.md"]], fixed = TRUE))
    expect_match(files[[".gitignore"]], ".cttir/report-receipt.json", fixed = TRUE)
    publication_paths <- grep("^publications/", names(original), value = TRUE)
    expect_true(length(publication_paths) > 0L)
    expect_identical(files[publication_paths], original[publication_paths])
    manifest <- project_manifest(files, "0.3.0", "workflowr")
    ownership <- stats::setNames(vapply(manifest, function(x) x$ownership, ""), names(files))
    expect_identical(unname(ownership["analysis/_site.yml"]), "managed")
    expect_identical(unname(ownership["metadata/workflow-template.json"]), "managed")
    expect_identical(unname(ownership["analysis/index.Rmd"]), "user")
  }
})


test_that("report presets are typed and cannot be silently ignored", {
  expect_error(validate_config(list(workflow = list(reporting_preset = "other"))), class = "cttir_schema_error")
  expect_error(project("Preset", "methods", "Describe measurements", tempdir(), dry_run = TRUE,
    options = list(workflow = list(reporting_preset = "minimal"))),
    "Reporting presets require")
})
