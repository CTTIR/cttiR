test_that("Exact-source candidate workflowr runner, descriptive HTML and content freshness", {
exact_library <- Sys.getenv("CTTIR_EXACT_WORKFLOWR_LIB", unset = "")
if (nzchar(exact_library)) {
  for (package in c("workflowr", "digest")) {
    stopifnot(normalizePath(find.package(package)) == normalizePath(file.path(exact_library, package)))
  }
}
evidence <- Sys.getenv("CTTIR_WORKFLOWR_EVIDENCE", unset = "")
skip_if(!nzchar(evidence), "Explicit lifecycle fixture; use tools/verify-workflowr-runner.R")
stopifnot(!file.exists(evidence))
dir.create(evidence, recursive = TRUE)
evidence <- normalizePath(evidence)
parent <- tempfile("cttir runner integration ")
dir.create(parent)
tryCatch({
  dataset <- file.path(parent, "external synthetic.csv")
  data <- data.frame(value = seq_len(20), group = rep(c("A", "B"), 10))
  names(data)[[1]] <- "<script>fixture_probe</script>"
  utils::write.csv(data, dataset, row.names = FALSE)
  data_source <- list(id = "synthetic", label = "Synthetic integration fixture", logical_uri = NULL,
    format = "csv", access_class = "public", checksum = NULL, checksum_status = "unknown", schema_ref = NULL)
  migration <- identical(Sys.getenv("CTTIR_WORKFLOWR_MIGRATION"), "1")
  workflow <- list(table_backend = "none", git = TRUE)
  if (!migration) workflow <- c(workflow, list(reporting = "workflowr", reporting_preset = "minimal"))
  p <- project("Report fixture", "methods", "Describe a synthetic fixture", path = parent,
    options = list(data_sources = list(data_source), workflow = workflow,
      analysis = list(aim = "descriptive", outcome_family = "continuous", unit_structure = "independent",
        approved = TRUE, mapping = list(data_source_id = "synthetic", outcome = names(data)[[1]], missing_data = "fail"))))
  if (migration) {
    migration_options <- list(workflow = list(reporting = "workflowr", reporting_preset = "minimal", git = TRUE))
    preview <- sync(p$path, options = migration_options)
    stopifnot(identical(preview$state, "planned"))
    applied <- sync(p$path, options = migration_options, dry_run = FALSE)
    stopifnot(identical(applied$state, "applied"))
  }
  root <- p$path
  yaml::write_yaml(list(bindings = list(list(id = "synthetic", path = dataset))), file.path(root, ".cttir/local.yml"))
  run <- function(script, tag) {
    result <- processx::run(file.path(R.home("bin"), "Rscript"), c("--vanilla", file.path(root, "code", script)),
      wd = tempdir(), timeout = 120000, error_on_status = FALSE)
    writeLines(c(result$stdout, result$stderr), file.path(evidence, paste0(tag, ".log")))
    result
  }
  calculation <- run("run_workflow.R", "computation")
  stopifnot(calculation$status == 0L)
  receipt_path <- file.path(root, ".cttir/report-receipt.json")
  receipt <- jsonlite::read_json(receipt_path)
  stopifnot(length(receipt$external_inputs) == 1L,
    receipt$external_inputs[[1]]$dataset_id == "synthetic",
    !grepl(dataset, paste(readLines(receipt_path), collapse = ""), fixed = TRUE))
  rendered <- run("render_report.R", "render")
  stopifnot(rendered$status == 0L, file.exists(file.path(root, "docs/index.html")))
  report <- paste(readLines(file.path(root, "docs/03_report.html"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("mean 10.5", report, fixed = TRUE),
    grepl("&lt;script&gt;fixture_probe&lt;/script&gt;", report, fixed = TRUE),
    !grepl("<script>fixture_probe</script>", report, fixed = TRUE))
  stopifnot(file.copy(file.path(root, "docs/03_report.html"), file.path(evidence, "descriptive-report.html")))
  expect_match(report, "workflowr-report", fixed = TRUE)
  publication_analysis <- file.path(root, "publications", p$spec$publications[[1L]]$slug, "analysis/new-model.R")
  writeLines("# A newly introduced analysis", publication_analysis)
  stale_publication <- run("render_report.R", "stale-publication-analysis")
  expect_true(stale_publication$status != 0L)
  expect_match(stale_publication$stderr, "changed")
  unlink(publication_analysis)
  old_output <- file.path(root, "output/previous-result.csv")
  writeLines("old result", old_output)
  untracked <- run("run_workflow.R", "untracked-output")
  stopifnot(untracked$status != 0L, grepl("untracked output", untracked$stderr), !file.exists(receipt_path))
  unlink(old_output)
  rerun <- run("run_workflow.R", "recomputation")
  stopifnot(rerun$status == 0L, file.exists(receipt_path))
  original <- readBin(dataset, "raw", file.info(dataset)$size)
  before <- file.info(dataset)$mtime
  changed <- charToRaw(sub("20", "21", rawToChar(original), fixed = TRUE))
  stopifnot(length(changed) == length(original))
  writeBin(changed, dataset)
  Sys.setFileTime(dataset, before)
  rejected <- run("render_report.R", "stale-external-input")
  stopifnot(rejected$status != 0L, grepl("changed", rejected$stderr))
  writeBin(original, dataset)
  writeLines("# added code", file.path(root, "code/new.R"))
  rejected_code <- run("render_report.R", "stale-code")
  stopifnot(rejected_code$status != 0L, grepl("changed", rejected_code$stderr))
  jsonlite::write_json(list(status = "candidate_runner_render_freshness_passed",
    external_input_same_size_mtime_rejected = TRUE, added_code_rejected = TRUE,
    receipt_omits_external_paths = TRUE, publication_analysis_change_rejected = TRUE, previous_outputs_not_adopted = TRUE,
    rendered_reference_mean = 10.5, html_labels_escaped = TRUE,
    libraries = as.list(stats::setNames(vapply(c("workflowr", "digest"), find.package, ""), c("workflowr", "digest"))),
    versions = as.list(stats::setNames(vapply(c("workflowr", "digest"), function(x) as.character(utils::packageVersion(x)), ""), c("workflowr", "digest"))),
    public_profile_selection = if (migration) "sync_migration" else "explicit_workflowr"),
    file.path(evidence, "receipt.json"), pretty = TRUE, auto_unbox = TRUE)
}, finally = unlink(parent, recursive = TRUE))

})
