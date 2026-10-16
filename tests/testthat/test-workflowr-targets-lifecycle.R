test_that("workflowr targets computes once and invalidates changed content and missing outputs", {
evidence <- Sys.getenv("CTTIR_TARGETS_EVIDENCE", unset = "")
skip_if(!nzchar(evidence), "Explicit scheduler lifecycle fixture")
stopifnot(!file.exists(evidence))
dir.create(evidence, recursive = TRUE)
evidence <- normalizePath(evidence)
parent <- tempfile('targets integration ')
dir.create(parent)
tryCatch({
  data <- file.path(parent, 'synthetic.csv')
  write.csv(data.frame(value = seq_len(20)), data, row.names = FALSE)
  p <- project('Targets report', 'methods', 'Describe measurements', parent,
    options = list(workflow = list(reporting = 'workflowr', pipeline = 'targets', git = TRUE, table_backend = 'none'),
      data_sources = list(list(id = 'synthetic', label = 'Synthetic', logical_uri = NULL, format = 'csv',
        access_class = 'public', checksum = NULL, checksum_status = 'unknown', schema_ref = NULL)),
      analysis = list(aim = 'descriptive', outcome_family = 'continuous', unit_structure = 'independent', approved = TRUE,
        mapping = list(data_source_id = 'synthetic', outcome = 'value', missing_data = 'fail'))))
  saved <- read_project(p$path)
  expect_identical(Filter(function(x) x$stage == "pipeline", saved$lock$workflow$stages)[[1L]]$adapter,
    "standard.workflowr_targets@0.4.0")
  report <- audit(p$path, scope = "project")
  saveRDS(report, file.path(evidence, "project-audit.rds"))
  expect_false(report$overall_status %in% c("fail", "not_tested"))
  yaml::write_yaml(list(bindings = list(list(id = 'synthetic', path = data))), file.path(p$path, '.cttir/local.yml'))
  run <- function(label) {
    result <- processx::run(file.path(R.home('bin'), 'Rscript'), c('--vanilla', '-e',
      'targets::tar_make(reporter = "balanced", callr_function = NULL)'),
      wd = p$path, timeout = 120000, error_on_status = FALSE)
    writeLines(c(result$stdout, result$stderr), file.path(evidence, paste0(label, '.log')))
    stopifnot(result$status == 0L)
  }
  run('initial')
  expect_identical(jsonlite::read_json(file.path(p$path, 'demo/receipt.json'))$status, 'passed')
  receipt <- file.path(p$path, '.cttir/report-receipt.json')
  before <- readBin(receipt, 'raw', file.info(receipt)$size)
  sentinel <- as.POSIXct('2000-01-01', tz = 'UTC')
  Sys.setFileTime(receipt, sentinel)
  run('unchanged')
  stopifnot(identical(before, readBin(receipt, 'raw', file.info(receipt)$size)),
    file.info(receipt)$mtime == sentinel)
  mtime <- file.info(data)$mtime
  original <- readBin(data, 'raw', file.info(data)$size)
  changed <- charToRaw(sub('20', '21', rawToChar(original), fixed = TRUE))
  stopifnot(length(changed) == length(original))
  writeBin(changed, data)
  Sys.setFileTime(data, mtime)
  run('same-size-mtime-edit')
  stopifnot(file.info(receipt)$mtime != sentinel,
    !identical(before, readBin(receipt, 'raw', file.info(receipt)$size)))
  table <- file.path(p$path, 'output/tables/descriptives.csv')
  expected_table <- readBin(table, 'raw', file.info(table)$size)
  unlink(table)
  run('deleted-output')
  stopifnot(identical(readBin(table, 'raw', file.info(table)$size), expected_table))
  Sys.setFileTime(receipt, sentinel)
  run('after-repair-unchanged')
  stopifnot(file.info(receipt)$mtime == sentinel)
  rendered <- processx::run(file.path(R.home('bin'), 'Rscript'), c('--vanilla', file.path(p$path, 'code/render_report.R')),
    wd = tempdir(), error_on_status = FALSE, timeout = 120000)
  writeLines(c(rendered$stdout, rendered$stderr), file.path(evidence, 'render.log'))
  stopifnot(rendered$status == 0L)
  html <- paste(readLines(file.path(p$path, 'docs/04_descriptive.html'), warn = FALSE), collapse = '\n')
  stopifnot(grepl('mean 10.6', html, fixed = TRUE), grepl('workflowr-report', html, fixed = TRUE))
  file.copy(file.path(p$path, 'docs/04_descriptive.html'), file.path(evidence, 'descriptive-report.html'))
  expect_match(html, 'workflowr-report', fixed = TRUE)
  blocked <- project('Blocked targets', 'methods', 'Undecided analysis', parent,
    options = list(workflow = list(reporting = 'workflowr', pipeline = 'targets', git = TRUE, table_backend = 'none')))
  check <- processx::run(file.path(R.home('bin'), 'Rscript'), c('--vanilla', '-e',
    'targets::tar_make(reporter = "silent", callr_function = NULL); jsonlite::write_json(unclass(targets::tar_read(study)), "blocked-status.json", auto_unbox = TRUE)'),
    wd = blocked$path, timeout = 120000, error_on_status = FALSE)
  writeLines(c(check$stdout, check$stderr), file.path(evidence, 'blocked.log'))
  expect_identical(check$status, 0L)
  status <- jsonlite::read_json(file.path(blocked$path, 'blocked-status.json'))
  expect_identical(status$status, 'blocked')
  expect_false(status$data_read)
  expect_false(file.exists(file.path(blocked$path, '.cttir/report-receipt.json')))

  jsonlite::write_json(list(status = 'candidate_targets_content_invalidation_passed',
    unchanged_study_reused = TRUE, same_size_mtime_edit_recomputed = TRUE, reference_mean = 10.55, rendered_mean = 10.6, deleted_output_recomputed = TRUE, missing_prerequisites_blocked = TRUE,
    public_selection = 'enabled'), file.path(evidence, 'receipt.json'), pretty = TRUE, auto_unbox = TRUE)
}, finally = unlink(parent, recursive = TRUE))

})
