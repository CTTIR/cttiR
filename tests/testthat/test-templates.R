legacy_project <- function(parent, name, version, goal = "Goal") {
  spec <- resolve_spec(name, "methods", goal, NULL, list())
  spec$provenance$template_version <- version
  bundle <- project_bundle(spec)
  root <- file.path(parent, safe_slug(name))
  for (path in names(bundle$files)) {
    dest <- file.path(root, path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(bundle$files[[path]]), dest)
  }
  structure(list(path = normalizePath(root, winslash = "/")), class = "cttir_project")
}

test_that("adapted templates are pinned, inert and structurally valid", {
  p <- legacy_project(new_parent(), "Template", "0.2.0", "<script>never execute</script>")
  manifest <- read_document(file.path(p$path, "metadata/reflowr-template.json"))
  expect_equal(manifest$mode, "adapted_templates")
  expect_false(manifest$initializer_invoked)
  expect_false(manifest$workflowr_invoked)
  expect_equal(manifest$source_revision, "cd1243a068ff2c8fb6796e34b58f6c6ce6af87e8")
  expect_equal(read_project(p$path)$lock$template_version, "0.2.0")
  for (path in names(manifest$files)) {
    expect_identical(file_hash(file.path(p$path, path)), manifest$files[[path]])
  }
  site <- yaml::read_yaml(file.path(p$path, "analysis/_site.yml"))
  expect_length(site$navbar$left[[2]]$menu, 3L)
  expect_named(site$output, "rmarkdown::html_document")
  expect_false(dir.exists(file.path(p$path, "docs")))
  expect_false(dir.exists(file.path(p$path, ".git")))
  template_text <- paste(unlist(reflowr_templates()), collapse = "\n")
  expect_false(grepl("<script>never execute", template_text, fixed = TRUE))
  before <- tree_hashes(p$path)
  expect_true(all(sync(p$path)$actions$action == "skip"))
  expect_identical(tree_hashes(p$path), before)
  writeLines("Reviewed page", file.path(p$path, "analysis/01_read_data.Rmd"))
  result <- sync(p$path, options = list(project = list(language = "de")), dry_run = FALSE)
  expect_equal(result$state, "applied")
  expect_equal(readLines(file.path(p$path, "analysis/01_read_data.Rmd")), "Reviewed page")
  writeLines("{}", file.path(p$path, "metadata/reflowr-template.json"))
  expect_contains(sync(p$path)$conflicts, "metadata/reflowr-template.json")
})

test_that("older accepted templates are not silently migrated", {
  parent <- new_parent()
  root <- legacy_project(parent, "Legacy", "0.1.0")$path
  before <- tree_hashes(root)
  expect_true(all(project("Legacy", "methods", "Goal", parent)$plan$action == "skip"))
  expect_true(all(sync(root)$actions$action == "skip"))
  expect_identical(tree_hashes(root), before)
  sync(root, options = list(project = list(language = "de")), dry_run = FALSE)
  expect_equal(read_project(root)$lock$template_version, "0.1.0")
  expect_false(file.exists(file.path(root, "code/render_report.R")))
  spec <- read_project(root)$spec
  spec$provenance$template_version <- "unknown"
  expect_error(project_bundle(spec), class = "cttir_api_mismatch")
})

test_that("synthetic template renders without importing study data", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())
  p <- project("Render", "methods", "Test templates", new_parent())
  result <- processx::run(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "code/render_report.R"), wd = p$path,
    timeout = 120000, error_on_status = FALSE)
  expect_equal(result$status, 0L, info = paste(result$stdout, result$stderr))
  html <- list.files(file.path(p$path, "docs"), pattern = "html$")
  expect_setequal(html, c("index.html", "01_read_data.html", "02_eda.html",
      "03_report.html", "synthetic_demo.html"))
  expect_false(dir.exists(file.path(p$path, "data/raw")))
})

test_that("a mismatched template lock is rejected before mutation", {
  p <- project("Mismatch", "methods", "Test pins", new_parent())
  lock <- read_document(file.path(p$path, "cttir-lock.json"))
  lock$template_version <- "0.1.0"
  write_bytes(json_text(lock, TRUE), file.path(p$path, "cttir-lock.json"))
  before <- tree_hashes(p$path)
  expect_error(sync(p$path, dry_run = FALSE), class = "cttir_schema_error")
  expect_identical(tree_hashes(p$path), before)
})
