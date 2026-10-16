workflowr_audit_fixture <- function() {
  spec <- default_spec("Audit report", "methods", "Describe measurements", "audit-report")
  spec$workflow$reporting <- "workflowr"
  spec$workflow$reporting_preset <- "minimal"
  files <- render_project(spec)
  root <- tempfile("workflowr-audit-")
  dir.create(root)
  for (path in names(files)) {
    dest <- file.path(root, path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    write_bytes(files[[path]], dest)
  }
  dir.create(file.path(root, ".git"))
  list(path = root, spec = spec, manifest = list(files = project_manifest(files, "0.3.0", "workflowr")))
}

test_that("workflowr audit verifies shipped profile identities without running reports", {
  p <- workflowr_audit_fixture()
  on.exit(unlink(p$path, recursive = TRUE))
  before <- tree_state(p$path)
  result <- audit_workflowr_bundle(p)
  expect_identical(result$status, "pass")
  expect_identical(tree_state(p$path), before)
  write_bytes("# An edited manuscript\n", file.path(p$path, "analysis/index.Rmd"))
  expect_identical(audit_workflowr_bundle(p)$status, "pass")
  write_bytes("Version: 1.0\n", file.path(p$path, "second.Rproj"))
  expect_identical(audit_workflowr_bundle(p)$status, "fail")
  unlink(file.path(p$path, "second.Rproj"))
  unlink(file.path(p$path, "analysis/index.Rmd"))
  expect_identical(audit_workflowr_bundle(p)$status, "fail")
})

test_that("workflowr audit refuses altered provenance, renderer and configuration", {
  for (path in c("metadata/workflow-template.json", "code/render_report.R", "_workflowr.yml",
      "analysis/_site.yml", "config/report-pages.json")) {
    p <- workflowr_audit_fixture()
    if (path == "metadata/workflow-template.json") {
      manifest <- read_document(file.path(p$path, path))
      manifest$workflowr_version <- "0.0.0"
      write_bytes(json_text(manifest), file.path(p$path, path))
    } else write_bytes("# changed\n", file.path(p$path, path))
    expect_identical(audit_workflowr_bundle(p)$status, "fail", info = path)
    unlink(p$path, recursive = TRUE)
  }
})

test_that("the standard bundle audit rejects a changed recorded template identity", {
  root <- tempfile("standard-manifest-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  spec <- default_spec("Standard", "methods", "Describe measurements", "standard")
  files <- render_project(spec)
  for (path in names(files)) {
    dest <- file.path(root, path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    write_bytes(files[[path]], dest)
  }
  p <- list(path = root, spec = spec, manifest = list(files = project_manifest(files, "0.3.0")))
  context <- audit_context(root, "project", FALSE, FALSE)
  assign("project", p, context$cache)
  expect_identical(audit_std_bundle(context)$status, "pass")
  path <- file.path(root, "metadata/workflow-template.json")
  manifest <- read_document(path)
  manifest$files[[1L]] <- strrep("0", 64L)
  write_bytes(json_text(manifest), path)
  expect_identical(audit_std_bundle(context)$status, "fail")
})


test_that("catalog approval requirements follow the selected reporting backend", {
  catalog <- resolve_catalog()
  catalog$packages <- Filter(function(p) !identical(p$name, "workflowr"), catalog$packages)
  generic <- standard_capability_approvals(catalog)
  workflowr <- standard_capability_approvals(catalog, reporting = "workflowr")
  expect_false("std.report.workflowr" %in% names(generic))
  expect_true("std.report.render" %in% names(generic))
  expect_false("std.report.render" %in% names(workflowr))
  expect_identical(workflowr$std.report.workflowr$status, "approval_pending")
})
