test_that("workflowr routing requires its own revision-scoped adapter approval", {
  spec <- default_spec("Report", "primary_research", "Describe measurements", "report")
  catalog <- resolve_catalog()
  catalog$packages <- Filter(function(p) !identical(p$name, "workflowr"), catalog$packages)
  generic <- route_workflow(spec, "standard_reflowR", catalog)
  report <- function(route) Filter(function(x) identical(x$stage, "report"), route$stages)[[1L]]
  expect_identical(report(generic)$capability, "std.report.render")
  spec$workflow$reporting <- "workflowr"
  routed <- route_workflow(spec, "standard_reflowR", catalog)
  expect_identical(report(routed)$capability, "std.report.workflowr")
  expect_identical(report(routed)$status, "approval_pending")
  expect_contains(routed$approval_pending, "std.report.workflowr")
  cap <- capability_registry()$capabilities$std.report.workflowr
  expect_setequal(cap$packages, c("workflowr", "digest", "jsonlite", "yaml", "knitr", "utils", "stats"))
  expect_contains(cap$callables, c("workflowr::wflow_build", "workflowr::wflow_html", "digest::digest"))
  expect_false(cap$specialist)
  expect_identical(route_summary(generic)$bundle, "standard-0.3.0")
  expect_identical(route_summary(routed)$bundle, "workflowr-0.4.0")
})
