test_that("application construction never creates a project", {
  skip_if_not_installed("shiny")
  parent <- new_parent()
  withr::local_dir(parent)
  expect_s3_class(setup_app(launch.browser = FALSE), "shiny.appobj")
  expect_length(list.files(parent, all.files = TRUE, no.. = TRUE), 0L)
  expect_error(app_config("{invalid}"), class = "cttir_input_error")
  expect_error(app_config('{"unknown":true}'), class = "cttir_schema_error")
})

test_that("Fast and Detailed share a draft and require a current preview", {
  skip_if_not_installed("shiny")
  parent <- new_parent()
  shiny::testServer(builder_server, args = list(worker = app_test_worker), {
    session$setInputs(name = "UI project", type = "methods", goal = "Goal", parent = parent, config = "{}", mode = "fast")
    session$setInputs(apply = 1)
    expect_match(state$status, "no accepted preview", fixed = TRUE)
    expect_false(dir.exists(file.path(parent, "ui_project")))
    session$setInputs(preview = 1)
    session$flushReact()
    expect_s3_class(state$preview, "cttir_project")
    initial <- draft()
    session$setInputs(mode = "detailed")
    expect_equal(draft(), initial)
    session$setInputs(goal = "Changed goal", apply = 2)
    expect_match(state$status, "draft changed", fixed = TRUE)
    expect_false(dir.exists(file.path(parent, "ui_project")))
    session$setInputs(preview = 2)
    session$flushReact()
    session$setInputs(apply = 3)
    session$flushReact()
    expect_true(dir.exists(file.path(parent, "ui_project")))
    expect_equal(read_project(file.path(parent, "ui_project"))$spec$project$goal, "Changed goal")
    expect_null(state$accepted)
  })
})

test_that("Configure uses synchronization and refuses files changed after preview", {
  skip_if_not_installed("shiny")
  parent <- new_parent()
  p <- project("Configure fixture", "methods", "Goal", parent)
  expect_s3_class(configure(p$path, launch.browser = FALSE), "shiny.appobj")
  shiny::testServer(builder_server, args = list(path = p$path, worker = app_test_worker), {
    session$setInputs(config = '{"project":{"language":"de"}}', preview = 1)
    session$flushReact()
    expect_s3_class(state$preview, "cttir_sync")
    file <- file.path(p$path, "cttir-project.yml")
    writeLines(c(readLines(file), "# local edit"), file)
    before <- tree_hashes(p$path)
    session$setInputs(apply = 1)
    expect_match(state$status, "Project files changed", fixed = TRUE)
    expect_equal(tree_hashes(p$path), before)
  })
})

test_that("read-only jobs can be cancelled without duplicate workers", {
  skip_if_not_installed("shiny")
  parent <- new_parent()
  control <- new.env(parent = emptyenv())
  control$count <- 0L
  control$alive <- TRUE
  worker <- function(...) {
    control$count <- control$count + 1L
    list(is_alive = function() control$alive, kill = function() {
      control$alive <- FALSE
    })
  }
  shiny::testServer(builder_server, args = list(worker = worker), {
    session$setInputs(name = "Cancel fixture", type = "methods", goal = "Goal", parent = parent, config = "{}", preview = 1)
    expect_equal(control$count, 1L)
    session$setInputs(preview = 2)
    expect_equal(control$count, 1L)
    session$setInputs(cancel = 1)
    expect_null(state$job)
    expect_false(control$alive)
    expect_false(dir.exists(file.path(parent, "cancel_fixture")))
  })
})

test_that("a catalog change invalidates an accepted UI preview", {
  skip_if_not_installed("shiny")
  parent <- new_parent()
  pointer <- new.env(parent = emptyenv())
  pointer$value <- current_catalog_manifest()
  local_mocked_bindings(current_catalog_manifest = function() pointer$value)
  shiny::testServer(builder_server, args = list(worker = app_test_worker), {
    session$setInputs(name = "Stale fixture", type = "methods", goal = "Goal", parent = parent, config = "{}", preview = 1)
    session$flushReact()
    pointer$value$manifest_id <- "changed"
    session$setInputs(apply = 1)
    expect_match(state$status, "catalog changed", fixed = TRUE)
    expect_false(dir.exists(file.path(parent, "stale_fixture")))
  })
  expect_error(app_worker("system", list()), class = "cttir_input_error")
})
