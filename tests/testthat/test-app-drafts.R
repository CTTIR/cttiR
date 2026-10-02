new_app_draft <- function() {
  list(schema_version = 1L, operation = "create", mode = "detailed",
    project = list(name = "", type = "methods", goal = "An unfinished\nquestion"),
    project_id = NULL, config = list(project = list(language = "de")))
}

test_that("unfinished drafts round trip as bounded data without paths or plans", {
  value <- new_app_draft()
  file <- tempfile(fileext = ".json")
  withr::defer(unlink(file))
  writeLines(json_text(value), file)
  expect_equal(app_draft_read(file), value)
  value$accepted <- list(signature = "forged")
  expect_error(app_draft_validate(value), class = "cttir_schema_error")
  value$accepted <- NULL
  value$path <- "/tmp/untrusted-destination"
  expect_error(app_draft_validate(value), class = "cttir_schema_error")
  writeLines('{"schema_version": 1, "schema_version": 2}', file)
  expect_error(app_draft_read(file), "unique")
  writeLines('!expr system("false")', file)
  expect_error(app_draft_read(file), "valid JSON")
  writeLines(paste(rep("x", 1048577L), collapse = ""), file)
  expect_error(app_draft_read(file), "1 MiB")
})

test_that("draft envelopes reject invalid identity and configuration", {
  value <- new_app_draft()
  value$config <- list(unknown = TRUE)
  expect_error(app_draft_validate(value), class = "cttir_schema_error")
  value <- new_app_draft()
  value$project$type <- "not-a-research-type"
  expect_error(app_draft_validate(value), class = "cttir_schema_error")
  value <- new_app_draft()
  value$project$name <- c("two", "names")
  expect_error(app_draft_validate(value), class = "cttir_schema_error")
  value <- new_app_draft()
  value$mode <- "execute"
  expect_error(app_draft_validate(value), class = "cttir_schema_error")
})

test_that("uploaded or entered JSON cannot request configuration file reads", {
  local_mocked_bindings(read_document = function(...) stop("unexpected local file read"))
  value <- new_app_draft()
  value$config <- "/private/config.yml"
  expect_error(app_draft_validate(value), "JSON object", class = "cttir_schema_error")
  expect_error(app_config('"/private/config.yml"'), "JSON object", class = "cttir_schema_error")
  expect_error(app_config("null"), "JSON object", class = "cttir_schema_error")
})

test_that("restoring a creation draft invalidates approval without creating files", {
  skip_if_not_installed("shiny")
  parent <- new_parent()
  shiny::testServer(builder_server, args = list(worker = app_test_worker), {
    session$setInputs(name = "Original", type = "methods", goal = "Goal", parent = parent, config = "{}", mode = "fast", preview = 1)
    session$flushReact()
    expect_false(is.null(state$accepted))
    expect_false("path" %in% names(draft_record()))
    saved <- new_app_draft()
    saved$project$name <- "Restored"
    restore_draft(saved)
    expect_null(state$accepted)
    expect_null(state$preview)
    session$setInputs(apply = 1)
    expect_match(state$status, "no accepted preview")
    expect_false(dir.exists(file.path(parent, "original")))
    session$setInputs(name = saved$project$name, goal = saved$project$goal,
      config = json_text(saved$config), mode = saved$mode, preview = 2)
    session$flushReact()
    expect_equal(draft_record(), saved)
    expect_equal(state$preview$spec$project$language, "de")
    session$setInputs(apply = 2)
    session$flushReact()
    expect_true(dir.exists(file.path(parent, "restored")))
    expect_equal(read_project(file.path(parent, "restored"))$spec$project$goal, saved$project$goal)
  })
})

test_that("Configure drafts require the same project and refuse active jobs", {
  skip_if_not_installed("shiny")
  p <- project("Draft target", "methods", "Goal", new_parent())
  shiny::testServer(builder_server, args = list(path = p$path, worker = app_test_worker), {
    session$setInputs(config = "{}", mode = "detailed")
    saved <- draft_record()
    expect_equal(saved$project_id, p$spec$project$id)
    wrong <- saved
    wrong$project_id <- "different-project"
    expect_error(restore_draft(wrong), "different project")
    expect_error(restore_draft(new_app_draft()), "different project")
    state$job <- list(mutating = TRUE)
    expect_error(restore_draft(saved), "current operation")
    state$job <- NULL
    expect_equal(restore_draft(saved), saved)
  })
})

test_that("answers changed during a background preview cannot become accepted", {
  skip_if_not_installed("shiny")
  control <- new.env(parent = emptyenv())
  control$alive <- TRUE
  worker <- function(operation, args) {
    value <- do.call(project, args)
    list(is_alive = function() control$alive, kill = function() NULL,
      get_result = function() list(ok = TRUE, value = value))
  }
  shiny::testServer(builder_server, args = list(worker = worker), {
    session$setInputs(name = "Changing", type = "methods", goal = "Before", parent = new_parent(), config = "{}", preview = 1)
    session$setInputs(goal = "After")
    control$alive <- FALSE
    session$elapse(200)
    session$flushReact()
    expect_null(state$accepted)
    expect_match(state$status, "draft changed while planning")
  })
})
