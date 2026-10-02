new_app_draft <- function() {
  list(schema_version = 1L, operation = "create", mode = "detailed",
    project = list(name = "", type = "methods", goal = "An unfinished\nquestion"),
    project_id = NULL, config = list(project = list(language = "de")))
}

new_answer_draft <- function() {
  value <- new_app_draft()
  value$answers <- list(
    analysis_aim = list(status = "answered", value = "explanatory"),
    analysis_outcome_family = list(status = "review", value = "binary"),
    data_sources = list(status = "answered", value = list("Cohort A")),
    research_design = list(status = "skipped", value = NULL),
    model_reviewed = list(status = "answered", value = FALSE)
  )
  value
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

test_that("questionnaire answers round trip and legacy drafts stay valid", {
  value <- new_answer_draft()
  file <- tempfile(fileext = ".json")
  withr::defer(unlink(file))
  writeLines(json_text(value, TRUE), file)
  expect_equal(app_draft_read(file), value)
  legacy <- new_app_draft()
  expect_false("answers" %in% names(app_draft_validate(legacy)))
  empty <- legacy
  empty$answers <- stats::setNames(list(), character())
  writeLines(json_text(empty), file)
  expect_equal(app_draft_read(file)$answers, stats::setNames(list(), character()))
})

test_that("draft answers are validated against the registry", {
  bad <- function(edit) {
    value <- new_answer_draft()
    value$answers <- edit(value$answers)
    expect_error(app_draft_validate(value), class = "cttir_schema_error")
  }
  bad(function(a) c(a, list(not_a_question = list(status = "answered", value = "x"))))
  bad(function(a) {
    a$analysis_aim$value <- "astrology"
    a
  })
  bad(function(a) {
    a$analysis_aim$status <- "approved"
    a
  })
  bad(function(a) {
    a$research_design$value <- "should be empty"
    a
  })
  bad(function(a) {
    a$model_reviewed$value <- "yes"
    a
  })
  bad(function(a) {
    a$data_sources$value <- list("bad\u0001label")
    a
  })
  bad(function(a) {
    a$analysis_aim$extra <- TRUE
    a
  })
  bad(function(a) unname(a))
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
  shiny::testServer(app_builder_server, args = app_test_args(), {
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
    expect_equal(state$status$key, "status.stale_preview")
    expect_false(dir.exists(file.path(parent, "original")))
    session$setInputs(name = saved$project$name, goal = saved$project$goal,
      config = json_text(saved$config), mode = saved$mode, preview = 2)
    session$flushReact()
    expected <- saved
    expected$answers <- stats::setNames(list(), character())
    expect_equal(draft_record(), expected)
    expect_equal(state$preview$spec$project$language, "de")
    session$setInputs(apply = 2)
    session$flushReact()
    expect_true(dir.exists(file.path(parent, "restored")))
    expect_equal(read_project(file.path(parent, "restored"))$spec$project$goal, saved$project$goal)
  })
})

test_that("restoring a draft brings back questionnaire answers including review state", {
  skip_if_not_installed("shiny")
  parent <- new_parent()
  shiny::testServer(app_builder_server, args = app_test_args(), {
    session$setInputs(name = "Answers", type = "methods", goal = "Goal", parent = parent, config = "{}", mode = "detailed")
    saved <- new_answer_draft()
    saved$project$name <- "Answers"
    saved$project$goal <- "Goal"
    restore_draft(saved)
    expect_equal(answers(), saved$answers)
    expect_equal(draft()$options$analysis$aim, "explanatory")
    expect_null(draft()$options$analysis$outcome_family)
    expect_equal(draft()$options$data_sources[[1]]$label, "Cohort A")
    session$flushReact()
    expect_true("analysis_outcome_family" %in% layout()$review)
    session$setInputs(config = json_text(saved$config))
    expect_equal(draft_record()$answers, saved$answers)
  })
})

test_that("Configure drafts require the same project and refuse active jobs", {
  skip_if_not_installed("shiny")
  p <- project("Draft target", "methods", "Goal", new_parent())
  shiny::testServer(app_builder_server, args = app_test_args(path = shiny::reactive(p$path)), {
    session$setInputs(config = "{}")
    saved <- draft_record()
    expect_equal(saved$project_id, p$spec$project$id)
    expect_equal(saved$mode, "detailed")
    wrong <- saved
    wrong$project_id <- "different-project"
    expect_error(restore_draft(wrong), "different project")
    expect_error(restore_draft(new_app_draft()), "different project")
    slot$state$job <- list(mutating = TRUE)
    expect_error(restore_draft(saved), "current operation")
    slot$state$job <- NULL
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
  shiny::testServer(app_builder_server, args = app_test_args(worker), {
    session$setInputs(name = "Changing", type = "methods", goal = "Before", parent = new_parent(), config = "{}", preview = 1)
    session$setInputs(goal = "After")
    control$alive <- FALSE
    session$elapse(200)
    session$flushReact()
    expect_null(state$accepted)
    expect_equal(state$status$key, "status.changed_while_planning")
  })
})
