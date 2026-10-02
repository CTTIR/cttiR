registry_doc <- function() jsonlite::fromJSON(resource_file("extdata", "questions.json"), simplifyVector = FALSE)

# Smallest set of upstream answers that makes a relevance condition true.
satisfy <- function(condition, questions, answers = list()) {
  if (is.null(condition)) return(answers)
  if (!is.null(condition$all)) {
    for (part in condition$all) answers <- satisfy(part, questions, answers)
    return(answers)
  }
  if (!is.null(condition$any)) return(satisfy(condition$any[[1]], questions, answers))
  q <- questions[[condition$question]]
  answers <- satisfy(q$relevance, questions, answers)
  value <- if (q$type == "list" || !is.null(condition$answered)) {
    list("Item")
  } else if (!is.null(condition[["in"]])) {
    condition[["in"]][[1]]
  } else {
    setdiff(unlist(q[["choices"]]), unlist(condition$not_in))[[1]]
  }
  answers[[q$id]] <- list(status = "answered", value = value)
  answers
}

sample_values <- function(q, questions, answers, base) {
  switch(q$type,
    choice = as.list(unname(app_question_choices(q, answers, questions, base))),
    text = list("Literal text"),
    boolean = list(TRUE, FALSE),
    list = list(list("First item", "Second item"))
  )
}

test_that("the question registry validates against its schema and loads", {
  doc <- registry_doc()
  valid <- jsonvalidate::json_validate(json_text(doc), resource_file("schema", "questions.schema.json"), engine = "ajv")
  expect_true(valid)
  registry <- app_questions()
  expect_gte(length(registry$questions), 40L)
  expect_setequal(unique(vapply(registry$questions, function(q) q$section, character(1))), registry$sections)
  for (q in registry$questions) {
    expect_true(q$optional, info = q$id)
    expect_true(all(nzchar(c(q$labels$en, q$labels$de, q$help$en, q$help$de))), info = q$id)
  }
  required <- c("/analysis/aim", "/workflow/table_backend", "/analysis/mapping/outcome", "/analysis/model/estimation",
    "/research/analysis_role", "/research/data_origin", "/publications", "/data_sources", "/figures/categorical_palette",
    "/workflow/profile", "/analysis/outcome_family", "/analysis/unit_structure")
  expect_true(all(required %in% vapply(registry$questions, function(q) q$pointer, character(1))))
})

test_that("relevance is declarative data and executable content is rejected", {
  doc <- registry_doc()
  doc$questions[[1]]$relevance <- list(expr = "system('true')")
  expect_error(app_questions_check(doc), class = "cttir_schema_error")
  doc <- registry_doc()
  doc$questions[[1]]$validate <- "function(x) TRUE"
  expect_error(app_questions_check(doc), class = "cttir_schema_error")
})

test_that("the dependency graph is acyclic and dependencies are declared", {
  registry <- app_questions()
  order <- app_question_order(registry$questions)
  expect_setequal(order, names(registry$questions))
  for (q in registry$questions) {
    for (dep in unlist(q$depends_on)) expect_lt(match(dep, order), match(q$id, order))
  }
  cyclic <- registry$questions[c("analysis_aim", "analysis_outcome_family")]
  cyclic$analysis_aim$depends_on <- list("analysis_outcome_family")
  expect_error(app_question_order(cyclic), class = "cttir_schema_error")
  doc <- registry_doc()
  ids <- vapply(doc$questions, function(q) q$id, character(1))
  doc$questions[[match("analysis_aim", ids)]]$depends_on <- list("analysis_outcome_family")
  doc$questions[[match("analysis_aim", ids)]]$relevance <- list(question = "analysis_outcome_family", answered = TRUE)
  expect_error(app_questions_check(doc), "earlier questions", class = "cttir_schema_error")
  doc <- registry_doc()
  doc$questions[[match("analysis_outcome_family", ids)]]$depends_on <- list()
  expect_error(app_questions_check(doc), "depends_on", class = "cttir_schema_error")
})

test_that("pointers exist in the configuration schema and choices equal its enums", {
  registry <- app_questions()
  for (q in registry$questions) {
    node <- app_schema_node(q$pointer)
    expect_false(is.null(node), info = q$pointer)
    if (q$type == "choice" && !is.null(node$enum)) expect_setequal(unlist(q[["choices"]]), unlist(node$enum))
    if (!is.null(node$enum) && "unknown" %in% unlist(node$enum)) expect_true("unknown" %in% unlist(q[["choices"]]), info = q$id)
  }
  expect_null(app_schema_node("/analysis/not_a_field"))
  expect_null(app_schema_node("/publications/type"))
  doc <- registry_doc()
  ids <- vapply(doc$questions, function(q) q$id, character(1))
  doc$questions[[match("analysis_aim", ids)]]$choices <- list("descriptive", "astrology")
  expect_error(app_questions_check(doc), "schema enum", class = "cttir_schema_error")
  doc <- registry_doc()
  doc$questions[[match("analysis_aim", ids)]]$pointer <- "/analysis/aims"
  expect_error(app_questions_check(doc), "pointer", class = "cttir_schema_error")
})

test_that("palette choices are colourblind-flagged RColorBrewer palettes", {
  skip_if_not_installed("RColorBrewer")
  info <- RColorBrewer::brewer.pal.info
  registry <- app_questions()
  qualitative <- unlist(registry$questions$figures_categorical_palette[["choices"]])
  diverging <- unlist(registry$questions$figures_diverging_palette[["choices"]])
  expect_true(all(info[qualitative, "colorblind"] & info[qualitative, "category"] == "qual"))
  expect_true(all(info[diverging, "colorblind"] & info[diverging, "category"] == "div"))
})

test_that("every choice, text, boolean and list answer validates and skip is always valid", {
  registry <- app_questions()
  questions <- registry$questions
  base <- app_default_base()
  for (q in questions) {
    skipped <- stats::setNames(list(list(status = "skipped", value = NULL)), q$id)
    expect_identical(app_answers_options(skipped, questions, base), list(), info = q$id)
    upstream <- satisfy(q$relevance, questions)
    expect_true(app_question_state(upstream, questions, base)$relevant[[q$id]], info = q$id)
    for (value in sample_values(q, questions, upstream, base)) {
      answers <- upstream
      answers[[q$id]] <- list(status = "answered", value = value)
      options <- app_answers_options(answers, questions, base)
      expect_silent(validate_config(options))
      if (is.null(q$entries)) {
        expected <- if (q$type == "list") as.list(unlist(value)) else value
        expect_identical(app_spec_value(options, q$pointer), expected, info = q$id)
      }
    }
  }
})

test_that("unknown answers and skipped questions are accepted by the builder", {
  registry <- app_questions()
  questions <- registry$questions
  base <- app_default_base()
  parent <- new_parent()
  for (q in Filter(function(q) "unknown" %in% unlist(q[["choices"]]), questions)) {
    answers <- satisfy(q$relevance, questions)
    answers[[q$id]] <- list(status = "answered", value = "unknown")
    options <- app_answers_options(answers, questions, base)
    p <- project("Unknown answers", "methods", "Goal", parent, options = options, dry_run = TRUE)
    expect_equal(app_spec_value(p$spec, q$pointer), "unknown", info = q$id)
  }
  skipped <- app_answers_skip(list(), names(questions))
  expect_identical(app_answers_options(skipped, questions, base), list())
  expect_s3_class(project("Skipped", "methods", "Goal", parent, options = list(), dry_run = TRUE), "cttir_project")
})

test_that("pending builder choices fail with a typed domain error", {
  questions <- app_questions()$questions
  base <- app_default_base()
  for (id in c("workflow_pipeline", "workflow_table_backend", "workflow_reporting")) {
    q <- questions[[id]]
    answers <- stats::setNames(list(list(status = "answered", value = unlist(q$pending_choices)[[1]])), id)
    options <- app_answers_options(answers, questions, base)
    expect_error(project("Pending", "methods", "Goal", new_parent(), options = options, dry_run = TRUE),
      class = "cttir_api_mismatch")
  }
})

test_that("changing an upstream answer marks dependents for review instead of dropping them", {
  questions <- app_questions()$questions
  answers <- list()
  answers <- app_answers_update(answers, "analysis_aim", "explanatory", questions)
  answers <- app_answers_update(answers, "analysis_outcome_family", "time_to_event", questions)
  answers <- app_answers_update(answers, "mapping_event", "died", questions)
  expect_identical(app_answers_update(answers, "analysis_aim", "explanatory", questions), answers)
  changed <- app_answers_update(answers, "analysis_aim", "predictive", questions)
  expect_equal(changed$analysis_outcome_family$status, "review")
  expect_equal(changed$mapping_event$status, "review")
  expect_equal(changed$mapping_event$value, "died")
  expect_identical(app_answers_update(changed, "mapping_event", "died", questions), changed)
  options <- app_answers_options(changed, questions)
  expect_equal(options$analysis$aim, "predictive")
  expect_null(options$analysis$outcome_family)
  expect_null(options$analysis$mapping$event)
  kept <- app_answers_confirm(changed, "analysis_outcome_family")
  expect_equal(kept$analysis_outcome_family$status, "answered")
  expect_equal(app_answers_options(kept, questions)$analysis$outcome_family, "time_to_event")
  cleared <- app_answers_update(kept, "analysis_outcome_family", NULL, questions)
  expect_null(cleared$analysis_outcome_family)
  expect_equal(cleared$mapping_event$status, "review")
  expect_equal(app_question_dependents("analysis_aim", questions)[1:2], c("analysis_outcome_family", "analysis_unit_structure"))
})

test_that("hidden answers never reach the configuration", {
  questions <- app_questions()$questions
  answers <- list(
    analysis_aim = list(status = "answered", value = "descriptive"),
    model_estimation = list(status = "answered", value = "REML"),
    mapping_subject = list(status = "answered", value = "id")
  )
  state <- app_question_state(answers, questions)
  expect_false(state$relevant[["model_estimation"]])
  options <- app_answers_options(answers, questions)
  expect_identical(options, list(analysis = list(aim = "descriptive")))
})

test_that("relevance falls back to the existing project, never to a preview", {
  questions <- app_questions()$questions
  base <- app_default_base()
  base$analysis$aim <- "explanatory"
  base$analysis$unit_structure <- "longitudinal"
  base$data_sources <- list(list(id = "cohort", label = "Cohort"))
  state <- app_question_state(list(), questions, base)
  expect_true(all(state$relevant[c("analysis_outcome_family", "mapping_subject", "mapping_data_source")]))
  expect_false(app_question_state(list(), questions, app_default_base())$relevant[["analysis_outcome_family"]])
  answers <- list(data_sources = list(status = "answered", value = list("Cohort", "Registry")))
  options <- app_answers_options(answers, questions, base)
  expect_equal(length(options$data_sources), 1L)
  expect_equal(options$data_sources[[1]]$id, "ds01")
  expect_equal(unname(app_question_choices(questions$mapping_data_source, answers, questions, base)), c("cohort", "ds01"))
  pubs <- app_answers_options(list(publications = list(status = "answered", value = list("Second paper: methods"))), questions, app_default_base())
  expect_equal(pubs$publications[[1]]$id, "pub02")
  expect_equal(pubs$publications[[1]]$slug, "pub02_second_paper_methods")
})

test_that("answer parsing is bounded to the declared type", {
  questions <- app_questions()$questions
  expect_null(app_answer_parse(questions$analysis_aim, ""))
  expect_null(app_answer_parse(questions$analysis_aim, "astrology"))
  expect_equal(app_answer_parse(questions$analysis_aim, "causal"), "causal")
  expect_null(app_answer_parse(questions$model_reviewed, ""))
  expect_false(app_answer_parse(questions$model_reviewed, "false"))
  expect_equal(app_answer_parse(questions$mapping_predictors, " age \n\nsex\nage "), list("age", "sex"))
  expect_null(app_answer_parse(questions$mapping_outcome, "   "))
  expect_equal(app_answer_input_value(questions$mapping_predictors, list("a", "b")), "a\nb")
  expect_equal(app_pointer_question("/publications/1/slug", questions), "publications")
  expect_equal(app_pointer_question("/publications/0/type", questions), "publication_type")
  expect_equal(app_pointer_question("/analysis/mapping/outcome", questions), "mapping_outcome")
  expect_null(app_pointer_question("/knowledge/refresh", questions))
})

test_that("the English and German message catalogs are complete", {
  messages <- app_messages()
  expect_setequal(names(messages$en), names(messages$de))
  expect_true(all(nzchar(unlist(messages$en))))
  expect_true(all(nzchar(unlist(messages$de))))
  expect_equal(app_t("nav.create", "de"), "Projekt anlegen")
  expect_equal(app_t("questions.step", "en", current = 2, total = 7, section = "Data"), "Section 2 of 7: Data")
  expect_equal(app_t("unknown.key", "de"), "unknown.key")
  questions <- app_questions()$questions
  labelled <- Filter(function(q) q$type == "choice" && !grepl("^/figures/", q$pointer), questions)
  values <- unique(unlist(lapply(labelled, function(q) q[["choices"]])))
  expect_true(all(paste0("value.", values) %in% names(messages$de)))
  source <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(source))
  code <- unlist(lapply(list.files(source, "^app.*[.]R$", full.names = TRUE), readLines))
  keys <- unique(regmatches(code, gregexpr('"(app|nav|home|create|configure|questions|review|result|draft|status|error|job|common|open|ask|knowledge|resources|audit|runtime|blocker|value|field|profile)[.][a-z_0-9]+[a-z0-9]"', code)))
  keys <- gsub('"', "", unlist(keys))
  keys <- setdiff(keys, "questions.json")
  expect_true(all(keys %in% names(messages$en)), info = paste(setdiff(keys, names(messages$en)), collapse = ", "))
})
