# Create and Configure share one module: one authoritative draft (inputs,
# questionnaire answers and the advanced JSON), a revision counter, and an
# accepted preview bound to the draft signature and catalog snapshot. Preview
# and apply always run project() or sync() in the background worker.

app_project_types <- function() unlist(app_schema_node("/project/type")$enum)

app_abort_status <- function(key, kind = "warning") {
  condition <- list(message = key, call = NULL, status = app_status(kind, key))
  stop(structure(condition, class = c("cttir_app_status", "error", "condition")))
}

app_draft_canonical <- function(value) {
  if (is.null(value$answers) || !length(value$answers)) value$answers <- stats::setNames(list(), character())
  value
}

app_format_value <- function(x, lang = "en") {
  if (is.null(x) || !length(x)) return("\u2014")
  if (is.logical(x) && length(x) == 1L) return(app_t(if (isTRUE(x)) "common.yes" else "common.no", lang))
  if (is.atomic(x) && length(x) == 1L) return(app_value_label(x, lang))
  json_text(x)
}

app_flatten <- function(x, prefix = "") {
  if (is.list(x) && length(x) && !is.null(names(x)) && all(nzchar(names(x)))) {
    out <- list()
    for (key in names(x)) out <- c(out, app_flatten(x[[key]], paste0(prefix, "/", key)))
    return(out)
  }
  if (is.list(x) && length(x) && is.null(names(x)) &&
      all(vapply(x, function(e) is.list(e) && is.character(e$id), logical(1)))) {
    out <- list()
    for (entry in x) {
      rest <- entry
      rest$id <- NULL
      out <- c(out, if (length(rest)) app_flatten(rest, paste0(prefix, "/", entry$id)) else stats::setNames(list(entry$id), paste0(prefix, "/", entry$id)))
    }
    return(out)
  }
  stats::setNames(list(x), prefix)
}

app_hint_value <- function(q, hints, lang) {
  value <- if (identical(q$default_source, "schema")) {
    app_schema_node(q$pointer)$default
  } else if (identical(q$default_source, "spec")) {
    if (!is.null(q$entries)) app_base_value(q, hints) else app_spec_value(hints, q$pointer)
  }
  if (is.null(value) || !length(value)) return(NULL)
  if (is.list(value)) return(paste(vapply(value, function(x) as.character(unlist(x))[[1]], character(1)), collapse = ", "))
  app_format_value(value, lang)
}

app_question_ui <- function(ns, q, entry, choices, hint, lang) {
  id <- ns(paste0("q_", q$id))
  label <- q$labels[[lang]]
  value <- app_answer_input_value(q, entry$value)
  pending <- unlist(q$pending_choices)
  suffix <- function(values, labels) {
    ifelse(values %in% pending, paste(labels, app_t("questions.pending_suffix", lang)), labels)
  }
  not_answered <- app_t("questions.not_answered", lang)
  control <- switch(q$type,
    choice = {
      values <- unname(choices)
      labels <- if (!is.null(names(choices)) && all(nzchar(names(choices)))) {
        paste0(names(choices), " (", values, ")")
      } else {
        vapply(values, app_value_label, character(1), lang = lang)
      }
      labels <- suffix(values, labels)
      if (length(values) <= 6L) {
        shiny::radioButtons(id, label, choiceNames = as.list(c(not_answered, labels)),
          choiceValues = c("", values), selected = value)
      } else {
        shiny::selectInput(id, label, stats::setNames(c("", values), c(not_answered, labels)),
          selected = value, selectize = FALSE, width = "100%")
      }
    },
    boolean = shiny::radioButtons(id, label, inline = TRUE,
      choiceNames = as.list(c(not_answered, suffix(c("true", "false"), c(app_t("common.yes", lang), app_t("common.no", lang))))),
      choiceValues = c("", "true", "false"), selected = value),
    text = shiny::textInput(id, label, value = value, width = "100%"),
    list = shiny::textAreaInput(id, label, value = value, rows = 3, width = "100%")
  )
  review <- identical(entry$status, "review")
  shiny::tags$div(class = paste("cttir-question", if (review) "cttir-question-review"), id = ns(paste0("qb_", q$id)),
    control,
    shiny::tags$p(class = "cttir-help", q$help[[lang]]),
    if (!is.null(hint)) shiny::tags$p(class = "cttir-hint", app_t("questions.current", lang, value = hint)),
    if (identical(entry$status, "skipped")) shiny::tags$p(class = "cttir-hint", app_badge("skipped", lang, kind = "unknown")),
    if (review) {
      shiny::tags$div(class = "cttir-review", role = "group", `aria-label` = app_t("questions.review_group", lang, question = label),
        app_badge("review", lang, kind = "warn", label = app_t("questions.needs_review", lang)),
        shiny::tags$span(app_t("questions.review_note", lang)),
        shiny::actionButton(ns(paste0("keep_", q$id)), app_t("questions.keep", lang), class = "btn-sm"),
        shiny::actionButton(ns(paste0("clear_", q$id)), app_t("questions.clear", lang), class = "btn-sm"))
    }
  )
}

app_questionnaire_ui <- function(ns, registry, layout, answers, step, hints, lang) {
  sections <- registry$sections
  questions <- registry$questions
  step <- min(max(1L, step), length(sections))
  section <- sections[[step]]
  answered <- names(Filter(function(x) identical(x$status, "answered"), answers))
  nav <- shiny::tags$nav(`aria-label` = app_t("questions.sections", lang),
    shiny::tags$ol(class = "cttir-steps", lapply(seq_along(sections), function(i) {
      ids <- names(Filter(function(q) q$section == sections[[i]], questions))
      visible <- intersect(ids, layout$visible)
      label <- paste0(registry$section_labels[[sections[[i]]]][[lang]], " (",
        length(intersect(visible, answered)), "/", length(visible), ")")
      shiny::tags$li(class = if (i == step) "active", `aria-current` = if (i == step) "step",
        shiny::actionLink(ns(paste0("goto_", sections[[i]])), label))
    })))
  shown <- Filter(function(q) q$section == section && q$id %in% layout$visible, questions)
  body <- if (!length(shown)) {
    shiny::tags$p(class = "cttir-empty", app_t("questions.none_apply", lang))
  } else {
    lapply(shown, function(q) {
      choices <- if (is.null(q$choices_from)) unlist(q[["choices"]]) else layout[["choices"]][[q$id]]
      app_question_ui(ns, q, answers[[q$id]], choices, app_hint_value(q, hints, lang), lang)
    })
  }
  hidden <- if (length(layout$hidden)) {
    shiny::tags$div(class = "cttir-hidden-answers", role = "note",
      shiny::tags$h3(app_t("questions.hidden_title", lang)),
      shiny::tags$p(app_t("questions.hidden_note", lang)),
      shiny::tags$ul(lapply(layout$hidden, function(id) {
        q <- questions[[id]]
        shiny::tags$li(paste0(q$labels[[lang]], ": ", app_format_value(answers[[id]]$value, lang), " "),
          shiny::actionButton(ns(paste0("clear_", id)), app_t("questions.clear", lang), class = "btn-sm"))
      })))
  }
  review_count <- length(layout$review)
  section_label <- registry$section_labels[[section]][[lang]]
  step_label <- app_t("questions.step", lang, current = step, total = length(sections), section = section_label)
  shiny::tagList(
    nav,
    shiny::tags$p(class = "cttir-step-label", step_label),
    if (review_count) shiny::tags$p(class = "cttir-warning-text", app_badge("review", lang, kind = "warn", label = app_t("questions.needs_review", lang)),
      " ", app_t("questions.review_count", lang, count = review_count)),
    shiny::tags$fieldset(class = "cttir-fieldset", shiny::tags$legend(registry$section_labels[[section]][[lang]]), body),
    hidden,
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("q_back"), app_t("questions.back", lang)),
      shiny::actionButton(ns("q_skip"), app_t("questions.skip_section", lang)),
      shiny::actionButton(ns("q_next"), app_t("questions.next", lang)))
  )
}

app_profile_label <- function(profile, lang) app_value_label(profile, lang, "profile.")

app_project_review <- function(p, lang, answers_note = NULL) {
  workflow <- p$readiness$workflow
  decisions <- p$spec$decisions
  reason <- NULL
  for (d in decisions) if (identical(d$field, "/workflow/profile")) reason <- d$reason
  stages <- workflow$stages
  stage_of <- function(name) {
    for (s in stages) if (identical(s$stage, name)) return(s)
    NULL
  }
  describe <- stage_of("describe")
  model <- stage_of("model")
  engine <- workflow$engine %||% p$readiness$analysis$candidate_engine
  inferred <- Filter(function(d) identical(d$origin, "inferred") && !identical(d$field, "/workflow/profile"), decisions)
  stage_rows <- if (length(stages)) {
    data.frame(stage = vapply(stages, function(s) as.character(s$stage %||% ""), character(1)),
      capability = vapply(stages, function(s) as.character(s$capability %||% ""), character(1)),
      enabled = vapply(stages, function(s) isTRUE(s$enabled), logical(1)),
      status = vapply(stages, function(s) as.character(s$status %||% "unknown"), character(1)),
      stringsAsFactors = FALSE)
  }
  missing <- unlist(p$readiness$analysis$missing_fields)
  plan <- p$plan
  shiny::tags$section(class = "cttir-review-panel", `aria-labelledby` = "cttir-review-heading",
    shiny::tags$h2(id = "cttir-review-heading", app_t("review.title", lang)),
    shiny::tags$dl(class = "cttir-dl cttir-summary",
      shiny::tags$dt(app_t("review.profile", lang)),
      shiny::tags$dd(if (is.null(workflow)) app_t("review.profile_unavailable", lang) else app_badge(workflow$profile, lang, kind = "neutral", label = app_profile_label(workflow$profile, lang))),
      shiny::tags$dt(app_t("review.reason", lang)),
      shiny::tags$dd(reason %||% app_t("common.none", lang)),
      shiny::tags$dt(app_t("review.table_tool", lang)),
      shiny::tags$dd(app_value_label(p$spec$workflow$table_backend, lang), " ",
        if (!is.null(describe)) app_badge(describe$status, lang)),
      shiny::tags$dt(app_t("review.model_tool", lang)),
      shiny::tags$dd(if (!is.null(engine)) {
        shiny::tagList(shiny::tags$code(engine), " ", app_t("review.model_tentative", lang),
          if (!is.null(model)) shiny::tagList(" ", app_badge(model$status, lang)))
      } else {
        app_t("review.model_none", lang)
      }),
      shiny::tags$dt(app_t("review.location", lang)),
      shiny::tags$dd(shiny::tags$code(p$path)),
      shiny::tags$dt(app_t("review.readiness", lang)),
      shiny::tags$dd(app_badge(p$readiness$level, lang))
    ),
    if (!is.null(answers_note)) shiny::tags$p(class = "cttir-note", answers_note),
    if (length(missing)) shiny::tags$details(
      shiny::tags$summary(app_t("review.missing_fields", lang, count = length(missing))),
      app_list_block(missing, lang)),
    shiny::tags$h3(app_t("review.pending", lang)),
    app_list_block(p$readiness$blockers, lang, "blocker."),
    if (length(workflow$gaps)) shiny::tags$details(shiny::tags$summary(app_t("review.gaps", lang, count = length(workflow$gaps))),
      app_list_block(workflow$gaps, lang)),
    if (length(inferred)) shiny::tagList(shiny::tags$h3(app_t("review.inferred", lang)),
      shiny::tags$ul(lapply(inferred, function(d) shiny::tags$li(shiny::tags$code(d$field), " ", d$reason)))),
    if (!is.null(stage_rows)) shiny::tags$details(shiny::tags$summary(app_t("review.stages", lang, count = nrow(stage_rows))),
      app_table(stage_rows, lang, badges = "status")),
    if (is.data.frame(plan)) shiny::tags$details(shiny::tags$summary(app_t("review.files", lang, count = nrow(plan))),
      app_table(plan, lang, columns = c("path", "action"), badges = "action"))
  )
}

app_project_result <- function(p, lang) {
  shiny::tags$section(class = "cttir-review-panel",
    shiny::tags$h2(app_t("result.created_title", lang)),
    shiny::tags$p(app_t("result.created_at", lang), " ", shiny::tags$code(p$path)),
    shiny::tags$p(app_badge(p$readiness$level, lang)),
    shiny::tags$h3(app_t("review.pending", lang)),
    app_list_block(p$readiness$blockers, lang, "blocker."),
    shiny::tags$p(class = "cttir-note", app_t("result.next_steps", lang))
  )
}

app_sync_review <- function(s, spec, args, lang, applied = FALSE) {
  rows <- list()
  for (origin in c("config", "options")) {
    flat <- app_flatten(args[[origin]] %||% list())
    for (pointer in names(flat)) {
      if (!nzchar(pointer)) next
      rows[[length(rows) + 1L]] <- data.frame(field = pointer,
        current = app_format_value(app_spec_value(spec, pointer), lang),
        requested = app_format_value(flat[[pointer]], lang),
        origin = app_t(paste0("configure.origin_", origin), lang), stringsAsFactors = FALSE)
    }
  }
  changes <- if (length(rows)) do.call(rbind, rows)
  actions <- s$actions
  counts <- if (is.data.frame(actions)) table(actions$action) else integer()
  shiny::tags$section(class = "cttir-review-panel", `aria-labelledby` = "cttir-sync-heading",
    shiny::tags$h2(id = "cttir-sync-heading", app_t(if (applied) "configure.result_title" else "configure.review_title", lang)),
    shiny::tags$p(app_badge(s$state, lang)),
    if (length(s$conflicts)) shiny::tags$p(class = "cttir-warning-text", app_badge("conflict", lang),
      " ", app_t("configure.conflicts", lang, count = length(s$conflicts))),
    shiny::tags$h3(app_t("configure.field_changes", lang)),
    if (is.null(changes)) shiny::tags$p(class = "cttir-empty", app_t("configure.no_field_changes", lang)) else app_table(changes, lang),
    shiny::tags$h3(app_t("configure.file_changes", lang)),
    shiny::tags$p(paste(paste0(vapply(names(counts), app_value_label, character(1), lang = lang), ": ", as.integer(counts)), collapse = "; ")),
    if (is.data.frame(actions)) shiny::tags$details(open = if (length(s$conflicts) || applied) NA else NULL,
      shiny::tags$summary(app_t("review.files", lang, count = nrow(actions))),
      app_table(actions, lang, columns = c("path", "action"), badges = "action")),
    if (!is.null(s$analysis)) shiny::tags$p(app_t("configure.analysis_state", lang), " ", app_badge(s$analysis$state, lang)),
    app_other_fields(s, c("path", "actions", "conflicts", "changed_files", "state", "journal", "readiness", "analysis"), lang)
  )
}

app_builder_ui <- function(id, mode = "fast", existing = FALSE) {
  ns <- shiny::NS(id)
  types <- app_project_types()
  detailed <- shiny::tagList(
    shiny::tags$section(class = "cttir-section", `aria-labelledby` = ns("q_heading"),
      shiny::tags$h2(id = ns("q_heading"), app_tr("questions.title")),
      shiny::tags$p(class = "cttir-note", app_tr("questions.intro")),
      shiny::uiOutput(ns("questionnaire"))),
    shiny::tags$details(class = "cttir-advanced",
      shiny::tags$summary(app_tr("create.advanced")),
      shiny::textAreaInput(ns("config"), app_tr("create.config"), value = "{}", rows = 8, width = "100%"),
      shiny::helpText(app_tr("create.config_help")))
  )
  shiny::tagList(
    shiny::tags$h1(app_tr(if (existing) "configure.title" else "create.title")),
    shiny::tags$p(class = "cttir-lead", app_tr(if (existing) "configure.intro" else "create.intro")),
    shiny::tags$p(class = "cttir-note", app_tr("create.scope_note")),
    if (existing) shiny::uiOutput(ns("project_summary")),
    if (!existing) shiny::radioButtons(ns("mode"), app_tr("create.mode"), inline = TRUE,
      choiceNames = list(app_tr("create.mode_fast"), app_tr("create.mode_detailed")),
      choiceValues = c("fast", "detailed"), selected = mode),
    if (!existing) shiny::tags$div(class = "cttir-fields",
      shiny::textInput(ns("name"), app_tr("create.name"), width = "100%"),
      shiny::selectInput(ns("type"), app_tr("create.type"), app_choice_names(types), selectize = FALSE, width = "100%"),
      shiny::textAreaInput(ns("goal"), app_tr("create.goal"), rows = 3, width = "100%"),
      shiny::textInput(ns("parent"), app_tr("create.parent"), value = getwd(), width = "100%"),
      shiny::helpText(app_tr("create.parent_help"))),
    if (existing) detailed else shiny::conditionalPanel(sprintf("input['%s'] === 'detailed'", ns("mode")), detailed),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("preview"), app_tr("create.preview"), class = "btn-primary"),
      shiny::actionButton(ns("cancel"), app_tr("common.cancel_preview"))),
    shiny::tags$div(class = "cttir-live", `aria-live` = "polite",
      shiny::uiOutput(ns("progress")), shiny::uiOutput(ns("status"))),
    shiny::uiOutput(ns("review")),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("apply"), app_tr(if (existing) "configure.apply" else "create.apply"), class = "btn-success")),
    shiny::tags$details(class = "cttir-advanced",
      shiny::tags$summary(app_tr("draft.title")),
      shiny::helpText(app_tr("draft.export_help")),
      shiny::downloadButton(ns("save_draft"), app_tr("draft.export")),
      shiny::fileInput(ns("load_draft"), app_tr("draft.restore"), accept = c(".json", "application/json")),
      shiny::helpText(app_tr("draft.restore_help")),
      shiny::tags$p(role = "status", shiny::textOutput(ns("draft_status"), inline = TRUE)))
  )
}

app_builder_server <- function(id, pool, lang = shiny::reactive("en"), path = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    existing <- !is.null(path)
    registry <- app_questions()
    questions <- registry$questions
    sections <- registry$sections
    slot <- app_job_slot(pool, session)
    state <- shiny::reactiveValues(preview = NULL, accepted = NULL, result = NULL, hint_spec = NULL,
      status = app_status("info", if (existing) "status.ready_configure" else "status.ready"),
      revision = 0L, draft_hash = NULL, exported_hash = NULL, applied_hash = NULL, refresh = 0L,
      focus = NULL, last_args = NULL)
    answers <- shiny::reactiveVal(stats::setNames(list(), character()))
    step <- shiny::reactiveVal(1L)
    layout <- shiny::reactiveVal(NULL)
    signature <- function(x) content_hash(json_text(x))
    project_path <- function() if (existing) path() else NULL
    project_record <- shiny::reactive({
      state$refresh
      p <- project_path()
      if (is.null(p)) return(NULL)
      tryCatch(read_project(p, edited = TRUE), error = function(e) NULL)
    })
    base_spec <- shiny::reactive(if (existing) project_record()$spec else app_default_base())

    draft <- shiny::reactive({
      config <- app_config(input$config)
      options <- app_answers_options(answers(), questions, base_spec())
      if (length(options)) validate_config(options)
      if (existing) {
        p <- project_path()
        if (is.null(p)) abort_cttir("Open a project before previewing changes.", field = "path")
        return(list(path = p, config = config, options = options))
      }
      list(name = input$name, type = input$type, goal = input$goal, path = input$parent, config = config, options = options)
    })
    draft_record <- function() {
      args <- draft()
      app_draft_validate(list(
        schema_version = 1L, operation = if (existing) "configure" else "create",
        mode = if (existing || identical(input$mode, "detailed")) "detailed" else "fast",
        project = if (existing) NULL else args[c("name", "type", "goal")],
        project_id = if (existing) project_record()$spec$project$id else NULL,
        config = args$config, answers = answers()
      ))
    }

    focus <- function(target) {
      state$focus <- target
      session$sendCustomMessage("cttir-focus", list(id = session$ns(target)))
    }
    focus_field <- function(field) {
      if (!is.character(field) || length(field) != 1L || !nzchar(field)) field <- NULL
      target <- if (is.null(field)) NULL else switch(field, name = "name", goal = "goal", type = "type",
        path = if (existing) NULL else "parent", NULL)
      if (!existing && is.null(target) && identical(field, "/project/type")) target <- "type"
      if (is.null(target) && !is.null(field)) {
        qid <- app_pointer_question(field, questions)
        if (!is.null(qid)) {
          step(match(questions[[qid]]$section, sections))
          target <- paste0("q_", qid)
        } else if (startsWith(field, "/")) {
          target <- "config"
        }
      }
      focus(target %||% "status")
    }
    guard <- function(expr) {
      tryCatch(expr,
        cttir_app_status = function(e) {
          state$status <- e$status
          focus("status")
        },
        error = function(e) {
          state$status <- app_condition_status(e)
          focus_field(app_error_field(e$field))
        })
    }
    context <- function() {
      if (!existing) return(current_catalog_manifest())
      p <- read_project(project_path(), edited = TRUE)
      snapshot_manifest(p$lock$catalog_id, p$lock$resource_snapshot)
    }

    # Any change to the draft bumps the revision and invalidates the accepted plan.
    shiny::observe({
      current <- signature(list(input$name, input$type, input$goal, input$parent, input$config, answers(), project_path()))
      if (!identical(current, shiny::isolate(state$draft_hash))) {
        state$draft_hash <- current
        state$revision <- shiny::isolate(state$revision) + 1L
        state$accepted <- NULL
        state$preview <- NULL
      }
    })
    if (existing) {
      shiny::observeEvent(project_path(), {
        answers(stats::setNames(list(), character()))
        step(1L)
        state$preview <- NULL
        state$result <- NULL
        state$accepted <- NULL
        state$exported_hash <- NULL
        shiny::updateTextAreaInput(session, "config", value = "{}")
      }, ignoreInit = TRUE)
    }

    # Questionnaire layout changes only when relevance, review state or dynamic
    # choices change, so typing does not re-render the form.
    shiny::observe({
      a <- answers()
      base <- base_spec()
      st <- app_question_state(a, questions, base)
      statuses <- vapply(a, function(x) x$status, character(1))
      visible <- names(st$relevant)[st$relevant]
      value <- list(
        visible = visible,
        review = names(statuses)[statuses == "review"],
        hidden = setdiff(names(statuses)[statuses %in% c("answered", "review")], visible),
        choices = lapply(Filter(function(q) !is.null(q$choices_from), questions),
          function(q) app_question_choices(q, a, questions, base))
      )
      if (!identical(value, shiny::isolate(layout()))) layout(value)
    })
    hints <- shiny::reactive(if (existing) project_record()$spec else state$hint_spec)
    output$questionnaire <- shiny::renderUI({
      current <- layout()
      if (is.null(current)) return(NULL)
      app_questionnaire_ui(session$ns, registry, current, shiny::isolate(answers()), step(), hints(), lang())
    })
    for (q in questions) {
      local({
        q <- q
        input_id <- paste0("q_", q$id)
        shiny::observeEvent(input[[input_id]], {
          current <- answers()
          choices <- app_question_choices(q, current, questions, base_spec())
          answers(app_answers_update(current, q$id, app_answer_parse(q, input[[input_id]], choices), questions))
        })
        shiny::observeEvent(input[[paste0("keep_", q$id)]], answers(app_answers_confirm(answers(), q$id)))
        shiny::observeEvent(input[[paste0("clear_", q$id)]], answers(app_answers_update(answers(), q$id, NULL, questions)))
      })
    }
    for (i in seq_along(sections)) {
      local({
        i <- i
        shiny::observeEvent(input[[paste0("goto_", sections[[i]])]], step(i))
      })
    }
    shiny::observeEvent(input$q_back, step(max(1L, step() - 1L)))
    shiny::observeEvent(input$q_next, step(min(length(sections), step() + 1L)))
    shiny::observeEvent(input$q_skip, {
      section <- sections[[step()]]
      visible <- layout()$visible
      ids <- names(Filter(function(q) q$section == section && q$id %in% visible, questions))
      answers(app_answers_skip(answers(), ids))
      step(min(length(sections), step() + 1L))
    })

    preview_done <- function(args, catalog, result) {
      if (!isTRUE(result$ok)) {
        state$status <- app_status_error(result)
        focus_field(result$field)
        return()
      }
      current <- tryCatch({
        x <- draft()
        x$dry_run <- TRUE
        signature(x)
      }, error = function(e) NULL)
      if (!identical(current, signature(args))) {
        state$status <- app_status("warning", "status.changed_while_planning")
        return()
      }
      if (!identical(tryCatch(content_hash(json_text(context())), error = function(e) NULL), catalog)) {
        state$status <- app_status("warning", "status.catalog_changed_while_planning")
        return()
      }
      state$preview <- result$value
      if (!existing) state$hint_spec <- result$value$spec
      state$accepted <- list(signature = signature(args), catalog = catalog)
      state$status <- if (length(result$value$conflicts)) {
        app_status("warning", "status.preview_conflicts")
      } else {
        app_status("success", "status.preview_ready")
      }
    }
    apply_done <- function(args, result) {
      if (!isTRUE(result$ok)) {
        state$status <- app_status_error(result)
        focus_field(result$field)
        return()
      }
      value <- result$value
      state$result <- value
      state$last_args <- args
      state$preview <- NULL
      state$accepted <- NULL
      state$applied_hash <- tryCatch(signature(app_draft_canonical(draft_record())), error = function(e) NULL)
      if (existing) state$refresh <- state$refresh + 1L
      state$status <- if (length(value$conflicts)) {
        app_status("warning", "status.apply_conflicts")
      } else {
        app_status("success", if (existing) "status.applied" else "status.created")
      }
    }

    shiny::observeEvent(input$preview, guard({
      args <- draft()
      args$dry_run <- TRUE
      state$preview <- NULL
      state$result <- NULL
      state$accepted <- NULL
      catalog <- content_hash(json_text(context()))
      state$status <- app_start_job(slot, if (existing) "sync" else "project", args, "job.preview",
        function(result) preview_done(args, catalog, result))
    }))
    shiny::observeEvent(input$apply, guard({
      args <- draft()
      args$dry_run <- TRUE
      if (is.null(state$accepted) || !identical(signature(args), state$accepted$signature)) {
        app_abort_status("status.stale_preview")
      }
      if (!identical(content_hash(json_text(context())), state$accepted$catalog)) {
        app_abort_status("status.stale_catalog")
      }
      if (existing) {
        current <- do.call(sync, args)
        if (!identical(current$actions, state$preview$actions) || length(current$conflicts)) {
          app_abort_status("status.stale_files")
        }
      }
      args$dry_run <- FALSE
      attr(args, "cttir_catalog_fingerprint") <- state$accepted$catalog
      if (existing) attr(args, "cttir_sync_plan") <- content_hash(json_text(state$preview$actions))
      state$status <- app_start_job(slot, if (existing) "sync" else "project", args, "job.apply",
        function(result) apply_done(args, result), mutating = TRUE)
    }))
    shiny::observeEvent(input$cancel, state$status <- app_cancel_status(slot))

    restore_draft <- function(value) {
      if (slot$busy()) abort_cttir("Wait for the current operation before restoring a draft.")
      value <- app_draft_validate(value)
      operation <- if (existing) "configure" else "create"
      if (!identical(value$operation, operation) ||
          (existing && !identical(value$project_id, project_record()$spec$project$id))) {
        abort_cttir("This draft belongs to a different project or builder mode.", "cttir_schema_error")
      }
      state$accepted <- NULL
      state$preview <- NULL
      state$result <- NULL
      if (!existing) shiny::updateRadioButtons(session, "mode", selected = value$mode)
      shiny::updateTextAreaInput(session, "config", value = json_text(value$config, TRUE))
      if (!existing) {
        shiny::updateTextInput(session, "name", value = value$project$name)
        shiny::updateSelectInput(session, "type", selected = value$project$type)
        shiny::updateTextAreaInput(session, "goal", value = value$project$goal)
      }
      answers(app_answers_validate(value$answers, questions))
      step(1L)
      state$exported_hash <- signature(app_draft_canonical(value))
      state$status <- app_status("success", "draft.restored")
      invisible(value)
    }
    output$save_draft <- shiny::downloadHandler(
      filename = function() "cttir-draft.json",
      contentType = "application/json",
      content = function(file) {
        value <- draft_record()
        writeLines(json_text(value, TRUE), file, useBytes = TRUE)
        state$exported_hash <- signature(app_draft_canonical(value))
      }
    )
    shiny::observeEvent(input$load_draft, guard(restore_draft(app_draft_read(input$load_draft$datapath))))

    dirty <- shiny::reactive({
      state$exported_hash
      state$applied_hash
      text <- function(x) if (is.character(x) && length(x) == 1L) trimws(x) else ""
      content <- length(answers()) > 0L || !text(input$config) %in% c("", "{}") ||
        (!existing && (nzchar(text(input$name)) || nzchar(text(input$goal))))
      if (!content) return(FALSE)
      current <- tryCatch(signature(app_draft_canonical(draft_record())), error = function(e) NULL)
      is.null(current) || !current %in% c(state$exported_hash, state$applied_hash)
    })
    output$draft_status <- shiny::renderText({
      if (!dirty()) {
        app_t("draft.saved", lang(), revision = state$revision)
      } else {
        app_t("draft.unsaved", lang(), revision = state$revision)
      }
    })
    shiny::observe({
      busy <- !is.null(slot$state$job)
      session$sendCustomMessage("cttir-busy", list(ids = list(session$ns("preview"), session$ns("apply")), busy = busy))
    })
    shiny::observeEvent(lang(), {
      if (!existing) {
        shiny::updateSelectInput(session, "type", choices = app_choice_names(app_project_types(), lang()),
          selected = shiny::isolate(input$type))
      }
    }, ignoreInit = TRUE)

    output$status <- shiny::renderUI(app_status_ui(state$status, lang()))
    output$progress <- shiny::renderUI({
      if (!is.null(slot$state$job)) shiny::invalidateLater(1000, session)
      app_progress_ui(slot, lang())
    })
    output$project_summary <- shiny::renderUI({
      p <- project_record()
      if (is.null(p)) return(shiny::tags$p(class = "cttir-empty", app_t("open.none", lang())))
      shiny::tags$dl(class = "cttir-dl cttir-summary",
        shiny::tags$dt(app_t("open.name", lang())), shiny::tags$dd(p$spec$project$name),
        shiny::tags$dt(app_t("create.type", lang())), shiny::tags$dd(app_value_label(p$spec$project$type, lang())),
        shiny::tags$dt(app_t("open.path", lang())), shiny::tags$dd(shiny::tags$code(p$path)),
        shiny::tags$dt(app_t("review.profile", lang())), shiny::tags$dd(app_profile_label(p$spec$workflow$profile, lang())))
    })
    output$review <- shiny::renderUI({
      language <- lang()
      value <- state$result %||% state$preview
      if (is.null(value)) return(NULL)
      if (inherits(value, "cttir_sync")) {
        args <- if (!is.null(state$result)) state$last_args else shiny::isolate(tryCatch(draft(), error = function(e) list()))
        return(app_sync_review(value, project_record()$spec, args, language, applied = !is.null(state$result)))
      }
      if (!is.null(state$result)) return(app_project_result(value, language))
      counts <- table(factor(vapply(shiny::isolate(answers()), function(x) x$status, character(1)), levels = app_answer_statuses))
      note <- if (sum(counts)) app_t("review.answers_note", language, answered = counts[["answered"]], review = counts[["review"]])
      app_project_review(value, language, note)
    })

    list(dirty = dirty, state = state, draft = draft, answers = answers, slot = slot)
  })
}
