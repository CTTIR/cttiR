# Ask CTTIR: grounded answers and API search through ask()/search(). Returned
# code is displayed and can be copied or downloaded; it is never executed.

app_answer_ui <- function(answer, lang, ns) {
  value <- unclass(answer)
  code <- value$code
  code <- if (is.character(code) && length(code) && any(nzchar(code))) paste(code, collapse = "\n") else NULL
  evidence <- value$evidence
  shiny::tags$section(class = "cttir-review-panel", `aria-labelledby` = ns("answer_heading"),
    shiny::tags$h2(id = ns("answer_heading"), app_t("ask.answer", lang)),
    shiny::tags$p(as.character(unlist(value$answer))[1] %||% ""),
    shiny::tags$p(app_t("ask.verification", lang), " ",
      if (length(value$verification_levels)) {
        lapply(unlist(value$verification_levels), function(level) shiny::tagList(app_badge(level, lang), " "))
      } else {
        app_badge("unknown", lang, label = app_t("ask.no_verified", lang))
      }),
    if (length(value$steps)) shiny::tagList(shiny::tags$h3(app_t("ask.steps", lang)),
      shiny::tags$ol(lapply(unlist(value$steps), function(step) shiny::tags$li(shiny::tags$code(step))))),
    shiny::tags$h3(app_t("ask.code", lang)),
    if (is.null(code)) {
      shiny::tags$p(class = "cttir-empty", app_t("ask.no_code", lang))
    } else {
      shiny::tagList(
        shiny::tags$p(class = "cttir-note", app_t("ask.code_note", lang)),
        shiny::tags$pre(shiny::tags$code(id = ns("code_text"), code)),
        shiny::tags$button(type = "button", class = "btn btn-default", `data-copy-target` = ns("code_text"), app_t("ask.copy", lang)),
        shiny::downloadButton(ns("download_code"), app_t("ask.download", lang)))
    },
    if (length(value$citations)) shiny::tagList(shiny::tags$h3(app_t("ask.citations", lang)),
      shiny::tags$ul(lapply(unique(unlist(value$citations)), function(url) shiny::tags$li(app_link(url))))),
    if (is.data.frame(evidence) && nrow(evidence)) shiny::tags$details(
      shiny::tags$summary(app_t("ask.evidence", lang, count = nrow(evidence))),
      app_table(evidence, lang, columns = c("symbol", "package", "revision", "kind", "verification", "approved", "evidence"),
        badges = "verification", links = "evidence")),
    shiny::tags$h3(app_t("ask.limitations", lang)),
    app_list_block(value$limitations, lang),
    app_other_fields(answer, c("answer", "steps", "code", "citations", "verification_levels", "evidence", "limitations"), lang)
  )
}

app_ask_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1(app_tr("ask.title")),
    shiny::tags$p(class = "cttir-lead", app_tr("ask.intro")),
    shiny::radioButtons(ns("action"), app_tr("ask.action"), inline = TRUE,
      choiceNames = list(app_tr("ask.action_ask"), app_tr("ask.action_search")), choiceValues = c("ask", "search")),
    shiny::textAreaInput(ns("question"), app_tr("ask.question"), rows = 3, width = "100%"),
    shiny::conditionalPanel(sprintf("input['%s'] === 'ask'", ns("action")),
      shiny::checkboxInput(ns("verified_only"), app_tr("ask.verified_only"), value = TRUE)),
    shiny::conditionalPanel(sprintf("input['%s'] === 'search'", ns("action")),
      shiny::numericInput(ns("limit"), app_tr("ask.limit"), value = 20, min = 1, max = 200, step = 1)),
    shiny::checkboxInput(ns("use_project"), app_tr("common.use_project"), value = FALSE),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("run"), app_tr("ask.run"), class = "btn-primary"),
      shiny::actionButton(ns("cancel"), app_tr("common.cancel"))),
    shiny::tags$div(class = "cttir-live", `aria-live` = "polite", shiny::uiOutput(ns("progress")), shiny::uiOutput(ns("status"))),
    shiny::uiOutput(ns("result"))
  )
}

app_ask_server <- function(id, pool, lang = shiny::reactive("en"), project = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    slot <- app_job_slot(pool, session)
    state <- shiny::reactiveValues(status = app_status("info", "ask.ready"), result = NULL, kind = NULL)
    project_path <- function() if (isTRUE(input$use_project)) project() else NULL
    shiny::observeEvent(input$run, {
      question <- input$question %||% ""
      if (!nzchar(trimws(question))) {
        state$status <- app_status("error", "ask.empty")
        session$sendCustomMessage("cttir-focus", list(id = session$ns("question")))
        return()
      }
      kind <- if (identical(input$action, "search")) "search" else "ask"
      limit <- suppressWarnings(as.integer(input$limit %||% 20L))
      args <- if (kind == "ask") {
        list(question = question, path = project_path(), verified_only = !isFALSE(input$verified_only))
      } else {
        list(query = question, path = project_path(), limit = if (is.na(limit)) 20L else limit)
      }
      state$status <- app_start_job(slot, kind, args, paste0("job.", kind), function(result) {
        if (!isTRUE(result$ok)) {
          state$status <- app_status_error(result)
          return()
        }
        state$result <- result$value
        state$kind <- kind
        state$status <- app_status("success", "ask.done")
      })
    })
    shiny::observeEvent(input$cancel, state$status <- app_cancel_status(slot))
    output$status <- shiny::renderUI(app_status_ui(state$status, lang()))
    output$progress <- shiny::renderUI({
      if (!is.null(slot$state$job)) shiny::invalidateLater(1000, session)
      app_progress_ui(slot, lang())
    })
    output$result <- shiny::renderUI({
      value <- state$result
      if (is.null(value)) return(NULL)
      if (is.data.frame(value)) {
        columns <- c("symbol", "package", "kind", "snippet", "verification", "approved", "evidence")
        table <- app_table(value, lang(), columns = columns, badges = "verification", links = "evidence")
        heading <- shiny::tags$h2(app_t("ask.search_results", lang(), count = nrow(value)))
        return(shiny::tags$section(class = "cttir-review-panel", heading, table))
      }
      app_answer_ui(value, lang(), session$ns)
    })
    output$download_code <- shiny::downloadHandler(
      filename = function() "cttir-answer-code.R",
      contentType = "text/plain",
      content = function(file) {
        code <- unclass(state$result)$code
        text <- if (is.character(code)) paste(code, collapse = "\n") else ""
        writeLines(c("# Code returned by cttiR::ask(); review before use. It was not executed.", text), file, useBytes = TRUE)
      }
    )
    list(state = state, slot = slot)
  })
}
