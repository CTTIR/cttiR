# Runtime setup view over setup() and the installation scope of audit(). A dry
# run never contacts a daemon; the explicit run may download and start the
# owned local runtime. Download sizes come from the packaged runtime manifest.

app_bytes <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) return(NULL)
  sprintf("%.2f GB", x / 1e9)
}

app_runtime_estimate <- function(lang) {
  manifest <- tryCatch(read_document(resource_file("runtime", "manifest.json")), error = function(e) NULL)
  if (is.null(manifest)) return(NULL)
  runtime <- app_bytes(manifest$archive_bytes)
  model <- app_bytes(manifest$model_bytes)
  platform <- paste(manifest$platform %||% "?", manifest$arch %||% "")
  text <- app_t("runtime.estimate", lang, runtime = runtime %||% "?", model = model %||% "?",
    version = manifest$runtime_version %||% "?", platform = platform)
  shiny::tags$p(class = "cttir-note", text)
}

app_setup_ui <- function(x, lang) {
  value <- unclass(x)
  steps <- value$steps
  step_row <- function(s) {
    data.frame(id = as.character(s$id %||% NA), state = as.character(s$state %||% "unknown"), stringsAsFactors = FALSE)
  }
  step_rows <- if (length(steps)) do.call(rbind, lapply(steps, step_row))
  shiny::tags$section(class = "cttir-review-panel",
    shiny::tags$h2(app_t("runtime.result_title", lang)),
    shiny::tags$dl(class = "cttir-dl cttir-summary",
      shiny::tags$dt(app_t("field.state", lang)), shiny::tags$dd(app_badge(value$state, lang)),
      shiny::tags$dt(app_t("runtime.runtime", lang)),
      shiny::tags$dd(paste(value$runtime$version %||% "?", value$runtime$endpoint %||% "")),
      shiny::tags$dt(app_t("runtime.locality", lang)),
      shiny::tags$dd(app_badge(value$runtime$locality %||% "not_tested", lang)),
      shiny::tags$dt(app_t("runtime.model", lang)),
      shiny::tags$dd(shiny::tags$code(value$model$name %||% "?"), " ", app_badge(value$model$validation %||% "unknown", lang),
        if (!is.null(value$model$digest)) shiny::tags$code(substr(value$model$digest, 1L, 12L)))),
    shiny::tags$h3(app_t("runtime.actions", lang)),
    app_list_block(value$actions, lang),
    if (!is.null(step_rows)) shiny::tagList(shiny::tags$h3(app_t("runtime.steps", lang)), app_table(step_rows, lang, badges = "state")),
    shiny::tags$h3(app_t("runtime.blockers", lang)),
    app_list_block(value$blockers, lang),
    app_other_fields(x, c("state", "steps", "runtime", "model", "actions", "blockers"), lang)
  )
}

app_runtime_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1(app_tr("runtime.title")),
    shiny::tags$p(class = "cttir-lead", app_tr("runtime.intro")),
    shiny::uiOutput(ns("estimate")),
    shiny::textInput(ns("model"), app_tr("runtime.model_tag"), value = "auto", width = "100%"),
    shiny::checkboxInput(ns("install"), app_tr("runtime.install"), value = TRUE),
    shiny::checkboxInput(ns("offline"), app_tr("runtime.offline"), value = FALSE),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("plan"), app_tr("runtime.plan"), class = "btn-primary"),
      shiny::actionButton(ns("status_check"), app_tr("runtime.check")),
      shiny::actionButton(ns("run"), app_tr("runtime.run"), class = "btn-warning"),
      shiny::actionButton(ns("cancel"), app_tr("common.cancel"))),
    shiny::helpText(app_tr("runtime.run_help")),
    shiny::tags$div(class = "cttir-live", `aria-live` = "polite", shiny::uiOutput(ns("progress")), shiny::uiOutput(ns("status"))),
    shiny::uiOutput(ns("locality")),
    shiny::uiOutput(ns("result"))
  )
}

app_runtime_server <- function(id, pool, lang = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {
    slot <- app_job_slot(pool, session)
    state <- shiny::reactiveValues(status = app_status("info", "runtime.ready"), result = NULL, locality = NULL)
    setup_args <- function(dry_run) {
      model <- trimws(input$model %||% "auto")
      list(model = if (nzchar(model)) model else "auto", install_ollama = !isFALSE(input$install),
        offline = isTRUE(input$offline), dry_run = dry_run)
    }
    on_setup <- function(result) {
      if (!isTRUE(result$ok)) {
        state$status <- app_status_error(result)
        if (identical(result$field, "model")) session$sendCustomMessage("cttir-focus", list(id = session$ns("model")))
        return()
      }
      state$result <- result$value
      state$status <- if (identical(result$value$state, "blocked")) {
        app_status("warning", "runtime.blocked")
      } else {
        app_status("success", "runtime.done", state = app_value_label(result$value$state, lang()))
      }
    }
    shiny::observeEvent(input$plan, {
      state$status <- app_start_job(slot, "setup", setup_args(TRUE), "job.setup_plan", on_setup)
    })
    shiny::observeEvent(input$run, {
      state$status <- app_start_job(slot, "setup", setup_args(FALSE), "job.setup_run", on_setup, mutating = TRUE)
    })
    shiny::observeEvent(input$status_check, {
      state$status <- app_start_job(slot, "audit", list(scope = "installation"), "job.runtime_status", function(result) {
        if (!isTRUE(result$ok)) {
          state$status <- app_status_error(result)
          return()
        }
        state$locality <- result$value
        state$status <- app_status("success", "runtime.status_done")
      })
    })
    shiny::observeEvent(input$cancel, state$status <- app_cancel_status(slot))
    output$estimate <- shiny::renderUI(app_runtime_estimate(lang()))
    output$status <- shiny::renderUI(app_status_ui(state$status, lang()))
    output$progress <- shiny::renderUI({
      if (!is.null(slot$state$job)) shiny::invalidateLater(1000, session)
      app_progress_ui(slot, lang())
    })
    output$locality <- shiny::renderUI({
      value <- state$locality
      if (is.null(value)) return(NULL)
      checks <- value$checks
      rows <- if (is.data.frame(checks)) checks[checks$scope == "installation", , drop = FALSE] else NULL
      shiny::tags$section(class = "cttir-review-panel",
        shiny::tags$h2(app_t("runtime.locality_title", lang())),
        app_table(rows, lang(), columns = c("id", "status", "message"), badges = "status"))
    })
    output$result <- shiny::renderUI(if (!is.null(state$result)) app_setup_ui(state$result, lang()))
    list(state = state, slot = slot)
  })
}
