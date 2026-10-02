# Resources browser over resources(). Nothing is queried at application start:
# filter facets load from the local snapshot when the view is first opened and
# searches run only on request. Candidate status never implies a tested adapter.

app_resource_columns <- c("name", "maturity", "category", "repository", "observed_version",
  "purpose", "api_verification", "freshness", "documentation_url")

app_resource_maturity <- function(df) {
  status <- if ("adapter_status" %in% names(df)) as.character(df$adapter_status) else rep(NA_character_, nrow(df))
  tested <- !is.na(status) & grepl("tested|validated", status) & !grepl("^not_|untested", status)
  ifelse(tested, "tested", "candidate")
}

app_resources_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1(app_tr("resources.title")),
    shiny::tags$p(class = "cttir-lead", app_tr("resources.intro")),
    shiny::tags$div(class = "cttir-filters",
      shiny::textInput(ns("query"), app_tr("resources.query"), width = "100%"),
      shiny::selectInput(ns("domain"), app_tr("resources.domain"), c("All" = ""), selectize = FALSE, width = "100%"),
      shiny::selectInput(ns("repository"), app_tr("resources.repository"), c("All" = ""), selectize = FALSE, width = "100%"),
      shiny::selectInput(ns("maturity"), app_tr("resources.maturity"), c("All" = "", "Candidate" = "candidate", "Tested" = "tested"),
        selectize = FALSE, width = "100%"),
      shiny::numericInput(ns("limit"), app_tr("resources.limit"), value = 50, min = 1, max = 10000, step = 1, width = "100%")),
    shiny::checkboxInput(ns("use_project"), app_tr("resources.use_project"), value = FALSE),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("search"), app_tr("resources.search"), class = "btn-primary"),
      shiny::actionButton(ns("cancel"), app_tr("common.cancel"))),
    shiny::tags$div(class = "cttir-live", `aria-live` = "polite", shiny::uiOutput(ns("progress")), shiny::uiOutput(ns("status"))),
    shiny::tags$p(class = "cttir-note", app_tr("resources.candidate_note")),
    shiny::uiOutput(ns("results"))
  )
}

app_resources_server <- function(id, pool, lang = shiny::reactive("en"), project = shiny::reactive(NULL),
  visible = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    slot <- app_job_slot(pool, session)
    state <- shiny::reactiveValues(status = app_status("info", "resources.ready"), results = NULL,
      facets = NULL, requested = FALSE, query = NULL)
    update_facets <- function(language) {
      facets <- state$facets
      if (is.null(facets)) return()
      all <- app_t("resources.all", language)
      shiny::updateSelectInput(session, "domain", choices = c(stats::setNames("", all), stats::setNames(facets$domain, facets$domain)),
        selected = shiny::isolate(input$domain))
      shiny::updateSelectInput(session, "repository", choices = c(stats::setNames("", all), stats::setNames(facets$repository, facets$repository)),
        selected = shiny::isolate(input$repository))
      maturity <- stats::setNames(c("", "candidate", "tested"),
        c(all, app_t("value.candidate", language), app_t("value.tested", language)))
      shiny::updateSelectInput(session, "maturity", choices = maturity, selected = shiny::isolate(input$maturity))
    }
    search <- function(args, label, facets = FALSE) {
      state$status <- app_start_job(slot, "resources", args, label, function(result) {
        if (!isTRUE(result$ok)) {
          state$status <- app_status_error(result)
          return()
        }
        value <- result$value
        if (facets && is.data.frame(value)) {
          state$facets <- list(
            domain = sort(unique(stats::na.omit(as.character(value$category)))),
            repository = sort(unique(stats::na.omit(as.character(value$repository)))))
          update_facets(lang())
          value <- utils::head(value, 50L)
        }
        state$results <- value
        state$query <- args
        state$status <- app_status("success", "resources.done", count = if (is.data.frame(value)) nrow(value) else 0L)
      })
    }
    # Lazy first load: only when the view is opened, from the local snapshot.
    shiny::observe({
      if (!isTRUE(visible()) || shiny::isolate(state$requested)) return()
      state$requested <- TRUE
      shiny::isolate(search(list(limit = 10000L), "job.resources_facets", facets = TRUE))
    })
    shiny::observeEvent(lang(), update_facets(lang()), ignoreInit = TRUE)
    shiny::observeEvent(input$search, {
      text <- function(x) if (is.character(x) && length(x) == 1L && nzchar(trimws(x))) trimws(x) else NULL
      limit <- suppressWarnings(as.integer(input$limit %||% 50L))
      if (is.na(limit) || limit < 1L || limit > 10000L) {
        state$status <- app_status("error", "resources.limit_invalid")
        session$sendCustomMessage("cttir-focus", list(id = session$ns("limit")))
        return()
      }
      args <- list(query = text(input$query), domain = text(input$domain), repository = text(input$repository),
        path = if (isTRUE(input$use_project)) project() else NULL, limit = limit)
      search(args, "job.resources")
    })
    shiny::observeEvent(input$cancel, state$status <- app_cancel_status(slot))
    output$status <- shiny::renderUI(app_status_ui(state$status, lang()))
    output$progress <- shiny::renderUI({
      if (!is.null(slot$state$job)) shiny::invalidateLater(1000, session)
      app_progress_ui(slot, lang())
    })
    filtered <- shiny::reactive({
      value <- state$results
      if (!is.data.frame(value)) return(value)
      value$maturity <- app_resource_maturity(value)
      maturity <- input$maturity %||% ""
      if (nzchar(maturity)) value <- value[value$maturity == maturity, , drop = FALSE]
      value
    })
    output$results <- shiny::renderUI({
      value <- filtered()
      if (is.null(value)) return(NULL)
      shiny::tags$section(class = "cttir-review-panel",
        shiny::tags$h2(app_t("resources.results", lang(), count = if (is.data.frame(value)) nrow(value) else 0L)),
        app_table(value, lang(), columns = app_resource_columns,
          badges = c("maturity", "api_verification", "freshness"), links = "documentation_url"))
    })
    list(state = state, slot = slot, filtered = filtered)
  })
}
