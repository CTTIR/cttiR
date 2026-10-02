app_require <- function() {
  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("callr", quietly = TRUE)) {
    abort_cttir("Install the suggested shiny and callr packages to use the local application.", "cttir_source_unavailable")
  }
}

app_config <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list())
  }
  if (nchar(text, type = "bytes") > 1048576L) abort_cttir("Configuration exceeds 1 MiB.")
  value <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) {
    abort_cttir("Detailed configuration must be valid JSON.", field = "/")
  })
  if (!is.list(value)) abort_cttir("Detailed configuration must be a JSON object.", "cttir_schema_error", field = "/")
  validate_config(value)
}

# Static assets are package files inlined into the page; nothing else is served.
app_asset <- function(name) paste(readLines(resource_file("app", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

app_head <- function() {
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$style(shiny::HTML(app_asset("cttir.css"))),
    shiny::tags$script(shiny::HTML(app_asset("cttir.js")))
  )
}

app_header <- function() {
  shiny::tags$div(class = "cttir-topbar",
    shiny::tags$a(class = "cttir-skip", href = "#cttir-content", app_tr("app.skip")),
    shiny::tags$span(class = "cttir-local-badge", role = "note",
      shiny::tags$span(class = "cttir-badge-icon", `aria-hidden` = "true", "\u2302"), app_tr("app.local_only")),
    shiny::tags$div(class = "cttir-lang",
      shiny::selectInput("lang", "Language / Sprache", app_languages, selected = "en", selectize = FALSE, width = "160px")),
    shiny::tags$div(id = "cttir-content", tabindex = "-1")
  )
}

app_ui <- function(mode = "fast", path = NULL) {
  selected <- if (!is.null(path)) "open" else if (identical(mode, "detailed")) "create" else "home"
  view <- function(title, value, content) {
    shiny::tabPanel(app_tr(title), value = value, shiny::tags$div(class = "cttir-view", content))
  }
  shiny::navbarPage(
    title = shiny::tags$span(class = "cttir-brand", "cttir"),
    id = "nav", selected = selected, collapsible = TRUE, windowTitle = "cttir", lang = "en",
    header = shiny::tagList(app_head(), app_header()),
    view("nav.home", "home", app_home_ui("home")),
    view("nav.create", "create", app_builder_ui("create", mode)),
    view("nav.open", "open", app_open_ui("open", path)),
    view("nav.ask", "ask", app_ask_ui("ask")),
    view("nav.knowledge", "knowledge", app_knowledge_ui("knowledge")),
    view("nav.resources", "resources", app_resources_ui("resources")),
    view("nav.audit", "audit", app_audit_ui("audit")),
    view("nav.runtime", "runtime", app_runtime_ui("runtime"))
  )
}

app_server <- function(path = NULL, worker = app_worker) {
  function(input, output, session) {
    pool <- app_job_pool(worker)
    lang <- shiny::reactive(app_lang(input$lang))
    shiny::observe({
      language <- lang()
      session$sendCustomMessage("cttir-i18n", list(lang = language, messages = app_messages()[[language]]))
    })
    home <- app_home_server("home")
    shiny::observeEvent(home(), shiny::updateNavbarPage(session, "nav", selected = home()$tab))
    create <- app_builder_server("create", pool, lang)
    opened <- app_open_server("open", pool, lang, path)
    app_ask_server("ask", pool, lang, project = opened$path)
    app_knowledge_server("knowledge", pool, lang, project = opened$path)
    app_resources_server("resources", pool, lang, project = opened$path,
      visible = shiny::reactive(identical(input$nav, "resources")))
    app_audit_server("audit", pool, lang, project = opened$path)
    app_runtime_server("runtime", pool, lang)
    dirty <- shiny::reactive(isTRUE(create$dirty()) || isTRUE(opened$builder$dirty()))
    shiny::observe(session$sendCustomMessage("cttir-dirty", list(dirty = dirty())))
    previous <- shiny::reactiveVal(NULL)
    notices <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$nav, {
      left <- previous()
      if ((identical(left, "create") && isTRUE(create$dirty())) || (identical(left, "open") && isTRUE(opened$builder$dirty()))) {
        notices(notices() + 1L)
        shiny::showNotification(app_t("draft.unsaved_notice", lang()), type = "warning", duration = 8)
      }
      previous(input$nav)
    })
  }
}

#' Open the local project builder
#'
#' Returns the local cttiR application. Printing it in an interactive R session
#' launches it on the loopback interface. Views: Home, Create (Fast and
#' Detailed share one draft), Open project/Configure, Ask, Knowledge (package
#' coverage, catalog update preview/apply and rollback preview), Resources,
#' Audit (report export and the documented repair allowlist) and Runtime setup.
#' Every operation calls the same exported functions as the R API
#' ([project()], [sync()], [ask()], [search()], [packages()], [resources()],
#' [update()], [rollback_knowledge()], [audit()], [setup()]) in a bounded
#' background R process; read-only work can be cancelled and duplicate
#' submissions are refused. Creation and configuration require a reviewed
#' preview of the current draft and catalog. Scientific analysis and returned
#' code are never executed by the interface.
#'
#' Fast asks only for name, research type and goal (plus the parent directory)
#' and shows the resolved workflow profile, routing reason and selected tools.
#' Detailed adds optional questions from a declarative registry; every question
#' can stay unanswered, changing an answer marks dependent answers for review,
#' and answers that no longer apply are not used. Answers are passed as
#' `options`, the advanced JSON as `config`.
#'
#' Drafts can be exported to JSON and restored explicitly. They retain answers
#' but exclude the parent directory, local data bindings and accepted plans.
#' Restored drafts require a new preview; Configure drafts are bound to the
#' existing project identity. Exported text may contain user-supplied sensitive
#' information. Drafts are not automatically saved. The interface is available
#' in English and German.
#' @param mode Initial `fast` or `detailed` view.
#' @param launch.browser Open the browser when the application is run.
#' @return A `shiny.appobj`. Shiny and callr must be installed. The background
#'   workers use the installed cttiR, which must match the running version. Run
#'   locally only; authentication and remote multi-user deployment are not
#'   supported.
#' @export
setup_app <- function(mode = c("fast", "detailed"), launch.browser = interactive()) {
  app_require()
  mode <- match.arg(mode)
  scalar_flag(launch.browser, "launch.browser")
  shiny::shinyApp(app_ui(mode), app_server(),
    options = list(host = "127.0.0.1", launch.browser = launch.browser)
  )
}

#' Configure an existing project with an explicit preview
#'
#' Opens the local application on the Configure view of an existing project.
#' Changes are collected from the optional questionnaire and advanced JSON,
#' previewed with [sync()] (field-level requested changes and the file plan)
#' and applied only on explicit request. Edited managed files are reported as
#' conflicts and nothing is written while conflicts remain.
#' @param path Exact existing project root.
#' @param launch.browser Open the browser when the application is run.
#' @return A local Shiny application using [sync()] for preview and application.
#' @export
configure <- function(path = ".", launch.browser = interactive()) {
  app_require()
  scalar_flag(launch.browser, "launch.browser")
  p <- read_project(path, edited = TRUE)
  shiny::shinyApp(app_ui("detailed", p$path), app_server(p$path),
    options = list(host = "127.0.0.1", launch.browser = launch.browser)
  )
}
