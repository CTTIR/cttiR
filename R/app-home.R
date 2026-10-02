# Home and Open project views.

app_home_cards <- c(create = "create", open = "open", ask = "ask", knowledge = "knowledge",
  resources = "resources", audit = "audit", runtime = "runtime")

app_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1(app_tr("home.title")),
    shiny::tags$p(class = "cttir-lead", app_tr("home.claim")),
    shiny::tags$p(class = "cttir-note", app_tr("home.local_note")),
    shiny::tags$div(class = "cttir-cards", lapply(names(app_home_cards), function(card) {
      shiny::tags$article(class = "cttir-card", `aria-labelledby` = ns(paste0("h_", card)),
        shiny::tags$h2(id = ns(paste0("h_", card)), app_tr(paste0("nav.", card))),
        shiny::tags$p(app_tr(paste0("home.", card))),
        shiny::actionButton(ns(paste0("go_", card)), app_tr(paste0("home.open_", card)), class = "btn-primary"))
    }))
  )
}

# Returns a reactive with the requested tab; the app switches the navbar.
app_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    target <- shiny::reactiveVal(NULL)
    for (card in names(app_home_cards)) {
      local({
        card <- card
        shiny::observeEvent(input[[paste0("go_", card)]], target(list(tab = app_home_cards[[card]], at = Sys.time())))
      })
    }
    target
  })
}

app_open_ui <- function(id, path = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (is.null(path)) {
      shiny::tags$section(class = "cttir-section",
        shiny::tags$h1(app_tr("open.title")),
        shiny::tags$p(class = "cttir-lead", app_tr("open.intro")),
        shiny::textInput(ns("path"), app_tr("open.path"), width = "100%"),
        shiny::helpText(app_tr("open.path_help")),
        shiny::actionButton(ns("open"), app_tr("open.button"), class = "btn-primary"),
        shiny::tags$div(class = "cttir-live", `aria-live` = "polite", shiny::uiOutput(ns("status"))))
    },
    app_builder_ui(ns("configure"), "detailed", existing = TRUE)
  )
}

app_open_server <- function(id, pool, lang = shiny::reactive("en"), path = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    opened <- shiny::reactiveVal(path)
    status <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$open, {
      tryCatch({
        p <- read_project(input$path)
        opened(p$path)
        status(app_status("success", "open.opened"))
      }, error = function(e) {
        status(app_condition_status(e))
        session$sendCustomMessage("cttir-focus", list(id = session$ns("path")))
      })
    })
    output$status <- shiny::renderUI(app_status_ui(status(), lang()))
    builder <- app_builder_server("configure", pool, lang, path = opened)
    list(path = opened, builder = builder)
  })
}
