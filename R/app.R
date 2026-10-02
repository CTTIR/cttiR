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
    abort_cttir("Detailed configuration must be valid JSON.")
  })
  if (!is.list(value)) abort_cttir("Detailed configuration must be a JSON object.", "cttir_schema_error")
  validate_config(value)
}

app_worker <- function(operation, args) {
  allowed <- c("project", "sync", "ask", "resources", "packages", "update", "audit", "setup")
  if (!operation %in% allowed) abort_cttir("Unknown application operation.")
  settings <- options()[intersect(names(options()), c("cttiR.catalog_dir", "cttiR.sources", "cttiR.runtime_dir", "cttiR.ollama_endpoint"))]
  callr::r_bg(
    function(operation, args, settings, version) {
      options(settings)
      if (!identical(as.character(utils::packageVersion("cttiR")), version)) {
        stop("The installed package version differs from the application. Reinstall the current package.")
      }
      expected <- attr(args, "cttir_catalog_fingerprint")
      attr(args, "cttir_catalog_fingerprint") <- NULL
      expected_plan <- attr(args, "cttir_sync_plan")
      attr(args, "cttir_sync_plan") <- NULL
      target <- getExportedValue("cttiR", operation)
      if (operation == "project" && !is.null(expected)) {
        target <- utils::getFromNamespace("project_impl", "cttiR")
        args$expected_catalog <- expected
      }
      if (operation == "sync" && !is.null(expected_plan)) {
        target <- utils::getFromNamespace("sync_impl", "cttiR")
        args$expected_plan <- expected_plan
      }
      tryCatch(list(ok = TRUE, value = do.call(target, args)),
        error = function(e) {
          list(ok = FALSE, message = if (inherits(e, "cttir_error")) {
            conditionMessage(e)
          } else {
            "The operation failed. Check the local inputs and package installation."
          })
        }
      )
    },
    args = list(operation, args, settings, as.character(utils::packageVersion("cttiR"))),
    libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE, supervise = FALSE
  )
}

builder_ui <- function(id, mode, path) {
  ns <- shiny::NS(id)
  existing <- !is.null(path)
  shiny::tags$div(
    class = "cttir-shell",
    shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(
      ".cttir-shell{max-width:1040px;margin:24px auto;padding:0 12px;overflow-wrap:anywhere;}",
      ".cttir-shell .shiny-input-container{max-width:100%;}",
      ".cttir-shell table{width:100%;table-layout:fixed;}",
      ".cttir-shell td,.cttir-shell th{overflow-wrap:anywhere;}",
      ".cttir-shell th:first-child{width:75%;}",
      ".cttir-shell pre{max-width:100%;overflow:auto;}",
      ".cttir-shell button{margin:4px 2px 8px 0;max-width:100%;}",
      ".cttir-shell [role=status]{padding:12px 0;font-weight:600;}"
    )))),
    shiny::tags$h1(if (existing) "Configure project" else "Create a research project"),
    shiny::tags$p(if (existing) {
      "Review explicit configuration changes before applying them to this project."
    } else {
      "Start with a name, research type and goal. Review the plan before applying it."
    }),
    shiny::tags$p(class = "text-muted", "Development preview: creates a scaffold. Analysis adapters and scientific approval remain pending."),
    shiny::radioButtons(ns("mode"), "Detail level", c("Fast" = "fast", "Detailed" = "detailed"), selected = mode, inline = TRUE),
    if (!existing) {
      shiny::tagList(
        shiny::textInput(ns("name"), "Project name"),
        shiny::selectInput(ns("type"), "Research type", c("primary_research", "secondary_research", "methods", "review", "software", "mixed", "other")),
        shiny::textAreaInput(ns("goal"), "Research goal", rows = 3),
        shiny::textInput(ns("parent"), "Parent directory", value = getwd())
      )
    } else {
      shiny::tags$p("Project: ", path)
    },
    shiny::conditionalPanel(
      sprintf("input['%s'] === 'detailed'", ns("mode")),
      shiny::textAreaInput(ns("config"), "Optional configuration (JSON)", value = "{}", rows = 8),
      shiny::helpText("Unknown is a valid scientific state. Detailed values remain in the draft when switching to Fast mode.")
    ),
    shiny::tags$details(
      shiny::tags$summary("Save or resume a draft"),
      shiny::helpText("Export a JSON file to a location you choose. It includes your text and configuration; review them for sensitive content. The parent directory, local bindings and accepted preview are not exported."),
      shiny::downloadButton(ns("save_draft"), "Export draft"),
      shiny::fileInput(ns("load_draft"), "Restore a draft and replace current answers", accept = ".json"),
      shiny::helpText("Restoring requires a new preview. A creation draft uses the currently selected parent directory; a Configure draft must belong to this project."),
      shiny::textOutput(ns("draft_status"))
    ),
    shiny::actionButton(ns("preview"), "Preview changes", class = "btn-primary"),
    shiny::actionButton(ns("apply"), if (existing) "Apply reviewed changes" else "Create reviewed project"),
    shiny::actionButton(ns("cancel"), "Cancel preview"),
    shiny::tags$div(role = "status", `aria-live` = "polite", shiny::textOutput(ns("status"))),
    shiny::tableOutput(ns("plan")),
    shiny::verbatimTextOutput(ns("result")),
    shiny::tags$hr(),
    shiny::tags$h2("Local tools"),
    shiny::selectInput(ns("tool"), "Tool", c("API evidence" = "ask", "Resources" = "resources", "Package coverage" = "packages", "Audit" = "audit", "Preview catalog update" = "update", "Preview runtime setup" = "setup")),
    shiny::textInput(ns("query"), "Question or resource search"),
    shiny::actionButton(ns("inspect"), "Run read-only tool"),
    shiny::verbatimTextOutput(ns("tool_result"))
  )
}

builder_server <- function(id, path = NULL, worker = app_worker) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveValues(job = NULL, preview = NULL, accepted = NULL, result = NULL, tool_result = NULL, status = "Ready. No files have been changed.", revision = 0L, draft_hash = NULL, exported_hash = NULL)
    draft <- shiny::reactive({
      config <- app_config(input$config)
      if (!is.null(path)) {
        return(list(path = path, config = config))
      }
      list(name = input$name, type = input$type, goal = input$goal, path = input$parent, config = config)
    })
    signature <- function(args) content_hash(json_text(args))
    draft_record <- function() {
      args <- draft()
      app_draft_validate(list(
        schema_version = 1L, operation = if (is.null(path)) "create" else "configure",
        mode = if (identical(input$mode, "detailed")) "detailed" else "fast",
        project = if (is.null(path)) args[c("name", "type", "goal")] else NULL,
        project_id = if (is.null(path)) NULL else read_project(path)$spec$project$id,
        config = args$config
      ))
    }
    shiny::observe({
      current <- signature(list(input$name, input$type, input$goal, input$parent, input$config))
      if (!identical(current, shiny::isolate(state$draft_hash))) {
        state$draft_hash <- current
        state$revision <- shiny::isolate(state$revision) + 1L
        state$accepted <- NULL
        state$preview <- NULL
      }
    })
    restore_draft <- function(value) {
      if (!is.null(state$job)) abort_cttir("Wait for the current operation before restoring a draft.")
      value <- app_draft_validate(value)
      operation <- if (is.null(path)) "create" else "configure"
      if (!identical(value$operation, operation) ||
          (!is.null(path) && !identical(value$project_id, read_project(path)$spec$project$id))) {
        abort_cttir("This draft belongs to a different project or builder mode.", "cttir_schema_error")
      }
      state$accepted <- NULL
      state$preview <- NULL
      state$result <- NULL
      shiny::updateRadioButtons(session, "mode", selected = value$mode)
      shiny::updateTextAreaInput(session, "config", value = json_text(value$config, TRUE))
      if (is.null(path)) {
        shiny::updateTextInput(session, "name", value = value$project$name)
        shiny::updateSelectInput(session, "type", selected = value$project$type)
        shiny::updateTextAreaInput(session, "goal", value = value$project$goal)
      }
      state$exported_hash <- signature(value)
      state$status <- "Draft restored. Check the answers and parent directory, then preview again."
      invisible(value)
    }
    output$save_draft <- shiny::downloadHandler(
      filename = function() "cttir-draft.json",
      contentType = "application/json",
      content = function(file) {
        value <- draft_record()
        writeLines(json_text(value, TRUE), file, useBytes = TRUE)
        state$exported_hash <- signature(value)
      }
    )
    shiny::observeEvent(input$load_draft, {
      tryCatch(restore_draft(app_draft_read(input$load_draft$datapath)),
        error = function(e) state$status <- if (inherits(e, "cttir_error")) conditionMessage(e) else "Could not restore the draft."
      )
    })
    output$draft_status <- shiny::renderText({
      current <- tryCatch(signature(draft_record()), error = function(e) NULL)
      if (!is.null(current) && identical(current, state$exported_hash)) {
        paste("Revision", state$revision, "matches the last exported or restored draft. Keep that file to resume.")
      } else {
        paste("Revision", state$revision, "has unexported answers. Export before leaving; drafts are not saved automatically.")
      }
    })
    context <- function() {
      if (is.null(path)) return(current_catalog_manifest())
      p <- read_project(path)
      snapshot_manifest(p$lock$catalog_id, p$lock$resource_snapshot)
    }
    begin <- function(operation, args, purpose, mutating = FALSE) {
      if (!is.null(state$job)) {
        state$status <- "An operation is already running."
        return(invisible(NULL))
      }
      tryCatch(
        {
          catalog <- if (purpose == "preview") content_hash(json_text(context())) else NULL
          process <- worker(operation, args)
          state$job <- list(
            process = process, purpose = purpose, args = args, mutating = mutating,
            catalog = catalog
          )
          state$status <- if (mutating) "Applying reviewed changes. Keep this session open until completion." else "Reading local inputs..."
        },
        error = function(e) state$status <- if (inherits(e, "cttir_error")) conditionMessage(e) else "Could not start the local worker."
      )
      invisible(NULL)
    }
    shiny::observeEvent(input$preview, {
      tryCatch(
        {
          args <- draft()
          args$dry_run <- TRUE
          state$preview <- NULL
          state$result <- NULL
          state$accepted <- NULL
          begin(if (is.null(path)) "project" else "sync", args, "preview")
        },
        error = function(e) state$status <- conditionMessage(e)
      )
    })
    shiny::observeEvent(input$apply, {
      tryCatch(
        {
          args <- draft()
          args$dry_run <- TRUE
          if (is.null(state$accepted) || !identical(signature(args), state$accepted$signature)) {
            abort_cttir("The draft changed or has no accepted preview. Preview it again before applying.")
          }
          if (!identical(content_hash(json_text(context())), state$accepted$catalog)) {
            abort_cttir("The active catalog changed. Preview again before applying.")
          }
          if (!is.null(path)) {
            current <- do.call(sync, args)
            if (!identical(current$actions, state$preview$actions) || length(current$conflicts)) {
              abort_cttir("Project files changed or contain conflicts. Preview again before applying.")
            }
          }
          args$dry_run <- FALSE
          attr(args, "cttir_catalog_fingerprint") <- state$accepted$catalog
          if (!is.null(path)) attr(args, "cttir_sync_plan") <- content_hash(json_text(state$preview$actions))
          begin(if (is.null(path)) "project" else "sync", args, "apply", TRUE)
        },
        error = function(e) state$status <- if (inherits(e, "cttir_error")) conditionMessage(e) else "Could not validate the current draft."
      )
    })
    shiny::observeEvent(input$cancel, {
      job <- state$job
      if (is.null(job)) {
        return()
      }
      if (job$mutating) {
        state$status <- "A write is in progress and cannot be cancelled safely."
        return()
      }
      job$process$kill()
      state$job <- NULL
      state$status <- "Preview cancelled."
    })
    shiny::observeEvent(input$inspect, {
      operation <- input$tool
      args <- switch(operation,
        ask = list(question = input$query, path = path),
        resources = list(query = if (nzchar(input$query)) input$query else NULL, path = path),
        packages = list(path = path),
        audit = list(path = path),
        update = list(dry_run = TRUE),
        setup = list(dry_run = TRUE),
        NULL
      )
      if (is.null(args)) {
        return()
      }
      begin(operation, args, "tool")
    })
    shiny::observe({
      shiny::invalidateLater(150, session)
      job <- state$job
      if (is.null(job) || job$process$is_alive()) {
        return()
      }
      state$job <- NULL
      result <- tryCatch(job$process$get_result(), error = function(e) list(ok = FALSE, message = "The worker stopped before returning a result. Inspect any interrupted operation before retrying."))
      if (!isTRUE(result$ok)) {
        state$status <- result$message
        return()
      }
      if (job$purpose == "preview") {
        current_args <- tryCatch(draft(), error = function(e) NULL)
        current_args$dry_run <- TRUE
        if (!identical(signature(current_args), signature(job$args))) {
          state$status <- "The draft changed while planning. Preview again."
          return()
        }
        if (!identical(job$catalog, content_hash(json_text(context())))) {
          state$status <- "The catalog changed while planning. Preview again."
          return()
        }
        state$preview <- result$value
        state$accepted <- list(signature = signature(job$args), catalog = job$catalog)
        state$status <- "Preview ready. Review the file plan and readiness limitations before applying."
      } else if (job$purpose == "apply") {
        state$result <- result$value
        state$preview <- NULL
        state$accepted <- NULL
        state$status <- "Operation completed. Review its readiness and any conflicts below."
      } else {
        state$tool_result <- result$value
        state$status <- "Read-only tool completed."
      }
    })
    session$onSessionEnded(function() {
      job <- shiny::isolate(state$job)
      if (!is.null(job) && !job$mutating && job$process$is_alive()) job$process$kill()
    })
    output$status <- shiny::renderText(state$status)
    output$plan <- shiny::renderTable({
      if (is.null(state$preview)) {
        return(NULL)
      }
      plan <- if (inherits(state$preview, "cttir_project")) state$preview$plan else state$preview$actions
      data.frame(File = plan$path, Change = plan$action)
    })
    output$result <- shiny::renderPrint(if (!is.null(state$result)) print(state$result) else if (!is.null(state$preview)) print(state$preview))
    output$tool_result <- shiny::renderPrint(if (!is.null(state$tool_result)) print(state$tool_result))
    list(draft = draft, state = state)
  })
}

#' Open the local project builder
#'
#' Returns a Shiny application. Printing it in an interactive R session launches
#' the local interface. Fast and Detailed share one draft and the package APIs.
#' Preview, catalog queries and audits use isolated background R workers. Creation
#' requires a reviewed preview. Scientific analysis is never executed by the UI.
#' Drafts can be exported to JSON and restored explicitly. They retain answers
#' but exclude the parent directory, local data bindings and accepted plans.
#' Restored drafts require a new preview; Configure drafts are bound to the
#' existing project identity. Exported text may contain user-supplied sensitive
#' information. Drafts are not automatically saved.
#' @param mode Initial `fast` or `detailed` view.
#' @param launch.browser Open the browser when the application is run.
#' @return A `shiny.appobj`. Shiny and callr must be installed. Run locally only;
#'   authentication and remote multi-user deployment are not supported.
#' @export
setup_app <- function(mode = c("fast", "detailed"), launch.browser = interactive()) {
  app_require()
  mode <- match.arg(mode)
  scalar_flag(launch.browser, "launch.browser")
  shiny::shinyApp(shiny::fluidPage(builder_ui("builder", mode, NULL)),
    function(input, output, session) builder_server("builder"),
    options = list(host = "127.0.0.1", launch.browser = launch.browser)
  )
}

#' Configure an existing project with an explicit preview
#' @param path Exact existing project root.
#' @param launch.browser Open the browser when the application is run.
#' @return A local Shiny application using [sync()] for preview and application.
#' @export
configure <- function(path = ".", launch.browser = interactive()) {
  app_require()
  scalar_flag(launch.browser, "launch.browser")
  p <- read_project(path)
  shiny::shinyApp(shiny::fluidPage(builder_ui("builder", "detailed", p$path)),
    function(input, output, session) builder_server("builder", p$path),
    options = list(host = "127.0.0.1", launch.browser = launch.browser)
  )
}
