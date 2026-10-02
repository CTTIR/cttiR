# Knowledge: package coverage via packages(), catalog refresh via update() and
# retained-snapshot restore via rollback_knowledge(). Apply is only offered for
# the exact reviewed request and the catalog revision the preview was based on.

app_split_names <- function(text) {
  if (!is.character(text) || length(text) != 1L) return(NULL)
  items <- trimws(unlist(strsplit(text, "[,\n]")))
  items <- unique(items[nzchar(items)])
  if (length(items)) items else NULL
}

app_update_ui <- function(x, lang, applied = FALSE) {
  value <- unclass(x)
  catalogs <- value$catalogs
  catalog_rows <- if (is.list(catalogs) && length(catalogs)) {
    data.frame(catalog = names(catalogs),
      previous = vapply(catalogs, function(c) as.character(c$previous %||% NA), character(1)),
      current = vapply(catalogs, function(c) as.character(c$current %||% NA), character(1)),
      changed = vapply(catalogs, function(c) !identical(c$previous, c$current), logical(1)),
      stringsAsFactors = FALSE)
  }
  sources <- value$sources
  source_row <- function(s) {
    data.frame(package = as.character(s$package %||% NA), revision = as.character(s$revision %||% NA),
      status = as.character(s$status %||% "unknown"), stringsAsFactors = FALSE)
  }
  source_rows <- if (length(sources)) do.call(rbind, lapply(sources, source_row))
  api <- value$api_diff
  breaking <- if (is.data.frame(api) && "breaking" %in% names(api)) sum(api$breaking %in% TRUE) else 0L
  docs <- value$documentation_diff
  shiny::tags$section(class = "cttir-review-panel",
    shiny::tags$h3(app_t(if (applied) "knowledge.applied_title" else "knowledge.preview_title", lang)),
    shiny::tags$dl(class = "cttir-dl cttir-summary",
      shiny::tags$dt(app_t("field.status", lang)), shiny::tags$dd(app_badge(value$status, lang)),
      shiny::tags$dt(app_t("knowledge.previous_id", lang)), shiny::tags$dd(shiny::tags$code(value$previous_id %||% "\u2014")),
      shiny::tags$dt(app_t("knowledge.new_id", lang)), shiny::tags$dd(shiny::tags$code(value$new_id %||% "\u2014")),
      shiny::tags$dt(app_t("knowledge.activation", lang)),
      shiny::tags$dd(app_t(if (isTRUE(value$activation)) "common.yes" else "common.no", lang))),
    shiny::tags$p(class = "cttir-note", app_t("knowledge.no_install", lang)),
    if (!is.null(catalog_rows)) shiny::tagList(shiny::tags$h4(app_t("knowledge.catalogs", lang)), app_table(catalog_rows, lang)),
    if (!is.null(source_rows)) shiny::tagList(shiny::tags$h4(app_t("knowledge.sources", lang)), app_table(source_rows, lang, badges = "status")),
    shiny::tags$h4(app_t("knowledge.api_diff", lang, count = if (is.data.frame(api)) nrow(api) else 0L, breaking = breaking)),
    if (is.data.frame(api)) app_table(api, lang) else app_render_value(api, lang),
    shiny::tags$h4(app_t("knowledge.doc_diff", lang, count = if (is.data.frame(docs)) nrow(docs) else 0L)),
    if (is.data.frame(docs) && nrow(docs)) shiny::tags$details(shiny::tags$summary(app_t("common.show", lang)), app_table(docs, lang)),
    shiny::tags$h4(app_t("knowledge.warnings", lang)),
    app_list_block(value$warnings, lang),
    app_other_fields(x, c("status", "previous_id", "new_id", "catalogs", "sources", "api_diff", "documentation_diff", "activation", "warnings"), lang)
  )
}

app_knowledge_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1(app_tr("knowledge.title")),
    shiny::tags$p(class = "cttir-lead", app_tr("knowledge.intro")),
    shiny::tags$div(class = "cttir-live", `aria-live` = "polite", shiny::uiOutput(ns("progress")), shiny::uiOutput(ns("status"))),
    app_section("knowledge.coverage",
      shiny::checkboxInput(ns("use_project"), app_tr("common.use_project"), value = FALSE),
      shiny::actionButton(ns("load_packages"), app_tr("knowledge.load_packages"), class = "btn-primary"),
      shiny::uiOutput(ns("packages"))),
    app_section("knowledge.update",
      shiny::tags$p(class = "cttir-note", app_tr("knowledge.update_note")),
      shiny::radioButtons(ns("mode"), app_tr("knowledge.mode"), inline = TRUE,
        choiceNames = list(app_tr("knowledge.mode_local"), app_tr("knowledge.mode_remote")), choiceValues = c("local", "remote")),
      shiny::checkboxGroupInput(ns("catalogs"), app_tr("knowledge.catalogs"), inline = TRUE,
        choiceNames = list(app_tr("knowledge.catalog_knowledge"), app_tr("knowledge.catalog_resources")),
        choiceValues = c("knowledge", "resources"), selected = c("knowledge", "resources")),
      shiny::textInput(ns("sources"), app_tr("knowledge.sources_filter"), width = "100%"),
      shiny::textInput(ns("packages_filter"), app_tr("knowledge.packages_filter"), width = "100%"),
      shiny::tags$div(class = "cttir-actions",
        shiny::actionButton(ns("preview_update"), app_tr("knowledge.preview_update"), class = "btn-primary"),
        shiny::actionButton(ns("apply_update"), app_tr("knowledge.apply_update"), class = "btn-success"),
        shiny::actionButton(ns("cancel"), app_tr("common.cancel"))),
      shiny::uiOutput(ns("update_result"))),
    app_section("knowledge.rollback",
      shiny::tags$p(class = "cttir-note", app_tr("knowledge.rollback_note")),
      shiny::textInput(ns("rollback_id"), app_tr("knowledge.rollback_id"), width = "100%"),
      shiny::tags$div(class = "cttir-actions",
        shiny::actionButton(ns("preview_rollback"), app_tr("knowledge.preview_rollback")),
        shiny::actionButton(ns("apply_rollback"), app_tr("knowledge.apply_rollback"))),
      shiny::uiOutput(ns("rollback_result")))
  )
}

app_knowledge_server <- function(id, pool, lang = shiny::reactive("en"), project = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    slot <- app_job_slot(pool, session)
    state <- shiny::reactiveValues(status = app_status("info", "knowledge.ready"), packages = NULL,
      update_preview = NULL, update_args = NULL, update_result = NULL,
      rollback_preview = NULL, rollback_args = NULL, rollback_result = NULL)
    signature <- function(x) content_hash(json_text(x))
    current_manifest <- function() tryCatch(current_catalog_manifest()$manifest_id, error = function(e) NULL)
    run <- function(operation, args, label, done, mutating = FALSE) {
      state$status <- app_start_job(slot, operation, args, label, function(result) {
        if (!isTRUE(result$ok)) {
          state$status <- app_status_error(result)
          return()
        }
        done(result$value)
      }, mutating)
    }
    update_args <- function() {
      list(sources = app_split_names(input$sources), packages = app_split_names(input$packages_filter),
        mode = if (identical(input$mode, "remote")) "remote" else "local",
        catalogs = if (length(input$catalogs)) input$catalogs else character())
    }
    shiny::observeEvent(input$load_packages, {
      run("packages", list(path = if (isTRUE(input$use_project)) project() else NULL), "job.packages", function(value) {
        state$packages <- value
        state$status <- app_status("success", "knowledge.packages_loaded", count = if (is.data.frame(value)) nrow(value) else 0L)
      })
    })
    shiny::observeEvent(input$preview_update, {
      args <- update_args()
      if (!length(args$catalogs)) {
        state$status <- app_status("error", "knowledge.catalogs_required")
        session$sendCustomMessage("cttir-focus", list(id = session$ns("catalogs")))
        return()
      }
      state$update_preview <- NULL
      state$update_result <- NULL
      args$dry_run <- TRUE
      run("update", args, "job.update_preview", function(value) {
        state$update_preview <- value
        state$update_args <- signature(args)
        state$status <- app_status("success", "knowledge.preview_ready")
      })
    })
    shiny::observeEvent(input$apply_update, {
      args <- update_args()
      args$dry_run <- TRUE
      preview <- state$update_preview
      if (is.null(preview) || !identical(signature(args), state$update_args)) {
        state$status <- app_status("warning", "knowledge.preview_first")
        return()
      }
      if (!identical(current_manifest(), preview$previous_id)) {
        state$update_preview <- NULL
        state$status <- app_status("warning", "knowledge.base_changed")
        return()
      }
      args$dry_run <- FALSE
      run("update", args, "job.update_apply", function(value) {
        state$update_result <- value
        state$update_preview <- NULL
        state$update_args <- NULL
        if (!is.null(value$previous_id) && !identical(value$previous_id, preview$previous_id)) {
          state$status <- app_status("warning", "knowledge.base_changed_during_apply")
        } else {
          state$status <- app_status("success", "knowledge.applied")
        }
        if (is.null(input$rollback_id) || !nzchar(input$rollback_id)) {
          shiny::updateTextInput(session, "rollback_id", value = value$previous_id %||% "")
        }
      }, mutating = TRUE)
    })
    shiny::observeEvent(input$preview_rollback, {
      version <- trimws(input$rollback_id %||% "")
      if (!nzchar(version)) {
        state$status <- app_status("error", "knowledge.rollback_required")
        session$sendCustomMessage("cttir-focus", list(id = session$ns("rollback_id")))
        return()
      }
      args <- list(version = version, dry_run = TRUE)
      state$rollback_result <- NULL
      run("rollback_knowledge", args, "job.rollback_preview", function(value) {
        state$rollback_preview <- value
        state$rollback_args <- signature(args)
        state$status <- app_status("success", "knowledge.rollback_ready")
      })
    })
    shiny::observeEvent(input$apply_rollback, {
      args <- list(version = trimws(input$rollback_id %||% ""), dry_run = TRUE)
      preview <- state$rollback_preview
      if (is.null(preview) || !identical(signature(args), state$rollback_args)) {
        state$status <- app_status("warning", "knowledge.preview_first")
        return()
      }
      if (!identical(current_manifest(), preview$previous_id)) {
        state$rollback_preview <- NULL
        state$status <- app_status("warning", "knowledge.base_changed")
        return()
      }
      args$dry_run <- FALSE
      run("rollback_knowledge", args, "job.rollback_apply", function(value) {
        state$rollback_result <- value
        state$rollback_preview <- NULL
        state$rollback_args <- NULL
        state$status <- app_status("success", "knowledge.rollback_applied")
      }, mutating = TRUE)
    })
    shiny::observeEvent(input$cancel, state$status <- app_cancel_status(slot))
    output$status <- shiny::renderUI(app_status_ui(state$status, lang()))
    output$progress <- shiny::renderUI({
      if (!is.null(slot$state$job)) shiny::invalidateLater(1000, session)
      app_progress_ui(slot, lang())
    })
    output$packages <- shiny::renderUI({
      value <- state$packages
      if (is.null(value)) return(shiny::tags$p(class = "cttir-empty", app_t("knowledge.packages_lazy", lang())))
      columns <- c("package", "version", "provider", "installed_version", "pinned_version",
        "exports", "resolved", "documented", "approved", "documents_stored", "freshness", "repository")
      app_table(value, lang(), columns = columns, badges = "freshness", links = "repository",
        optional = c("installed_version", "pinned_version", "resolved", "documented", "documents_stored", "repository"),
        stack = FALSE, label = app_t("knowledge.coverage", lang()), drop_empty = TRUE)
    })
    output$update_result <- shiny::renderUI({
      if (!is.null(state$update_result)) return(app_update_ui(state$update_result, lang(), applied = TRUE))
      if (!is.null(state$update_preview)) return(app_update_ui(state$update_preview, lang()))
      NULL
    })
    output$rollback_result <- shiny::renderUI({
      if (!is.null(state$rollback_result)) return(app_update_ui(state$rollback_result, lang(), applied = TRUE))
      if (!is.null(state$rollback_preview)) return(app_update_ui(state$rollback_preview, lang()))
      NULL
    })
    list(state = state, slot = slot)
  })
}
