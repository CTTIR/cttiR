# Audit view over audit(): scopes, status filters, next actions, report export
# to a chosen directory and the documented repair allowlist. The repair preview
# shows the read-only repair plan of the last audit run: triggering checks and
# the target files a repair would create or replace. A repair is bound to that
# plan; when the project changed since the preview it is refused and the new
# plan is shown for review instead.

app_audit_scopes <- c("installation", "knowledge", "project", "integration")
app_audit_statuses <- c("fail", "warning", "not_tested", "not_applicable", "pass")
app_audit_known_fields <- c("schema_version", "timestamp", "scopes", "live", "checks", "overall_status", "repairs",
  "repair_plan", "limitations", "reports", "next_actions", "actions")
app_audit_columns <- c("id", "scope", "status", "severity", "required", "message", "evidence")

# Check IDs whose failure the documented repair allowlist addresses.
app_repair_candidates <- function(checks) {
  if (!is.data.frame(checks) || !nrow(checks) || !all(c("id", "status") %in% names(checks))) return(checks[0, , drop = FALSE])
  repairable <- if ("repair_id" %in% names(checks)) {
    !is.na(checks$repair_id) & nzchar(checks$repair_id)
  } else {
    grepl("^PRJ-002", checks$id) | checks$id %in% c("PRJ-007", "PRJ-008")
  }
  checks[repairable & checks$status == "fail", , drop = FALSE]
}

# One row per target file of each planned repair (or per skipped repair).
app_repair_plan_rows <- function(plan) {
  rows <- lapply(plan, function(entry) {
    targets <- entry$targets
    missing <- vapply(targets, function(hash) is.null(hash) || is.na(hash[[1]]), logical(1))
    data.frame(id = paste(unlist(entry$trigger), collapse = ", "), change = entry$id %||% NA_character_,
      path = if (length(targets)) names(targets) else NA_character_,
      action = if (length(targets)) ifelse(missing, "create", "update") else NA_character_,
      status = entry$status %||% NA_character_, message = entry$reason %||% entry$description %||% NA_character_,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

app_audit_next_actions <- function(result, lang) {
  value <- unclass(result)
  explicit <- value$next_actions %||% value$actions
  if (length(explicit)) return(app_render_value(explicit, lang))
  checks <- value$checks
  if (!is.data.frame(checks) || !nrow(checks)) return(app_list_block(NULL, lang))
  keys <- character()
  if (nrow(app_repair_candidates(checks))) keys <- c(keys, "audit.next_repair")
  if (sum(checks$status == "fail") > nrow(app_repair_candidates(checks))) keys <- c(keys, "audit.next_fail")
  if (any(checks$status == "warning")) keys <- c(keys, "audit.next_warning")
  if (any(checks$status == "not_tested")) keys <- c(keys, "audit.next_not_tested")
  if (!length(keys)) keys <- "audit.next_none"
  shiny::tags$ul(lapply(keys, function(key) shiny::tags$li(app_t(key, lang))))
}

app_audit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1(app_tr("audit.title")),
    shiny::tags$p(class = "cttir-lead", app_tr("audit.intro")),
    shiny::checkboxGroupInput(ns("scope"), app_tr("audit.scope"), inline = TRUE,
      choiceNames = lapply(app_audit_scopes, function(x) app_tr(paste0("value.", x))), choiceValues = app_audit_scopes,
      selected = c("installation", "knowledge", "project")),
    shiny::textInput(ns("path"), app_tr("audit.path"), width = "100%"),
    shiny::helpText(app_tr("audit.path_help")),
    shiny::checkboxInput(ns("live"), app_tr("audit.live"), value = FALSE),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("run"), app_tr("audit.run"), class = "btn-primary"),
      shiny::actionButton(ns("cancel"), app_tr("common.cancel"))),
    shiny::tags$div(class = "cttir-live", `aria-live` = "polite", shiny::uiOutput(ns("progress")), shiny::uiOutput(ns("status"))),
    shiny::uiOutput(ns("summary")),
    shiny::checkboxGroupInput(ns("show"), app_tr("audit.filter"), inline = TRUE,
      choiceNames = lapply(app_audit_statuses, function(x) app_tr(paste0("value.", x))), choiceValues = app_audit_statuses,
      selected = c("fail", "warning", "not_tested")),
    shiny::checkboxInput(ns("required_only"), app_tr("audit.required_only"), value = FALSE),
    shiny::uiOutput(ns("checks")),
    app_section("audit.export",
      shiny::textInput(ns("output"), app_tr("audit.output"), width = "100%"),
      shiny::helpText(app_tr("audit.output_help")),
      shiny::actionButton(ns("export"), app_tr("audit.export_button")),
      shiny::uiOutput(ns("reports"))),
    app_section("audit.repair",
      shiny::tags$p(class = "cttir-note", app_tr("audit.repair_note")),
      shiny::uiOutput(ns("repair_preview")),
      shiny::actionButton(ns("repair"), app_tr("audit.repair_button"), class = "btn-warning"),
      shiny::uiOutput(ns("repairs")))
  )
}

app_audit_server <- function(id, pool, lang = shiny::reactive("en"), project = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    slot <- app_job_slot(pool, session)
    state <- shiny::reactiveValues(status = app_status("info", "audit.ready"), result = NULL, args = NULL,
      preview = NULL, plan = NULL, reports = NULL, repaired = NULL)
    signature <- function(x) content_hash(json_text(x))
    plan_of <- function(value) audit_plan_fingerprint(unclass(value)$repair_plan)
    previewed <- function(value) {
      state$preview <- value
      state$plan <- plan_of(value)
    }
    shiny::observeEvent(project(), {
      if (!nzchar(input$path %||% "")) shiny::updateTextInput(session, "path", value = project())
    }, ignoreNULL = TRUE)
    audit_args <- function() {
      scope <- intersect(input$scope, app_audit_scopes)
      if (!length(scope)) {
        state$status <- app_status("error", "audit.scope_required")
        session$sendCustomMessage("cttir-focus", list(id = session$ns("scope")))
        return(NULL)
      }
      path <- trimws(input$path %||% "")
      list(path = if (nzchar(path)) path else NULL, scope = scope,
        live = isTRUE(input$live) && "integration" %in% scope)
    }
    run <- function(args, label, done, mutating = FALSE) {
      state$status <- app_start_job(slot, "audit", args, label, function(result) {
        if (!isTRUE(result$ok)) {
          state$status <- app_status_error(result)
          return()
        }
        done(result$value)
      }, mutating)
    }
    shiny::observeEvent(input$run, {
      args <- audit_args()
      if (is.null(args)) return()
      state$reports <- NULL
      state$repaired <- NULL
      run(args, "job.audit", function(value) {
        state$result <- value
        state$args <- signature(args)
        previewed(value)
        kind <- if (identical(value$overall_status, "pass")) "success" else "warning"
        state$status <- app_status(kind, "audit.done", status = app_value_label(value$overall_status, lang()))
      })
    })
    shiny::observeEvent(input$export, {
      args <- audit_args()
      if (is.null(args)) return()
      output_dir <- trimws(input$output %||% "")
      if (!nzchar(output_dir)) {
        state$status <- app_status("error", "audit.output_required")
        session$sendCustomMessage("cttir-focus", list(id = session$ns("output")))
        return()
      }
      args$output <- output_dir
      run(args, "job.audit_export", function(value) {
        state$result <- value
        state$reports <- value$reports
        state$status <- app_status("success", "audit.exported", count = length(value$reports))
      }, mutating = TRUE)
    })
    shiny::observeEvent(input$repair, {
      args <- audit_args()
      if (is.null(args)) return()
      if (is.null(args$path)) {
        state$status <- app_status("error", "audit.repair_path_required")
        session$sendCustomMessage("cttir-focus", list(id = session$ns("path")))
        return()
      }
      if (is.null(state$preview) || !identical(signature(args), state$args)) {
        state$status <- app_status("warning", "audit.repair_preview_first")
        return()
      }
      expected <- state$plan
      # Recompute the plan read-only; repair only what the preview showed.
      run(args, "job.audit", function(value) {
        if (!identical(plan_of(value), expected)) {
          state$result <- value
          previewed(value)
          state$status <- app_status("warning", "audit.plan_changed")
          return()
        }
        args$repair <- TRUE
        # A worker that runs audit_impl() also refuses a plan changed since now.
        attr(args, "cttir_audit_plan") <- expected
        run(args, "job.audit_repair", function(value) {
          state$result <- value
          state$repaired <- value$repairs %||% list()
          state$args <- NULL
          state$preview <- NULL
          state$plan <- NULL
          state$status <- app_status("success", "audit.repaired", count = length(value$repairs))
        }, mutating = TRUE)
      })
    })
    shiny::observeEvent(input$cancel, state$status <- app_cancel_status(slot))
    output$status <- shiny::renderUI(app_status_ui(state$status, lang()))
    output$progress <- shiny::renderUI({
      if (!is.null(slot$state$job)) shiny::invalidateLater(1000, session)
      app_progress_ui(slot, lang())
    })
    output$summary <- shiny::renderUI({
      value <- state$result
      if (is.null(value)) return(NULL)
      language <- lang()
      checks <- value$checks
      counts <- if (is.data.frame(checks)) table(checks$status) else integer()
      shiny::tags$section(class = "cttir-review-panel",
        shiny::tags$h2(app_t("audit.result_title", language)),
        shiny::tags$p(app_t("audit.overall", language), " ", app_badge(value$overall_status, language)),
        shiny::tags$p(paste(paste0(vapply(names(counts), app_value_label, character(1), lang = language), ": ", as.integer(counts)), collapse = "; ")),
        shiny::tags$h3(app_t("audit.next_actions", language)),
        app_audit_next_actions(value, language),
        shiny::tags$h3(app_t("audit.limitations", language)),
        app_list_block(value$limitations, language),
        app_other_fields(value, app_audit_known_fields, language))
    })
    output$checks <- shiny::renderUI({
      checks <- state$result$checks
      if (!is.data.frame(checks)) return(NULL)
      keep <- checks$status %in% (input$show %||% character()) | !checks$status %in% app_audit_statuses
      shown <- checks[keep, , drop = FALSE]
      if (isTRUE(input$required_only) && "required" %in% names(shown)) shown <- shown[shown$required %in% TRUE, , drop = FALSE]
      columns <- c(app_audit_columns, setdiff(names(shown), app_audit_columns))
      shiny::tagList(
        shiny::tags$p(class = "cttir-note", app_t("audit.shown", lang(), shown = nrow(shown), total = nrow(checks))),
        app_table(shown, lang(), columns = columns, badges = c("status", "severity"), optional = c("scope", "required")))
    })
    output$reports <- shiny::renderUI({
      reports <- state$reports
      if (!length(reports)) return(NULL)
      shiny::tagList(shiny::tags$p(app_t("audit.reports_written", lang())),
        shiny::tags$ul(lapply(reports, function(x) shiny::tags$li(shiny::tags$code(x)))))
    })
    output$repair_preview <- shiny::renderUI({
      value <- state$preview
      language <- lang()
      if (is.null(value)) return(shiny::tags$p(class = "cttir-empty", app_t("audit.repair_needs_audit", language)))
      candidates <- app_repair_candidates(value$checks)
      plan <- unclass(value)$repair_plan
      if (!nrow(candidates) && !length(plan)) return(shiny::tags$p(app_t("audit.repair_none", language)))
      shiny::tagList(shiny::tags$p(app_t("audit.repair_candidates", language, count = nrow(candidates))),
        if (length(plan)) {
          app_table(app_repair_plan_rows(plan), language, columns = c("id", "change", "path", "action", "status", "message"),
            badges = c("action", "status"))
        } else {
          app_table(candidates, language, columns = intersect(c("id", "status", "message", "evidence"), names(candidates)),
            badges = "status")
        })
    })
    output$repairs <- shiny::renderUI({
      repaired <- state$repaired
      if (is.null(repaired)) return(NULL)
      if (!length(repaired)) return(shiny::tags$p(app_t("audit.repair_nothing_done", lang())))
      shiny::tagList(shiny::tags$h3(app_t("audit.repairs_done", lang())), app_render_value(repaired, lang()))
    })
    list(state = state, slot = slot)
  })
}
