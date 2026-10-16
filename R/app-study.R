# A closed example workflow, independent of general project()/sync() planning.
app_study_operations <- c("study_preview", "study_compile", "study_run", "study_render")

app_study_hash <- function(value) content_hash(json_text(value))

app_study_request <- function(request) {
  study_keys(request, c("recipe", "directory", "author", "synthetic_acknowledged", "independent_pairs"), "Example request")
  if (!is.character(request$recipe) || length(request$recipe) != 1L ||
      is.na(request$recipe) || !request$recipe %in% c("association", "paired")) study_fail("Choose a supported synthetic example.")
  study_text(request$directory, "new project directory", 4096L)
  study_text(request$author, "author", 160L)
  scalar_flag(request$synthetic_acknowledged, "synthetic_acknowledged")
  scalar_flag(request$independent_pairs, "independent_pairs")
  if (!isTRUE(request$synthetic_acknowledged)) study_fail("Acknowledge that this workflow uses only the fixed synthetic example.")
  if (request$recipe == "paired" && !isTRUE(request$independent_pairs))
    study_fail("Explicitly acknowledge the independent synthetic pairs before proceeding.")
  assert_plain_path(request$directory)
  request$directory <- as.character(fs::path_abs(request$directory))
  request
}

app_study_fixture <- function(recipe) {
  if (!identical(recipe, "association") && !identical(recipe, "paired")) study_fail("Unsupported synthetic example.")
  kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  prior_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (had_seed) assign(".Random.seed", prior_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  seed <- if (recipe == "association") 20261016L else 20261017L
  set.seed(seed)
  if (recipe == "association") {
    exposure <- rep(seq_len(10), 4)
    age <- stats::runif(40, min = 25, max = 65)
    data <- data.frame(observation_id = sprintf("example_%03d", seq_len(40)),
      pressure = 110 + 2 * exposure + 0.15 * age + stats::rnorm(40, sd = 5), exposure = exposure, age = age)
    units <- c(pressure = "mmHg", exposure = "mg", age = "years")
    spec <- study_spec("synthetic_association", "Synthetic association example",
      "How is pressure associated with exposure after adjusting for age?",
      "Forty independently generated synthetic observations", "observation_id",
      list(name = "pressure", type = "continuous", unit = "mmHg"),
      list(name = "exposure", type = "continuous", unit = "mg"),
      list(list(name = "age", type = "continuous", unit = "years")),
      research = list(domain = "clinical", data_origin = "primary", design = "observational"))
  } else {
    pair_id <- sprintf("example_%03d", seq_len(24))
    baseline <- stats::rnorm(24, mean = 120, sd = 12)
    difference <- stats::rnorm(24, mean = -3, sd = 5)
    values <- rbind(data.frame(pair_id = pair_id, visit = "baseline", pressure = baseline),
      data.frame(pair_id = pair_id, visit = "followup", pressure = baseline + difference))
    data <- values[sample(seq_len(nrow(values))), , drop = FALSE]
    units <- c(pressure = "mmHg")
    spec <- study_paired_spec("synthetic_paired_change", "Synthetic paired pressure example",
      "What is the mean followup-minus-baseline pressure change?",
      "Twenty-four independently generated synthetic pairs", "pair_id", "visit",
      list(name = "pressure", type = "continuous", unit = "mmHg"),
      "baseline", "followup", independent_pairs = TRUE,
      research = list(domain = "clinical", data_origin = "primary", design = "observational"))
  }
  study_validate(spec, data, units)
  list(spec = spec, data = data, units = units, seed = seed)
}

app_study_preview <- function(request) {
  request <- app_study_request(request)
  if (file.exists(request$directory) || dir.exists(request$directory) || !dir.exists(dirname(request$directory)))
    study_fail("Use a new project directory below an existing physical parent.")
  app_study_preview_value(request, app_study_fixture(request$recipe))
}

app_study_preview_value <- function(request, fixture) {
  lock <- study_dependency_lock()
  runtime <- study_installed_runtime()
  value <- list(schema_version = 1L, request = request, request_sha256 = app_study_hash(request),
    recipe_id = fixture$spec$recipe_id, spec = fixture$spec, units = fixture$units, seed = fixture$seed,
    rows = nrow(fixture$data), pairs = if (request$recipe == "paired") 24L else NULL,
    data_sha256 = app_study_hash(fixture$data), dependency_lock = lock, runtime = runtime)
  value$fingerprint <- app_study_hash(value)
  value
}

app_study_compile <- function(request, expected_fingerprint) {
  scalar_text(expected_fingerprint, "expected_fingerprint")
  preview <- app_study_preview(request)
  if (!identical(preview$fingerprint, expected_fingerprint)) study_fail("The example preview is stale; preview again before compiling.")
  fixture <- app_study_fixture(preview$request$recipe)
  value <- study_project(preview$request$directory, fixture$spec, fixture$data,
    preview$dependency_lock, preview$request$author, fixture$units, fixture$seed)
  list(schema_version = 1L, phase = "compiled", request = preview$request,
    request_sha256 = preview$request_sha256, preview_fingerprint = preview$fingerprint,
    recipe_id = preview$recipe_id, directory = value$directory, declaration = value$declaration,
    result = NULL, validation = NULL)
}

app_study_fixture_binding <- function(fixture) {
  layout <- study_data_layout(fixture$spec)
  projected <- fixture$data[, layout$columns, drop = FALSE]
  projected[[layout$id]] <- as.character(projected[[layout$id]])
  scratch <- tempfile("app-study-fixture-", fileext = ".csv")
  on.exit(unlink(scratch), add = TRUE)
  old <- options(digits = 17L); on.exit(options(old), add = TRUE)
  utils::write.csv(projected, scratch, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  list(sha256 = file_hash(scratch), rows = nrow(fixture$data), columns = as.list(layout$columns),
    classes = as.list(layout$classes), units = as.list(fixture$units), seed = fixture$seed)
}

app_study_record <- function(record, phase) {
  study_keys(record, c("schema_version", "phase", "request", "request_sha256", "preview_fingerprint",
    "recipe_id", "directory", "declaration", "result", "validation"), "Server-held example record")
  if (!identical(record$schema_version, 1L) || !identical(record$phase, phase)) study_fail("The example operation is not available in this phase.")
  request <- app_study_request(record$request)
  fixture <- app_study_fixture(request$recipe)
  if (!identical(record$request, request) || !identical(record$request_sha256, app_study_hash(request)) ||
      !identical(record$directory, request$directory) || !identical(record$recipe_id, fixture$spec$recipe_id) ||
      !identical(json_text(record$declaration$structure$composition$study), json_text(fixture$spec)))
    study_fail("The example record does not match its fixed request and recipe.")
  scalar_text(record$preview_fingerprint, "preview_fingerprint")
  if (!identical(record$preview_fingerprint, app_study_preview_value(request, fixture)$fingerprint))
    study_fail("The example record does not match its original fixed preview and runtime.")
  declared <- record$declaration$structure$composition$recipe$input
  binding <- list(sha256 = declared$sha256, rows = declared$rows, columns = declared$columns,
    classes = declared$classes, units = declared$units, seed = record$declaration$structure$profile$seed)
  if (!identical(json_text(binding), json_text(app_study_fixture_binding(fixture))))
    study_fail("The example record does not bind the fixed fixture data, units and seed.")
  checked <- study_validate_project(record$directory, record$declaration,
    require_run = phase %in% c("analyzed", "rendered"), require_render = phase == "rendered")
  if (!isTRUE(checked$valid)) study_fail(paste(checked$reason, collapse = "; "))
  invisible(record)
}

app_study_run <- function(record) {
  app_study_record(record, "compiled")
  value <- study_run(record$directory, record$declaration)
  record$phase <- "analyzed"
  record$declaration <- value$declaration
  record$result <- value$result
  app_study_record(record, "analyzed")
  record
}

app_study_render <- function(record) {
  app_study_record(record, "analyzed")
  value <- study_render(record$directory, record$declaration, timeout = 600)
  if (!isTRUE(value$validation$valid) || isTRUE(value$validation$scientific_approval))
    study_fail("The synthetic render was not verified.")
  record$phase <- "rendered"
  record$declaration <- value$declaration
  record$validation <- value$validation
  app_study_record(record, "rendered")
  record
}

app_study_dispatch <- function(operation, args) {
  target <- switch(operation, study_preview = app_study_preview, study_compile = app_study_compile,
    study_run = app_study_run, study_render = app_study_render, NULL)
  if (is.null(target)) study_fail("Unknown example operation.")
  do.call(target, args)
}

app_study_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1(app_tr("study.title")),
    shiny::tags$p(class = "cttir-lead", app_tr("study.intro")),
    shiny::tags$p(class = "cttir-note", app_tr("study.scope")),
    shiny::radioButtons(ns("recipe"), app_tr("study.recipe"),
      choiceNames = list(app_tr("study.association"), app_tr("study.paired")),
      choiceValues = c("association", "paired"), selected = "association"),
    shiny::textInput(ns("directory"), app_tr("study.directory"), value = "", width = "100%"),
    shiny::textInput(ns("author"), app_tr("study.author"), value = "", width = "100%"),
    shiny::checkboxInput(ns("synthetic_acknowledged"), app_tr("study.ack"), value = FALSE),
    shiny::conditionalPanel(sprintf("input['%s'] === 'paired'", ns("recipe")),
      shiny::checkboxInput(ns("independent_pairs"), app_tr("study.pairs_ack"), value = FALSE)),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("preview"), app_tr("study.preview"), class = "btn-primary"),
      shiny::actionButton(ns("cancel"), app_tr("common.cancel_preview"))),
    shiny::tags$div(class = "cttir-live", `aria-live` = "polite", shiny::uiOutput(ns("status"))),
    shiny::uiOutput(ns("review")),
    shiny::tags$div(class = "cttir-actions",
      shiny::actionButton(ns("compile"), app_tr("study.compile"), disabled = TRUE),
      shiny::actionButton(ns("run"), app_tr("study.run"), disabled = TRUE),
      shiny::actionButton(ns("render"), app_tr("study.render"), disabled = TRUE)),
    shiny::tags$p(class = "cttir-note", app_tr("study.external")),
    shiny::downloadButton(ns("declaration"), app_tr("study.download"))
  )
}

app_study_preview_details <- function(value, lang) {
  spec <- value$spec
  independence <- if (study_is_paired(spec)) {
    if (isTRUE(spec$independent_pairs)) app_t("study.independence.paired", lang)
    else app_t("study.independence.unconfirmed", lang)
  } else if (identical(spec$dependence, "independent")) app_t("study.independence.observations", lang)
  else app_t("study.independence.unconfirmed", lang)
  shiny::tags$dl(class = "cttir-dl",
    shiny::tags$dt(app_t("study.units", lang)),
    shiny::tags$dd(shiny::tags$ul(lapply(names(value$units), function(column)
      shiny::tags$li(shiny::tags$code(column), ": ", value$units[[column]])))),
    shiny::tags$dt(app_t("study.domain", lang)), shiny::tags$dd(app_value_label(spec$research$domain, lang, "study.domain.")),
    shiny::tags$dt(app_t("study.origin", lang)), shiny::tags$dd(app_value_label(spec$research$data_origin, lang, "study.origin.")),
    shiny::tags$dt(app_t("study.design", lang)), shiny::tags$dd(app_value_label(spec$research$design, lang, "study.design.")),
    shiny::tags$dt(app_t("study.confidence", lang)), shiny::tags$dd(paste0(format(100 * spec$confidence, trim = TRUE), "%")),
    shiny::tags$dt(app_t("study.independence", lang)), shiny::tags$dd(independence))
}

app_study_result_table <- function(coefficients, lang) {
  shown <- coefficients
  for (column in names(shown)) if (is.numeric(shown[[column]]))
    shown[[column]] <- trimws(formatC(shown[[column]], digits = 5L, format = "g",
      decimal.mark = if (identical(lang, "de")) "," else "."))
  if ("term" %in% names(shown)) shown$term <- vapply(shown$term, app_value_label,
    character(1), lang = lang, prefix = "study.term.")
  app_table(shown, lang, label_prefix = "study.column.")
}

app_study_review <- function(value, lang) {
  if (is.null(value)) return(NULL)
  compiled <- !is.null(value$phase)
  phase <- value$phase %||% "previewed"
  shiny::tags$section(class = "cttir-review-panel",
    shiny::tags$h2(app_t(paste0("study.phase.", phase), lang)),
    shiny::tags$p(app_t("study.pending_review", lang)),
    shiny::tags$dl(class = "cttir-dl",
      shiny::tags$dt(app_t("study.recipe", lang)), shiny::tags$dd(shiny::tags$code(value$recipe_id)),
      shiny::tags$dt(app_t("study.directory", lang)), shiny::tags$dd(shiny::tags$code(value$request$directory))),
    if (!compiled) shiny::tagList(
      shiny::tags$p(app_t("study.rows", lang, rows = value$rows, seed = value$seed)),
      shiny::tags$p(value$spec$question), shiny::tags$p(value$spec$population),
      app_study_preview_details(value, lang),
      shiny::tags$p(app_t("study.preview_only", lang))),
    if (compiled) shiny::tags$p(app_t("study.own_git", lang)),
    if (!is.null(value$result)) app_study_result_table(value$result$coefficients, lang),
    if (identical(phase, "rendered")) shiny::tagList(
      shiny::tags$p(app_t("study.rendered", lang)),
      shiny::tags$code(file.path(value$directory, "docs/index.html"))))
}

app_study_server <- function(id, pool, lang = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {
    slot <- app_job_slot(pool, session)
    state <- shiny::reactiveValues(preview = NULL, accepted = NULL, record = NULL,
      phase = "draft", status = app_status("info", "study.ready"), signature = NULL,
      exported_sha256 = NULL, failed_write = FALSE)
    request <- shiny::reactive(app_study_request(list(recipe = input$recipe, directory = input$directory,
      author = input$author, synthetic_acknowledged = input$synthetic_acknowledged,
      independent_pairs = input$independent_pairs)))
    request_hash <- function() tryCatch(app_study_hash(request()), error = function(e) NULL)
    guard <- function(expr) tryCatch(expr,
      cttir_app_status = function(e) state$status <- e$status,
      error = function(e) state$status <- app_condition_status(e))
    require_idle <- function() {
      if (slot$busy()) app_abort_status("status.duplicate")
      if (isTRUE(state$failed_write)) study_fail("A write step failed. Its artifacts and last successful declaration are retained; use a new app session and destination for another attempt.")
    }
    require_selection <- function(record) {
      if (is.null(record) || !identical(request_hash(), record$request_sha256))
        study_fail("Restore the compiled example selections before continuing; no declaration is recovered from project files.")
    }
    shiny::observe({
      sig <- app_study_hash(list(input$recipe, input$directory, input$author,
        input$synthetic_acknowledged, input$independent_pairs))
      if (!identical(sig, shiny::isolate(state$signature))) {
        state$signature <- sig
        state$preview <- NULL
        state$accepted <- NULL
        if (is.null(shiny::isolate(state$record))) state$phase <- "draft"
        else state$status <- app_status("warning", "study.selection_changed")
      }
    })
    completed <- function(result, expected, operation) {
      if (!isTRUE(result$ok)) {
        if (operation != "study_preview") state$failed_write <- TRUE
        state$status <- app_status_error(result)
        return()
      }
      value <- result$value
      if (!is.list(value) || !identical(value$request_sha256, expected)) {
        if (operation != "study_preview") state$failed_write <- TRUE
        state$status <- app_status("error", "error.unexpected")
        return()
      }
      current <- identical(request_hash(), expected)
      if (operation == "study_preview") {
        if (!current) {
          state$status <- app_status("warning", "status.changed_while_planning")
          return()
        }
        state$preview <- value
        state$accepted <- value$fingerprint
        state$phase <- "previewed"
      } else {
        state$record <- value
        state$phase <- value$phase
        state$preview <- NULL
        state$accepted <- NULL
      }
      state$status <- app_status(if (current) "success" else "warning",
        if (current) paste0("study.phase.", state$phase) else "study.selection_changed")
    }
    shiny::observeEvent(input$preview, guard({
      require_idle()
      if (!is.null(state$record)) study_fail("This session already owns an example project. Continue it or export its declaration; use a new app session for another project.")
      args <- list(request = request()); expected <- app_study_hash(args$request)
      state$preview <- NULL; state$accepted <- NULL
      state$status <- app_start_job(slot, "study_preview", args, "study.job.preview",
        function(result) completed(result, expected, "study_preview"))
    }))
    shiny::observeEvent(input$compile, guard({
      require_idle()
      if (!is.null(state$record)) study_fail("An example project already exists in this session; it will not be replaced.")
      if (is.null(state$accepted) || is.null(state$preview) ||
          !identical(request_hash(), state$preview$request_sha256)) study_fail("Preview the current example before compiling.")
      args <- list(request = request(), expected_fingerprint = state$accepted)
      expected <- app_study_hash(args$request)
      state$status <- app_start_job(slot, "study_compile", args, "study.job.compile",
        function(result) completed(result, expected, "study_compile"), mutating = TRUE)
    }))
    for (operation in c("run", "render")) local({
      op <- operation
      shiny::observeEvent(input[[op]], guard({
        require_idle(); require_selection(state$record)
        required <- if (op == "run") "compiled" else "analyzed"
        if (!identical(state$record$phase, required)) study_fail("Complete the preceding explicit example step first; completed steps are not repeated.")
        expected <- state$record$request_sha256
        state$status <- app_start_job(slot, paste0("study_", op), list(record = state$record),
          paste0("study.job.", op), function(result) completed(result, expected, paste0("study_", op)), mutating = TRUE)
      }))
    })
    shiny::observeEvent(input$cancel, state$status <- app_cancel_status(slot))
    shiny::observe({
      busy <- !is.null(slot$state$job) || isTRUE(state$failed_write)
      current <- request_hash()
      matching <- !is.null(state$record) && identical(current, state$record$request_sha256)
      ready <- list(preview = !busy && is.null(state$record) && !is.null(current),
        compile = !busy && is.null(state$record) && !is.null(state$accepted) &&
          !is.null(state$preview) && identical(current, state$preview$request_sha256),
        run = !busy && matching && identical(state$record$phase, "compiled"),
        render = !busy && matching && identical(state$record$phase, "analyzed"))
      for (button in names(ready)) session$sendCustomMessage("cttir-busy",
        list(ids = list(session$ns(button)), busy = !isTRUE(ready[[button]])))
    })
    output$status <- shiny::renderUI(app_status_ui(state$status, lang()))
    output$review <- shiny::renderUI(app_study_review(state$record %||% state$preview, lang()))
    export_declaration <- function(file) {
      record <- shiny::isolate(state$record)
      if (is.null(record)) study_fail("Compile the example before exporting its external declaration.")
      saveRDS(record$declaration, file, version = 3L)
      state$exported_sha256 <- app_study_hash(record$declaration)
      invisible(file)
    }
    output$declaration <- shiny::downloadHandler(
      filename = function() {
        record <- state$record
        if (is.null(record)) "cttir-study-declaration.rds" else
          paste0("cttir-", record$request$recipe, "-", record$phase, "-declaration.rds")
      }, contentType = "application/octet-stream", content = export_declaration)
    dirty <- shiny::reactive(!is.null(state$record) &&
      !identical(state$exported_sha256, app_study_hash(state$record$declaration)))
    list(state = state, request = request, slot = slot, export_declaration = export_declaration, dirty = dirty)
  })
}
