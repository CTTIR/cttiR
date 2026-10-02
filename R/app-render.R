# Defensive renderers for domain results. All text goes through htmltools
# escaping; only http(s) links become anchors. Unknown fields are shown
# generically and never interpreted.

app_status_kinds <- list(
  ok = c("pass", "approved", "verified", "succeeded", "applied", "runtime_ready", "scaffold_ready",
    "static_api_verified", "configuration_recorded", "installed_versions_match", "adapter_tested",
    "tested", "created", "unchanged", "skip"),
  warn = c("warning", "approval_pending", "planned", "incomplete", "versions_unverified", "preserve",
    "update", "review", "pending", "documentation_indexed", "candidate", "not_rechecked"),
  fail = c("fail", "blocked", "error", "conflict", "dependencies_missing", "breaking"),
  unknown = c("not_tested", "unknown", "not_applicable", "candidate_gap", "no_dependencies_recorded",
    "not_implemented", "not_function_level_verified")
)

app_status_icons <- c(ok = "\u2713", warn = "!", fail = "\u2715", unknown = "?", neutral = "\u2022", busy = "\u2026")

app_status_kind <- function(value) {
  value <- as.character(value)[[1]]
  for (kind in names(app_status_kinds)) if (value %in% app_status_kinds[[kind]]) return(kind)
  "neutral"
}

app_badge <- function(value, lang = "en", kind = NULL, label = NULL) {
  if (is.null(value) || !length(value) || is.na(value[[1]])) value <- "unknown"
  kind <- kind %||% app_status_kind(value)
  shiny::tags$span(class = paste0("cttir-badge cttir-badge-", kind),
    shiny::tags$span(class = "cttir-badge-icon", `aria-hidden` = "true", app_status_icons[[kind]]),
    label %||% app_value_label(value, lang))
}

app_link <- function(url, text = sub("^https?://(www[.])?(github[.]com/)?", "", url)) {
  if (is.character(url) && length(url) == 1L && !is.na(url) && grepl("^https?://[^[:space:]<>\"']+$", url)) {
    return(shiny::tags$a(href = url, target = "_blank", rel = "noopener noreferrer", text))
  }
  if (is.null(url) || !length(url) || is.na(url[[1]])) "\u2014" else as.character(url)
}

app_cell <- function(value, column, lang, badges, links) {
  if (is.null(value) || !length(value) || (is.atomic(value) && is.na(value[[1]]))) return("\u2014")
  if (column %in% badges) return(app_badge(value, lang))
  if (column %in% links) return(app_link(value))
  if (is.logical(value)) return(app_t(if (isTRUE(value)) "common.yes" else "common.no", lang))
  if (is.list(value)) return(paste(vapply(value, function(x) paste(as.character(unlist(x)), collapse = " "), character(1)), collapse = "; "))
  as.character(value)
}

# Responsive table: on narrow screens each row becomes a labelled block and
# optional columns are hidden to keep the page short.
app_table <- function(df, lang = "en", columns = NULL, badges = character(), links = character(),
  caption = NULL, max_rows = 200L, label_prefix = "field.", optional = character(), stack = TRUE, label = NULL, drop_empty = FALSE) {
  if (!is.data.frame(df) || !nrow(df)) return(shiny::tags$p(class = "cttir-empty", app_t("common.no_rows", lang)))
  columns <- intersect(columns %||% names(df), names(df))
  if (drop_empty) columns <- Filter(function(column) !all(is.na(df[[column]])), columns)
  shown <- utils::head(df, max_rows)
  labels <- vapply(columns, app_value_label, character(1), lang = lang, prefix = label_prefix)
  # Columns holding long unbroken tokens (hashes, paths, URLs) may wrap anywhere;
  # other cells wrap at word boundaries so headers and words stay intact.
  long_token <- vapply(columns, function(column) {
    values <- unlist(lapply(shown[[column]], function(x) as.character(unlist(x))))
    if (column %in% links) values <- sub("^https?://(www[.])?(github[.]com/)?", "", values)
    tokens <- unlist(strsplit(values[!is.na(values)], "[[:space:]]+"))
    length(tokens) > 0L && max(nchar(tokens)) > 24L
  }, logical(1))
  cell_class <- function(j, cell = FALSE) {
    classes <- c(if (columns[[j]] %in% optional) "cttir-optional", if (cell && long_token[[j]]) "cttir-break")
    if (length(classes)) paste(classes, collapse = " ")
  }
  rows <- lapply(seq_len(nrow(shown)), function(i) {
    shiny::tags$tr(lapply(seq_along(columns), function(j) {
      value <- shown[[columns[[j]]]][[i]]
      shiny::tags$td(class = cell_class(j, TRUE), `data-label` = labels[[j]], app_cell(value, columns[[j]], lang, badges, links))
    }))
  })
  shiny::tags$div(class = if (stack) "cttir-table-wrap" else "cttir-table-wrap cttir-table-scroll",
    tabindex = if (!stack) "0", role = if (!stack) "region", `aria-label` = if (!stack) label %||% caption %||% app_t("common.table", lang),
    shiny::tags$table(class = "cttir-table",
      if (!is.null(caption)) shiny::tags$caption(caption),
      shiny::tags$thead(shiny::tags$tr(lapply(seq_along(labels), function(j) shiny::tags$th(scope = "col", class = cell_class(j), labels[[j]])))),
      shiny::tags$tbody(rows)
    ),
    if (nrow(df) > max_rows) shiny::tags$p(class = "cttir-note", app_t("common.rows_truncated", lang, shown = max_rows, total = nrow(df)))
  )
}

# Generic bounded renderer for result fields this interface does not know.
app_render_value <- function(x, lang = "en", depth = 0L) {
  if (depth > 4L) return("\u2026")
  if (is.null(x) || !length(x)) return(shiny::tags$span(class = "cttir-empty", "\u2014"))
  if (is.data.frame(x)) return(app_table(x, lang, max_rows = 50L))
  if (is.atomic(x)) {
    values <- as.character(x)
    if (length(values) == 1L) return(if (is.logical(x)) app_t(if (isTRUE(x)) "common.yes" else "common.no", lang) else app_link(values))
    return(shiny::tags$ul(lapply(utils::head(values, 50L), function(v) shiny::tags$li(app_link(v)))))
  }
  if (!is.list(x)) return(app_t("common.unsupported_value", lang))
  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    return(shiny::tags$ol(lapply(utils::head(x, 50L), function(v) shiny::tags$li(app_render_value(v, lang, depth + 1L)))))
  }
  shiny::tags$dl(class = "cttir-dl", lapply(names(x), function(key) {
    shiny::tagList(shiny::tags$dt(app_value_label(key, lang, "field.")), shiny::tags$dd(app_render_value(x[[key]], lang, depth + 1L)))
  }))
}

app_other_fields <- function(x, known, lang) {
  rest <- unclass(x)[setdiff(names(x), known)]
  if (!length(rest)) return(NULL)
  shiny::tags$details(class = "cttir-advanced", shiny::tags$summary(app_t("common.other_details", lang)),
    app_render_value(rest, lang))
}

app_list_block <- function(items, lang, prefix = NULL) {
  items <- unlist(items, use.names = FALSE)
  if (!length(items)) return(shiny::tags$p(class = "cttir-empty", app_t("common.none", lang)))
  shiny::tags$ul(lapply(items, function(item) {
    shiny::tags$li(if (is.null(prefix)) item else app_value_label(item, lang, prefix))
  }))
}

app_status_ui <- function(status, lang = "en", id = NULL) {
  if (is.null(status)) return(NULL)
  kind <- switch(status$kind, success = "ok", warning = "warn", error = "fail", busy = "busy", info = "neutral", "neutral")
  text <- do.call(app_t, c(list(status$key, lang), status$args))
  shiny::tags$div(class = paste0("cttir-status cttir-status-", status$kind), id = id, tabindex = "-1",
    role = if (identical(status$kind, "error")) "alert" else NULL,
    shiny::tags$span(class = "cttir-badge-icon", `aria-hidden` = "true", app_status_icons[[kind]]),
    shiny::tags$strong(class = "cttir-status-kind", paste0(app_t(paste0("status.kind_", status$kind), lang), ": ")),
    shiny::tags$span(text),
    if (!is.null(status$detail)) shiny::tags$p(class = "cttir-status-detail", status$detail),
    if (!is.null(status$remediation)) shiny::tags$p(class = "cttir-status-detail", status$remediation)
  )
}

app_status_text <- function(status, lang = "en") {
  paste(c(do.call(app_t, c(list(status$key, lang), status$args)), status$detail, status$remediation), collapse = " ")
}

app_progress_ui <- function(slot, lang) {
  job <- slot$state$job
  if (is.null(job)) return(NULL)
  waited <- round(as.numeric(difftime(Sys.time(), job$started, units = "secs")))
  shiny::tags$p(class = "cttir-progress", role = "status",
    shiny::tags$span(class = "cttir-spinner", `aria-hidden` = "true"),
    app_t("status.progress", lang, task = app_t(job$label, lang), seconds = waited),
    if (job$mutating) paste0(" ", app_t("status.keep_open", lang)))
}

app_section <- function(title_key, ..., level = 2L, class = NULL) {
  heading <- if (level == 2L) shiny::tags$h2 else shiny::tags$h3
  shiny::tags$section(class = paste("cttir-section", class), heading(app_tr(title_key)), ...)
}
