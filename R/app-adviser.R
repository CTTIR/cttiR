# Ask-view rendering of an advise() result. The plan is shown as a reviewed
# draft next to the deterministic catalog answer; nothing here executes code.
app_adviser_ui <- function(value, lang, ns) {
  proposal <- value$proposal
  plan <- proposal$plan
  field <- function(key, text) if (length(text) && nzchar(text[[1]])) shiny::tags$p(
    shiny::tags$strong(app_t(key, lang)), ": ", text)
  section <- function(key, items) if (length(unlist(items))) shiny::tagList(
    shiny::tags$h3(app_t(key, lang)), app_list_block(items, lang))
  reason_key <- paste0("adviser.reason.", value$reason %||% "unavailable")
  if (!app_has_message(reason_key)) reason_key <- "adviser.reason.unavailable"
  cited <- unlist(proposal$source_ids, use.names = FALSE)
  sources <- Filter(function(e) e$source_id %in% cited, value$evidence)
  shiny::tagList(
    shiny::tags$section(class = "cttir-review-panel cttir-adviser", `aria-labelledby` = ns("adviser_heading"),
      shiny::tags$h2(id = ns("adviser_heading"), app_t("adviser.heading", lang)),
      shiny::tags$p(class = "cttir-status cttir-status-warning", app_t("adviser.experimental_note", lang)),
      if (is.null(proposal)) shiny::tags$p(app_t("adviser.fallback", lang), " ", app_t(reason_key, lang)),
      if (!is.null(proposal)) shiny::tagList(
        shiny::tags$p(shiny::tags$strong(app_t("adviser.status", lang)), " ",
          app_t(paste0("adviser.status.", proposal$status), lang)),
        shiny::tags$p(class = "cttir-lead", proposal$summary),
        if (is.list(plan)) shiny::tagList(
          field("adviser.method", plan$method),
          field("adviser.estimand", plan$estimand),
          field("adviser.unit", plan$unit),
          field("adviser.dependence", plan$dependence),
          section("adviser.assumptions", plan$assumptions),
          if (length(plan$steps)) shiny::tagList(shiny::tags$h3(app_t("adviser.steps", lang)),
            shiny::tags$ol(lapply(plan$steps, function(step) shiny::tags$li(step$action)))),
          section("adviser.unresolved", plan$unresolved)),
        section("adviser.questions", proposal$questions),
        section("adviser.limitations", proposal$limitations),
        if (length(value$checks$unknown_apis)) shiny::tags$p(class = "cttir-status cttir-status-warning",
          app_t("adviser.unknown_apis", lang), " ", shiny::tags$code(paste(value$checks$unknown_apis, collapse = ", "))),
        if (length(sources)) shiny::tagList(shiny::tags$h3(app_t("adviser.sources", lang)),
          shiny::tags$ul(lapply(sources, function(e) shiny::tags$li(shiny::tags$code(e$source_id), " ",
            e$package, " ", shiny::tags$code(substr(e$revision %||% "", 1L, 12L)))))),
        if (!is.null(value$provenance)) shiny::tags$p(class = "cttir-empty", app_t("adviser.provenance", lang,
          model = value$provenance$model, prompt = value$provenance$prompt_tokens %||% "?",
          generated = value$provenance$generated_tokens %||% "?", seconds = value$provenance$seconds %||% "?"))
      )),
    app_answer_ui(value$deterministic, lang, ns)
  )
}
