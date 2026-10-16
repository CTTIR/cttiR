app_assistance_ui <- function(value, lang, ns) {
  proposal <- value$proposal
  shiny::tagList(
    shiny::tags$section(class = "cttir-review-panel", `aria-labelledby` = ns("assistance_heading"),
      shiny::tags$h2(id = ns("assistance_heading"), app_t("assistance.heading", lang)),
      if (any(vapply(value$context$references, function(ref)
        identical(ref$content_authority, "installed_package_documentation_not_project_readiness"), logical(1)))) {
        shiny::tags$p(app_t("assistance.project_evidence", lang))
      },
      if (any(vapply(value$context$references, function(ref)
        isTRUE(ref$content_authority %in% c("versioned_policy_contract_not_implementation_evidence",
          "implementation_source_not_execution_evidence")), logical(1)))) {
        shiny::tags$p(app_t("assistance.policy_evidence", lang))
      },
      if (any(vapply(value$context$gaps, function(gap)
        identical(gap$reason, "lexical_reference_candidates_require_review"), logical(1)))) {
        shiny::tags$p(app_t("assistance.lexical_evidence", lang))
      },
      if (any(vapply(value$context$gaps, function(gap)
        identical(gap$reason, "workflow_approval_missing_read_only_reference"), logical(1)))) {
        shiny::tags$p(app_t("assistance.read_only_evidence", lang))
      },
      if (!is.null(proposal)) shiny::tags$p(app_t("assistance.draft_note", lang)),
      if (is.null(proposal)) shiny::tags$p(app_t("assistance.fallback", lang), " ",
        app_t(if (app_has_message(paste0("assistance.reason.", value$reason)))
          paste0("assistance.reason.", value$reason) else "assistance.reason.unavailable", lang)),
      if (!is.null(proposal)) shiny::tagList(
        shiny::tags$p(proposal$summary),
        shiny::tags$p(app_t("assistance.qualification", lang), " ", shiny::tags$code(value$exchange$qualification)),
        shiny::tags$p(app_t("assistance.validation", lang), " ", shiny::tags$code(value$exchange$validation_level)),
        shiny::tags$h3(app_t("ask.limitations", lang)),
        app_list_block(proposal$limitations, lang),
        lapply(proposal$changes, function(change) shiny::tags$details(
          shiny::tags$summary(change$path), shiny::tags$p(change$reason),
          shiny::tags$pre(shiny::tags$code(change$content)))),
        shiny::tags$h3(app_t("ask.citations", lang)),
        shiny::tags$ul(lapply(proposal$citation_ids, function(id) {
          refs <- Filter(function(ref) identical(ref$citation_id, id), value$context$references)
          shiny::tags$li(shiny::tags$code(id), lapply(refs, function(ref) shiny::tagList(
            " ", ref$package, " ", shiny::tags$code(ref$revision), " ", app_link(ref$evidence_url))))
        }))
      )),
    app_answer_ui(value$deterministic, lang, ns)
  )
}

app_suggestion_review_ui <- function(preview, applied, lang, ns) {
  shiny::tags$section(class = "cttir-review-panel", `aria-labelledby` = ns("suggestion_review_heading"),
    shiny::tags$h2(id = ns("suggestion_review_heading"), app_t("assistance.review", lang)),
    if (!is.null(applied)) shiny::tags$p(app_t("assistance.applied", lang)),
    if (is.null(applied)) shiny::actionButton(ns("preview_suggestion"), app_t("assistance.preview", lang)),
    if (!is.null(preview)) shiny::tagList(
      lapply(preview$changes, function(change) shiny::tags$details(
        shiny::tags$summary(change$path, " \u2014 ", change$role, " \u2014 ", change$status),
        shiny::tags$h3(app_t("assistance.before", lang)),
        shiny::tags$pre(shiny::tags$code(change$before %||% "")),
        shiny::tags$h3(app_t("assistance.after", lang)),
        shiny::tags$pre(shiny::tags$code(change$after)))),
      shiny::tags$p(app_t("assistance.apply_note", lang)),
      if (isTRUE(preview$conflicts)) shiny::tags$p(app_t("assistance.conflicts", lang)) else
        shiny::actionButton(ns("apply_suggestion"), app_t("assistance.apply", lang))
    ))
}
