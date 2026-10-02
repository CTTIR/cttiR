# Grounded, deterministic answers from the pinned catalog revision.
#
# A question is read as untrusted text. It is matched to registered capabilities
# and exact package::export names; nothing in it is executed or sent anywhere.
# Code comes only from reviewed snippets and is returned only after it validates
# against the approved function table of the same catalog revision.

ask_stage_order <- c("project", "import", "check", "tidy", "describe", "figures", "model", "effects",
  "demo", "report", "pipeline", "environment")

ask_injection_markers <- c("ignore previous", "ignore all previous", "ignore the above", "disregard",
  "system prompt", "you are now", "new instructions", "vergiss", "ignoriere")

ask_snippets <- function() {
  doc <- read_document(resource_file("extdata", "ask-snippets.json"))
  stats::setNames(doc$snippets, vapply(doc$snippets, function(x) x$capability, character(1)))
}

# Engine capability implied by explicit design words in the question.
ask_engine_capability <- function(signals) {
  analysis <- list(aim = "explanatory", outcome_family = signals$outcome_family, unit_structure = signals$unit_structure)
  if (identical(signals$unit_structure, "unknown") && !identical(signals$outcome_family, "unknown")) {
    analysis$unit_structure <- "independent"
  }
  # Repeated/clustered designs without a stated outcome type are suggested the
  # continuous mixed model; the answer lists the outcome type as a prerequisite.
  if (isTRUE(signals$unit_structure %in% c("longitudinal", "clustered")) && identical(signals$outcome_family, "unknown")) {
    analysis$outcome_family <- "continuous"
  }
  engine <- tryCatch(analysis_configuration(list(analysis = analysis))$candidate_engine, error = function(e) NULL)
  if (is.null(engine)) NULL else engine_capability[[engine]]
}

ask_matches <- function(text, registry, signals) {
  ids <- character()
  for (cap in registry$capabilities) {
    if (keyword_hit(text, cap$keywords) || grepl(tolower(cap$title), text, fixed = TRUE)) ids <- c(ids, cap$id)
  }
  # A named but unsupported method must not be answered with a nearby engine.
  unsupported_method <- any(vapply(ids, function(id) {
    cap <- registry$capabilities[[id]]
    identical(cap$stage, "model") && identical(cap$status, "candidate")
  }, logical(1)))
  engine <- if (unsupported_method) NULL else ask_engine_capability(signals)
  if (!is.null(engine)) ids <- c(ids, engine)
  model_ids <- c("std.model.lm", "std.model.glm_binomial", "std.model.lme", "std.model.coxph")
  if (any(ids %in% model_ids)) ids <- c(ids, "std.effects.broom")
  if (!signals$modality %in% c("unknown", "tabular")) {
    for (cap in registry$capabilities) {
      if (signals$modality %in% cap$applies$modality && !identical(cap$family, "standard")) ids <- c(ids, cap$id)
    }
  }
  unique(ids)
}

ask_symbol <- function(symbol, catalog, verified_only) {
  parts <- strsplit(symbol, ":{2,3}")[[1]]
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  validation <- validate_generated_code(symbol, catalog, approved_only = verified_only)
  package <- index[[parts[[1]]]]
  entry <- if (is.null(package)) NULL else Filter(function(x) identical(x$name, parts[[2]]), package$exports)
  list(symbol = symbol, found = length(entry) > 0L, package = parts[[1]], export = parts[[2]],
    status = validation$status[[1]], reason = validation$reason[[1]],
    signature = if (length(entry)) entry[[1]]$signature else NA_character_,
    revision = if (is.null(package)) NA_character_ else package$revision,
    citation = if (length(entry) && !is.null(entry[[1]]$documentation)) catalog_evidence_url(package, entry[[1]]$documentation$path) else NA_character_)
}

ask_evidence <- function(validation, catalog, adapter = NULL) {
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  rows <- validation[!is.na(validation$package) & validation$package != "base" & validation$status == "ok", , drop = FALSE]
  out <- data.frame(id = character(), package = character(), version = character(), revision = character(),
    export = character(), verification = character(), approval = character(), citation = character(),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(rows))) {
    package <- index[[rows$package[[i]]]]
    entry <- Filter(function(x) identical(x$name, rows$export[[i]]), package$exports)[[1]]
    owner <- sub("^workflow_approved_reexport:", "", rows$reason[[i]])
    approving <- if (startsWith(rows$reason[[i]], "workflow_approved_reexport:")) index[[owner]] else package
    approvals <- approved_export_index(approving)[[entry$name]]
    role <- if (is.null(adapter)) NULL else sub("^[a-z]+[.]", "", adapter)
    preferred <- Filter(function(a) identical(a$role, role), approvals)
    if (length(preferred)) approvals <- preferred
    doc <- entry$documentation
    out[nrow(out) + 1L, ] <- list(content_hash(paste(package$source_hash, entry$name)), package$name, package$version,
      package$revision, entry$name, rows$reason[[i]],
      if (length(approvals)) approvals[[1]]$approval_id else NA_character_,
      if (is.null(doc)) NA_character_ else catalog_evidence_url(package, doc$path))
  }
  unique(out)
}

ask_prerequisites <- function(id) {
  mapping <- switch(id,
    std.model.lm = c("outcome", "predictors", "estimand", "missing_data"),
    std.model.glm_binomial = c("outcome", "predictors", "event_value", "non_event_value", "estimand", "missing_data"),
    std.model.lme = c("outcome (continuous)", "predictors", "subject", "time (longitudinal)", "estimand", "missing_data"),
    std.model.coxph = c("time", "event", "event_value", "non_event_value", "time_origin", "time_unit", "predictors"),
    character())
  if (length(mapping)) paste0("Map analysis$mapping fields: ", paste(mapping, collapse = ", "), ".") else character()
}

#' Ask for grounded workflow advice
#'
#' Reads the question as untrusted text and matches it to registered workflow
#' capabilities and exact `package::export` names in the pinned catalog
#' revision. Supported capabilities return ordered steps, prerequisites, the
#' approved package revisions and a reviewed, namespaced code snippet that is
#' validated against the approved function table before it is returned. Nothing
#' is executed, no model is called and no question text leaves the machine.
#' Unsupported procedures return a precise gap and the nearest approved
#' alternatives instead of invented code.
#' @param question Nonempty question, optionally naming `package::export`.
#' @param path Optional exact project root selecting its pinned catalog.
#' @param verified_only If `TRUE`, every returned API claim and code call must be
#'   covered by a complete workflow approval of the pinned revision. If `FALSE`,
#'   statically verified but unapproved APIs and candidate capabilities may be
#'   listed, clearly labelled; nonexistent exports are never presented as real.
#' @return A `cttir_answer` with `answer`, `steps`, `prerequisites`, `packages`,
#'   `code`, `citations`, `verification_levels`, `evidence`, `symbols`, `gaps`
#'   and `limitations`. Code uses placeholders (`<...>`) for mapped data and is
#'   illustrative; generated projects use the full reviewed stage library.
#' @export
ask <- function(question, path = NULL, verified_only = TRUE) {
  scalar_text(question, "question")
  scalar_flag(verified_only, "verified_only")
  catalog <- resolve_catalog(path)
  registry <- capability_registry()
  symbol_pattern <- "[A-Za-z][A-Za-z0-9.]*:{2,3}[A-Za-z._][A-Za-z0-9._]*"
  # Package names inside exact symbols must not trigger capability keywords.
  text <- tolower(enc2utf8(gsub(symbol_pattern, " ", question)))
  injected <- any(vapply(ask_injection_markers, function(k) grepl(k, text, fixed = TRUE), logical(1)))
  signals <- if (injected) {
    list(aim = "unknown", outcome_family = "unknown", unit_structure = "unknown", modality = "unknown")
  } else {
    infer_goal(text, registry)
  }
  symbols <- unique(regmatches(question, gregexpr(symbol_pattern, question))[[1]])
  symbol_rows <- lapply(symbols, ask_symbol, catalog = catalog, verified_only = verified_only)
  ids <- if (injected) character() else ask_matches(text, registry, signals)
  routed <- lapply(ids, function(id) route_stage(registry, catalog, registry$capabilities[[id]]$stage, id))
  approved <- Filter(function(x) identical(x$status, "approved"), routed)
  pending <- Filter(function(x) !identical(x$status, "approved"), routed)
  order <- order(match(vapply(approved, function(x) x$stage, character(1)), ask_stage_order), na.last = TRUE)
  approved <- approved[order]
  snippets <- ask_snippets()
  code_blocks <- character()
  evidence <- ask_evidence(data.frame(package = character(), status = character(), stringsAsFactors = FALSE), catalog)
  limitations <- c("Snippets are illustrative and use placeholders; review mappings, assumptions and diagnostics before use.",
    "Approval covers the pinned package revisions and adapter, not a scientific conclusion.",
    "No local model is used: the tested local models did not qualify for planning (see setup()).")
  for (stage in approved) {
    snippet <- snippets[[stage$capability]]
    if (is.null(snippet)) next
    validation <- validate_generated_code(snippet$code, catalog, approved_only = verified_only)
    if (isTRUE(attr(validation, "valid"))) {
      code_blocks <- c(code_blocks, snippet$code)
      evidence <- rbind(evidence, ask_evidence(validation, catalog, registry$capabilities[[stage$capability]]$adapter$id))
    } else {
      limitations <- c(limitations, paste0("The snippet for ", stage$capability, " did not validate against this revision and was withheld."))
    }
  }
  if (!verified_only) {
    for (stage in pending) {
      snippet <- snippets[[stage$capability]]
      if (is.null(snippet)) next
      validation <- validate_generated_code(snippet$code, catalog, approved_only = FALSE)
      if (isTRUE(attr(validation, "valid"))) {
        code_blocks <- c(code_blocks, paste0("# UNAPPROVED (statically verified only)\n", snippet$code))
        evidence <- rbind(evidence, ask_evidence(validation, catalog))
      }
    }
  }
  for (row in symbol_rows) {
    if (identical(row$status, "ok")) {
      evidence[nrow(evidence) + 1L, ] <- list(content_hash(paste(row$revision, row$symbol)), row$package, NA_character_,
        row$revision, row$export, row$reason, NA_character_, row$citation)
    }
  }
  if (!verified_only) {
    # Unverified mode also cites literal documentation excerpts, labelled as such.
    documents <- search(question, path = path, limit = 10L)
    documents <- documents[documents$kind == "document", , drop = FALSE]
    for (i in seq_len(nrow(documents))) {
      evidence[nrow(evidence) + 1L, ] <- list(documents$id[[i]], documents$package[[i]], NA_character_,
        documents$revision[[i]], NA_character_, "documentation_indexed", NA_character_, documents$evidence[[i]])
    }
  }
  evidence <- unique(evidence)
  gaps <- character()
  for (stage in pending) {
    cap <- registry$capabilities[[stage$capability]]
    same_stage <- Filter(function(x) {
      identical(x$stage, cap$stage) && identical(x$status, "adapter_tested") &&
        identical(capability_approval(x, catalog)$status, "approved")
    }, registry$capabilities)
    alternatives <- vapply(same_stage, function(x) x$id, character(1))
    nearest <- if (length(alternatives)) paste0("; nearest approved alternatives: ", paste(alternatives, collapse = ", ")) else ""
    gaps <- c(gaps, paste0(stage$capability, " (", cap$title, "): ", stage$status, nearest))
  }
  for (row in symbol_rows) {
    if (!identical(row$status, "ok")) gaps <- c(gaps, paste0(row$symbol, ": ", if (row$found) row$reason else "not in the pinned catalog revision"))
  }
  if (injected) limitations <- c(limitations, "Instruction-like text in the question was treated as data; no capability was inferred from it.")
  steps <- vapply(approved, function(x) {
    cap <- registry$capabilities[[x$capability]]
    paste0(x$stage, ": ", cap$title, " [", x$capability, "; ", paste(unlist(x$packages), collapse = ", "), "]")
  }, character(1))
  requirements <- lapply(approved, function(x) unlist(registry$capabilities[[x$capability]]$requirements))
  mapping_needs <- lapply(approved, function(x) ask_prerequisites(x$capability))
  prerequisites <- unique(unlist(c(requirements, mapping_needs)))
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  used <- unique(unlist(lapply(approved, function(x) setdiff(unlist(x$packages), "base"))))
  packages <- data.frame(package = used,
    version = vapply(used, function(n) if (is.null(index[[n]])) NA_character_ else index[[n]]$version, character(1)),
    revision = vapply(used, function(n) if (is.null(index[[n]])) NA_character_ else index[[n]]$revision, character(1)),
    stringsAsFactors = FALSE, row.names = NULL)
  answer <- if (length(approved)) {
    paste0("Supported with approved adapters: ", paste(vapply(approved, function(x) x$capability, character(1)), collapse = ", "), ".")
  } else if (length(symbol_rows) && any(vapply(symbol_rows, function(x) identical(x$status, "ok"), logical(1)))) {
    "The named API is present in the pinned revision; see evidence for its verification level."
  } else if (length(pending)) {
    "No approved adapter covers this request; see gaps for candidates and the nearest approved alternatives."
  } else {
    "No supported capability or exact API matched. Ask about import, checks, tidy roles, descriptive tables, figures, lm/glm/mixed/Cox models, effects, reports or pipelines."
  }
  structure(list(
    answer = answer, steps = as.list(steps), prerequisites = as.list(prerequisites), packages = packages,
    code = paste(code_blocks, collapse = "\n\n"), citations = as.list(unique(stats::na.omit(evidence$citation))),
    verification_levels = unique(evidence$verification), evidence = evidence,
    symbols = symbol_rows, gaps = as.list(gaps), capabilities = as.list(ids),
    approved_capabilities = as.list(vapply(approved, function(x) x$capability, character(1))),
    limitations = limitations, catalog_id = catalog$content_id
  ), class = "cttir_answer")
}

#' @export
print.cttir_answer <- function(x, ...) {
  cat(x$answer, "\n")
  if (length(x$steps)) cat(paste0("- ", unlist(x$steps), collapse = "\n"), "\n")
  if (length(x$gaps)) cat("Gaps:\n", paste0("- ", unlist(x$gaps), collapse = "\n"), "\n")
  if (nzchar(x$code)) cat("\n", x$code, "\n", sep = "")
  if (length(x$citations)) cat(length(x$citations), "citations; see $evidence\n")
  invisible(x)
}
