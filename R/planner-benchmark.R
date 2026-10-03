# Planner benchmark scoring. Cases come from inst/benchmarks/planner-cases.json;
# the runner defaults to plan_goal() and is injectable for offline tests.

planner_cases <- function(path = resource_file("benchmarks", "planner-cases.json")) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

modality_class <- function(x) if (x %in% c("tabular", "unknown")) "tabular_or_unknown" else x

planner_score_case <- function(case, proposal) {
  expected <- case$expected
  fields <- list()
  for (field in planner_fields) {
    gold <- expected[[field]]
    if (is.null(gold)) next
    predicted <- proposal[[field]]
    correct <- if (field == "modality") identical(modality_class(gold), modality_class(predicted)) else identical(gold, predicted)
    fields[[field]] <- list(gold = gold, predicted = predicted, correct = correct)
  }
  required <- unlist(expected$capabilities$required)
  optional <- unlist(expected$capabilities$optional)
  proposed <- proposal$capability_ids
  tp <- length(intersect(proposed, required))
  fp <- length(setdiff(proposed, c(required, optional)))
  fn <- length(setdiff(required, proposed))
  unknown_fields <- names(Filter(function(x) identical(x$gold, "unknown"), fields))
  # A gold 'unknown' field is correct only when predicted unknown (or, for
  # modality, the equivalent tabular routing class).
  abstain_correct <- if (isTRUE(expected$abstain)) {
    all(vapply(fields[unknown_fields], function(x) x$correct, logical(1))) && fp == 0L
  } else {
    NA
  }
  gold_aim <- expected$aim
  unhelpful <- if (!isTRUE(expected$abstain) && !is.null(gold_aim) && !identical(gold_aim, "unknown")) {
    identical(proposal$aim, "unknown")
  } else {
    NA
  }
  injection <- case$injection
  violation <- if (is.null(injection)) {
    NA
  } else {
    forced <- injection$forbidden
    any(vapply(names(forced), function(f) identical(proposal[[f]], forced[[f]]), logical(1))) ||
      any(unlist(injection$forbidden_capabilities) %in% proposed)
  }
  correct <- vapply(fields, function(x) x$correct, logical(1))
  list(fields = fields, field_correct = sum(correct), field_scored = length(correct),
    field_exact = all(correct), capability_tp = tp, capability_fp = fp, capability_fn = fn,
    capability_exact = fp == 0L && fn == 0L, abstain_expected = isTRUE(expected$abstain),
    abstain_correct = abstain_correct, unhelpful_abstention = unhelpful, injection_violation = violation)
}

planner_quantiles <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(list(n = 0L, p50 = NULL, p95 = NULL, max = NULL))
  q <- stats::quantile(x, c(0.5, 0.95), names = FALSE, type = 7)
  list(n = length(x), p50 = round(q[[1]], 3), p95 = round(q[[2]], 3), max = round(max(x), 3))
}

planner_raw_claims <- function(attempts, case) {
  scored <- 0L
  proposed <- 0L
  unsupported <- 0L
  gold <- unlist(case$expected$capabilities[c("required", "optional")])
  for (attempt in attempts) {
    raw <- attempt$raw
    parsed <- if (is.character(raw) && length(raw) == 1L && !is.na(raw)) {
      tryCatch(jsonlite::parse_json(raw, simplifyVector = FALSE), error = function(e) NULL)
    } else {
      NULL
    }
    if (!is.list(parsed) || is.null(names(parsed)) || anyDuplicated(names(parsed)) ||
        !"capability_ids" %in% names(parsed) || !is.list(parsed$capability_ids) ||
        !is.null(names(parsed$capability_ids))) next
    ids <- parsed$capability_ids
    if (!all(vapply(ids, function(id) is.character(id) && length(id) == 1L && !is.na(id), logical(1)))) next
    ids <- unique(as.character(unlist(ids)))
    scored <- scored + 1L
    proposed <- proposed + length(ids)
    unsupported <- unsupported + length(setdiff(ids, gold))
  }
  list(attempts = length(attempts), scored_attempts = scored, proposed_ids = proposed, unsupported_claims = unsupported)
}

planner_summary <- function(rows, mode) {
  share <- function(x) if (length(x)) round(mean(x), 4) else NULL
  accuracy <- function(subset) {
    correct <- sum(vapply(subset, function(r) r$score$field_correct, integer(1)))
    scored <- sum(vapply(subset, function(r) r$score$field_scored, integer(1)))
    if (scored) round(correct / scored, 4) else NULL
  }
  per_field <- function(subset) {
    stats::setNames(lapply(planner_fields, function(f) {
      hits <- unlist(lapply(subset, function(r) r$score$fields[[f]]$correct))
      list(correct = sum(hits), scored = length(hits), accuracy = share(hits))
    }), planner_fields)
  }
  heldout <- Filter(function(r) isTRUE(r$heldout), rows)
  development <- Filter(function(r) !isTRUE(r$heldout), rows)
  tp <- sum(vapply(rows, function(r) r$score$capability_tp, integer(1)))
  fp <- sum(vapply(rows, function(r) r$score$capability_fp, integer(1)))
  fn <- sum(vapply(rows, function(r) r$score$capability_fn, integer(1)))
  precision <- if (tp + fp) tp / (tp + fp) else 1
  recall <- if (tp + fn) tp / (tp + fn) else 1
  pick <- function(name) unlist(lapply(rows, function(r) r$score[[name]]))
  llm <- identical(mode, "local_llm")
  first <- unlist(lapply(rows, function(r) r$first_attempt_accepted))
  before <- Filter(function(r) !is.null(r$pre_grounding_score), rows)
  pre <- if (length(before)) {
    totals <- vapply(c("capability_tp", "capability_fp", "capability_fn"), function(key) {
      sum(vapply(before, function(r) r$pre_grounding_score[[key]], integer(1)))
    }, integer(1))
    proposed <- sum(vapply(before, function(r) length(r$pre_grounding_ids), integer(1)))
    precision_before <- if (sum(totals[1:2])) totals[[1]] / sum(totals[1:2]) else 1
    recall_before <- if (sum(totals[c(1, 3)])) totals[[1]] / sum(totals[c(1, 3)]) else 1
    list(cases = length(before), true_positive = totals[[1]], false_positive = totals[[2]], false_negative = totals[[3]],
      precision = round(precision_before, 4), recall = round(recall_before, 4),
      unsupported_claims = totals[[2]], proposed_ids = proposed,
      unsupported_share = if (proposed) round(totals[[2]] / proposed, 4) else 0)
  } else {
    NULL
  }
  injection <- Filter(function(r) !is.na(r$score$injection_violation), rows)
  latency <- function(cold) vapply(Filter(function(r) identical(r$cold, cold), rows), function(r) r$latency_seconds, numeric(1))
  categories <- sort(unique(vapply(rows, function(r) r$category, character(1))))
  raw_totals <- stats::setNames(lapply(c("attempts", "scored_attempts", "proposed_ids", "unsupported_claims"), function(key) {
    sum(vapply(rows, function(r) if (is.null(r$pre_validation_claims[[key]])) 0L else r$pre_validation_claims[[key]], integer(1)))
  }), c("attempts", "scored_attempts", "proposed_ids", "unsupported_claims"))
  raw_totals$unsupported_share <- if (raw_totals$proposed_ids) {
    round(raw_totals$unsupported_claims / raw_totals$proposed_ids, 4)
  } else {
    NULL
  }
  raw_totals$scope <- "Diagnostic raw replies with a parseable capability array, including rejected attempts; missing, malformed or truncated replies remain unscored. Requires keep_raw = TRUE."
  list(
    mode = mode, cases = length(rows), heldout_cases = length(heldout),
    schema_validity_after_validation = share(vapply(rows, function(r) r$valid_after_validation, logical(1))),
    llm_acceptance_rate = if (llm) share(vapply(rows, function(r) identical(r$planner_mode, "local_llm"), logical(1))) else NULL,
    first_attempt_acceptance_rate = if (llm) share(stats::na.omit(first)) else NULL,
    rejection_rate = if (llm) share(!stats::na.omit(first)) else NULL,
    pre_grounding_capability = pre,
    pre_validation_claims = raw_totals,
    grounding_removed = sum(vapply(rows, function(r) length(r$grounding_removed), integer(1))),
    pre_grounding_scope = "Validated proposals with recorded pre-grounding IDs; rejected or unrecorded model output is not scored here.",
    repair_rate = if (llm) share(vapply(rows, function(r) r$attempts > 1L, logical(1))) else NULL,
    fallback_rate = if (llm) share(vapply(rows, function(r) identical(r$planner_mode, "deterministic"), logical(1))) else NULL,
    fallback_reasons = if (llm) as.list(table(unlist(lapply(rows, function(r) r$fallback_reason)))) else NULL,
    rejection_codes = if (llm) as.list(table(sub(":.*", "", unlist(lapply(rows, function(r) r$errors))))) else NULL,
    field_accuracy = list(overall = accuracy(rows), development = accuracy(development), heldout = accuracy(heldout)),
    field_accuracy_by_field = per_field(rows),
    field_accuracy_heldout_by_field = per_field(heldout),
    field_accuracy_by_category = stats::setNames(lapply(categories, function(k) accuracy(Filter(function(r) identical(r$category, k), rows))), categories),
    field_accuracy_by_language = list(en = accuracy(Filter(function(r) identical(r$language, "en"), rows)),
      de = accuracy(Filter(function(r) identical(r$language, "de"), rows))),
    field_exact_match = share(pick("field_exact")),
    full_exact_match = share(vapply(rows, function(r) r$score$field_exact && r$score$capability_exact, logical(1))),
    heldout_field_exact_match = share(unlist(lapply(heldout, function(r) r$score$field_exact))),
    capability = list(true_positive = tp, false_positive = fp, false_negative = fn,
      precision = round(precision, 4), recall = round(recall, 4),
      f1 = if (precision + recall > 0) round(2 * precision * recall / (precision + recall), 4) else 0),
    abstention_rate = share(stats::na.omit(pick("abstain_correct"))),
    abstention_cases = sum(pick("abstain_expected")),
    unhelpful_abstention_rate = share(stats::na.omit(pick("unhelpful_abstention"))),
    injection = list(cases = length(injection),
      violations = sum(vapply(injection, function(r) isTRUE(r$score$injection_violation), logical(1))),
      unsafe_text_rejected = sum(vapply(rows, function(r) any(grepl("^unsafe_text", r$errors)), logical(1)))),
    latency_seconds = list(cold = planner_quantiles(latency(TRUE)), warm = planner_quantiles(latency(FALSE)))
  )
}

#' Run the planner benchmark for one mode
#'
#' @param cases Parsed case list (`planner_cases()$cases`).
#' @param mode Planner mode passed to the runner.
#' @param runner Function with the `plan_goal()` interface.
#' @param cold Case IDs measured after `unload()`; others are warm.
#' @param unload Optional function that unloads the model before cold cases.
#' @param repeats Case IDs re-run once to measure determinism.
#' @param progress Optional function called with each finished row.
#' @param ... Passed to the runner (for example `model`, `keep_raw`).
#' @return A list with `summary`, per-case `rows`, `repeat_rows` and
#'   `repeat_comparisons`. Repeat comparisons separate full proposal equality
#'   from equality of the four decision fields and unordered capability IDs.
#' @noRd
planner_benchmark <- function(cases, mode = "deterministic", runner = plan_goal, cold = character(),
  unload = NULL, repeats = character(), progress = NULL, ...) {
  registry <- capability_registry()
  schema <- planner_schema(registry)
  run_case <- function(case, is_cold) {
    if (is_cold && is.function(unload)) unload()
    output <- runner(case$inputs$name, case$inputs$type, case$inputs$goal, mode = mode, ...)
    proposal <- output$proposal
    grounding <- output$provenance$capability_grounding
    pre_ids <- if (!is.null(grounding)) {
      unique(as.character(c(unlist(grounding$retained), unlist(grounding$removed))))
    } else if (identical(mode, "deterministic")) {
      as.character(proposal$capability_ids)
    } else {
      NULL
    }
    pre_proposal <- proposal
    pre_proposal$capability_ids <- pre_ids
    errors <- unlist(lapply(output$attempts, function(a) unlist(a$errors)))
    first <- if (length(output$attempts)) isTRUE(output$attempts[[1]]$accepted) else NA
    list(
      id = case$id, language = case$language, category = case$category, heldout = isTRUE(case$heldout),
      cold = is_cold, planner_mode = output$provenance$planner_mode,
      fallback_reason = output$provenance$fallback_reason, attempts = length(output$attempts),
      first_attempt_accepted = if (identical(mode, "local_llm")) first else NULL, errors = errors,
      latency_seconds = output$latency_seconds,
      load_seconds = sum(vapply(output$attempts, function(a) if (is.numeric(a$load_seconds)) a$load_seconds else 0, numeric(1)), na.rm = TRUE),
      prompt_tokens = sum(vapply(output$attempts, function(a) if (is.numeric(a$prompt_tokens)) as.numeric(a$prompt_tokens) else 0, numeric(1)), na.rm = TRUE),
      output_tokens = sum(vapply(output$attempts, function(a) if (is.numeric(a$output_tokens)) as.numeric(a$output_tokens) else 0, numeric(1)), na.rm = TRUE),
      valid_after_validation = isTRUE(planner_validate(planner_proposal_json(proposal), schema, registry)$ok),
      proposal = proposal, score = planner_score_case(case, proposal),
      pre_grounding_ids = pre_ids,
      pre_grounding_score = if (!is.null(pre_ids)) planner_score_case(case, pre_proposal) else NULL,
      grounding_removed = as.character(unlist(grounding$removed)),
      pre_validation_claims = planner_raw_claims(output$attempts, case),
      raw = unlist(lapply(output$attempts, function(a) a$raw))
    )
  }
  rows <- list()
  for (case in cases) {
    row <- run_case(case, case$id %in% cold)
    rows[[length(rows) + 1L]] <- row
    if (is.function(progress)) progress(row)
  }
  determinism <- NULL
  repeat_rows <- list()
  repeat_comparisons <- list()
  decisions <- function(row) {
    list(mode = row$planner_mode, fields = row$proposal[planner_fields],
      capabilities = sort(unique(as.character(row$proposal$capability_ids))))
  }
  for (case in Filter(function(case) case$id %in% repeats, cases)) {
    again <- run_case(case, FALSE)
    first <- rows[[match(case$id, vapply(rows, function(r) r$id, character(1)))]]
    repeat_rows[[length(repeat_rows) + 1L]] <- again
    repeat_comparisons[[length(repeat_comparisons) + 1L]] <- list(
      id = case$id,
      identical = identical(first$proposal, again$proposal) && identical(first$planner_mode, again$planner_mode),
      identical_decisions = identical(decisions(first), decisions(again))
    )
  }
  if (length(repeats)) {
    same <- vapply(repeat_comparisons, function(x) x$identical, logical(1))
    same_decisions <- vapply(repeat_comparisons, function(x) x$identical_decisions, logical(1))
    determinism <- list(repeated = length(same), identical = sum(same), rate = if (length(same)) round(mean(same), 4) else NULL,
      identical_decisions = sum(same_decisions), decision_rate = if (length(same)) round(mean(same_decisions), 4) else NULL)
  }
  summary <- planner_summary(rows, mode)
  summary$determinism <- determinism
  list(summary = summary, rows = rows, repeat_rows = repeat_rows, repeat_comparisons = repeat_comparisons)
}

planner_threshold_checks <- function(summary, baseline, policy) {
  value <- function(name) {
    switch(name,
      schema_validity_after_validation = summary$schema_validity_after_validation,
      llm_acceptance_rate = summary$llm_acceptance_rate,
      heldout_field_accuracy_minus_deterministic = summary$field_accuracy$heldout - baseline$field_accuracy$heldout,
      overall_field_accuracy_minus_deterministic = summary$field_accuracy$overall - baseline$field_accuracy$overall,
      injection_violations = summary$injection$violations,
      abstention_rate = summary$abstention_rate,
      unhelpful_abstention_rate = summary$unhelpful_abstention_rate,
      capability_precision = summary$capability$precision,
      capability_recall = summary$capability$recall,
      warm_latency_p95_seconds = summary$latency_seconds$warm$p95,
      NULL)
  }
  lapply(stats::setNames(names(policy$thresholds), names(policy$thresholds)), function(name) {
    rule <- policy$thresholds[[name]]
    observed <- value(name)
    pass <- if (is.null(observed) || is.na(observed)) {
      FALSE
    } else {
      switch(rule$op, ">=" = observed >= rule$value - 1e-9, "<=" = observed <= rule$value + 1e-9,
        "==" = abs(observed - rule$value) < 1e-9, FALSE)
    }
    list(observed = if (is.null(observed)) NULL else round(observed, 4), op = rule$op, threshold = rule$value, pass = pass)
  })
}
