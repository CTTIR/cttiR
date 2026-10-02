# Scores ask() against the bundled grounded-answer benchmark. Deterministic and
# offline; used by the test suite and the release evidence script.
ask_benchmark <- function(cases = read_document(resource_file("benchmarks", "ask-cases.json")), path = NULL) {
  catalog <- resolve_catalog(path)
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  rows <- list()
  for (case in cases$cases) {
    started <- proc.time()[["elapsed"]]
    a <- ask(case$question, path = path)
    elapsed <- proc.time()[["elapsed"]] - started
    approved <- unlist(a$approved_capabilities)
    evidence_ok <- all(vapply(seq_len(nrow(a$evidence)), function(i) {
      p <- index[[a$evidence$package[[i]]]]
      !is.null(p) && any(vapply(p$exports, function(x) identical(x$name, a$evidence$export[[i]]), logical(1))) &&
        startsWith(a$evidence$verification[[i]], "workflow_approved") && !is.na(a$evidence$citation[[i]])
    }, logical(1)))
    code_ok <- !nzchar(a$code) || isTRUE(attr(validate_generated_code(a$code, catalog), "valid"))
    expected <- unlist(case$expect)
    outcome <- switch(case$kind,
      supported = , cross = all(expected %in% approved),
      negative = {
        absent <- if (is.null(case$symbol_absent)) NULL else case$symbol_absent
        reported <- !is.null(absent) && any(grepl(absent, unlist(a$gaps), fixed = TRUE))
        clean <- is.null(absent) || (!grepl(absent, a$code, fixed = TRUE) && reported)
        clean && !any(grepl("^std[.]model[.]", approved))
      },
      gap = case$gap %in% unlist(a$capabilities) && !case$gap %in% approved &&
        any(startsWith(unlist(a$gaps), case$gap)),
      injection = !length(approved) && !nzchar(a$code),
      ambiguous = !length(approved) && !nzchar(a$code),
      FALSE)
    rows[[length(rows) + 1L]] <- data.frame(id = case$id, lang = case$lang, kind = case$kind,
      passed = isTRUE(outcome), citations_correct = evidence_ok, code_valid = code_ok,
      approved = paste(approved, collapse = ","), seconds = elapsed, stringsAsFactors = FALSE)
  }
  results <- do.call(rbind, rows)
  rate <- function(kinds) {
    x <- results$passed[results$kind %in% kinds]
    if (length(x)) mean(x) else NA_real_
  }
  metrics <- list(
    cases = nrow(results),
    citation_correctness = mean(results$citations_correct),
    code_validity = mean(results$code_valid),
    supported_recall = rate(c("supported", "cross")),
    negative_abstention = rate(c("negative", "gap", "ambiguous")),
    injection_resistance = rate("injection"),
    latency_p50 = unname(stats::quantile(results$seconds, 0.5)),
    latency_p95 = unname(stats::quantile(results$seconds, 0.95))
  )
  thresholds <- cases$thresholds
  checks <- vapply(names(thresholds), function(name) isTRUE(metrics[[name]] >= thresholds[[name]]), logical(1))
  list(metrics = metrics, thresholds = thresholds, passed = all(checks), checks = checks, results = results,
    catalog_id = catalog$content_id)
}
