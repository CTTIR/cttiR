# Keep deterministic advice available independently of the optional local model.
# This boundary returns drafts and evidence only; it never applies suggestions.
assistant_advice <- function(question, path = NULL, language = "en", kind = "answer",
                             use_model = FALSE, endpoint = NULL, model = NULL,
                             allow_unqualified = FALSE, timeout = 120) {
  scalar_flag(use_model, "use_model")
  scalar_flag(allow_unqualified, "allow_unqualified")
  kind <- match.arg(kind, c("answer", "suggestion"))
  if (!is.character(language) || length(language) != 1L || is.na(language) ||
      !language %in% c("en", "de")) abort_cttir("Unsupported response language.")
  question <- utf8_input(question)
  scalar_text(question, "question")
  if (nchar(question, type = "bytes") > 8000L) abort_cttir("Evidence request exceeds its size limits.")
  deterministic <- ask(question, path = path, verified_only = TRUE)
  fallback <- function(reason, exchange = NULL, context = NULL) {
    list(state = "deterministic", reason = reason, deterministic = deterministic,
      proposal = NULL, exchange = exchange, context = context,
      requested_language = language, execution_authorized = FALSE)
  }
  if (!use_model) return(fallback("model_not_requested"))
  context <- tryCatch(assistant_context(question, path = path,
    approved_only = identical(kind, "suggestion")), error = function(e) NULL)
  if (is.null(context)) return(fallback("evidence_unavailable"))
  if (!identical(context$catalog_id, deterministic$catalog_id)) return(fallback("catalog_changed"))
  exchange <- tryCatch(assistant_exchange(context, language = language, kind = kind,
    endpoint = endpoint, model = model, allow_unqualified = allow_unqualified,
    timeout = timeout), error = function(e) NULL)
  if (is.null(exchange)) return(fallback("runtime_unavailable", context = context))
  if (!identical(exchange$state, "proposal")) return(fallback(exchange$reason, exchange, context))
  current <- tryCatch(resolve_catalog(path)$content_id, error = function(e) NULL)
  if (!identical(current, context$catalog_id)) return(fallback("catalog_changed"))
  if (!evidence_context_current(context)) return(fallback("evidence_changed"))
  list(state = "proposal", reason = NULL, deterministic = deterministic,
    proposal = exchange$proposal, exchange = exchange, context = context,
    requested_language = language, execution_authorized = FALSE)
}
