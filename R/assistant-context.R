# Assemble bounded, inert evidence from a project's pinned catalog. This helper
# does not contact a model, read arbitrary project files or execute examples.
assistant_context <- function(question, path = NULL, symbols = NULL, approved_only = TRUE,
                              max_documents = 8L, max_reference_bytes = 24000L,
                              evidence_registry = getOption("cttiR.evidence_registry")) {
  question <- utf8_input(question)
  scalar_text(question, "question")
  scalar_flag(approved_only, "approved_only")
  bounded_integer <- function(x, upper) is.numeric(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x >= 1 && x <= upper && x == floor(x)
  if (nchar(question, type = "bytes") > 8000L || !bounded_integer(max_documents, 16L) ||
      !bounded_integer(max_reference_bytes, 48000L)) abort_cttir("Evidence request exceeds its size limits.")
  catalog <- resolve_catalog(path)
  gaps <- list(); references <- list(); bytes <- 0L
  gap <- function(symbol, reason) gaps[[length(gaps) + 1L]] <<- list(symbol = symbol, reason = reason)
  injected <- instruction_like(question, hard_only = TRUE)
  if (!injected) for (reference in assistant_project_references(question, catalog$content_id)) {
    size <- nchar(reference$reference, type = "bytes")
    if (length(references) < max_documents && bytes + size <= max_reference_bytes) {
      references[[reference$citation_id]] <- reference
      bytes <- bytes + size
    } else gap(reference$documented_symbol, "reference_budget_exceeded")
  }
  if (is.null(symbols)) {
    pattern <- "[A-Za-z][A-Za-z0-9.]*:{2,3}[\\p{L}._][\\p{L}\\p{N}._]*"
    symbols <- ask_namespaced_symbols(question, catalog, pattern)
    if (!injected) {
      # Exact namespaced references and bare calls can coexist in a question.
      # Remove the former before resolving the latter so neither is discarded
      # or mistaken for an ambiguous unqualified name.
      named <- ask_function_names(gsub(pattern, " ", question, perl = TRUE), catalog)
      symbols <- unique(c(symbols, named$symbols))
      for (name in named$absent) gap(name, "export_absent_from_catalog_revision")
      exports <- sub(".*::", "", named$symbols)
      for (name in unique(exports[duplicated(exports)])) gap(name, "ambiguous_unqualified_export")
      # A named but absent function is an explicit evidence gap, not an
      # invitation to substitute lexically similar unrelated APIs.
      if (!length(symbols) && !length(references) && !length(named$absent)) {
        symbols <- assistant_reference_candidates(question, catalog, limit = min(4L, max_documents))
        if (!length(symbols)) symbols <- assistant_reference_candidates(question, catalog,
          limit = min(4L, max_documents), allow_unnamed = TRUE)
        if (length(symbols)) gap("", "lexical_reference_candidates_require_review")
      }
      if (!length(symbols) && !length(references) && !length(named$absent)) {
        advice <- ask(question, path = path, verified_only = approved_only)
        if (nrow(advice$evidence)) symbols <- unique(paste0(advice$evidence$package, "::", advice$evidence$export))
      }
    }
  }
  if (!is.character(symbols) || anyNA(symbols) || length(symbols) > 32L ||
      any(nchar(symbols, type = "bytes") > 240L)) abort_cttir("Invalid evidence symbols.")
  if (injected) {
    gap("", "instruction_request_rejected")
    symbols <- character()
  } else if (!length(symbols)) gap("", "no_revision_scoped_api_match")
  packages <- stats::setNames(catalog$packages, vapply(catalog$packages, function(x) x$name, ""))
  for (symbol in unique(symbols)) {
    if (!grepl("^[A-Za-z][A-Za-z0-9.]*::[\\p{L}._][\\p{L}\\p{N}._]*$", symbol, perl = TRUE)) {
      gap(symbol, "unsupported_symbol"); next
    }
    parts <- strsplit(symbol, "::", fixed = TRUE)[[1L]]
    package <- packages[[parts[[1L]]]]
    entries <- Filter(function(x) identical(x$name, parts[[2L]]), package$exports)
    if (length(entries) != 1L) { gap(symbol, "export_absent_from_catalog_revision"); next }
    entry <- entries[[1L]]
    approved <- !is.null(approved_export_index(package)[[entry$name]]) &&
      identical(entry$verification, "static_api_verified")
    if (approved_only && !approved) { gap(symbol, "workflow_approval_missing"); next }
    if (!identical(entry$verification, "static_api_verified")) { gap(symbol, "callable_identity_unverified"); next }
    docs <- Filter(function(x) identical(x$path, entry$documentation$path) && identical(x$storage, "source_text"),
      package$documentation_corpus$documents)
    if (length(docs) != 1L) { gap(symbol, "reference_text_not_stored"); next }
    doc <- docs[[1L]]
    if (!identical(doc$source_sha256, entry$documentation$sha256) ||
        !identical(content_hash(doc$content), doc$content_sha256)) {
      abort_cttir("Retrieved reference differs from its callable source pin.", "cttir_catalog_corrupt")
    }
    id <- paste0("ref:", content_hash(paste(catalog$content_id, package$source_hash, doc$path, doc$source_sha256, doc$content_sha256)))
    fact <- list(symbol = symbol, signature = entry$signature, verification = entry$verification,
      workflow_approved = approved, dispatch = "requires_separate_validation")
    if (nchar(json_text(fact), type = "bytes") > 4000L) { gap(symbol, "api_metadata_limit"); next }
    if (!is.null(references[[id]])) {
      references[[id]]$callables <- append(references[[id]]$callables, list(fact))
      if (!approved_only && !approved) gap(symbol, "workflow_approval_missing_read_only_reference")
      next
    }
    size <- nchar(doc$content, type = "bytes")
    if (length(references) >= max_documents || bytes + size > max_reference_bytes) {
      gap(symbol, "reference_budget_exceeded"); next
    }
    references[[id]] <- list(citation_id = id, package = package$name, version = package$version,
      revision = package$revision, source_path = doc$path, source_sha256 = doc$source_sha256,
      reference_sha256 = doc$content_sha256,
      evidence_url = catalog_evidence_url(package, doc$path), reference = doc$content,
      callables = list(fact), content_authority = "untrusted_documentation_only")
    bytes <- bytes + size
    if (!approved_only && !approved) gap(symbol, "workflow_approval_missing_read_only_reference")
  }
  policy_reference <- if (!injected) assistant_policy_reference(question, catalog$content_id) else NULL
  if (!is.null(policy_reference)) {
    for (reference in c(list(policy_reference), assistant_policy_contract_references(catalog$content_id))) {
      size <- nchar(reference$reference, type = "bytes")
      if (length(references) < max_documents && bytes + size <= max_reference_bytes) {
        references[[reference$citation_id]] <- reference
        bytes <- bytes + size
      } else gap("cttir-code-1", "reference_budget_exceeded")
    }
  }
  if (!injected && !is.null(evidence_registry)) {
    if (!is.list(evidence_registry) || !setequal(names(evidence_registry), c("directory", "manifest_sha256")))
      abort_cttir("Host evidence configuration requires directory and external manifest_sha256.")
    found <- evidence_search(evidence_registry$directory, evidence_registry$manifest_sha256,
      question, limit = min(8L, max_documents))
    for (row in found$results) {
      reference <- evidence_context_reference(row$evidence)
      size <- nchar(reference$reference, type = "bytes")
      if (length(references) < max_documents && bytes + size <= max_reference_bytes) {
        references[[reference$citation_id]] <- reference; bytes <- bytes + size
      } else gap(reference$citation_id, "reference_budget_exceeded")
    }
    gap("evidence_registry", "Retrieval does not establish claim support, current legal applicability or execution approval.")
  }
  result <- list(schema_version = 1L, catalog_id = catalog$content_id, question = question,
    policy = read_document(resource_file("extdata", "code-policy-v1.json")),
    references = unname(references), gaps = gaps, reference_bytes = bytes,
    token_count = NULL, execution_authorized = FALSE, evidence_registry = evidence_registry)
  if (nchar(json_text(result), type = "bytes") > 96000L) abort_cttir("Evidence context exceeds its total size limit.")
  result
}
