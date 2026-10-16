# Versioned proposal boundary for local specialization. This does not apply or
# evaluate returned content. Validation evidence is assigned by the caller,
# never accepted from model output.
validate_assistant_proposal <- function(proposal, kind, catalog_id, citation_ids) {
  kind <- match.arg(kind, c("answer", "suggestion"))
  scalar_text(catalog_id, "catalog_id")
  if (!is.character(citation_ids) || anyNA(citation_ids)) {
    abort_cttir("citation_ids must be a character vector.")
  }
  value <- validate_document(proposal, paste0("assistant-", kind))
  if (!identical(value$catalog_id, catalog_id)) {
    abort_cttir("Proposal catalog differs from the request pin.", "cttir_schema_error", "catalog_mismatch")
  }
  if (any(!unlist(value$citation_ids, use.names = FALSE) %in% citation_ids)) {
    abort_cttir("Proposal cites evidence outside the request.", "cttir_schema_error", "unknown_citation")
  }
  paths <- character()
  for (change in value$changes) {
    relative_file(change$path)
    # Only manuscript/analysis code can be proposed here. Configuration,
    # dependencies, raw inputs, Git metadata and managed templates have their
    # own reviewed operations. These checks grant no permission to apply.
    if (!grepl("^(analysis/|publications/[a-z0-9-]+/(analysis|manuscript)/).+\\.(R|Rmd|md)$", change$path)) {
      abort_cttir("Suggestion targets a protected or unsupported path.", "cttir_schema_error", "suggestion_path")
    }
    paths <- c(paths, tolower(change$path))
  }
  if (anyDuplicated(paths)) {
    abort_cttir("Suggestion contains duplicate target paths.", "cttir_schema_error", "duplicate_target")
  }
  value
}
