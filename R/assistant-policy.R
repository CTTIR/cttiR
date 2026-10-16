assistant_policy_reference <- function(question, catalog_id) {
  pattern <- paste0("cttir-code-1|data/raw/|\\.\\./|fixture_tested|scientifically_reviewed|static_complete|",
    "\\b(raw data|raw input|preview|apply|execute|execution|publish|citation|citations|instruction attack|anweisungsangriff|",
    "rohdaten|rohdatei|vorschau|anwenden|ausf\u00fchren|ausf\u00fchrung|ver\u00f6ffentlichen|quelle|quellen)\\b")
  if (!grepl(pattern, tolower(question), perl = TRUE)) return(NULL)
  file <- resource_file("extdata", "code-policy-v1.json")
  reference <- read_source_text(file)
  source_hash <- file_hash(file)
  reference_hash <- content_hash(reference)
  list(citation_id = paste0("policy:", content_hash(paste(catalog_id, source_hash))), package = "cttiR",
    version = "cttir-code-1", revision = source_hash, revision_kind = "policy_file_sha256",
    source_path = "inst/extdata/code-policy-v1.json", source_sha256 = source_hash,
    reference_sha256 = reference_hash, evidence_url = NULL, reference = reference, callables = list(),
    content_authority = "versioned_policy_contract_not_implementation_evidence")
}

assistant_policy_contract_references <- function(catalog_id) {
  document <- assistant_guidance_bundle()
  lapply(document$policy_documents, function(doc) {
    if (!identical(doc$reference_sha256, content_hash(doc$reference))) {
      abort_cttir("Bundled validation source failed its integrity check.", "cttir_catalog_corrupt")
    }
    list(citation_id = paste0("contract:", content_hash(paste(catalog_id, document$content_id, doc$source_path))),
      package = "cttiR", version = document$version, revision = document$content_id,
      revision_kind = "bundled_guidance_sha256", source_path = doc$source_path,
      source_sha256 = doc$source_sha256, reference_sha256 = doc$reference_sha256,
      evidence_url = NULL, reference = doc$reference, callables = list(),
      content_authority = "implementation_source_not_execution_evidence")
  })
}
