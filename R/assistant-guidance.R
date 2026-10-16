assistant_guidance_bundle <- function() {
  document <- read_document(resource_file("extdata", "assistant-guidance.json"))
  id <- document$content_id
  document$content_id <- NULL
  if (!identical(id, content_hash(json_text(document))) || !identical(document$schema_version, 1L)) {
    abort_cttir("Bundled project guidance failed its integrity check.", "cttir_catalog_corrupt")
  }
  document$content_id <- id
  document
}

assistant_project_references <- function(question, catalog_id) {
  if (!grepl("\\b(cttir|workflowr|reflowr)\\b", tolower(question), perl = TRUE)) return(list())
  # An explicitly named external callable has its own pinned reference. Generic
  # project/sync documentation must not masquerade as that callable's behavior.
  explicit <- regmatches(question, gregexpr("[A-Za-z][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*", question, perl = TRUE))[[1L]]
  if (length(explicit) && !any(explicit %in% c("cttiR::project", "cttiR::sync"))) return(list())
  document <- assistant_guidance_bundle()
  id <- document$content_id
  lapply(document$documents, function(doc) {
    if (!identical(doc$reference_sha256, content_hash(doc$reference))) {
      abort_cttir("Bundled project reference failed its integrity check.", "cttir_catalog_corrupt")
    }
    list(citation_id = paste0("project:", content_hash(paste(catalog_id, id, doc$source_path))),
      package = "cttiR", version = document$version, revision = id,
      revision_kind = "bundled_guidance_sha256", source_path = doc$source_path,
      source_sha256 = doc$source_sha256, reference_sha256 = doc$reference_sha256,
      evidence_url = NULL, reference = doc$reference, callables = list(),
      documented_symbol = doc$symbol, content_authority = "installed_package_documentation_not_project_readiness")
  })
}
