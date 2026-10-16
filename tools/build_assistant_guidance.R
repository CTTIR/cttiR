# Bundle literal help sources for the installed project's public operations.
pkgload::load_all(quiet = TRUE)
documents <- lapply(c("project", "sync"), function(name) {
  path <- paste0("man/", name, ".Rd")
  reference <- cttiR:::read_source_text(path)
  list(symbol = paste0("cttiR::", name), source_path = path,
    source_sha256 = cttiR:::file_hash(path), reference = reference,
    reference_sha256 = cttiR:::content_hash(reference))
})
value <- list(schema_version = 1L, package = "cttiR", version = read.dcf("DESCRIPTION")[[1, "Version"]],
  scope = "Installed public operation documentation; does not certify an existing project's readiness",
  documents = documents)
contract_path <- "R/assistant-contract.R"
contract_text <- cttiR:::read_source_text(contract_path)
value$policy_documents <- list(list(source_path = contract_path, source_sha256 = cttiR:::file_hash(contract_path),
  reference = contract_text, reference_sha256 = cttiR:::content_hash(contract_text)))
value$content_id <- cttiR:::content_hash(cttiR:::json_text(value))
jsonlite::write_json(value, "inst/extdata/assistant-guidance.json", auto_unbox = TRUE, pretty = TRUE)
