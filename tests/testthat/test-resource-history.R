test_that("retained bundled resources resolve old project pins after a base change", {
  original_read <- read_document
  old <- names(resource_history())[[1]]
  original <- resource_snapshot_by_id(old)
  local_mocked_bindings(read_document = function(file) {
    value <- original_read(file)
    if (identical(file, resource_file("extdata", "resource-manifest.json"))) value$content_id <- strrep("f", 64)
    value
  })
  retained <- resource_snapshot_by_id(old)
  expect_identical(retained$id, old)
  expect_identical(retained$sha256, original$sha256)
  expect_match(retained$file, "resource-history", fixed = TRUE)
  expect_identical(readBin(retained$file, "raw", file.info(retained$file)$size),
    readBin(original$file, "raw", file.info(original$file)$size))
  manifest <- snapshot_manifest(read_catalog(resource_file("extdata", "api-catalog.json.gz"))$content_id, old)
  expect_identical(validate_manifest(manifest), manifest)
  expect_identical(validate_snapshot_content(manifest), manifest)
  project <- new_parent()
  writeLines(json_text(list(resource_snapshot = old)), file.path(project, "cttir-lock.json"))
  expect_identical(resource_snapshot(project)$sha256, original$sha256)
  expect_equal(resources("Seurat", path = project, limit = 1)$name, "Seurat")
  for (id in list("../outside", "sha256:unknown", NA_character_, c(old, old), NULL)) {
    expect_error(resource_snapshot_by_id(id), class = "cttir_source_unavailable")
  }
})

test_that("retained resources are verified before use", {
  old <- names(resource_history())[[1]]
  original_read <- read_document
  original_file <- resource_file
  damaged <- file.path(new_parent(), "damaged.sqlite")
  writeLines("corrupted", damaged)
  local_mocked_bindings(read_document = function(file) {
    value <- original_read(file)
    if (identical(file, original_file("extdata", "resource-manifest.json"))) value$content_id <- strrep("f", 64)
    value
  }, resource_file = function(...) {
    parts <- c(...)
    if ("resource-history" %in% parts && any(grepl("[.]sqlite$", parts))) damaged else original_file(...)
  })
  expect_error(resource_snapshot_by_id(old), class = "cttir_catalog_corrupt")
})

test_that("resource maintainer fields distinguish matched descriptions from unknown history", {
  matched <- resources(limit = 10000)
  expect_equal(sum(!is.na(matched$maintainer_description_sha256)), 16L)
  expect_true(all(matched$maintainer_evidence_status[!is.na(matched$maintainer_evidence_status)] ==
    "description_hash_matched_not_ownership_verification"))
  expect_false(is.na(resources("BPCells", limit = 1)$maintainer_description_sha256))
  expect_true(is.na(resources("BPCells", limit = 1)$maintainer))
  parent <- new_parent()
  writeLines(json_text(list(resource_snapshot = names(resource_history())[[1]])), file.path(parent, "cttir-lock.json"))
  historical <- resources(path = parent, limit = 10000)
  expect_true(all(is.na(historical$maintainer_description_sha256)))
  expect_identical(sort(matched$name), sort(historical$name))
  expect_true(resource_json_parity()$hash_matches)
  expect_length(resource_json_parity()$mismatched_tables, 0L)
})
