test_that("supplemental descriptions preserve the distinct seed observation", {
  rows <- resources("glmGamPoi", limit = 1L)
  evidence <- jsonlite::fromJSON(rows$supplemental_maintainer_evidence_json, simplifyVector = FALSE)
  expect_identical(evidence$package, "glmGamPoi")
  expect_identical(evidence$version, rows$observed_version)
  expect_identical(evidence$relation_to_seed, "separate_description_same_version")
  expect_identical(evidence$description_sha256, "1a7d65d79e7a9080c883b088342d556959e7bfcf15f0f745931ab6375da2816b")
  expect_true(is.na(rows$maintainer_description_sha256))
  expect_identical(evidence$ownership, "not_inferred")
  expect_true(description_has_maintainer(description_roles(evidence$authors_r_literal)))
  expect_match(evidence$source_url, "#glmGamPoi/DESCRIPTION", fixed = TRUE)
  for (name in c("BPCells", "presto", "SeuratDisk")) {
    x <- resources(name, limit = 1L)
    e <- jsonlite::fromJSON(x$supplemental_maintainer_evidence_json)
    expect_identical(e$relation_to_seed, "description_hash_matches")
    expect_identical(e$description_sha256, x$maintainer_description_sha256)
  }
  expect_true(resource_json_parity()$hash_matches)
  expect_length(resource_json_parity()$mismatched_tables, 0L)
})

test_that("previous resource pins do not acquire later supplemental evidence", {
  parent <- new_parent()
  old <- "sha256:278fba39a5ccfee5ddb0cbf30c1db057d788ea0015538298191b75f9e6a2b385"
  writeLines(json_text(list(resource_snapshot = old)), file.path(parent, "cttir-lock.json"))
  rows <- resources("glmGamPoi", path = parent)
  expect_true(all(is.na(rows$supplemental_maintainer_evidence_json)))
  expect_identical(resource_snapshot_by_id(old)$sha256,
    "1572ca50bcc298f44cdee764024f39957ca5785d569e1bf9093b769dae502f91")
})
