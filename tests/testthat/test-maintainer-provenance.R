test_that("source extraction retains maintainer metadata without evaluating Authors@R", {
  root <- fixture_source(file.path(new_parent(), "source"), "fit <- function(x) x", "fit")
  sentinel <- file.path(root, "executed")
  description <- file.path(root, "DESCRIPTION")
  cat(paste0("\nAuthor: Fixture Author\nMaintainer: Fixture Maintainer <fixture@example.org>\n",
    "Authors@R: stop('must not evaluate')\n"), file = description, append = TRUE)
  # A blank line separates DCF records; keep this fixture a single record.
  lines <- readLines(description)
  writeLines(lines[nzchar(lines)], description)
  record <- extract_source(root, "https://example.org/source", "fixture")
  evidence <- record$maintainer_evidence
  expect_identical(evidence$maintainer, "Fixture Maintainer <fixture@example.org>")
  expect_identical(evidence$authors_r_literal, "stop('must not evaluate')")
  expect_identical(evidence$description_sha256, digest::digest(file = description, algo = "sha256"))
  expect_identical(evidence$source_hash, record$source_hash)
  expect_identical(evidence$ownership, "not_inferred")
  expect_false(file.exists(sentinel))
  expect_true(companion_provenance_valid(record))
  for (field in c("source_hash", "description_sha256", "maintainer", "author", "authors_r_literal")) {
    changed <- record
    changed$maintainer_evidence[[field]] <- if (field %in% c("source_hash", "description_sha256")) "stale" else ""
    if (field %in% c("author", "authors_r_literal")) {
      changed$maintainer_evidence$author <- ""
      changed$maintainer_evidence$authors_r_literal <- ""
    }
    expect_false(companion_provenance_valid(changed), info = field)
  }
})

test_that("companion approval cannot bypass missing or stale maintainer evidence", {
  local_mocked_bindings(adapter_decisions = function(...) list(list(approval_id = "fixture-approved")))
  for (name in c("SeuratDisk", "BPCells", "presto", "glmGamPoi")) {
    cap <- list(packages = name, adapter = list(id = "fixture", version = "1"), callables = list())
    record <- list(name = name, source_hash = strrep("a", 64), source_files = list(DESCRIPTION = strrep("b", 64)))
    pending <- capability_approval(cap, list(packages = list(record)))
    expect_identical(pending$status, "approval_pending")
    expect_contains(pending$missing, paste0(name, ":maintainer_provenance"))
    record$maintainer_evidence <- list(source_hash = record$source_hash,
      description_file = "DESCRIPTION", description_sha256 = record$source_files$DESCRIPTION,
      author = "Fixture Author", maintainer = "Fixture Maintainer", extraction = "dcf_text_no_execution")
    expect_identical(capability_approval(cap, list(packages = list(record)))$status, "approved")
    record$maintainer_evidence$source_hash <- strrep("c", 64)
    expect_identical(capability_approval(cap, list(packages = list(record)))$status, "approval_pending")
  }
})
