test_that("rights-aware literal extraction stores inventories without executing code", {
  f <- document_fixture()
  restricted <- extract_source(f$source, "fixture", "one")
  docs <- restricted$documentation_corpus
  expect_equal(docs$coverage$stored, 0L)
  expect_equal(docs$coverage$vignette_status, "discovered")
  expect_true(all(vapply(docs$documents, function(x) is.null(x$content), logical(1))))
  permitted <- extract_source(f$source, "fixture", "one", documentation_rights = "Synthetic fixture authored for testing")
  expect_equal(permitted$documentation_corpus$coverage$stored, 5L)
  expect_true(validate_document_corpus(permitted$documentation_corpus))
  hits <- document_hits(permitted, "vignette_token_alpha")
  expect_length(hits, 1L)
  expect_match(hits[[1]]$snippet, "THIS MUST NEVER EXECUTE", fixed = TRUE)
  expect_false(any(vapply(permitted$exports, function(x) x$approved, logical(1))))
  broken <- permitted$documentation_corpus
  broken$documents[[1]]$content <- "tampered"
  expect_error(validate_document_corpus(broken), class = "cttir_catalog_corrupt")
})

test_that("document-only updates preserve pinned corpus and rollback", {
  f <- document_fixture()
  withr::local_options(cttiR.sources = list(list(id = "fixture", path = f$source,
        documentation_rights = "Synthetic fixture authored for testing")))
  first <- update()
  p <- project("Documentation pins", "methods", "Keep old evidence", f$parent)
  before <- tree_hashes(p$path)
  expect_equal(nrow(search("vignette_token_alpha")), 1L)
  expect_equal(search("vignette_token_alpha")$verification, "documentation_indexed")
  expect_length(ask("vignette_token_alpha")$citations, 0L)
  expect_equal(nrow(ask("vignette_token_alpha", verified_only = FALSE)$evidence), 1L)
  writeLines("news_token_beta", file.path(f$source, "NEWS.md"))
  unlink(file.path(f$source, "vignettes/guide.Rmd"))
  second <- update()
  expect_true(any(second$documentation_diff$path == "vignettes/guide.Rmd" & second$documentation_diff$change == "removed"))
  expect_true(any(second$documentation_diff$path == "NEWS.md" & second$documentation_diff$change == "changed"))
  expect_false(identical(first$new_id, second$new_id))
  expect_equal(nrow(search("news_token_alpha")), 0L)
  expect_equal(nrow(search("vignette_token_alpha")), 0L)
  expect_equal(nrow(search("news_token_beta")), 1L)
  expect_equal(nrow(search("vignette_token_alpha", path = p$path)), 1L)
  expect_equal(tree_hashes(p$path), before)
  expect_true(rollback_knowledge(first$new_id, dry_run = FALSE)$activation)
  expect_equal(nrow(search("news_token_alpha")), 1L)
  expect_equal(update(dry_run = TRUE)$new_id, second$new_id)
})

test_that("oversized and linked documentation abort before activation", {
  f <- document_fixture()
  update()
  pointer <- read_document(file.path(f$store, "active.json"))
  writeBin(as.raw(rep(65L, 1048577L)), file.path(f$source, "NEWS.md"))
  expect_error(update(), class = "cttir_source_unavailable")
  expect_equal(read_document(file.path(f$store, "active.json")), pointer)
  unlink(file.path(f$source, "NEWS.md"))
  if (.Platform$OS.type != "windows") {
    expect_true(file.symlink(tempdir(), file.path(f$source, "vignettes/linked")))
    expect_error(update(), class = "cttir_path_conflict")
    expect_equal(read_document(file.path(f$store, "active.json")), pointer)
  }
})

test_that("retrieval has bounded excerpts and never promotes indexed text", {
  f <- document_fixture()
  writeLines(paste(rep("bounded_token", 5000L), collapse = " "), file.path(f$source, "NEWS.md"))
  entry <- extract_source(f$source, "fixture", "one", documentation_rights = "Synthetic test text")
  hits <- document_hits(entry, "bounded_token")
  expect_lte(nchar(hits[[1]]$snippet), 1200L)
  expect_equal(entry$coverage$approved, 0L)
  expect_equal(entry$documentation_corpus$coverage$approval, "pending")
})

test_that("bundled corpus is licensed and historical catalog pins remain available", {
  catalog <- resolve_catalog()
  entry <- Filter(function(x) x$name == "reflowR", catalog$packages)[[1]]
  expect_true(validate_document_corpus(entry$documentation_corpus))
  expect_gt(entry$documentation_corpus$coverage$stored, 0L)
  expect_true(any(vapply(entry$documentation_corpus$documents, function(x) x$path == "LICENSE.md", logical(1))))
  expect_true(any(vapply(entry$documentation_corpus$documents, function(x) x$kind == "vignette", logical(1))))
  legacy <- catalog_snapshot("b0012136be1f85f1104ab1ecb1c87ad644b28e3b19a40c43bc4e043e53b04f5a")
  old <- Filter(function(x) x$name == "reflowR", legacy$packages)[[1]]
  expect_null(old$documentation_corpus)
  expect_equal(old$exports, entry$exports)
  expect_false(identical(old$source_hash, entry$source_hash))
})

test_that("document evidence cites the exact source revision", {
  hits <- search("color scheme", packages = "reflowR", limit = 100L)
  docs <- hits[hits$kind == "document", ]
  expect_gt(nrow(docs), 0L)
  expect_true(all(grepl("/blob/cd1243a068ff2c8fb6796e34b58f6c6ce6af87e8/", docs$evidence, fixed = TRUE)))
})

test_that("diff failures cannot activate a candidate snapshot", {
  f <- document_fixture()
  update()
  before <- read_document(file.path(f$store, "active.json"))
  writeLines("Changed documentation", file.path(f$source, "NEWS.md"))
  local_mocked_bindings(documentation_diff = function(...) abort_cttir("Injected diff failure", "cttir_catalog_corrupt"))
  expect_error(update(), class = "cttir_catalog_corrupt")
  expect_equal(read_document(file.path(f$store, "active.json")), before)
})
