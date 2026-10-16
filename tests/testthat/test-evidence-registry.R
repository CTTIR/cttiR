evidence_fixture <- function(family = "packages") {
  root <- tempfile("evidence-registry-"); dir.create(root)
  source <- file.path(root, "source.txt")
  text <- "Independent observations and regression assumptions require scientific review."
  write_bytes(text, source)
  record <- list(schema_version = 1L, evidence_id = paste0("evidence-v1:", strrep("a", 64)),
    source_id = "source:fixture", family = family, package = "Example", version = "1.2.3",
    revision = "pinned-test", document_sha256 = file_hash(source), text = text,
    text_sha256 = content_hash(text), source_pins = list(list(path = source, sha256 = file_hash(source))),
    restriction = NULL, permissions = "evidence_only_no_execution_or_approval",
    training_admission = "not_granted_by_indexing", claim_support = "not_established_by_retrieval",
    locator = list(kind = "Rd_topic", topic = "regress"), version_scope = "pinned_source_package_not_installed_runtime")
  restricted <- record; restricted$evidence_id <- paste0("evidence-v1:", strrep("b", 64))
  restricted$restriction <- "incomplete_document"
  db <- DBI::dbConnect(RSQLite::SQLite(), file.path(root, "evidence.sqlite"))
  DBI::dbExecute(db, "CREATE TABLE evidence(evidence_id TEXT PRIMARY KEY, source_id TEXT, family TEXT, package TEXT, version TEXT, restricted INTEGER, record TEXT)")
  DBI::dbExecute(db, "CREATE VIRTUAL TABLE search USING fts5(evidence_id UNINDEXED, text, tokenize='unicode61')")
  for (r in list(record, restricted)) {
    DBI::dbExecute(db, "INSERT INTO evidence VALUES(?,?,?,?,?,?,?)", params = list(r$evidence_id,
      r$source_id, r$family, r$package, r$version, as.integer(!is.null(r$restriction)), json_text(r)))
    DBI::dbExecute(db, "INSERT INTO search VALUES(?,?)", params = list(r$evidence_id, r$text))
  }
  DBI::dbDisconnect(db)
  write_bytes("{}\n", file.path(root, "coverage.jsonl"))
  manifest <- list(schema_version = 1L, files_sha256 = list(
    evidence.sqlite = file_hash(file.path(root, "evidence.sqlite")),
    coverage.jsonl = file_hash(file.path(root, "coverage.jsonl"))), limits = list("Test fixture only"))
  write_bytes(json_text(manifest), file.path(root, "manifest.json"))
  list(directory = root, manifest_sha256 = file_hash(file.path(root, "manifest.json")), record = record,
    restricted = restricted, source = source)
}

test_that("evidence retrieval pins source versions and excludes restricted records", {
  x <- evidence_fixture()
  found <- evidence_search(x$directory, x$manifest_sha256, "regression", package = "Example", version = "1.2.3")
  expect_length(found$results, 1L)
  expect_identical(found$results[[1]]$evidence, x$record)
  expect_false(found$execution_authorized)
  expect_identical(evidence_resolve(x$directory, x$manifest_sha256, x$record$evidence_id), x$record)
  expect_length(evidence_search(x$directory, x$manifest_sha256, "regression", package = "Example", version = "2.0")$results, 0L)
  expect_error(evidence_resolve(x$directory, x$manifest_sha256, x$restricted$evidence_id), "Restricted")
  expect_error(evidence_search(x$directory, x$manifest_sha256, "regression", version = "1.2.3"), "requires a package")
  expect_length(evidence_search(x$directory, x$manifest_sha256, "regression", package = "Example' OR 1=1 --")$results, 0L)
})

test_that("registry, source and manifest mutations fail closed", {
  x <- evidence_fixture()
  expect_error(evidence_search(x$directory, strrep("0", 64), "regression"), "external pin")
  write_bytes("Changed source", x$source)
  expect_error(evidence_resolve(x$directory, x$manifest_sha256, x$record$evidence_id), "source has changed")
  x <- evidence_fixture(); cat("changed", file = file.path(x$directory, "coverage.jsonl"), append = TRUE)
  expect_error(evidence_search(x$directory, x$manifest_sha256, "regression"), "artifact has changed")
})

test_that("cited-reference filtering preserves restrictions and exact resolution", {
  x <- evidence_fixture("cited_references")
  found <- evidence_search(x$directory, x$manifest_sha256, "regression", family = "cited_references")
  expect_length(found$results, 1L)
  expect_identical(found$results[[1]]$evidence, x$record)
  expect_identical(evidence_resolve(x$directory, x$manifest_sha256, x$record$evidence_id), x$record)
  expect_length(evidence_search(x$directory, x$manifest_sha256, "regression", family = "packages")$results, 0L)
  expect_error(evidence_resolve(x$directory, x$manifest_sha256, x$restricted$evidence_id), "Restricted")
})

test_that("optional evidence context supplies citations without callable authority", {
  x <- evidence_fixture()
  withr::local_options(cttiR.evidence_registry = x[c("directory", "manifest_sha256")],
    cttiR.catalog_dir = tempfile("unused-catalog-"))
  context <- assistant_context("independent regression assumptions", approved_only = FALSE)
  selected <- Filter(function(r) identical(r$citation_id, x$record$evidence_id), context$references)
  expect_length(selected, 1L); expect_length(selected[[1]]$callables, 0L)
  expect_identical(selected[[1]]$locator$topic, "regress")
  expect_false(context$execution_authorized)
  expect_true(evidence_context_current(context))
  write_bytes("Stale source", x$source)
  expect_false(evidence_context_current(context))
})

test_that("changed external evidence discards a model draft", {
  x <- evidence_fixture()
  withr::local_options(cttiR.evidence_registry = x[c("directory", "manifest_sha256")],
    cttiR.catalog_dir = tempfile("unused-catalog-"))
  local_mocked_bindings(assistant_exchange = function(...) {
    write_bytes("Stale source", x$source)
    list(state = "proposal", proposal = list(summary = "Stale draft"))
  })
  result <- assistant_advice("independent regression assumptions", use_model = TRUE)
  expect_identical(result$reason, "evidence_changed")
  expect_null(result$proposal)
})

test_that("unpinned SQLite journals cannot influence immutable evidence reads", {
  x <- evidence_fixture()
  dbpath <- file.path(x$directory, "evidence.sqlite")
  db <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  on.exit(DBI::dbDisconnect(db), add = TRUE)
  DBI::dbGetQuery(db, "PRAGMA journal_mode=WAL")
  DBI::dbGetQuery(db, "PRAGMA wal_checkpoint(TRUNCATE)")
  # Pin the checkpointed main file, then leave the subsequent edit in WAL.
  manifest <- jsonlite::read_json(file.path(x$directory, "manifest.json"), simplifyVector = FALSE)
  manifest$files_sha256$evidence.sqlite <- file_hash(dbpath)
  write_bytes(json_text(manifest), file.path(x$directory, "manifest.json"))
  pin <- file_hash(file.path(x$directory, "manifest.json"))
  altered <- x$record; altered$text <- "Altered unpinned content"; altered$text_sha256 <- content_hash(altered$text)
  DBI::dbExecute(db, "UPDATE evidence SET record=? WHERE evidence_id=?", params = list(json_text(altered), altered$evidence_id))
  expect_identical(file_hash(dbpath), manifest$files_sha256$evidence.sqlite)
  expect_error(evidence_resolve(x$directory, pin, x$record$evidence_id), "SQLite sidecars")
})
