curated_columns <- c("package_id", "name", "category", "purpose", "selection_tier", "tidy_alignment", "interop_expectation",
  "interop_verification", "metadata_verification", "api_verification", "adapter_status", "default_install",
  "eligible_after_adapter_validation", "notes_json", "documentation_url")

curated <- function(path = NULL) {
  x <- unique(resources(path = path, limit = 10000)[, curated_columns])
  x[order(x$package_id), , drop = FALSE]
}

active_db_query <- function(sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), resource_snapshot()$file, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

resource_only_fixture <- function(env = parent.frame()) {
  f <- local_update_fixture(env)
  withr::local_options(cttiR.sources = list(), .local_envir = env)
  f
}

test_that("remote refresh updates observations, reports impacts and preserves curation", {
  f <- resource_only_fixture()
  cran <- bundled_stanzas("CRAN")
  cran$Seurat[["Version"]] <- "9.0.0"
  cran$Seurat[["License"]] <- "GPL-3"
  cran$Seurat[["Depends"]] <- "R (>= 4.6.0), methods, SeuratObject (>= 9.0.0)"
  mock <- local_repository_mock(repository_files(cran = cran))
  old <- curated()
  old_seurat <- resources("Seurat", limit = 1)
  pinned <- project("Pinned resources", "methods", "Goal", f$parent)
  pinned_rows <- resources(path = pinned$path, limit = 10000)
  result <- update(mode = "remote", catalogs = "resources")
  expect_equal(result$status, "succeeded")
  expect_equal(result$catalogs$knowledge$current, result$catalogs$knowledge$previous)
  expect_equal(sum(mock$calls == cran_index_url), 1L)
  expect_equal(sum(mock$calls == bioc_index_url()), 1L)
  seurat <- result$resource_changes[result$resource_changes$package == "Seurat", ]
  expect_true(all(c("version_changed", "license_changed", "dependency_floor_increased") %in% seurat$change))
  expect_true(any(seurat$change == "dependency_floor_increased" & grepl("^R ", seurat$after)))
  expect_true(any(seurat$change == "dependency_floor_increased" & grepl("^SeuratObject", seurat$after)))
  expect_true(any(grepl("resource impact warnings", result$warnings)))
  expect_equal(curated(), old)
  now <- resources("Seurat", limit = 1)
  expect_equal(now$observed_version, "9.0.0")
  expect_equal(now$license, "GPL-3")
  expect_equal(now$r_dependency, "R (>= 4.6.0)")
  expect_equal(now$observed_at, "2026-10-02T00:00:00Z")
  expect_equal(now$lifecycle_status, old_seurat$lifecycle_status)
  obs <- active_db_query("SELECT observation_id, fetch_status, freshness FROM observations WHERE package_id = 'r:Seurat'")
  expect_equal(obs$observation_id, "cran:r:Seurat")
  expect_equal(obs$freshness, "remote_index_checked")
  dep <- active_db_query("SELECT version_constraint FROM dependencies WHERE package_id = 'r:Seurat' AND dependency = 'SeuratObject'")
  expect_equal(dep$version_constraint, ">= 9.0.0")
  expect_equal(resources(path = pinned$path, limit = 10000), pinned_rows)
  expect_equal(update(mode = "remote", catalogs = "resources")$status, "unchanged")
  expect_true(rollback_knowledge(result$previous_id, dry_run = FALSE)$activation)
  expect_equal(resources("Seurat", limit = 1), old_seurat)
  fns <- Filter(is.function, mget(ls(asNamespace("cttiR"), all.names = TRUE), envir = asNamespace("cttiR")))
  symbols <- unique(unlist(lapply(fns, function(fn) all.names(body(fn)))))
  expect_false(any(c("install.packages", "BiocManager", "sys.source") %in% symbols))
})

test_that("absence from an index is lifecycle evidence and a renamed package stays new", {
  resource_only_fixture()
  cran <- bundled_stanzas("CRAN")
  renamed <- cran$renv
  renamed[["Package"]] <- "renvNext"
  cran$renv <- NULL
  cran$renvNext <- renamed
  mock <- local_repository_mock(repository_files(cran = cran))
  result <- update(mode = "remote", catalogs = "resources")
  missing <- result$resource_changes[result$resource_changes$package == "renv", ]
  expect_equal(missing$change, "missing_from_selected_index")
  expect_match(missing$impact, "not proof of retirement")
  renv <- resources("renv", limit = 10)
  renv <- renv[renv$name == "renv", ]
  expect_equal(nrow(renv), 1L)
  expect_equal(renv$lifecycle_status, "missing_from_selected_index")
  expect_equal(renv$observed_version, "1.2.4")
  expect_equal(nrow(resources("renvNext", limit = 10)), 0L)
  evidence <- active_db_query("SELECT status, url FROM evidence WHERE evidence_id = 'index:cran:r:renv'")
  expect_equal(evidence$status, "missing_from_selected_index")
  expect_equal(evidence$url, cran_index_url)
  expect_equal(update(mode = "remote", catalogs = "resources")$status, "unchanged")
  mock$files <- repository_files()
  again <- update(mode = "remote", catalogs = "resources")
  renv_changes <- again$resource_changes$change[again$resource_changes$package == "renv"]
  expect_true(any(c("listed_in_selected_index", "observation_refreshed") %in% renv_changes))
  expect_equal(resources("renv", limit = 1)$lifecycle_status, "listed_in_selected_repository")
})

test_that("an unavailable required index fails both catalogs while an optional one is partial", {
  f <- resource_only_fixture()
  local_repository_mock()
  update(mode = "remote", catalogs = "resources")
  pointer <- read_document(file.path(f$store, "active.json"))
  before <- resources(limit = 10000)
  local_repository_mock(fail = cran_index_url)
  err <- expect_error(update(mode = "remote"), class = "cttir_source_unavailable")
  expect_equal(err$report$status, "failed")
  statuses <- vapply(err$report$resource_sources, function(x) x$status, character(1))
  expect_true("unavailable" %in% statuses)
  expect_equal(read_document(file.path(f$store, "active.json")), pointer)
  expect_equal(resources(limit = 10000), before)
  withr::local_options(cttiR.resource_refresh = list(optional = "cran"))
  result <- update(mode = "remote", catalogs = "resources")
  expect_equal(result$status, "partial")
  expect_true(result$partial)
  expect_true(result$activation)
  expect_true(any(grepl("Optional cran index was unavailable", result$warnings)))
  stale <- active_db_query("SELECT fetch_status, freshness, observed_version FROM observations WHERE observation_id = 'cran:r:Seurat'")
  expect_equal(stale$fetch_status, "unavailable_observation")
  expect_equal(stale$freshness, "source_unavailable")
  expect_equal(resources("Seurat", limit = 1)$lifecycle_status, "listed_in_selected_repository")
  bioc <- active_db_query("SELECT fetch_status FROM observations WHERE observation_id = 'bioc-3.23:r:SummarizedExperiment'")
  expect_equal(bioc$fetch_status, "listed_in_selected_index")
  expect_equal(update(mode = "remote", catalogs = "resources")$status, "partial")
})

test_that("conflicting dual-repository listings are stored separately with a warning", {
  resource_only_fixture()
  bioc <- bundled_stanzas("Bioconductor")
  bioc$Seurat <- c(Package = "Seurat", Version = "0.0.1", License = "MIT", MD5sum = paste(rep("a", 32), collapse = ""))
  local_repository_mock(repository_files(bioc = bioc))
  result <- update(mode = "remote", catalogs = "resources")
  conflict <- result$resource_changes[result$resource_changes$change == "repository_conflict", ]
  expect_equal(conflict$package, "Seurat")
  expect_equal(conflict$after, "0.0.1")
  rows <- resources("Seurat", limit = 10)
  rows <- rows[rows$name == "Seurat", ]
  expect_setequal(rows$repository, c("CRAN", "Bioconductor"))
  expect_equal(rows$observed_version[rows$repository == "CRAN"], "5.5.1")
  secondary <- active_db_query("SELECT fetch_status FROM observations WHERE observation_id = 'bioc-3.23:r:Seurat'")
  expect_equal(secondary$fetch_status, "secondary_repository_listing")
  deps <- active_db_query("SELECT count(*) AS n FROM dependencies WHERE package_id = 'r:Seurat'")$n
  expect_gt(deps, 0)
})

test_that("user exclusions keep curated and observed fields untouched", {
  resource_only_fixture()
  cran <- bundled_stanzas("CRAN")
  cran$Seurat[["Version"]] <- "9.9.9"
  local_repository_mock(repository_files(cran = cran))
  withr::local_options(cttiR.resource_refresh = list(exclude = "Seurat"))
  old <- resources("Seurat", limit = 1)
  result <- update(mode = "remote", catalogs = "resources")
  expect_false("Seurat" %in% result$resource_changes$package)
  expect_equal(resources("Seurat", limit = 1), old)
  expect_error(withr::with_options(list(cttiR.resource_refresh = list(exclude = "bad name")), update(mode = "remote", dry_run = TRUE)),
    class = "cttir_input_error")
})

test_that("metadata-only S4 claims and discovered candidates stay unverified and quarantined", {
  resource_only_fixture()
  bioc <- bundled_stanzas("Bioconductor")
  bioc$SummarizedExperiment[["Depends"]] <- "R (>= 4.0.0), methods, S4Vectors (>= 0.99.0)"
  bioc$cytoFixtureS4 <- c(Package = "cytoFixtureS4", Version = "1.0.0", License = "MIT", MD5sum = paste(rep("b", 32), collapse = ""),
    Title = "S4 classes for cytometry", biocViews = "FlowCytometry, Software")
  bioc$viewOnlyFixture <- c(Package = "viewOnlyFixture", Version = "1.0.0", License = "MIT", MD5sum = paste(rep("c", 32), collapse = ""),
    biocViews = "Software, SingleCellFixtureView")
  cran <- bundled_stanzas("CRAN")
  for (name in c("cytoFixtureA", "cytoFixtureB", "cytoFixtureC")) {
    cran[[name]] <- c(Package = name, Version = "0.1.0", License = "GPL-3", MD5sum = paste(rep("d", 32), collapse = ""))
  }
  mock <- local_repository_mock(repository_files(cran = cran, bioc = bioc))
  withr::local_options(cttiR.discovery = list(keywords = "cytofixture", biocViews = "SingleCellFixtureView", limit = 3L))
  before <- resources("SummarizedExperiment", limit = 1)
  plain <- update(mode = "remote", catalogs = "resources")
  expect_false("discovered_candidate" %in% plain$resource_changes$change)
  expect_equal(nrow(resources("cytoFixture", limit = 10)), 0L)
  after <- resources("SummarizedExperiment", limit = 1)
  for (field in c("api_verification", "interop_verification", "adapter_status", "metadata_verification", "selection_tier")) {
    expect_equal(after[[field]], before[[field]])
  }
  expect_equal(after$api_verification, "not_function_level_verified")
  result <- update(mode = "remote", catalogs = "resources", discover = TRUE)
  found <- result$resource_changes[result$resource_changes$change == "discovered_candidate", ]
  expect_equal(nrow(found), 3L)
  expect_equal(found$package, c("cytoFixtureA", "cytoFixtureB", "cytoFixtureC"))
  expect_true("discovery_limit_reached" %in% result$resource_changes$change)
  rows <- resources("cytoFixture", limit = 10)
  expect_true(all(rows$selection_tier == "discovered_candidate"))
  expect_true(all(rows$api_verification == "not_function_level_verified"))
  expect_true(all(rows$adapter_status == "not_implemented"))
  expect_true(all(rows$default_install == 0L & rows$eligible_after_adapter_validation == 0L))
  second <- update(mode = "remote", catalogs = "resources", discover = TRUE)
  added <- second$resource_changes$package[second$resource_changes$change == "discovered_candidate"]
  expect_setequal(added, c("cytoFixtureS4", "viewOnlyFixture"))
  s4 <- resources("cytoFixtureS4", limit = 1)
  expect_equal(s4$selection_tier, "discovered_candidate")
  expect_equal(s4$api_verification, "not_function_level_verified")
  expect_equal(s4$interop_verification, "not_reviewed")
  expect_equal(resources("cytoFixtureA", limit = 1)$selection_tier, "discovered_candidate")
  calls <- length(mock$calls)
  expect_error(update(discover = TRUE, dry_run = TRUE), class = "cttir_source_unavailable")
  expect_error(update(mode = "remote", catalogs = "knowledge", discover = TRUE, dry_run = TRUE), class = "cttir_input_error")
  withr::local_options(cttiR.discovery = list(keywords = "x", limit = 101L))
  expect_error(update(mode = "remote", discover = TRUE, dry_run = TRUE), class = "cttir_input_error")
  expect_equal(length(mock$calls), calls)
})

test_that("a failed second-catalog activation leaves both previous snapshots active", {
  f <- local_update_fixture()
  mock <- local_repository_mock()
  update(mode = "remote")
  pointer <- read_document(file.path(f$store, "active.json"))
  api <- resolve_catalog()$content_id
  res <- resource_snapshot()$id
  writeLines("keep <- function(x = 5) x", file.path(f$source, "R", "api.R"))
  cran <- bundled_stanzas("CRAN")
  cran$Seurat[["Version"]] <- "7.0.0"
  mock$files <- repository_files(cran = cran)
  local_mocked_bindings(retain_resource_snapshot = function(...) abort_cttir("injected resource snapshot failure", "cttir_catalog_corrupt"))
  err <- expect_error(update(mode = "remote"), class = "cttir_catalog_corrupt")
  expect_false(err$report$activation)
  expect_equal(read_document(file.path(f$store, "active.json")), pointer)
  expect_equal(resolve_catalog()$content_id, api)
  expect_equal(resource_snapshot()$id, res)
  expect_equal(resources("Seurat", limit = 1)$observed_version, "5.5.1")
  expect_equal(nrow(search("cttirFixtureA::old")), 1L)
  expect_false(dir.exists(file.path(f$store, "write-lock")))
})

test_that("Bioconductor release policy stages new releases without touching pins", {
  f <- resource_only_fixture()
  mock <- local_repository_mock(c(repository_files(), repository_files(release = "3.22")))
  for (bad in list("release", "devel", "9.99", "3.24", c("3.22", "3.23"), NA_character_, 3.22)) {
    expect_error(update(mode = "remote", bioc_version = bad, dry_run = TRUE), class = "cttir_input_error")
  }
  expect_error(update(bioc_version = "3.22", dry_run = TRUE), class = "cttir_source_unavailable")
  expect_length(mock$calls, 0L)
  expect_equal(bioc_release_policy()$release, "3.23")
  expect_equal(bioc_release_policy()$source, "derived_from_running_r")
  first <- update(mode = "remote", catalogs = "resources")
  expect_equal(first$bioc_release$release, "3.23")
  pinned <- project("Release pinned", "methods", "Goal", f$parent)
  pinned_rows <- resources(path = pinned$path, limit = 10000)
  pinned_files <- tree_hashes(pinned$path)
  plan <- update(mode = "remote", catalogs = "resources", bioc_version = "3.22", dry_run = TRUE)
  expect_equal(plan$status, "planned")
  expect_false(file.exists(file.path(f$store, "bioc-policy.json")))
  result <- update(mode = "remote", catalogs = "resources", bioc_version = "3.22")
  expect_equal(result$status, "succeeded")
  expect_true(result$bioc_release$recorded)
  expect_false(result$bioc_release$compatible)
  expect_true(any(grepl("Bioconductor 3.22 targets R 4.5", result$warnings, fixed = TRUE)))
  expect_true(bioc_index_url("3.22") %in% mock$calls)
  se <- resources("SummarizedExperiment", limit = 10)
  se <- se[se$name == "SummarizedExperiment", ]
  expect_setequal(se$bioconductor_release, c("3.22", "3.23"))
  ids <- active_db_query("SELECT observation_id FROM observations WHERE package_id = 'r:SummarizedExperiment'")$observation_id
  expect_setequal(ids, c("bioc-3.22:r:SummarizedExperiment", "bioc-3.23:r:SummarizedExperiment"))
  expect_equal(read_document(file.path(f$store, "bioc-policy.json"))$release, "3.22")
  expect_equal(bioc_release_policy()$source, "store_policy")
  expect_equal(resources(path = pinned$path, limit = 10000), pinned_rows)
  expect_equal(tree_hashes(pinned$path), pinned_files)
  mock$calls <- character()
  expect_equal(update(mode = "remote", catalogs = "resources")$status, "unchanged")
  expect_true(bioc_index_url("3.22") %in% mock$calls)
  expect_false(bioc_index_url("3.23") %in% mock$calls)
  expect_error(update(mode = "remote", catalogs = "resources", bioc_version = "3.21"), class = "cttir_source_unavailable")
  expect_equal(read_document(file.path(f$store, "bioc-policy.json"))$release, "3.22")
  writeLines('{"schema_version": 1, "release": "release"}', file.path(f$store, "bioc-policy.json"))
  expect_error(bioc_release_policy(), class = "cttir_catalog_corrupt")
  expect_error(update(mode = "remote", dry_run = TRUE), class = "cttir_catalog_corrupt")
})
