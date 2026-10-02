test_that("static extraction never executes code and preserves unresolved exports", {
  parent <- new_parent()
  sentinel <- file.path(parent, "should-not-exist")
  code <- c(
    paste0("writeLines('executed', ", encodeString(sentinel, quote = '"'), ")"),
    "keep <- function(x, value = stop('do not run default')) x", "old <- function(...) NULL"
  )
  source <- fixture_source(file.path(parent, "source"), code, c("old", "keep", "dynamic"))
  p <- extract_source(source, "fixture://cttirFixtureA", "v1")
  expect_false(file.exists(sentinel))
  expect_equal(p$coverage$exports, 3)
  expect_equal(p$coverage$resolved, 2)
  expect_false(any(vapply(p$exports, function(x) x$approved, logical(1))))
  unresolved <- Filter(function(x) x$name == "dynamic", p$exports)[[1]]
  expect_equal(unresolved$verification, "unknown")
  expect_match(Filter(function(x) x$name == "keep", p$exports)[[1]]$signature, "stop", fixed = TRUE)
  file <- file.path(parent, "catalog.json.gz")
  id <- write_catalog(list(p), file)
  expect_equal(read_catalog(file)$content_id, id)
  writeLines("corrupt", file)
  expect_error(read_catalog(file), class = "cttir_catalog_corrupt")
})

test_that("the public catalog exposes revision-scoped evidence and approvals only where reviewed", {
  p <- packages()
  expect_gte(nrow(p), 28)
  expect_true(all(p$approved[p$provider == "CTTIR"] == 0L))
  expect_true(any(p$approved > 0L))
  hit <- search("reflowR::reflow_init")
  expect_equal(nrow(hit), 1L)
  expect_match(hit$snippet, "git = TRUE", fixed = TRUE)
  expect_match(hit$evidence, "/blob/[a-f0-9]{40}/R/reflow_init.R")
  expect_equal(hit$verification, "static_api_verified")
  expect_equal(nrow(search("reflowR::reflow_init", packages = "annotatR")), 0L)
  expect_equal(nrow(search("' OR 1=1 --")), 0L)
  expect_equal(ask("reflowR::reflow_init")$code, "")
  expect_error(search("", limit = 1), class = "cttir_input_error")
  expect_error(search("reflowR", limit = Inf), class = "cttir_input_error")
  parent <- new_parent()
  project <- project("Catalog pin", "methods", "Goal", parent)
  expect_equal(search("reflowR::reflow_init", path = project$path), hit)
})

test_that("multiple definitions in one source file cannot claim a verified signature", {
  f <- static_functions("f <- function(x) x; f <- function(y) y", "R/api.R")
  expect_match(f$f$signature, "unresolved", fixed = TRUE)
})

test_that("reassignment and conditional writes cannot retain verified callables", {
  cases <- c(
    "f <- function(x) x; f <- 1",
    "f <- 1; f <- function(x) x",
    "f <- function(x) x; if (TRUE) f <- function(y) y",
    "f <- function(x) x; formals(f) <- alist(y = )",
    "f <- function(x) x; assign('f', 1)",
    "if (FALSE) f <- function(x) x"
  )
  for (code in cases) {
    f <- static_functions(code, "R/api.R")
    expect_match(f$f$signature, "unresolved", fixed = TRUE)
    expect_length(f$f$arguments, 0L)
  }
  f <- static_functions("f <- function(x) { f <- 1; x }", "R/api.R")
  expect_equal(f$f$signature, "function(x)")
  parent <- new_parent()
  source <- fixture_source(file.path(parent, "source"), "f <- function(x) x", "f")
  writeLines("f <- NULL", file.path(source, "R/overwrite.R"))
  entry <- extract_source(source, "fixture", "v1")
  expect_equal(entry$exports[[1]]$verification, "unknown")
  expect_equal(entry$exports[[1]]$kind, "unresolved_export")
  expect_length(entry$exports[[1]]$arguments, 0L)
  expect_equal(entry$coverage$resolved, 0L)
})

test_that("reindexed bundled evidence retains the previous immutable snapshot", {
  current <- resolve_catalog()
  previous <- catalog_snapshot("8e5a591daa5b55552bc7ca52a659fe422ec369aeab19f87adf9de60f94971788")
  expect_false(identical(current$content_id, previous$content_id))
  expect_true(all(vapply(current$packages, function(x) identical(x$static_assignment_version, 2L), logical(1))))
  expect_true(all(vapply(previous$packages, function(x) is.null(x$static_assignment_version), logical(1))))
  hit <- search("delphyr::accept_panel_invitation")
  expect_match(hit$evidence, "/packages/delphyr/R/invitations.R", fixed = TRUE)
  cttir <- Filter(function(x) identical(x$family, "CTTIR"), current$packages)
  expect_equal(sum(vapply(cttir, function(x) x$coverage$approved, numeric(1))), 0)
  expect_equal(sum(vapply(previous$packages, function(x) x$coverage$approved, numeric(1))), 0)
})

test_that("citations name the pinned version or commit, never a moving package page", {
  catalog <- resolve_catalog()
  a <- ask("Fit a Cox regression and report the hazard ratio")
  survival <- Filter(function(p) identical(p$name, "survival"), catalog$packages)[[1]]
  cites <- a$evidence$citation[a$evidence$package == "survival"]
  expect_true(length(cites) > 0L)
  expect_true(all(grepl(paste0("survival_", survival$version, ".tar.gz#survival/man/"), cites, fixed = TRUE)))
  expect_false(any(grepl("cran.r-project.org/package=", unlist(a$citations), fixed = TRUE)))
  base_r <- Filter(function(p) identical(p$name, "stats"), catalog$packages)[[1]]
  stats <- a$evidence$citation[a$evidence$package == "stats"]
  expect_true(all(grepl(paste0("/R-", base_r$version, ".tar.gz#src/library/stats/man/"), stats, fixed = TRUE)))
  for (i in seq_len(nrow(a$evidence))) {
    p <- Filter(function(x) identical(x$name, a$evidence$package[[i]]), catalog$packages)[[1]]
    expect_true(grepl(catalog_revision_token(p), a$evidence$citation[[i]], fixed = TRUE))
  }
  moving <- list(name = "x", version = "1.0", repository = "https://cran.r-project.org/package=x", revision = "cran:x@1.0:abc")
  expect_identical(catalog_evidence_url(moving, "man/f.Rd"), "https://cran.r-project.org/package=x#x_1.0/man/f.Rd")
})

test_that("nested source citations add exactly one directory prefix", {
  p <- list(repository = "https://github.com/example/project", revision = "exact-revision", source_subdir = "packages/nested")
  expected <- "https://github.com/example/project/blob/exact-revision/packages/nested/R/api.R"
  expect_identical(catalog_evidence_url(p, "R/api.R"), expected)
  expect_identical(catalog_evidence_url(p, "packages/nested/R/api.R"), expected)
  p$source_subdir <- "packages/nested/"
  expect_identical(catalog_evidence_url(p, "R/api.R"), expected)
  expect_identical(catalog_evidence_url(p, "packages/nested/R/api.R"), expected)
})

test_that("the bundled catalog verifies and serializes identically in a C locale", {
  skip_if_not_installed("callr")
  parent <- new_parent()
  source <- fixture_source(file.path(parent, "source"), "keep <- function(x = 1) x", "keep")
  utf8 <- function(...) {
    x <- rawToChar(as.raw(c(...)))
    Encoding(x) <- "UTF-8"
    x
  }
  title <- paste0("Tools by M", utf8(0xc3, 0xbc), "ller")
  description <- c("Package: cttirFixtureA", "Version: 1.0.0", paste("Title:", title), "License: MIT")
  writeBin(charToRaw(enc2utf8(paste0(paste(description, collapse = "\n"), "\n"))), file.path(source, "DESCRIPTION"))
  local <- extract_source(source, "local-source:fixture", "local")
  root <- find.package("cttiR")
  result <- callr::r(function(root, store, source, parent) {
    if (!grepl("^(C|POSIX)$", Sys.getlocale("LC_CTYPE")) || isTRUE(l10n_info()[["UTF-8"]])) return(NULL)
    if (file.exists(file.path(root, "R", "conditions.R"))) {
      pkgload::load_all(root, quiet = TRUE, export_all = TRUE, helpers = FALSE)
    } else {
      library(cttiR, lib.loc = dirname(root))
    }
    ns <- asNamespace("cttiR")
    options(cttiR.catalog_dir = store)
    plan <- cttiR::project("C locale", "methods", "Goal", parent, dry_run = TRUE)
    extracted <- ns$extract_source(source, "local-source:fixture", "local")
    list(id = ns$read_catalog(ns$resource_file("extdata", "api-catalog.json.gz"))$content_id,
      search = nrow(cttiR::search("reflowR::reflow_init")), packages = nrow(cttiR::packages()),
      resources = nrow(cttiR::resources("Seurat", limit = 5L)), plan = class(plan)[[1]],
      answer = class(cttiR::ask("reflowR::reflow_init"))[[1]],
      title = charToRaw(extracted$title), hash = ns$content_hash(ns$json_text(extracted)))
  }, args = list(root = root, store = file.path(parent, "store"), source = source, parent = parent),
  env = c(callr::rcmd_safe_env(), LC_ALL = "C", LANG = "C"))
  skip_if(is.null(result), "The C locale is unavailable on this platform.")
  expect_identical(result$id, read_catalog(resource_file("extdata", "api-catalog.json.gz"))$content_id)
  expect_equal(result$search, 1L)
  expect_gte(result$packages, 28L)
  expect_gte(result$resources, 1L)
  expect_identical(result$plan, "cttir_project")
  expect_identical(result$answer, "cttir_answer")
  expect_identical(result$title, charToRaw(title))
  expect_identical(result$hash, content_hash(json_text(local)))
  expect_false(file.exists(file.path(parent, "c_locale")))
})
