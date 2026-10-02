evidence_fixture <- function(env = parent.frame()) {
  parent <- tempfile("cttir-evidence-")
  dir.create(parent)
  withr::defer(unlink(parent, recursive = TRUE), envir = env)
  sentinel <- file.path(parent, "evaluated")
  source <- fixture_source(file.path(parent, "a"), c(
    paste0("writeLines('evaluated', ", encodeString(sentinel, quote = '"'), ")"),
    "keep <- function(x = 1) x",
    "`%op%` <- function(lhs, rhs) lhs",
    "print.thing <- function(x, ...) invisible(x)",
    "setClass('Thing', representation(x = 'numeric'), prototype(x = stop('never evaluated')))",
    "methods::setValidity('Thing', function(object) TRUE)",
    "Gen <- setClass('Gen', contains = 'Thing')",
    "setClass(paste0('Dyn', 'amic'))",
    "setGeneric('area', function(shape, ...) standardGeneric('area'))",
    "setMethod('area', 'Thing', function(shape, ...) 1)",
    "setMethod('area', signature(shape = 'Gen', extra = 'ANY'), function(shape, ...) 2)",
    "setMethod(generic_name, 'Thing', function(shape, ...) 3)",
    "Point <- S7::new_class('Point', properties = list(x = S7::class_double))",
    "describe <- S7::new_generic('describe', 'x')",
    "Unbound <- new_class('Unbound')",
    "Unbound <- NULL"
  ), c("keep", "helper", "mystery", "area", "Point", "describe", "Gen", "Unbound", "\"%op%\""))
  writeLines(c(readLines(file.path(source, "NAMESPACE")), "importFrom(cttirFixtureB, helper)", "import(cttirFixtureB)",
      "exportClasses(Thing, Dynamic)", "exportMethods(area)", "S3method(print, thing)"), file.path(source, "NAMESPACE"))
  dir.create(file.path(source, "man"))
  writeLines(c("\\name{ops}", "\\alias{\\%op\\%}", "\\alias{keep}", "\\title{Operators}"), file.path(source, "man", "ops.Rd"))
  owner <- fixture_source(file.path(parent, "b"), "helper <- function(x) x", "helper")
  writeLines(sub("cttirFixtureA", "cttirFixtureB", readLines(file.path(owner, "DESCRIPTION")), fixed = TRUE),
    file.path(owner, "DESCRIPTION"))
  list(parent = parent, source = source, owner = owner, sentinel = sentinel)
}

export_named <- function(entry, name) Filter(function(x) identical(x$name, name), entry$exports)[[1]]

test_that("reexports link to their owning package and unknown provenance stays unresolved", {
  f <- evidence_fixture()
  entry <- extract_source(f$source, "fixture://a", "one")
  helper <- export_named(entry, "helper")
  expect_identical(helper$kind, "reexport")
  expect_identical(helper$owner_package, "cttirFixtureB")
  expect_identical(helper$verification, "reexport_declared")
  expect_length(helper$arguments, 0L)
  mystery <- export_named(entry, "mystery")
  expect_identical(mystery$kind, "unresolved_export")
  expect_identical(mystery$verification, "unknown")
  expect_null(mystery$owner_package)
  owner <- extract_source(f$owner, "fixture://b", "one")
  expect_identical(export_named(owner, "helper")$verification, "static_api_verified")
  expect_equal(entry$coverage$reexports, 1L)
  expect_identical(entry$evidence_version, 3L)
  expect_identical(entry$static_assignment_version, 2L)
  expect_false(file.exists(f$sentinel))
})

test_that("S4 declarations are literal, static and never evaluated", {
  f <- evidence_fixture()
  entry <- extract_source(f$source, "fixture://a", "one")
  expect_false(file.exists(f$sentinel))
  s4 <- entry$s4
  expect_identical(s4$verification, "static_declaration_only")
  expect_setequal(vapply(s4$classes, function(x) x$name, character(1)), c("Thing", "Gen"))
  thing <- Filter(function(x) x$name == "Thing", s4$classes)[[1]]
  expect_true(thing$exported)
  expect_identical(thing$verification, "static_declaration_only")
  expect_identical(s4$validity[[1]]$class, "Thing")
  expect_identical(s4$generics[[1]]$signature, "function(shape, ...)")
  expect_length(s4$methods, 2L)
  expect_identical(unlist(s4$methods[[1]]$signature), "Thing")
  expect_identical(unlist(s4$methods[[2]]$signature), c("Gen", "ANY"))
  expect_identical(unlist(s4$methods[[2]]$signature_arguments), c("shape", "extra"))
  expect_true(all(vapply(s4$methods, function(x) x$exported, logical(1))))
  reasons <- vapply(s4$unresolved, function(x) x$reason, character(1))
  expect_setequal(reasons, c("nonliteral_class_name", "nonliteral_generic_name"))
  expect_true(all(vapply(s4$unresolved, function(x) x$verification == "unknown", logical(1))))
  expect_true(any(grepl("paste0", vapply(s4$unresolved, function(x) x$declaration, character(1)), fixed = TRUE)))
  expect_identical(unlist(s4$exports$undeclared_classes), "Dynamic")
  expect_false("Dynamic" %in% vapply(s4$classes, function(x) x$name, character(1)))
  expect_identical(export_named(entry, "area")$kind, "s4_generic")
  expect_identical(export_named(entry, "area")$verification, "static_declaration_only")
  expect_identical(export_named(entry, "Gen")$kind, "s4_class_generator")
  expect_match(s4$limitation, "dispatch", fixed = TRUE)
  expect_equal(entry$coverage$s4_unresolved, 2L)
})

test_that("S7 declarations label exports without claiming callables", {
  f <- evidence_fixture()
  entry <- extract_source(f$source, "fixture://a", "one")
  expect_identical(export_named(entry, "Point")$kind, "s7_class")
  expect_identical(export_named(entry, "Point")$verification, "static_declaration_only")
  expect_identical(export_named(entry, "describe")$kind, "s7_generic")
  expect_identical(unlist(entry$s7$generics[[1]]$dispatch_args), "x")
  expect_identical(entry$s7$classes[[1]]$class_name, "Point")
  # A second binding of the same name keeps the export unresolved.
  expect_identical(export_named(entry, "Unbound")$kind, "unresolved_export")
  expect_equal(entry$coverage$s7_classes, 2L)
  expect_equal(entry$coverage$resolved, 2L)
  expect_false(any(vapply(entry$exports, function(x) x$approved, logical(1))))
})

test_that("Rd-escaped aliases document operator exports", {
  f <- evidence_fixture()
  entry <- extract_source(f$source, "fixture://a", "one")
  op <- export_named(entry, "%op%")
  expect_identical(op$verification, "static_api_verified")
  expect_identical(op$documentation$path, "man/ops.Rd")
})

test_that("non-callable kinds appear in search with their own verification labels", {
  f <- evidence_fixture()
  withr::local_options(cttiR.sources = list(list(id = "a", path = f$source)),
    cttiR.catalog_dir = file.path(f$parent, "store"))
  update()
  hit <- search("cttirFixtureA::helper")
  expect_identical(hit$kind, "reexport")
  expect_identical(hit$verification, "reexport_declared")
  expect_false(hit$approved)
  expect_match(hit$snippet, "cttirFixtureB::helper", fixed = TRUE)
  methods <- search("area", limit = 100L)
  expect_true(all(c("s4_generic", "s4_method") %in% methods$kind))
  expect_true(all(methods$verification[methods$kind == "s4_method"] == "static_declaration_only"))
  expect_true(any(search("Thing", limit = 100L)$kind == "s4_class"))
  expect_identical(search("cttirFixtureA::Point")$verification, "static_declaration_only")
  expect_false(any(ask("area")$evidence$kind %in% c("s4_method", "s4_generic")))
  expect_equal(nrow(ask("cttirFixtureA::helper")$evidence), 0L)
  expect_identical(packages()$approved_roles[packages()$package == "cttirFixtureA"], "")
  expect_false(file.exists(f$sentinel))
})

test_that("oversize rendered vignettes and binary assets are inventoried but not stored", {
  f <- evidence_fixture()
  dir.create(file.path(f$source, "inst", "doc"), recursive = TRUE)
  dir.create(file.path(f$source, "man", "figures"))
  writeLines(c("<html>", strrep("x", 1100000L), "</html>"), file.path(f$source, "inst", "doc", "guide.html"))
  writeBin(as.raw(rep(1L, 1100000L)), file.path(f$source, "man", "figures", "logo.png"))
  entry <- extract_source(f$source, "fixture://a", "one", documentation_rights = "Synthetic fixture")
  docs <- stats::setNames(entry$documentation_corpus$documents,
    vapply(entry$documentation_corpus$documents, function(x) x$path, character(1)))
  expect_identical(docs[["inst/doc/guide.html"]]$storage, "oversize_metadata_only")
  expect_null(docs[["inst/doc/guide.html"]]$content)
  expect_match(docs[["inst/doc/guide.html"]]$source_sha256, "^[a-f0-9]{64}$")
  expect_identical(docs[["man/figures/logo.png"]]$storage, "asset_metadata_only")
  expect_equal(entry$documentation_corpus$coverage$oversize_metadata_only, 1L)
  expect_true(validate_document_corpus(entry$documentation_corpus))
  dir.create(file.path(f$source, "vignettes"))
  writeLines(strrep("y", 1100000L), file.path(f$source, "vignettes", "large.Rmd"))
  expect_error(extract_source(f$source, "fixture://a", "two"), class = "cttir_source_unavailable")
})

test_that("historical snapshots without approval or object evidence remain readable", {
  withr::local_options(cttiR.catalog_dir = file.path(new_parent(), "store"))
  catalog <- resolve_catalog()
  expect_true(all(vapply(catalog$packages, function(x) is.null(x$approvals) && is.null(x$s4), logical(1))))
  expect_true(all(packages()$approved_roles == ""))
  expect_equal(nrow(approved_callables(catalog)), 0L)
  expect_equal(nrow(approval_diff(catalog, catalog)), 0L)
  expect_identical(search("reflowR::reflow_init")$verification, "static_api_verified")
})
