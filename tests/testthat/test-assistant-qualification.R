test_that("qualification binds helper implementations and static validation rules", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  digest <- strrep("c", 64)
  # Keep the manifest-reader implementation stable while registering a record.
  read_original <- read_document
  testthat::local_mocked_bindings(read_document = function(path, ...) {
    if (identical(path, resource_file("runtime", "manifest.json"))) return(manifest)
    read_original(path, ...)
  })
  context <- assistant_context("Explain dplyr::filter")
  request <- assistant_request(context, "local:qualification")
  manifest$tested_models <- list(list(tag = "local:qualification", digest = digest,
    assistant_qualifications = list(list(status = "qualified_for_assistance",
      context = assistant_qualification_context(context, request, "answer", manifest)))))
  qualified <- function() assistant_model_qualification("local:qualification", digest,
    context, request, "answer")
  expect_identical(qualified(), "qualified_for_assistance")
  # Previously only assistant_context's body was bound; changing this helper
  # could alter evidence while retaining the positive qualification label.
  local({
    testthat::local_mocked_bindings(ask_function_names = function(...) list(symbols = character()))
    expect_identical(qualified(), "not_qualified_for_assistance")
  })
  expect_identical(qualified(), "qualified_for_assistance")
  local({
    testthat::local_mocked_bindings(forbidden_calls = c(forbidden_calls, "synthetic_forbidden_call"))
    expect_identical(qualified(), "not_qualified_for_assistance")
  })
  expect_identical(qualified(), "qualified_for_assistance")
})

test_that("qualification fingerprints bind defaults without depending on cache contents", {
  before <- model_implementation_fingerprint()
  helper <- assistant_reference_candidates
  formals(helper)$limit <- 3L
  local({
    testthat::local_mocked_bindings(assistant_reference_candidates = helper)
    expect_false(identical(model_implementation_fingerprint(), before))
  })
  expect_identical(model_implementation_fingerprint(), before)
  context <- assistant_context("Explain dplyr::filter")
  expect_gt(length(context$references), 0L)
  expect_identical(model_implementation_fingerprint(), before)
})

test_that("qualification records transitive validation and transport dependencies", {
  dependencies <- model_dependency_versions()
  expect_true(all(c("V8", "curl", "openssl") %in% names(dependencies)))
  expect_identical(dependencies$V8, as.character(utils::packageVersion("V8")))
  before <- model_implementation_fingerprint()
  metadata <- dependencies
  # Keep the metadata provider's body stable, then change the observed version.
  testthat::local_mocked_bindings(model_dependency_versions = function() metadata)
  bound <- model_implementation_fingerprint()
  metadata$V8 <- "changed-transitive-version"
  expect_false(identical(model_implementation_fingerprint(), bound))
  expect_true(is.character(before) && nchar(before) == 64L)
})
