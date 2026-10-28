test_that("suite reference additions preserve API pins and workflow approvals", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  old <- catalog_snapshot("031ca5226128b5a69a347bb775e545c9ab41eba64557b1caf623c4c5a8fc515f")
  current <- catalog_snapshot("7ce5825c796fbde5988f0836ce86bc84d89a90d900e560880399ec1ff5a51c17")
  expect_identical(approved_callables(current), approved_callables(old))
  pick <- function(catalog) Filter(function(p) p$name == "brainwritR", catalog$packages)[[1]]
  before <- pick(old); after <- pick(current)
  expect_identical(before$exports, after$exports)
  expect_identical(before$source_hash, after$source_hash)
  expect_identical(before$source_files, after$source_files)
  expect_true(length(after$documentation_notices) > 0L)
  context <- assistant_context("Explain brainwritR::topic_for", approved_only = FALSE)
  expect_length(context$references, 1L)
  expect_identical(context$references[[1]]$revision, before$revision)
  expect_false(context$references[[1]]$callables[[1]]$workflow_approved)
  expect_length(assistant_context("Explain brainwritR::topic_for", approved_only = TRUE)$references, 0L)
})

test_that("structural review adds only the three revision-scoped callables", {
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  before <- approved_callables(catalog_snapshot("7ce5825c796fbde5988f0836ce86bc84d89a90d900e560880399ec1ff5a51c17"))
  current <- resolve_catalog()
  after <- approved_callables(current)
  added <- after[!after$approval_id %in% before$approval_id, , drop = FALSE]
  expect_setequal(paste0(added$package, "::", added$export),
    c("cellspecR::cs_new", "cellspecR::cs_validate", "gatekeepR::gk_cellspec"))
  kept <- after[after$approval_id %in% before$approval_id, , drop = FALSE]
  rownames(kept) <- NULL; rownames(before) <- NULL
  expect_identical(kept, before)
  expect_true(all(added$role == "cell_structure"))
  for (name in c("cellspecR", "gatekeepR")) {
    p <- Filter(function(p) p$name == name, current$packages)[[1L]]
    expect_true(p$documentation_corpus$coverage$source_manifest_complete)
    expect_true(all(vapply(p$approvals, function(a) identical(approval_coverage(p, a)$state, "complete"), logical(1))))
  }
})
