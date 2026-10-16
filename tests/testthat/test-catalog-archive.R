test_that("bundled archive preserves every pinned snapshot identity", {
  index <- catalog_archive_manifest()
  expect_identical(read_bundled_catalog()$content_id, index$current_id)
  current <- system.file("extdata", "api-catalog.json.gz", package = "cttiR")
  if (nzchar(current)) expect_identical(read_bundled_catalog(), read_catalog(current))
  for (record in index$snapshots) {
    recovered <- read_bundled_catalog(record$content_id)
    expect_identical(recovered$content_id, record$content_id)
    # Source checkouts retain original gzip snapshots for exact compatibility
    # comparison. Installed tarballs intentionally contain only the archive.
    original <- system.file("extdata", "history", paste0(record$content_id, ".json.gz"), package = "cttiR")
    if (nzchar(original)) expect_identical(recovered, read_catalog(original))
  }
  expect_error(read_bundled_catalog("../escape"), class = "cttir_catalog_corrupt")
  expect_error(read_bundled_catalog(strrep("0", 64)), class = "cttir_source_unavailable")
})

test_that("bundled archive rejects corrupt ranges and bytes even after caching", {
  source <- resource_file("extdata", "catalog-archive")
  copy <- tempfile("archive-"); dir.create(copy)
  expect_true(all(file.copy(list.files(source, full.names = TRUE), copy)))
  original <- resource_file
  testthat::local_mocked_bindings(resource_file = function(...) {
    parts <- c(...)
    if (length(parts) >= 2L && identical(parts[1:2], c("extdata", "catalog-archive"))) {
      if (length(parts) == 2L) return(copy)
      return(file.path(copy, parts[[3L]]))
    }
    original(...)
  })
  index_file <- file.path(copy, "manifest.json")
  index <- read_document(index_file)
  id <- index$current_id
  expect_identical(read_bundled_catalog(id)$content_id, id)
  altered <- index; altered$snapshots[[1]]$offset <- -1
  jsonlite::write_json(altered, index_file, auto_unbox = TRUE)
  expect_error(read_bundled_catalog(id), class = "cttir_catalog_corrupt")
  altered <- index; altered$snapshots[[2]]$content_id <- altered$snapshots[[1]]$content_id
  jsonlite::write_json(altered, index_file, auto_unbox = TRUE)
  expect_error(read_bundled_catalog(id), class = "cttir_catalog_corrupt")
  altered <- index; altered$snapshots[[1]]$sha256 <- strrep("0", 64)
  jsonlite::write_json(altered, index_file, auto_unbox = TRUE)
  expect_error(read_bundled_catalog(id), class = "cttir_catalog_corrupt")
  jsonlite::write_json(index, index_file, auto_unbox = TRUE)
  file <- file.path(copy, "catalogs.xz")
  con <- file(file, "r+b"); writeBin(as.raw(0), con); close(con)
  expect_error(read_bundled_catalog(id), class = "cttir_catalog_corrupt")
})
