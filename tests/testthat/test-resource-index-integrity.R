test_that("retained resource identity mappings cannot be silently replaced", {
  original_file <- resource_file
  corrupted <- file.path(new_parent(), "index.json")
  writeLines('{"schema_version":1,"snapshots":{}}', corrupted)
  local_mocked_bindings(resource_file = function(...) {
    parts <- c(...)
    if ("reshist" %in% parts && "index.json" %in% parts) corrupted else original_file(...)
  })
  expect_error(resource_history(), class = "cttir_catalog_corrupt")
})
