test_that("compiled schemas preserve validation results and bounded reuse", {
  local_mocked_bindings(.schema_validators = new.env(parent = emptyenv()))
  original <- jsonvalidate::json_validator
  compilations <- 0L
  local_mocked_bindings(json_validator = function(...) {
    compilations <<- compilations + 1L
    original(...)
  }, .package = "jsonvalidate")
  schema <- '{"type":"integer","minimum":1}'
  for (json in c("1", "0", '"wrong"', "2")) {
    expected <- original(schema, engine = "ajv")(json, verbose = TRUE)
    expect_identical(json_schema_validate(json, schema, engine = "ajv", verbose = TRUE), expected)
  }
  expect_identical(compilations, 1L)
  expect_error(json_schema_validate("0", schema, engine = "ajv", error = TRUE))
  for (i in seq_len(12)) {
    expect_true(json_schema_validate("20", sprintf('{"type":"integer","minimum":%d}', i), engine = "ajv"))
  }
  expect_lte(length(.schema_validators$entries), 8L)
  .schema_validators$pid <- -1L
  expect_true(json_schema_validate("1", schema, engine = "ajv"))
  expect_length(.schema_validators$entries, 1L)
})

test_that("file edits and external references never reuse stale schemas", {
  local_mocked_bindings(.schema_validators = new.env(parent = emptyenv()))
  root <- withr::local_tempdir()
  schema <- file.path(root, "schema.json")
  writeLines('{"type":"integer","minimum":1}', schema)
  expect_true(json_schema_validate("1", schema, engine = "ajv"))
  writeLines('{"type":"integer","minimum":2}', schema)
  expect_false(json_schema_validate("1", schema, engine = "ajv"))
  expect_true(schema_is_self_contained('{"$ref":"#/definitions/value"}'))
  expect_false(schema_is_self_contained('{"items":{"$ref":"other.json"}}'))
  expect_false(schema_is_self_contained('{"$dynamicRef":"other.json"}'))
  expect_false(schema_is_self_contained('{"$recursiveRef":"other.json"}'))
  expect_false(schema_is_self_contained('{"$\\u0072ef":"other.json"}'))
  expect_false(schema_is_self_contained("not JSON"))
  child <- file.path(root, "value.json")
  writeLines('{"type":"integer","minimum":1}', child)
  writeLines('{"$ref":"value.json"}', schema)
  expect_true(json_schema_validate("1", schema, engine = "ajv"))
  writeLines('{"type":"integer","minimum":2}', child)
  expect_false(json_schema_validate("1", schema, engine = "ajv"))
})

test_that("cache hits and misses preserve present and absent random state", {
  local_mocked_bindings(.schema_validators = new.env(parent = emptyenv()))
  withr::local_seed(42)
  saved <- .Random.seed
  schema <- '{"type":"integer"}'
  for (i in seq_len(2)) {
    expect_true(json_schema_validate("1", schema, engine = "ajv"))
    expect_identical(.Random.seed, saved)
  }
  rm(".Random.seed", envir = globalenv())
  for (candidate in c(schema, '{"type":"number"}')) {
    expect_true(json_schema_validate("1", candidate, engine = "ajv"))
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  }
})
