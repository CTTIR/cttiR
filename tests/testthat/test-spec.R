test_that("directory slugs fold Latin letters without depending on the locale", {
  expect_equal(safe_slug("Ünïcödé Prøject"), "unicode_project")
  expect_equal(safe_slug("Straße der Ärzte"), "strasse_der_arzte")
  expect_equal(safe_slug("Łódź cohort"), "lodz_cohort")
  expect_match(safe_slug("日本語の研究"), "^project_[0-9a-f]{8}$")
  expect_match(safe_slug("Étude 名前"), "^etude_[0-9a-f]{8}$")
})

test_that("unmarked UTF-8 input is marked so it compares and hashes the same everywhere", {
  marked <- "Straße"
  unmarked <- rawToChar(charToRaw(marked))
  input <- utf8_input(list(name = unmarked, nested = list(goal = unmarked), n = 1L))
  expect_identical(Encoding(input$name), "UTF-8")
  expect_identical(input$nested$goal, marked)
  expect_identical(input$n, 1L)
  expect_identical(content_hash(utf8_input(unmarked)), content_hash(marked))
})
