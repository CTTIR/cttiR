test_that("German rendering translates fixed advice while preserving the API object", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  answer <- ask("Explain dplyr::filter")
  saved <- serialize(answer, NULL)
  german <- as.character(app_answer_ui(answer, "de", shiny::NS("ask")))
  english <- as.character(app_answer_ui(answer, "en", shiny::NS("ask")))
  expect_match(german, "Die genannte API", fixed = TRUE)
  expect_match(german, "Codebeispiele dienen", fixed = TRUE)
  expect_match(german, "ask() arbeitet deterministisch", fixed = TRUE)
  expect_false(grepl("The named API is present", german, fixed = TRUE))
  expect_match(english, answer$answer, fixed = TRUE)
  expect_identical(serialize(answer, NULL), saved)
  expect_identical(app_answer_text("Source excerpt: <script>literal</script>", "de"),
    "Source excerpt: <script>literal</script>")
  expect_identical(app_answer_text("Supported with approved adapters: std.model.lm.", "de"),
    "Unterstützt durch freigegebene Adapter: std.model.lm.")
})

test_that("German display translates model conflicts and partial support without altering evidence", {
  skip_if_not_installed("shiny")
  withr::local_options(cttiR.catalog_dir = tempfile("unused-catalog-"))
  for (question in c("Fit logistic regression to repeated binary outcomes per participant",
    "Fit a binary model for repeated outcomes per participant")) {
    answer <- ask(question)
    saved <- serialize(answer, NULL)
    german <- as.character(app_answer_ui(answer, "de", shiny::NS("ask")))
    english <- as.character(app_answer_ui(answer, "en", shiny::NS("ask")))
    expect_match(german, "keinen passenden freigegebenen Adapter", fixed = TRUE)
    expect_match(german, "kein Ersatzmodell vorgeschlagen", fixed = TRUE)
    expect_match(german, "model_configuration_mismatch:", fixed = TRUE)
    expect_false(grepl("no model is substituted", german, fixed = TRUE))
    expect_match(english, "no model is substituted", fixed = TRUE)
    expect_identical(serialize(answer, NULL), saved)
  }
  partial <- ask("Make a baseline table and use Seurat clustering")
  saved <- serialize(partial, NULL)
  german <- as.character(app_answer_ui(partial, "de", shiny::NS("ask")))
  expect_match(german, "Teilweise unterstützt", fixed = TRUE)
  expect_match(german, "Gesamtanfrage nicht ab", fixed = TRUE)
  expect_match(german, "std.describe.descrtab2", fixed = TRUE)
  expect_match(german, "seurat.single_cell.exploration", fixed = TRUE)
  expect_identical(serialize(partial, NULL), saved)
  literal <- "Source excerpt: Partially supported with approved adapters: literal documentation"
  expect_identical(app_answer_text(literal, "de"), literal)
})
