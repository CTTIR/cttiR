test_that("fixed preview display names units and actual independence in both languages", {
  for (recipe in c("association", "paired")) {
    fixture <- app_study_fixture(recipe)
    value <- list(recipe_id = fixture$spec$recipe_id, request = list(directory = "/synthetic/example"),
      spec = fixture$spec, units = fixture$units, seed = fixture$seed, rows = nrow(fixture$data))
    before <- serialize(value, NULL)
    for (lang in c("en", "de")) {
      html <- as.character(app_study_review(value, lang))
      expect_match(html, "pressure", fixed = TRUE)
      expect_match(html, "mmHg", fixed = TRUE)
      expect_match(html, "95%", fixed = TRUE)
      for (key in c("units", "domain", "origin", "design", "confidence", "independence",
                    "domain.clinical", "origin.primary", "design.observational"))
        expect_match(html, app_t(paste0("study.", key), lang), fixed = TRUE)
      key <- if (recipe == "paired") "study.independence.paired" else "study.independence.observations"
      expect_match(html, app_t(key, lang), fixed = TRUE)
      expect_false(grepl("null|<pre>", html))
      if (recipe == "association") {
        for (unit in c("exposure", "mg", "age", "years")) expect_match(html, unit, fixed = TRUE)
      }
    }
    expect_identical(serialize(value, NULL), before)
  }
})

test_that("numerical display is concise and leaves exact result values unchanged", {
  coefficients <- data.frame(term = "mean_after_minus_before", estimate = -3.4391344042933585,
    std_error = 0.987654321098765, statistic = -3.48123456789, p_value = 0.0000000123456789,
    conf_low = -5.123456789, conf_high = -1.754812019)
  before <- serialize(coefficients, NULL)
  en <- as.character(app_study_result_table(coefficients, "en"))
  de <- as.character(app_study_result_table(coefficients, "de"))
  expect_match(en, "-3.4391", fixed = TRUE)
  expect_match(de, "-3,4391", fixed = TRUE)
  expect_match(en, "1.2346e-08", fixed = TRUE)
  expect_match(de, "1,2346e-08", fixed = TRUE)
  expect_false(grepl("3.439134404", paste(en, de), fixed = TRUE))
  for (lang in c("en", "de")) {
    html <- if (lang == "en") en else de
    for (column in names(coefficients)) expect_match(html, app_t(paste0("study.column.", column), lang), fixed = TRUE)
    expect_match(html, app_t("study.term.mean_after_minus_before", lang), fixed = TRUE)
  }
  expect_identical(serialize(coefficients, NULL), before)
})
