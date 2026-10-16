test_that("short negative imperatives suppress later keywords but stop at a new clause", {
  keyword <- list(en = "excel", de = character())
  negative <- c("Do not import Excel sheets", "Please don't read Excel workbooks", "We should not use Excel",
    "Never open the Excel workbook", "Bitte keine Excel-Dateien einlesen", "Keine Tabellen aus Excel importieren",
    "Nicht mit Excel arbeiten")
  for (text in negative) expect_false(keyword_hit(tolower(text), keyword), info = text)
  positive <- c("Please import Excel sheets", "We should use Excel", "Bitte Tabellen aus Excel importieren",
    "Do not plot results; import Excel", "Do not plot results. Import Excel", "Never plot results, but import Excel",
    "Bitte nicht plotten; Tabellen aus Excel importieren", "Keine Diagramme, sondern Excel importieren",
    "Do not only inspect Excel", "Nicht nur Excel untersuchen")
  for (text in positive) expect_true(keyword_hit(tolower(text), keyword), info = text)
  expect_true(keyword_hit("do not import excel; import excel", keyword))
  expect_false(affirmed_hit("do not read records.csv", "\\bcsv\\b"))
})

test_that("ask applies shared clause negation without dropping the positive task", {
  for (question in c("Do not import CSV files", "Keine CSV-Dateien importieren")) {
    answer <- ask(question)
    expect_false("std.import.delimited" %in% unlist(answer$approved_capabilities), info = question)
    expect_identical(answer$code, "", info = question)
  }
  for (question in c("Do not import CSV files; plot the measurements", "Keine CSV-Dateien importieren; die Messwerte plotten")) {
    answer <- ask(question)
    expect_false("std.import.delimited" %in% unlist(answer$approved_capabilities), info = question)
    expect_contains(unlist(answer$approved_capabilities), "std.figures.accessible")
  }
})
