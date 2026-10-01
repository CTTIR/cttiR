fixture_source <- function(path, code, exports) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  writeLines(c("Package: cttirFixtureA", "Version: 1.0.0", "Title: Synthetic Test Package", "License: MIT"), file.path(path, "DESCRIPTION"))
  writeLines(paste0("export(", exports, ")"), file.path(path, "NAMESPACE"))
  writeLines(code, file.path(path, "R", "api.R"))
  path
}
