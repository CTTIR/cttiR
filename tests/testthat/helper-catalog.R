fixture_source <- function(path, code, exports) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  writeLines(c("Package: cttirFixtureA", "Version: 1.0.0", "Title: Synthetic Test Package", "License: MIT"), file.path(path, "DESCRIPTION"))
  writeLines(paste0("export(", exports, ")"), file.path(path, "NAMESPACE"))
  writeLines(code, file.path(path, "R", "api.R"))
  path
}

local_update_fixture <- function(env = parent.frame()) {
  parent <- tempfile("cttir-update-")
  dir.create(parent)
  withr::defer(unlink(parent, recursive = TRUE), envir = env)
  source <- fixture_source(file.path(parent, "source"), c("old <- function(x) x", "keep <- function(x = 1) x"), c("old", "keep"))
  withr::local_options(list(cttiR.catalog_dir = file.path(parent, "store"), cttiR.sources = list(list(id = "fixture", path = source))), .local_envir = env)
  list(parent = parent, source = source, store = file.path(parent, "store"))
}

document_fixture <- function(env = parent.frame()) {
  f <- local_update_fixture(env)
  dir.create(file.path(f$source, "vignettes"))
  dir.create(file.path(f$source, "man"))
  writeLines(c("\\name{keep}", "\\alias{keep}", "\\title{A synthetic reference}",
      "\\usage{keep(x = 1)}", "\\description{Reference evidence.}"), file.path(f$source, "man/keep.Rd"))
  writeLines(c("# Synthetic tutorial", "vignette_token_alpha", "```{r}",
      "stop('THIS MUST NEVER EXECUTE')", "```"), file.path(f$source, "vignettes/guide.Rmd"))
  writeLines("# Changes\nnews_token_alpha", file.path(f$source, "NEWS.md"))
  f
}
