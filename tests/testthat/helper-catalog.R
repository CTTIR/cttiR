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

remote_fixture <- function(env = parent.frame()) {
  f <- local_update_fixture(env)
  files <- list(DESCRIPTION = paste(readLines(file.path(f$source, "DESCRIPTION")), collapse = "\n"),
    NAMESPACE = "export(keep)", "R/api.R" = "keep <- function(x = 1) x",
    "README.md" = "Remote fixture documentation")
  blobs <- lapply(files, charToRaw)
  sha <- paste(rep("a", 40), collapse = "")
  tree <- lapply(names(blobs), function(path) {
    list(path = path, type = "blob", mode = "100644", size = length(blobs[[path]]),
      sha = digest::digest(c(charToRaw(paste0("blob ", length(blobs[[path]]))), as.raw(0L), blobs[[path]]), algo = "sha1", serialize = FALSE))
  })
  json <- function(url, ...) {
    if (grepl("/commits/", url, fixed = TRUE)) return(list(sha = sha))
    list(truncated = FALSE, tree = tree)
  }
  download <- function(url, path, max_bytes) {
    rel <- sub(paste0("^.*", sha, "/"), "", url)
    writeBin(blobs[[rel]], path)
    invisible(path)
  }
  list(f = f, json = json, download = download, tree = tree, sha = sha,
    record = list(id = "remote", github = "CTTIR/syntheticFixture", package = "cttirFixtureA",
      documentation_rights = "Synthetic test fixture"))
}
