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

cran_index_url <- "https://cloud.r-project.org/src/contrib/PACKAGES"
bioc_index_url <- function(release = "3.23") paste0("https://bioconductor.org/packages/", release, "/bioc/src/contrib/PACKAGES")

dcf_text <- function(stanzas) {
  paste(vapply(stanzas, function(s) paste(paste0(names(s), ": ", s), collapse = "\n"), character(1)), collapse = "\n\n")
}

# Index stanzas reproducing the bundled resource observations.
bundled_stanzas <- function(repository) {
  con <- DBI::dbConnect(RSQLite::SQLite(), system.file("extdata", "package-resources.sqlite", package = "cttiR"), flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con))
  sql <- paste("SELECT p.package_id, p.name, o.observed_version, o.license, o.needs_compilation",
    "FROM packages p JOIN observations o USING(package_id) WHERE o.repository = ? AND (o.subrepository IS NULL OR o.subrepository = 'bioc')",
    "ORDER BY p.name")
  rows <- DBI::dbGetQuery(con, sql, params = list(repository))
  deps <- DBI::dbGetQuery(con, "SELECT package_id, role, dependency, version_constraint FROM dependencies")
  stanzas <- lapply(seq_len(nrow(rows)), function(i) {
    s <- c(Package = rows$name[[i]], Version = rows$observed_version[[i]])
    mine <- deps[deps$package_id == rows$package_id[[i]], , drop = FALSE]
    for (role in c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")) {
      d <- mine[mine$role == role, , drop = FALSE]
      if (nrow(d)) s[[role]] <- paste(ifelse(is.na(d$version_constraint), d$dependency, paste0(d$dependency, " (", d$version_constraint, ")")), collapse = ", ")
    }
    s[["License"]] <- if (is.na(rows$license[[i]])) "Unknown" else rows$license[[i]]
    s[["MD5sum"]] <- digest::digest(rows$name[[i]], algo = "md5", serialize = FALSE)
    s[["NeedsCompilation"]] <- if (is.na(rows$needs_compilation[[i]])) "no" else rows$needs_compilation[[i]]
    s
  })
  stats::setNames(stanzas, rows$name)
}

repository_files <- function(cran = bundled_stanzas("CRAN"), bioc = bundled_stanzas("Bioconductor"), release = "3.23") {
  files <- list()
  files[[cran_index_url]] <- charToRaw(dcf_text(cran))
  files[[bioc_index_url(release)]] <- charToRaw(dcf_text(bioc))
  files
}

# Replaces the only repository network primitive. Unknown or failing URLs raise
# the typed unavailable condition; every request is recorded. No network is used.
local_repository_mock <- function(files = repository_files(), fail = character(), env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$calls <- character()
  state$files <- files
  testthat::local_mocked_bindings(
    repository_download = function(url, path, max_bytes) {
      state$calls <- c(state$calls, url)
      if (url %in% fail || is.null(state$files[[url]])) {
        abort_cttir("Mock repository returned HTTP 404.", "cttir_source_unavailable", "repository_http_error")
      }
      writeBin(state$files[[url]], path)
      list(status = 200L, last_modified = "Fri, 02 Oct 2026 00:00:00 GMT", etag = NA_character_)
    },
    utc_timestamp = function() "2026-10-02T00:00:00Z",
    running_r_minor = function() "4.6",
    .env = env
  )
  state
}

tar_header <- function(name, size, type = "0", linkname = "") {
  put <- function(header, offset, text) {
    bytes <- charToRaw(text)
    header[offset + seq_along(bytes)] <- bytes
    header
  }
  header <- raw(512)
  fields <- list(c(0, name), c(100, "0000644"), c(108, "0000000"), c(116, "0000000"), c(124, sprintf("%011o", size)),
    c(136, "00000000000"), c(148, "        "), c(156, type), c(257, "ustar"), c(263, "00"))
  if (nzchar(linkname)) fields[[length(fields) + 1L]] <- c(157, linkname)
  for (field in fields) header <- put(header, as.integer(field[[1]]), field[[2]])
  header <- put(header, 148, sprintf("%06o", sum(as.integer(header))))
  header[155] <- as.raw(0L)
  header
}

# Writes a gzip tar from explicit members so malicious layouts can be built.
make_tarball <- function(path, members) {
  blocks <- list()
  for (m in members) {
    data <- if (is.null(m$data)) raw() else if (is.raw(m$data)) m$data else charToRaw(m$data)
    blocks[[length(blocks) + 1L]] <- tar_header(m$name, length(data), if (is.null(m$type)) "0" else m$type,
      if (is.null(m$linkname)) "" else m$linkname)
    if (length(data)) blocks[[length(blocks) + 1L]] <- c(data, raw((512 - length(data) %% 512) %% 512))
  }
  blocks[[length(blocks) + 1L]] <- raw(1024)
  con <- gzfile(path, "wb")
  on.exit(close(con))
  writeBin(unlist(blocks), con)
  path
}

package_members <- function(package = "cttirFixtureA", version = "1.0.0", code = "keep <- function(x = 1) x",
  exports = "keep", license = "MIT", man = TRUE) {
  description <- paste0("Package: ", package, "\nVersion: ", version, "\nTitle: Synthetic Repository Fixture\nLicense: ", license, "\n")
  members <- list(
    list(name = paste0(package, "/DESCRIPTION"), data = description),
    list(name = paste0(package, "/NAMESPACE"), data = paste0("export(", exports, ")\n", collapse = "")),
    list(name = paste0(package, "/R/api.R"), data = paste0(paste(code, collapse = "\n"), "\n"))
  )
  if (man) {
    members[[length(members) + 1L]] <- list(name = paste0(package, "/man/keep.Rd"),
      data = "\\name{keep}\n\\alias{keep}\n\\title{Keep}\n\\usage{keep(x = 1)}\n\\description{Synthetic.}\n")
  }
  members
}

tarball_bytes <- function(members) {
  file <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(file))
  make_tarball(file, members)
  readBin(file, "raw", n = file.info(file)$size)
}
