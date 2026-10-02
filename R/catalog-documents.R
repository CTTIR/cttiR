document_inventory <- function(path, rights = NULL) {
  if (!is.null(rights)) scalar_text(rights, "documentation rights basis")
  # Enumerate only documentation roots; never follow directory links.
  collect <- function(root, prefix = "", depth = 0L) {
    if (!dir.exists(root)) return(character())
    assert_plain_path(root)
    if (depth > 12L) abort_cttir("Documentation nesting exceeds the bound.", "cttir_source_unavailable")
    entries <- list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(entries) > 5000L) abort_cttir("Documentation inventory exceeds the bound.", "cttir_source_unavailable")
    out <- character()
    for (file in entries) {
      assert_plain_path(file)
      rel <- paste0(prefix, basename(file))
      if (dir.exists(file)) {
        out <- c(out, collect(file, paste0(rel, "/"), depth + 1L))
      } else {
        out <- c(out, rel)
      }
      if (length(out) > 5000L) abort_cttir("Documentation inventory exceeds the bound.", "cttir_source_unavailable")
    }
    out
  }
  root <- list.files(path, all.files = FALSE)
  root <- root[grepl("^(DESCRIPTION([.]in)?|NAMESPACE|README([.].*)?|NEWS([.].*)?|CHANGELOG([.].*)?|LICENSE([.].*)?|LICENCE([.].*)?|COPYING([.].*)?)$", root)]
  files <- unique(c(root, collect(file.path(path, "man"), "man/"),
      collect(file.path(path, "vignettes"), "vignettes/"),
      collect(file.path(path, "inst", "doc"), "inst/doc/"),
      if (file.exists(file.path(path, "inst", "CITATION"))) "inst/CITATION"))
  files <- sort(files, method = "radix")
  if (length(files) > 5000L) abort_cttir("Documentation inventory exceeds the bound.", "cttir_source_unavailable")
  budget <- new.env(parent = emptyenv())
  budget$total <- 0
  documents <- lapply(files, function(rel) {
    file <- file.path(path, rel)
    assert_plain_path(file)
    size <- file.info(file)$size
    if (is.na(size) || dir.exists(file) || size > 1048576L) {
      abort_cttir("A documentation file exceeds the one-megabyte bound or is not regular.", "cttir_source_unavailable")
    }
    budget$total <- budget$total + size
    if (budget$total > 10000000L) abort_cttir("Documentation exceeds the ten-megabyte package bound.", "cttir_source_unavailable")
    hash <- digest::digest(file = file, algo = "sha256")
    text_format <- tolower(tools::file_ext(rel)) %in% c("rd", "rmd", "rnw", "snw", "rtex", "qmd", "md", "txt", "r", "html", "tex", "bib") ||
      basename(rel) %in% c("DESCRIPTION", "DESCRIPTION.in", "NAMESPACE", "README", "NEWS", "LICENSE", "LICENCE", "COPYING", "CITATION", "CHANGELOG")
    kind <- if (startsWith(rel, "man/")) "reference" else if (startsWith(rel, "vignettes/") || startsWith(rel, "inst/doc/")) "vignette" else "package_document"
    stored <- !is.null(rights) && text_format
    content <- if (stored) read_source_text(file) else NULL
    if (stored && (anyNA(iconv(content, from = "UTF-8", to = "UTF-8")))) {
      abort_cttir("Documentation is not valid UTF-8.", "cttir_source_unavailable")
    }
    list(path = rel, kind = kind, source_sha256 = hash, bytes = size,
      storage = if (stored) "source_text" else if (!text_format) "asset_metadata_only" else "rights_not_confirmed",
      rights_basis = rights, content = content,
      content_sha256 = if (stored) content_hash(content) else NULL,
      extraction = "literal_no_execution")
  })
  stored <- vapply(documents, function(x) x$storage == "source_text", logical(1))
  list(schema_version = 1L, documents = documents,
    coverage = list(discovered = length(documents), stored = sum(stored),
      metadata_only = sum(!stored), source_manifest_complete = TRUE,
      vignette_status = if (any(vapply(documents, function(x) x$kind == "vignette", logical(1)))) "discovered" else "not_present_in_source",
      approval = "pending"))
}

validate_document_corpus <- function(corpus) {
  if (is.null(corpus)) return(invisible(FALSE))
  fail <- function() abort_cttir("Documentation corpus integrity check failed.", "cttir_catalog_corrupt")
  if (!identical(corpus$schema_version, 1L) || !is.list(corpus$documents)) fail()
  paths <- character()
  for (doc in corpus$documents) {
    relative_file(doc$path)
    paths <- c(paths, doc$path)
    if (!is.character(doc$source_sha256) || length(doc$source_sha256) != 1L ||
        !grepl("^[0-9a-f]{64}$", doc$source_sha256)) fail()
    if (identical(doc$storage, "source_text")) {
      if (!is.character(doc$content) || length(doc$content) != 1L || is.na(doc$content) ||
          !identical(content_hash(doc$content), doc$content_sha256) || is.null(doc$rights_basis)) fail()
    } else if (!doc$storage %in% c("asset_metadata_only", "rights_not_confirmed") || !is.null(doc$content)) {
      fail()
    }
  }
  if (anyDuplicated(paths) || corpus$coverage$discovered != length(paths) ||
      corpus$coverage$stored != sum(vapply(corpus$documents, function(x) x$storage == "source_text", logical(1)))) fail()
  invisible(TRUE)
}

document_hits <- function(package, query, max_chars = 1200L) {
  corpus <- package$documentation_corpus
  if (is.null(corpus)) return(list())
  validate_document_corpus(corpus)
  hits <- list()
  for (doc in corpus$documents) {
    if (doc$storage != "source_text") next
    at <- regexpr(tolower(query), tolower(doc$content), fixed = TRUE)[[1]]
    if (at < 0L) next
    start <- max(1L, at - 150L)
    hits[[length(hits) + 1L]] <- list(
      id = content_hash(paste(package$source_hash, doc$path, start)),
      snippet = substr(doc$content, start, start + max_chars - 1L),
      path = doc$path, offset = start)
    if (length(hits) >= 20L) break
  }
  hits
}

documentation_diff <- function(before, after) {
  rows <- data.frame(package = character(), path = character(), change = character())
  old <- stats::setNames(before$packages, vapply(before$packages, function(x) x$name, character(1)))
  for (package in after$packages) {
    index <- function(x) {
      if (is.null(x)) return(list())
      stats::setNames(x, vapply(x, function(doc) doc$path, character(1)))
    }
    a <- index(old[[package$name]]$documentation_corpus$documents)
    b <- index(package$documentation_corpus$documents)
    for (path in union(names(a), names(b))) {
      if (identical(a[[path]], b[[path]])) next
      change <- if (is.null(a[[path]])) "added" else if (is.null(b[[path]])) "removed" else "changed"
      rows[nrow(rows) + 1L, ] <- list(package$name, path, change)
    }
  }
  rows
}
