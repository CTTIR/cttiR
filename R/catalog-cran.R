# Public CRAN and Bioconductor source backends. Only fixed public endpoints are
# contacted, without redirects or credentials. Downloaded archives are parsed by a
# bounded tar reader; no archive tool, build step, installer or package code runs.

cran_root <- "https://cloud.r-project.org/src/contrib"

repository_package_pattern <- "[A-Za-z][A-Za-z0-9.]*"
repository_version_pattern <- "[0-9]+([.-][0-9]+)+"

repository_url_allowed <- function(url) {
  is.character(url) && length(url) == 1L && !is.na(url) && (
    grepl("^https://cloud[.]r-project[.]org/src/contrib/PACKAGES$", url) ||
      grepl("^https://cloud[.]r-project[.]org/src/contrib/[A-Za-z][A-Za-z0-9.]*_[0-9]+([.-][0-9]+)+[.]tar[.]gz$", url) ||
      grepl("^https://cloud[.]r-project[.]org/src/contrib/Archive/([A-Za-z][A-Za-z0-9.]*)/\\1_[0-9]+([.-][0-9]+)+[.]tar[.]gz$", url) ||
      grepl("^https://bioconductor[.]org/packages/[0-9]+[.][0-9]+/bioc/src/contrib/PACKAGES$", url) ||
      grepl("^https://bioconductor[.]org/packages/[0-9]+[.][0-9]+/bioc/src/contrib/[A-Za-z][A-Za-z0-9.]*_[0-9]+([.-][0-9]+)+[.]tar[.]gz$", url))
}

# Network primitive. Tests replace it; callers re-check every bound afterwards.
repository_download <- function(url, path, max_bytes) {
  if (!repository_url_allowed(url)) {
    abort_cttir("Repository sources must use the fixed public CRAN or Bioconductor endpoints.", "cttir_source_unavailable", "endpoint_not_allowed")
  }
  request <- httr2::request(url)
  request <- httr2::req_user_agent(request, "cttiR public-repository metadata")
  request <- httr2::req_timeout(request, 60)
  request <- httr2::req_error(request, is_error = function(resp) FALSE)
  request <- httr2::req_options(request, followlocation = FALSE, maxfilesize = max_bytes, netrc = 0L)
  response <- tryCatch(httr2::req_perform(request, path = path), error = function(e) {
    abort_cttir("Public repository retrieval failed; the prior snapshot is unchanged.", "cttir_source_unavailable", "repository_fetch_failed")
  })
  status <- httr2::resp_status(response)
  if (status != 200L) {
    abort_cttir(paste0("Public repository returned HTTP ", status, "; redirects and errors are not followed."),
      "cttir_source_unavailable", if (status >= 300L && status < 400L) "redirect_refused" else "repository_http_error")
  }
  header <- function(name) {
    value <- httr2::resp_header(response, name)
    if (is.null(value)) NA_character_ else value
  }
  list(status = status, last_modified = header("last-modified"), etag = header("etag"))
}

new_fetch_context <- function(directory, policy = NULL, budget = 600) {
  context <- new.env(parent = emptyenv())
  context$directory <- directory
  context$policy <- policy
  context$indices <- list()
  context$calls <- character()
  context$started <- Sys.time()
  context$budget <- budget
  context$report <- list()
  context
}

repository_fetch <- function(url, path, max_bytes, context) {
  if (!repository_url_allowed(url)) {
    abort_cttir("Repository sources must use the fixed public CRAN or Bioconductor endpoints.", "cttir_source_unavailable", "endpoint_not_allowed")
  }
  if (as.numeric(difftime(Sys.time(), context$started, units = "secs")) > context$budget) {
    abort_cttir("Repository retrieval exceeded the run time budget.", "cttir_source_unavailable", "fetch_budget")
  }
  context$calls <- c(context$calls, url)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  headers <- repository_download(url, path, max_bytes)
  if (!file.exists(path) || dir.exists(path) || is.na(file.info(path)$size) || file.info(path)$size > max_bytes) {
    unlink(path)
    abort_cttir("Repository response is incomplete or exceeds its byte bound.", "cttir_source_unavailable", "response_bound")
  }
  if (!is.list(headers)) headers <- list()
  list(url = url, bytes = file.info(path)$size, sha256 = digest::digest(file = path, algo = "sha256"),
    retrieved_at = utc_timestamp(),
    last_modified = if (is.character(headers$last_modified)) headers$last_modified else NA_character_,
    etag = if (is.character(headers$etag)) headers$etag else NA_character_)
}

repository_index_url <- function(repository, release = NULL) {
  if (identical(repository, "CRAN")) return(paste0(cran_root, "/PACKAGES"))
  paste0("https://bioconductor.org/packages/", release, "/bioc/src/contrib/PACKAGES")
}

repository_index_key <- function(repository, release = NULL) {
  if (identical(repository, "CRAN")) "cran" else paste0("bioc-", release)
}

parse_repository_index <- function(file) {
  fields <- tryCatch(read.dcf(file), error = function(e) {
    abort_cttir("Repository index could not be parsed.", "cttir_source_unavailable", "index_parse")
  })
  if (!nrow(fields) || !all(c("Package", "Version", "MD5sum") %in% colnames(fields))) {
    abort_cttir("Repository index lacks required package fields.", "cttir_source_unavailable", "index_parse")
  }
  valid <- !is.na(fields[, "Package"]) & grepl(paste0("^", repository_package_pattern, "$"), fields[, "Package"]) &
    !is.na(fields[, "Version"]) & grepl(paste0("^", repository_version_pattern, "$"), fields[, "Version"]) &
    !is.na(fields[, "MD5sum"]) & grepl("^[a-f0-9]{32}$", fields[, "MD5sum"])
  list(fields = fields[valid, , drop = FALSE], malformed = sum(!valid))
}

# One fetch per index per run: successes and failures are both memoized.
repository_index <- function(context, repository, release = NULL) {
  key <- repository_index_key(repository, release)
  cached <- context$indices[[key]]
  if (is.null(cached)) {
    url <- repository_index_url(repository, release)
    file <- file.path(context$directory, paste0(key, "-PACKAGES"))
    cached <- tryCatch({
      fetched <- repository_fetch(url, file, 50000000L, context)
      parsed <- parse_repository_index(file)
      unlink(file)
      entry <- list(status = "fetched", repository = repository, release = if (is.null(release)) NA_character_ else release,
        fields = parsed$fields, malformed_rows = parsed$malformed)
      c(entry, fetched)
    }, cttir_error = function(e) {
      unlink(file)
      list(status = "unavailable", repository = repository, release = if (is.null(release)) NA_character_ else release,
        url = url, retrieved_at = utc_timestamp(), message = conditionMessage(e), code = e$code)
    })
    context$indices[[key]] <- cached
  }
  if (!identical(cached$status, "fetched")) {
    abort_cttir(paste0("The ", repository, if (!is.na(cached$release)) paste0(" ", cached$release), " index is unavailable: ", cached$message),
      "cttir_source_unavailable", "index_unavailable")
  }
  cached
}

# Prefer the ordinary stanza when CRAN also lists an alternate `Path` copy.
index_stanza <- function(index, package) {
  rows <- which(index$fields[, "Package"] == package)
  if (!length(rows)) return(NULL)
  if ("Path" %in% colnames(index$fields)) {
    plain <- rows[is.na(index$fields[rows, "Path"])]
    if (length(plain)) rows <- plain
  }
  stanza <- index$fields[rows[[1]], ]
  stanza[!is.na(stanza)]
}

tar_octal <- function(bytes) {
  if (length(bytes) && bitwAnd(as.integer(bytes[[1]]), 128L) != 0L) {
    abort_cttir("Archive uses an unsupported numeric encoding.", "cttir_source_unavailable", "archive_member")
  }
  bytes <- bytes[bytes != as.raw(0L)]
  text <- trimws(rawToChar(bytes))
  if (!nzchar(text)) return(0)
  if (!grepl("^[0-7]+$", text) || nchar(text) > 11L) {
    abort_cttir("Archive header is malformed.", "cttir_source_unavailable", "archive_member")
  }
  sum(as.integer(strsplit(text, "")[[1]]) * 8^rev(seq_len(nchar(text)) - 1L))
}

tar_text <- function(bytes) {
  end <- which(bytes == as.raw(0L))
  if (length(end)) bytes <- bytes[seq_len(end[[1]] - 1L)]
  text <- rawToChar(bytes)
  if (!validUTF8(text)) abort_cttir("Archive member names must be UTF-8.", "cttir_source_unavailable", "archive_member")
  text
}

read_tar_bytes <- function(tarball, max_bytes) {
  con <- gzfile(tarball, "rb")
  on.exit(close(con), add = TRUE)
  chunks <- list()
  total <- 0
  repeat {
    chunk <- tryCatch(readBin(con, "raw", n = 1048576L), error = function(e) {
      abort_cttir("Archive could not be decompressed.", "cttir_source_unavailable", "archive_corrupt")
    })
    if (!length(chunk)) break
    total <- total + length(chunk)
    if (total > max_bytes) abort_cttir("Archive exceeds the uncompressed size bound.", "cttir_source_unavailable", "archive_bound")
    chunks[[length(chunks) + 1L]] <- chunk
  }
  unlist(chunks, use.names = FALSE)
}

pax_records <- function(bytes) {
  out <- list()
  while (length(bytes)) {
    space <- which(bytes == charToRaw(" "))
    size <- if (length(space)) suppressWarnings(as.integer(rawToChar(bytes[seq_len(space[[1]] - 1L)]))) else NA_integer_
    if (is.na(size) || size <= space[[1]] || size > length(bytes)) {
      abort_cttir("Archive extended header is malformed.", "cttir_source_unavailable", "archive_member")
    }
    body <- bytes[(space[[1]] + 1L):size]
    bytes <- bytes[-seq_len(size)]
    if (length(body) && body[[length(body)]] == charToRaw("\n")) body <- body[-length(body)]
    equals <- which(body == charToRaw("="))
    if (!length(equals)) abort_cttir("Archive extended header is malformed.", "cttir_source_unavailable", "archive_member")
    key <- tar_text(body[seq_len(equals[[1]] - 1L)])
    out[[key]] <- tar_text(body[-seq_len(equals[[1]])])
  }
  out
}

tar_checksum_valid <- function(header) {
  expected <- tar_octal(header[149:156])
  check <- as.integer(header)
  check[149:156] <- 32L
  signed <- ifelse(check > 127L, check - 256L, check)
  sum(check) == expected || sum(signed) == expected
}

# List every member before writing anything. Links, devices, absolute or parent
# paths, duplicate names and oversize totals reject the complete archive.
tar_members <- function(tarball, package, max_bytes = 120000000L, max_members = 20000L) {
  bytes <- read_tar_bytes(tarball, max_bytes)
  if (length(bytes) < 512L || length(bytes) %% 512L != 0L) {
    abort_cttir("Archive is truncated or not a tar stream.", "cttir_source_unavailable", "archive_corrupt")
  }
  members <- list()
  offset <- 0
  pending <- list()
  while (offset + 512 <= length(bytes)) {
    header <- bytes[(offset + 1):(offset + 512)]
    if (all(header == as.raw(0L))) break
    if (!tar_checksum_valid(header)) {
      abort_cttir("Archive header checksum mismatch.", "cttir_source_unavailable", "archive_corrupt")
    }
    type <- rawToChar(header[157][header[157] != as.raw(0L)])
    if (!nzchar(type)) type <- "0"
    meta <- type %in% c("x", "g", "L")
    size <- tar_octal(header[125:136])
    if (!meta && !is.null(pending$size)) size <- suppressWarnings(as.numeric(pending$size))
    if (is.na(size) || size < 0 || size != floor(size)) {
      abort_cttir("Archive member size is invalid.", "cttir_source_unavailable", "archive_member")
    }
    data_start <- offset + 512
    if (data_start + size > length(bytes)) abort_cttir("Archive member is truncated.", "cttir_source_unavailable", "archive_corrupt")
    offset <- data_start + ceiling(size / 512) * 512
    if (meta) {
      if (size > 1048576) abort_cttir("Archive metadata header exceeds its bound.", "cttir_source_unavailable", "archive_member")
      data <- if (size > 0) bytes[(data_start + 1):(data_start + size)] else raw()
      if (type == "x") {
        records <- pax_records(data)
        if (!is.null(records$linkpath)) abort_cttir("Archive contains a link.", "cttir_source_unavailable", "archive_link")
        if (!is.null(records$path)) pending$name <- records$path
        if (!is.null(records$size)) pending$size <- records$size
      } else if (type == "L") {
        pending$name <- tar_text(data)
      }
      next
    }
    name <- tar_text(header[1:100])
    if (identical(header[258:263], c(charToRaw("ustar"), as.raw(0L)))) {
      prefix <- tar_text(header[346:500])
      if (nzchar(prefix)) name <- paste0(prefix, "/", name)
    }
    if (!is.null(pending$name)) name <- pending$name
    pending <- list()
    if (!type %in% c("0", "7", "5")) {
      abort_cttir("Archive contains a link, device or unsupported member type.", "cttir_source_unavailable", "archive_link")
    }
    name <- sub("^(\\./)+", "", name)
    directory <- type == "5"
    if (!directory && grepl("/$", name)) abort_cttir("Archive member type is inconsistent.", "cttir_source_unavailable", "archive_member")
    name <- sub("/+$", "", name)
    parts <- strsplit(name, "/", fixed = TRUE)[[1]]
    if (!nzchar(name) || grepl("^[/\\\\]|^[A-Za-z]:|\\\\", name) || grepl("[[:cntrl:]]", name) ||
        any(parts %in% c("", ".", ".."))) {
      abort_cttir("Archive contains an absolute, parent or empty path.", "cttir_source_unavailable", "archive_path")
    }
    if (!identical(parts[[1]], package)) {
      abort_cttir("Archive contains members outside its package directory.", "cttir_source_unavailable", "archive_path")
    }
    members[[length(members) + 1L]] <- list(name = name, directory = directory, size = size, start = data_start)
    if (length(members) > max_members) abort_cttir("Archive exceeds the member count bound.", "cttir_source_unavailable", "archive_bound")
  }
  names <- vapply(members, function(x) x$name, character(1))
  files <- !vapply(members, function(x) x$directory, logical(1))
  if (anyDuplicated(names) || anyDuplicated(tolower(names[files]))) {
    abort_cttir("Archive contains duplicate or case-conflicting members.", "cttir_source_unavailable", "archive_path")
  }
  list(members = members, bytes = bytes)
}

# Writes only the files static extraction reads. Oversize documentation assets are
# omitted with their size and hash recorded; oversize API inputs fail closed.
stage_package_archive <- function(tarball, package, stage) {
  archive <- tar_members(tarball, package)
  members <- archive$members
  root <- file.path(stage, package)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  omitted <- list()
  written <- character()
  for (member in members) {
    if (member$directory) next
    rel <- substring(member$name, nchar(package) + 2L)
    api <- rel %in% c("DESCRIPTION", "NAMESPACE") || grepl("^R/[^/]+[.][Rr]$", rel) || grepl("^man/[^/]+[.]Rd$", rel)
    docs <- grepl("^(man|vignettes|inst/doc)/", rel) || identical(rel, "inst/CITATION") ||
      grepl("^(README|NEWS|CHANGELOG|LICENSE|LICENCE|COPYING)([.][^/]*)?$", rel)
    if (!api && !docs) next
    tryCatch(relative_file(rel), cttir_error = function(e) {
      abort_cttir("Archive contains a path that is unsafe on supported platforms.", "cttir_source_unavailable", "archive_path")
    })
    data <- if (member$size > 0) archive$bytes[(member$start + 1):(member$start + member$size)] else raw()
    if (member$size > 1048576) {
      if (api) abort_cttir("A package source file exceeds the one-megabyte extraction bound.", "cttir_source_unavailable", "archive_bound")
      omitted[[length(omitted) + 1L]] <- list(path = rel, bytes = member$size,
        sha256 = digest::digest(data, algo = "sha256", serialize = FALSE),
        reason = "documentation_file_exceeds_one_megabyte_bound")
      next
    }
    dest <- file.path(root, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeBin(data, dest)
    written <- c(written, rel)
  }
  if (!all(c("DESCRIPTION", "NAMESPACE") %in% written)) {
    abort_cttir("The archive is not a complete package source.", "cttir_source_unavailable", "archive_incomplete")
  }
  list(root = root, files = length(written), members = length(members), omitted = omitted)
}

repository_archive_source <- function(package, version, url, expected_md5, checksum_source, repository,
  revision_prefix, family, documentation_rights, context) {
  directory <- tempfile("archive-", tmpdir = context$directory)
  dir.create(directory, recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  tarball <- file.path(directory, paste0(package, "_", version, ".tar.gz"))
  fetched <- repository_fetch(url, tarball, 30000000L, context)
  md5 <- unname(tools::md5sum(tarball))
  if (!is.null(expected_md5) && !identical(md5, expected_md5)) {
    abort_cttir("Downloaded archive does not match the repository MD5 integrity metadata.", "cttir_source_unavailable", "checksum_mismatch")
  }
  staged <- stage_package_archive(tarball, package, file.path(directory, "stage"))
  entry <- extract_source(staged$root, repository = repository,
    revision = paste0(revision_prefix, ":", package, "@", version, ":", md5),
    family = family, documentation_rights = documentation_rights)
  if (!identical(entry$name, package) || !identical(entry$version, version)) {
    abort_cttir("Archive package identity or version differs from the request.", "cttir_source_unavailable", "identity_mismatch")
  }
  entry$archive <- list(url = url, bytes = fetched$bytes, sha256 = fetched$sha256, md5 = md5,
    checksum_source = checksum_source,
    checksum_meaning = if (identical(checksum_source, "none_published")) {
      "No repository checksum is published for this archived version; the MD5 was computed locally and proves neither integrity nor authenticity."
    } else {
      "MD5 is repository integrity metadata, not publisher authenticity."
    },
    retrieved_at = fetched$retrieved_at, members = staged$members, staged_files = staged$files,
    omitted_documents = staged$omitted)
  entry
}

cran_source <- function(record, context) {
  package <- record$cran
  scalar_text(package, "CRAN package")
  if (!grepl(paste0("^", repository_package_pattern, "$"), package)) abort_cttir("Invalid CRAN package name.", field = "cran")
  if (!is.null(record$version)) {
    scalar_text(record$version, "source version")
    if (!grepl(paste0("^", repository_version_pattern, "$"), record$version)) abort_cttir("Invalid exact package version.", field = "version")
  }
  if (!is.null(record$md5)) {
    scalar_text(record$md5, "source md5")
    if (!grepl("^[a-f0-9]{32}$", record$md5)) abort_cttir("md5 must be 32 lowercase hexadecimal characters.", field = "md5")
  }
  index <- repository_index(context, "CRAN")
  stanza <- index_stanza(index, package)
  if (is.null(stanza) && is.null(record$version)) {
    abort_cttir("The package is not listed in the current CRAN index; register an exact archived version.",
      "cttir_source_unavailable", "missing_from_selected_index")
  }
  version <- if (is.null(record$version)) stanza[["Version"]] else record$version
  current <- !is.null(stanza) && identical(stanza[["Version"]], version)
  if (current && !is.na(stanza["Path"])) {
    abort_cttir("This CRAN package is only listed under an alternate path, which is not an allowlisted endpoint.",
      "cttir_source_unavailable", "unsupported_repository_path")
  }
  if (current && !is.null(record$md5) && !identical(record$md5, stanza[["MD5sum"]])) {
    abort_cttir("The registered MD5 differs from the CRAN index.", "cttir_source_unavailable", "checksum_mismatch")
  }
  url <- if (current) {
    paste0(cran_root, "/", package, "_", version, ".tar.gz")
  } else {
    paste0(cran_root, "/Archive/", package, "/", package, "_", version, ".tar.gz")
  }
  entry <- repository_archive_source(package, version, url,
    expected_md5 = if (current) stanza[["MD5sum"]] else record$md5,
    checksum_source = if (current) "repository_index" else if (!is.null(record$md5)) "registered_record" else "none_published",
    repository = paste0("https://cran.r-project.org/package=", package), revision_prefix = "cran",
    family = "configured_cran", documentation_rights = record$documentation_rights, context = context)
  entry$freshness <- if (current) "repository_current_fetched" else "repository_archive_fetched"
  entry$repository_index <- list(url = index$url, sha256 = index$sha256, retrieved_at = index$retrieved_at)
  entry
}

bioc_source <- function(record, context) {
  package <- record$bioc
  scalar_text(package, "Bioconductor package")
  if (!grepl(paste0("^", repository_package_pattern, "$"), package)) abort_cttir("Invalid Bioconductor package name.", field = "bioc")
  release <- if (is.null(record$bioc_version)) require_bioc_release(context$policy) else validate_bioc_release(record$bioc_version, "source bioc_version")
  index <- repository_index(context, "Bioconductor", release)
  stanza <- index_stanza(index, package)
  if (is.null(stanza)) {
    abort_cttir(paste0("The package is not listed in the Bioconductor ", release, " software index."),
      "cttir_source_unavailable", "missing_from_selected_index")
  }
  version <- stanza[["Version"]]
  if (!is.null(record$version) && !identical(record$version, version)) {
    abort_cttir("Only the version listed in the selected Bioconductor release index can be fetched.",
      "cttir_source_unavailable", "version_not_in_release")
  }
  base <- paste0("https://bioconductor.org/packages/", release, "/bioc")
  entry <- repository_archive_source(package, version, paste0(base, "/src/contrib/", package, "_", version, ".tar.gz"),
    expected_md5 = stanza[["MD5sum"]], checksum_source = "repository_index",
    repository = paste0(base, "/html/", package, ".html"), revision_prefix = paste0("bioc-", release),
    family = "configured_bioconductor", documentation_rights = record$documentation_rights, context = context)
  entry$freshness <- "repository_current_fetched"
  entry$bioconductor_release <- release
  entry$repository_index <- list(url = index$url, sha256 = index$sha256, retrieved_at = index$retrieved_at)
  entry
}

validate_repository_record <- function(entry) {
  if (!is.null(entry$cran) && !is.null(entry$bioc)) abort_cttir("A source cannot declare both CRAN and Bioconductor.")
  if (!is.null(entry$path) || !is.null(entry$github) || !is.null(entry$r_distribution)) {
    abort_cttir("A repository source cannot declare another location.")
  }
  name <- if (is.null(entry$cran)) entry$bioc else entry$cran
  scalar_text(name, if (is.null(entry$cran)) "source bioc" else "source cran")
  if (!grepl(paste0("^", repository_package_pattern, "$"), name)) abort_cttir("Invalid repository package name.")
  if (!is.null(entry$package) && !identical(entry$package, name)) abort_cttir("A repository source package must match its registration.")
  if (!is.null(entry$cran) && !is.null(entry$bioc_version)) abort_cttir("bioc_version applies only to Bioconductor sources.")
  if (!is.null(entry$bioc_version)) validate_bioc_release(entry$bioc_version, "source bioc_version")
  if (!is.null(entry$bioc) && !is.null(entry$md5)) abort_cttir("Bioconductor sources are verified against the selected release index only.")
  invisible(entry)
}

registered_package <- function(record) {
  if (!is.null(record$cran)) return(record$cran)
  if (!is.null(record$bioc)) return(record$bioc)
  record$package
}

# Optional sources degrade to a recorded unavailable state; required ones raise.
remote_source <- function(record, context) {
  optional <- isTRUE(record$optional)
  tryCatch({
    entry <- if (!is.null(record$cran)) {
      cran_source(record, context)
    } else if (!is.null(record$bioc)) {
      bioc_source(record, context)
    } else {
      github_source(record)
    }
    context$report[[record$id]] <- list(id = record$id, package = entry$name, revision = entry$revision,
      status = "static_extracted", optional = optional, code = NA_character_, message = NA_character_)
    entry
  }, cttir_source_unavailable = function(e) {
    context$report[[record$id]] <- list(id = record$id, package = registered_package(record), revision = NA_character_,
      status = if (optional) "unavailable_optional" else "failed_required", optional = optional,
      code = if (is.null(e$code)) NA_character_ else e$code, message = conditionMessage(e))
    if (!optional) stop(e)
    NULL
  })
}
