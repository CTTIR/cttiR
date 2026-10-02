github_download <- function(url, path, max_bytes) {
  if (!grepl("^https://(api[.]github[.]com|raw[.]githubusercontent[.]com)/", url)) {
    abort_cttir("Remote sources must use the fixed public GitHub endpoints.", "cttir_source_unavailable")
  }
  request <- httr2::request(url)
  request <- httr2::req_user_agent(request, "cttiR public-source catalog")
  request <- httr2::req_timeout(request, 30)
  request <- httr2::req_options(request, followlocation = FALSE, maxfilesize = max_bytes, netrc = 0L)
  tryCatch({
    response <- httr2::req_perform(request, path = path)
    if (httr2::resp_status(response) != 200L || !file.exists(path) || file.info(path)$size > max_bytes) {
      abort_cttir("Remote source response is incomplete or exceeds its bound.", "cttir_source_unavailable")
    }
  }, error = function(e) {
    abort_cttir("Public GitHub source retrieval failed; the prior snapshot is unchanged.", "cttir_source_unavailable")
  })
  invisible(path)
}

github_json <- function(url, max_bytes = 8000000L) {
  file <- tempfile("cttir-github-", fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  github_download(url, file, max_bytes)
  tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE), error = function(e) {
    abort_cttir("GitHub metadata could not be decoded.", "cttir_source_unavailable")
  })
}

github_source <- function(record) {
  started <- Sys.time()
  repository <- record$github
  scalar_text(repository, "GitHub repository")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9][A-Za-z0-9._-]*$", repository)) {
    abort_cttir("github must be an owner/repository identifier.")
  }
  ref <- if (is.null(record$ref)) "HEAD" else record$ref
  scalar_text(ref, "GitHub ref")
  if (nchar(ref) > 200L || !grepl("^[A-Za-z0-9][A-Za-z0-9._/-]*$", ref) || grepl("..", ref, fixed = TRUE)) {
    abort_cttir("Invalid GitHub ref.")
  }
  prefix <- if (is.null(record$subdir)) "" else record$subdir
  if (!identical(prefix, "")) {
    relative_file(prefix)
    prefix <- paste0(prefix, "/")
  }
  api <- paste0("https://api.github.com/repos/", repository)
  commit <- github_json(paste0(api, "/commits/", utils::URLencode(ref, reserved = TRUE)))
  sha <- commit$sha
  if (!is.character(sha) || length(sha) != 1L || !grepl("^[a-f0-9]{40}$", sha)) {
    abort_cttir("GitHub did not resolve a complete commit identity.", "cttir_source_unavailable")
  }
  tree <- github_json(paste0(api, "/git/trees/", sha, "?recursive=1"))
  if (!identical(tree$truncated, FALSE) || !is.list(tree$tree) || length(tree$tree) > 5000L) {
    abort_cttir("GitHub tree is incomplete or exceeds the inventory bound.", "cttir_source_unavailable")
  }
  stage <- tempfile("cttir-remote-source-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  total <- 0
  paths <- character()
  for (entry in tree$tree) {
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) > 120) {
      abort_cttir("Remote source exceeded the two-minute fetch budget.", "cttir_source_unavailable")
    }
    if (!is.list(entry) || !is.character(entry$path) || length(entry$path) != 1L || is.na(entry$path) ||
        !is.character(entry$type) || length(entry$type) != 1L || is.na(entry$type)) {
      abort_cttir("Remote tree contains malformed entries.", "cttir_source_unavailable")
    }
    if (!startsWith(entry$path, prefix)) next
    rel <- substring(entry$path, nchar(prefix) + 1L)
    if (!nzchar(rel) || entry$type == "tree") next
    selected <- rel %in% c("R", "man", "vignettes", "inst/doc") ||
      grepl("^(DESCRIPTION|NAMESPACE|README([.].*)?|NEWS([.].*)?|CHANGELOG([.].*)?|LICENSE([.].*)?|LICENCE([.].*)?|COPYING([.].*)?)$", rel) ||
      grepl("^R/[^/]+[.][Rr]$", rel) || grepl("^(man/|vignettes/|inst/doc/|inst/CITATION$)", rel)
    if (!selected) next
    relative_file(rel)
    if (!identical(entry$type, "blob") || !is.character(entry$mode) || length(entry$mode) != 1L || !entry$mode %in% c("100644", "100755") ||
        !is.numeric(entry$size) || length(entry$size) != 1L || is.na(entry$size) || entry$size < 0 || entry$size > 1048576L || entry$size != as.integer(entry$size) ||
        !is.character(entry$sha) || length(entry$sha) != 1L || !grepl("^[a-f0-9]{40}$", entry$sha)) {
      abort_cttir("Remote source contains an unsupported file or link.", "cttir_source_unavailable")
    }
    total <- total + entry$size
    if (total > 20000000L || tolower(rel) %in% tolower(paths)) {
      abort_cttir("Remote source exceeds its byte bound or has conflicting paths.", "cttir_source_unavailable")
    }
    paths <- c(paths, rel)
    if (length(paths) > 250L) abort_cttir("Remote source exceeds the 250-file fetch bound.", "cttir_source_unavailable")
    dest <- file.path(stage, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    url <- paste0("https://raw.githubusercontent.com/", repository, "/", sha, "/", paste(vapply(strsplit(entry$path, "/", fixed = TRUE)[[1]], utils::URLencode, character(1), reserved = TRUE), collapse = "/"))
    github_download(url, dest, 1048576L)
    bytes <- readBin(dest, "raw", n = 1048577L)
    header <- c(charToRaw(paste0("blob ", length(bytes))), as.raw(0L))
    if (length(bytes) != entry$size || !identical(digest::digest(c(header, bytes), algo = "sha1", serialize = FALSE), entry$sha)) {
      abort_cttir("Remote file does not match its committed Git blob.", "cttir_source_unavailable")
    }
  }
  if (!all(c("DESCRIPTION", "NAMESPACE") %in% paths)) {
    abort_cttir("The selected remote root is not a complete package source.", "cttir_source_unavailable")
  }
  result <- extract_source(stage, paste0("https://github.com/", repository), sha,
    family = "configured_github", documentation_rights = record$documentation_rights)
  result$source_subdir <- prefix
  result$freshness <- "public_commit_fetched"
  result
}
