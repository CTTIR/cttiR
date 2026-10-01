abort_cttir <- function(message, class = "cttir_input_error", code = "invalid_input",
  field = NULL, remediation = "Review the supplied input.") {
  stop(structure(
    list(
      message = message, call = NULL, code = code, field = field,
      remediation = remediation
    ),
    class = c(class, "cttir_error", "error", "condition")
  ))
}

scalar_text <- function(x, field) {
  checked <- if (is.character(x) && field %in% c("goal", "query", "question")) gsub("[\r\n\t]", "", x) else x
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)) ||
      nchar(x, type = "bytes") > 65536L || grepl("[[:cntrl:]]", checked)) {
    abort_cttir(paste(field, "must be one nonempty string without control characters."), field = field)
  }
  invisible(x)
}

scalar_flag <- function(x, field) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    abort_cttir(paste(field, "must be TRUE or FALSE."), field = field)
  }
  invisible(x)
}

resource_file <- function(...) {
  p <- system.file(..., package = "cttiR")
  if (!nzchar(p) || !file.exists(p)) {
    abort_cttir("A required installed resource is missing.", "cttir_source_unavailable",
      "missing_resource",
      remediation = "Reinstall cttiR from an intact source archive."
    )
  }
  p
}

json_text <- function(x, pretty = FALSE) {
  as.character(jsonlite::toJSON(x,
      auto_unbox = TRUE, null = "null",
      na = "null", pretty = pretty, digits = NA
    ))
}

content_hash <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)

read_document <- function(path) {
  scalar_text(path, "path")
  if (!file.exists(path) || dir.exists(path) || file.info(path)$size > 1048576) {
    abort_cttir("Configuration must be an existing file no larger than 1 MiB.")
  }
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tryCatch(
    {
      if (tolower(tools::file_ext(path)) == "json") {
        jsonlite::fromJSON(text, simplifyVector = FALSE)
      } else {
        yaml::yaml.load(text,
          eval.expr = FALSE,
          handlers = list(expr = function(x) structure(x, class = "cttir_forbidden_yaml_tag"))
        )
      }
    },
    error = function(e) {
      if (inherits(e, "cttir_error")) stop(e)
      abort_cttir("Configuration could not be parsed.", "cttir_schema_error", "parse_error")
    }
  )
}

check_tree <- function(x, depth = 0L) {
  if (depth > 32L) abort_cttir("Configuration nesting exceeds 32 levels.")
  if (is.object(x)) abort_cttir("Configuration must contain plain values without class attributes.")
  if (is.list(x)) {
    n <- names(x)
    if (!is.null(n) && (any(!nzchar(n)) || anyDuplicated(n))) {
      abort_cttir("Configuration keys must be unique and nonempty.")
    }
    for (v in x) check_tree(v, depth + 1L)
  } else if (!is.null(x) && (!is.atomic(x) || is.object(x) || anyNA(x))) {
    abort_cttir("Configuration must contain plain values, with NULL for unknowns.")
  }
  invisible(x)
}
