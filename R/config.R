normalize_schema_value <- function(x, schema) {
  # Preserve JSON arrays of length one and empty objects across R/YAML round trips.
  type <- schema$type
  if (identical(type, "object") && is.list(x)) {
    if (!length(x)) {
      return(stats::setNames(list(), character()))
    }
    for (key in names(x)) {
      child <- schema$properties[[key]]
      if (!is.null(child)) x[key] <- list(normalize_schema_value(x[[key]], child))
    }
  } else if (identical(type, "array") && !is.null(x)) {
    if (is.atomic(x)) x <- as.list(x)
    if (is.list(x)) x <- lapply(x, normalize_schema_value, schema = schema$items)
  }
  x
}

# jsonvalidate's V8 engine initialises R's random number generator, which would
# create or advance .Random.seed in the user's workspace. Validation restores the
# caller's random state so it has no side effects.
.schema_validators <- new.env(parent = emptyenv())

json_schema_validate <- function(json, schema, verbose = FALSE, greedy = FALSE,
  error = FALSE, engine = "imjv", reference = NULL,
  query = NULL, strict = FALSE) {
  workspace <- globalenv()
  had <- exists(".Random.seed", envir = workspace, inherits = FALSE)
  saved <- if (had) get(".Random.seed", envir = workspace, inherits = FALSE) else NULL
  on.exit({
    if (had) {
      assign(".Random.seed", saved, envir = workspace)
    } else if (exists(".Random.seed", envir = workspace, inherits = FALSE)) {
      rm(".Random.seed", envir = workspace)
    }
  })
  cache <- .schema_validators
  if (!identical(cache$pid, Sys.getpid())) {
    cache$pid <- Sys.getpid()
    cache$entries <- list()
  }
  text <- if (is.character(schema) && length(schema) == 1L && !is.na(schema)) {
    if (file.exists(schema)) paste(readLines(schema, warn = FALSE), collapse = "\n") else schema
  } else {
    NULL
  }
  # A referenced file can change independently of the root schema. Compile
  # those schemas afresh; only self-contained schemas enter the bounded cache.
  reusable <- !is.null(text) && is.null(reference) && schema_is_self_contained(text)
  if (reusable) {
    key <- digest::digest(list(text, engine, strict), algo = "sha256")
    validator <- cache$entries[[key]]
    if (is.null(validator)) {
      validator <- jsonvalidate::json_validator(schema, engine, strict = strict)
      if (length(cache$entries) >= 8L) cache$entries <- cache$entries[-1L]
      cache$entries[[key]] <- validator
    }
  } else {
    validator <- jsonvalidate::json_validator(schema, engine, reference = reference,
      strict = strict)
  }
  validator(json, verbose, greedy, error, query)
}

schema_is_self_contained <- function(text) {
  external <- function(node) {
    if (!is.list(node)) return(FALSE)
    refs <- node[names(node) %in% c("$ref", "$dynamicRef", "$recursiveRef")]
    if (any(vapply(refs, function(x) {
      !is.character(x) || length(x) != 1L || is.na(x) || !startsWith(x, "#")
    }, logical(1)))) return(TRUE)
    any(vapply(node, external, logical(1)))
  }
  tryCatch(!external(jsonlite::fromJSON(text, simplifyVector = FALSE)),
    error = function(e) FALSE)
}

validate_document <- function(x, kind) {
  check_tree(x)
  suffix <- if (is.list(x) && isTRUE(x$schema_version == 1L) && kind %in% c("config", "project-spec")) "-v1" else ""
  schema_path <- resource_file("schema", paste0(kind, suffix, ".schema.json"))
  schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  normalized <- normalize_schema_value(x, schema)
  encoded <- json_text(normalized)
  if (nchar(encoded, type = "bytes") > 1048576L) {
    abort_cttir("Configuration exceeds 1 MiB.")
  }
  valid <- json_schema_validate(encoded, schema_path, engine = "ajv", verbose = TRUE)
  if (!isTRUE(valid)) {
    abort_cttir(paste("Invalid", kind, "document; check field names, types and allowed values."),
      "cttir_schema_error", "schema_validation",
      field = attr(valid, "errors")
    )
  }
  normalized
}

#' Validate customization input
#'
#' Validates a named list or a local JSON/YAML file without executing expressions,
#' making network requests or creating files. Unknown keys are rejected.
#' Version 1 configuration is normalized to version 2 in memory; the input file
#' is not rewritten. Explicit interoperability pins must match the active catalog.
#' @param config A named list or path to a JSON/YAML file.
#' @return Invisibly, the normalized configuration list. Invalid input raises a
#'   `cttir_schema_error` or `cttir_input_error` condition.
#' @export
#' @examples
#' validate_config(list(research = list(data_origin = "existing_dataset")))
validate_config <- function(config) {
  if (is.character(config)) config <- read_document(config)
  check_schema_version(config, "configuration")
  config <- validate_document(config, "config")
  if (isTRUE(config$schema_version == 1L)) {
    config$schema_version <- 2L
    config <- validate_document(config, "config")
  }
  check_empty_strings(config)
  for (key in c("publications", "data_sources", "packages")) {
    field <- if (key == "packages") "name" else "id"
    ids <- vapply(config[[key]], function(x) {
      if (is.null(x[[field]])) abort_cttir(paste(key, "entries require", field))
      x[[field]]
    }, character(1))
    if (anyDuplicated(tolower(ids))) abort_cttir(paste("Duplicate", key, "identities."), "cttir_schema_error")
  }
  validate_extension_pins(config$packages)
  invisible(config)
}

merge_config <- function(base, incoming) {
  for (key in names(incoming)) {
    value <- incoming[[key]]
    if (key %in% c("publications", "data_sources", "packages") && length(value)) {
      id <- if (key == "packages") "name" else "id"
      rows <- base[[key]]
      for (row in value) {
        if (is.null(row[[id]])) abort_cttir(paste(key, "entries require", id))
        ids <- vapply(rows, function(x) x[[id]], character(1))
        at <- match(row[[id]], ids)
        if (is.na(at)) rows <- append(rows, list(row)) else rows[[at]] <- merge_config(rows[[at]], row)
      }
      base[key] <- list(rows)
    } else if (is.list(value) && length(value) && !is.null(names(value)) &&
        is.list(base[[key]]) && !is.null(names(base[[key]]))) {
      base[key] <- list(merge_config(base[[key]], value))
    } else {
      base[key] <- list(value)
    }
  }
  base
}

# Requested interoperability versions must agree with the selected immutable
# catalog. This checks evidence only and never checks or installs local packages.
validate_extension_pins <- function(packages, catalog = NULL) {
  if (!length(packages)) return(invisible(TRUE))
  extension_names <- unique(unlist(lapply(interop_capabilities()$capabilities, function(x) x$packages)))
  requested <- Filter(function(x) {
    x$name %in% extension_names && (!is.null(x$version) || !is.null(x$revision))
  }, packages)
  if (!length(requested)) return(invisible(TRUE))
  if (is.null(catalog)) catalog <- resolve_catalog()
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(x) x$name, character(1)))
  for (pin in requested) {
    record <- index[[pin$name]]
    for (field in c("version", "revision")) {
      if (!is.null(pin[[field]]) && (is.null(record) || !identical(pin[[field]], record[[field]]))) {
        abort_cttir(paste0("The requested ", pin$name, " ", field, " does not match the selected catalog pin."),
          "cttir_schema_error", "incompatible_extension_pin", paste0("/packages/", pin$name, "/", field),
          remediation = "Use the selected catalog revision; update and review catalog evidence separately before changing pins.")
      }
    }
  }
  invisible(TRUE)
}
