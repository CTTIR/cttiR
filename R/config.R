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
json_schema_validate <- function(...) {
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
  jsonvalidate::json_validate(...)
}

validate_document <- function(x, kind) {
  check_tree(x)
  schema_path <- resource_file("schema", paste0(kind, ".schema.json"))
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
  check_empty_strings(config)
  for (key in c("publications", "data_sources", "packages")) {
    field <- if (key == "packages") "name" else "id"
    ids <- vapply(config[[key]], function(x) {
      if (is.null(x[[field]])) abort_cttir(paste(key, "entries require", field))
      x[[field]]
    }, character(1))
    if (anyDuplicated(tolower(ids))) abort_cttir(paste("Duplicate", key, "identities."), "cttir_schema_error")
  }
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
