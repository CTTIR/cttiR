app_draft_validate <- function(value) {
  check_tree(value)
  fields <- c("schema_version", "operation", "mode", "project", "project_id", "config")
  if (!is.list(value) || !setequal(names(value), fields) ||
      !identical(value$schema_version, 1L) ||
      !is.character(value$operation) || length(value$operation) != 1L ||
      !value$operation %in% c("create", "configure") ||
      !is.character(value$mode) || length(value$mode) != 1L ||
      !value$mode %in% c("fast", "detailed")) {
    abort_cttir("This is not a supported cttiR draft.", "cttir_schema_error")
  }
  if (!is.list(value$config)) abort_cttir("Draft configuration must be a JSON object.", "cttir_schema_error")
  value$config <- validate_config(value$config)
  if (value$operation == "create") {
    project <- value$project
    if (!is.null(value$project_id) || !is.list(project) ||
        !setequal(names(project), c("name", "type", "goal"))) {
      abort_cttir("A creation draft must contain only name, type and goal.", "cttir_schema_error")
    }
    for (key in names(project)) {
      text <- project[[key]]
      if (!is.character(text) || length(text) != 1L || nchar(text, type = "bytes") > 65536L ||
          grepl("[[:cntrl:]]", if (key == "goal") gsub("[\r\n\t]", "", text) else text)) {
        abort_cttir("Draft identity values must be bounded plain text.", "cttir_schema_error")
      }
    }
    validate_config(list(project = list(type = project$type)))
  } else {
    if (!is.null(value$project)) abort_cttir("Configure drafts cannot change project identity.", "cttir_schema_error")
    scalar_text(value$project_id, "project_id")
  }
  if (nchar(json_text(value, TRUE), type = "bytes") > 1048576L) {
    abort_cttir("Draft exceeds 1 MiB.", "cttir_schema_error")
  }
  value
}

app_draft_read <- function(path) {
  if (!file.exists(path) || dir.exists(path) || is.na(file.info(path)$size) ||
      file.info(path)$size > 1048576L) {
    abort_cttir("Choose a draft JSON file no larger than 1 MiB.", "cttir_schema_error")
  }
  value <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) {
    abort_cttir("Draft must be valid JSON.", "cttir_schema_error")
  })
  app_draft_validate(value)
}
