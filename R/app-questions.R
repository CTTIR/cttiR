# Adaptive questionnaire registry for the Detailed view. Questions are static
# data: each targets one JSON Pointer of the customization schema and uses only
# declarative relevance conditions. Answers become explicit `options` for the
# same project()/sync() calls that the R API uses.

.app_question_cache <- new.env(parent = emptyenv())

app_answer_statuses <- c("answered", "skipped", "review")

app_config_schema <- function() {
  if (is.null(.app_question_cache$schema)) {
    .app_question_cache$schema <- jsonlite::fromJSON(resource_file("schema", "config.schema.json"), simplifyVector = FALSE)
  }
  .app_question_cache$schema
}

app_questions <- function() {
  if (is.null(.app_question_cache$registry)) {
    doc <- jsonlite::fromJSON(resource_file("extdata", "questions.json"), simplifyVector = FALSE)
    .app_question_cache$registry <- app_questions_check(doc)
  }
  .app_question_cache$registry
}

# Resolve a pointer against the schema; numeric segments address array items.
app_schema_node <- function(pointer, schema = app_config_schema()) {
  parts <- strsplit(sub("^/", "", pointer), "/", fixed = TRUE)[[1]]
  node <- schema
  for (part in parts) {
    if ("array" %in% unlist(node$type)) {
      if (!grepl("^[0-9]+$", part)) return(NULL)
      node <- node$items
    } else {
      if (grepl("^[0-9]+$", part)) return(NULL)
      node <- node$properties[[part]]
    }
    if (is.null(node)) return(NULL)
  }
  node
}

app_condition_refs <- function(condition) {
  if (is.null(condition)) return(character())
  if (!is.null(condition$all)) return(unique(unlist(lapply(condition$all, app_condition_refs))))
  if (!is.null(condition$any)) return(unique(unlist(lapply(condition$any, app_condition_refs))))
  condition$question
}

app_condition_values <- function(condition) {
  if (is.null(condition)) return(list())
  if (!is.null(condition$all) || !is.null(condition$any)) {
    return(do.call(c, lapply(c(condition$all, condition$any), app_condition_values)))
  }
  values <- c(unlist(condition[["in"]]), unlist(condition[["not_in"]]))
  if (!length(values)) return(list())
  stats::setNames(list(values), condition$question)
}

# Kahn's algorithm over depends_on; a cycle is a registry error.
app_question_order <- function(questions) {
  ids <- names(questions)
  remaining <- stats::setNames(lapply(questions, function(q) unlist(q$depends_on)), ids)
  order <- character()
  while (length(remaining)) {
    ready <- names(remaining)[vapply(remaining, function(d) all(d %in% order), logical(1))]
    if (!length(ready)) {
      abort_cttir("The questionnaire dependency graph contains a cycle.", "cttir_schema_error", "question_cycle")
    }
    order <- c(order, ready)
    remaining <- remaining[setdiff(names(remaining), ready)]
  }
  order
}

app_questions_check <- function(doc) {
  fail <- function(message) abort_cttir(message, "cttir_schema_error", "invalid_question_registry")
  valid <- jsonvalidate::json_validate(json_text(doc), resource_file("schema", "questions.schema.json"),
    engine = "ajv", verbose = TRUE)
  if (!isTRUE(valid)) fail("The questionnaire registry does not match its schema.")
  sections <- vapply(doc$sections, function(s) s$id, character(1))
  if (anyDuplicated(sections)) fail("Questionnaire sections must be unique.")
  ids <- vapply(doc$questions, function(q) q$id, character(1))
  if (anyDuplicated(ids)) fail("Question IDs must be unique.")
  questions <- stats::setNames(doc$questions, ids)
  for (i in seq_along(questions)) {
    q <- questions[[i]]
    where <- function(text) fail(paste0("Question ", q$id, ": ", text))
    if (!q$section %in% sections) where("unknown section.")
    node <- app_schema_node(q$pointer)
    if (is.null(node)) where("pointer is not part of the configuration schema.")
    types <- unlist(node$type)
    compatible <- switch(q$type,
      choice = !is.null(node$enum) || "string" %in% types,
      text = "string" %in% types,
      boolean = "boolean" %in% types,
      list = "array" %in% types
    )
    if (!compatible) where("type does not match the schema.")
    deps <- unlist(q$depends_on)
    if (!all(deps %in% ids[seq_len(i - 1L)])) where("dependencies must name earlier questions.")
    refs <- app_condition_refs(q$relevance)
    if (!setequal(deps, unique(c(refs, q$choices_from)))) where("depends_on must equal the questions used by relevance and choices_from.")
    if (q$type == "choice") {
      if (is.null(q[["choices"]]) == is.null(q$choices_from)) where("needs exactly one of choices or choices_from.")
      if (!is.null(q[["choices"]]) && !is.null(node$enum) && !setequal(unlist(q[["choices"]]), unlist(node$enum))) {
        where("choices must equal the schema enum.")
      }
      if (!is.null(q$choices_from) && !identical(questions[[q$choices_from]]$type, "list")) where("choices_from must name a list question.")
    } else if (!is.null(q[["choices"]]) || !is.null(q$choices_from)) {
      where("only choice questions have choices.")
    }
    allowed <- if (q$type == "boolean") c("true", "false") else unlist(q[["choices"]])
    if (!all(unlist(q$pending_choices) %in% allowed)) where("pending choices must be valid choices.")
    if (!is.null(q$entries)) {
      item <- node$items
      if (q$type != "list" || !identical(item$type, "object")) where("entries require a list of objects.")
      fields <- c("id", q$entries$label_field, if (isTRUE(q$entries$slug)) "slug", names(q$entries$defaults))
      if (!all(fields %in% names(item$properties))) where("entry fields must exist in the item schema.")
    } else if (q$type == "list" && identical(node$items$type, "object")) {
      where("a list of objects requires entries.")
    }
    used <- app_condition_values(q$relevance)
    for (ref in names(used)) {
      target <- questions[[ref]]
      if (identical(target$type, "choice") && !is.null(target[["choices"]]) && !all(used[[ref]] %in% unlist(target[["choices"]]))) {
        where("relevance values must be choices of the referenced question.")
      }
    }
  }
  app_question_order(questions)
  section_labels <- stats::setNames(lapply(doc$sections, function(s) s$labels), sections)
  list(sections = sections, section_labels = section_labels, questions = questions)
}

app_question_dependents <- function(id, questions) {
  found <- character()
  frontier <- id
  while (length(frontier)) {
    next_ids <- names(questions)[vapply(questions, function(q) any(unlist(q$depends_on) %in% frontier), logical(1))]
    next_ids <- setdiff(next_ids, c(found, id))
    found <- c(found, next_ids)
    frontier <- next_ids
  }
  found
}

# Resolve a pointer inside a resolved spec; array segments are positions or IDs.
app_spec_value <- function(spec, pointer) {
  if (is.null(spec)) return(NULL)
  parts <- strsplit(sub("^/", "", pointer), "/", fixed = TRUE)[[1]]
  node <- spec
  for (part in parts) {
    if (is.null(node)) return(NULL)
    if (is.list(node) && is.null(names(node))) {
      if (grepl("^[0-9]+$", part)) {
        index <- as.integer(part) + 1L
        node <- if (index <= length(node)) node[[index]] else NULL
      } else {
        ids <- vapply(node, function(x) if (is.list(x) && is.character(x$id)) x$id else "", character(1))
        at <- match(part, ids)
        node <- if (is.na(at)) NULL else node[[at]]
      }
    } else if (is.list(node)) {
      node <- node[[part]]
    } else {
      return(NULL)
    }
  }
  node
}

# Static defaults of the builder, used to number new entries in create mode.
app_default_base <- function() {
  default_spec("Draft", "other", "Draft", "draft", provenance = list(catalog_id = "none"))
}

app_base_value <- function(q, base) {
  value <- app_spec_value(base, q$pointer)
  if (q$type != "list" || is.null(q$entries)) return(value)
  if (isTRUE(q$entries$skip_first_base) && length(value)) value <- value[-1]
  labels <- vapply(value, function(x) {
    label <- x[[q$entries$label_field]]
    if (is.character(label) && length(label) == 1L) label else ""
  }, character(1))
  if (length(labels)) as.list(labels) else NULL
}

app_condition_true <- function(condition, values) {
  if (is.null(condition)) return(TRUE)
  if (!is.null(condition$all)) return(all(vapply(condition$all, app_condition_true, logical(1), values = values)))
  if (!is.null(condition$any)) return(any(vapply(condition$any, app_condition_true, logical(1), values = values)))
  value <- values[[condition$question]]
  if (!is.null(condition$answered)) return(identical(length(value) > 0L, isTRUE(condition$answered)))
  if (is.null(value) || !length(value)) return(FALSE)
  value <- as.character(unlist(value))[[1]]
  if (!is.null(condition[["in"]])) return(value %in% unlist(condition[["in"]]))
  !value %in% unlist(condition[["not_in"]])
}

# Relevance and effective values in dependency order. Only explicit answers and
# the static base specification participate, never a preview result.
app_question_state <- function(answers, questions, base = NULL) {
  values <- list()
  relevant <- stats::setNames(logical(length(questions)), names(questions))
  for (id in app_question_order(questions)) {
    q <- questions[[id]]
    relevant[[id]] <- app_condition_true(q$relevance, values)
    entry <- answers[[id]]
    value <- if (!is.null(entry) && identical(entry$status, "answered")) entry$value else app_base_value(q, base)
    if (relevant[[id]] && !is.null(value)) values[id] <- list(value)
  }
  list(relevant = relevant, values = values)
}

app_entry_slug <- function(id, label) {
  slug <- tryCatch(safe_slug(label), error = function(e) "")
  slug <- if (nzchar(slug)) paste0(id, "_", slug) else id
  sub("_+$", "", substr(slug, 1L, 80L))
}

app_list_entries <- function(q, labels, base) {
  spec <- q$entries
  existing <- app_spec_value(base, q$pointer)
  used <- vapply(existing, function(x) as.character(x$id), character(1))
  known <- if (isTRUE(spec$skip_first_base) && length(existing)) existing[-1] else existing
  known_labels <- vapply(known, function(x) {
    label <- x[[spec$label_field]]
    if (is.character(label) && length(label) == 1L) label else ""
  }, character(1))
  counter <- 0L
  out <- list()
  for (label in unlist(labels)) {
    at <- match(label, known_labels)
    if (!is.na(at)) {
      out[[length(out) + 1L]] <- list(id = known[[at]]$id, label = label, existing = TRUE, entry = NULL)
      next
    }
    repeat {
      counter <- counter + 1L
      id <- sprintf("%s%02d", spec$id_prefix, counter)
      if (!tolower(id) %in% tolower(used)) break
    }
    used <- c(used, id)
    entry <- list(id = id)
    entry[[spec$label_field]] <- label
    if (isTRUE(spec$slug)) entry$slug <- app_entry_slug(id, label)
    entry <- c(entry, spec$defaults)
    out[[length(out) + 1L]] <- list(id = id, label = label, existing = FALSE, entry = entry)
  }
  out
}

# Choices for a question; choices_from lists the IDs of entries of a list question.
app_question_choices <- function(q, answers, questions, base = NULL) {
  if (is.null(q$choices_from)) return(unlist(q[["choices"]]))
  source <- questions[[q$choices_from]]
  entry <- answers[[source$id]]
  labels <- if (!is.null(entry) && identical(entry$status, "answered")) entry$value else app_base_value(source, base)
  current <- app_spec_value(base, source$pointer)
  ids <- vapply(current, function(x) as.character(x$id), character(1))
  rows <- app_list_entries(source, labels, base)
  ids <- unique(c(ids, vapply(rows, function(x) x$id, character(1))))
  labels_by_id <- stats::setNames(vapply(current, function(x) {
    label <- x[[source$entries$label_field]]
    if (is.character(label) && length(label) == 1L) label else x$id
  }, character(1)), vapply(current, function(x) as.character(x$id), character(1)))
  for (row in rows) labels_by_id[[row$id]] <- row$label
  stats::setNames(ids, unname(labels_by_id[ids]))
}

app_answer_parse <- function(q, raw, choices = unlist(q[["choices"]])) {
  if (is.null(raw) || !length(raw)) return(NULL)
  text <- as.character(raw)[[1]]
  if (is.na(text)) return(NULL)
  switch(q$type,
    choice = if (nzchar(text) && text %in% choices) text else NULL,
    text = if (nzchar(trimws(text))) trimws(text) else NULL,
    boolean = if (identical(text, "true")) TRUE else if (identical(text, "false")) FALSE else NULL,
    list = {
      items <- trimws(unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE))
      items <- unique(items[nzchar(items)])
      if (length(items)) as.list(items) else NULL
    }
  )
}

app_answer_input_value <- function(q, value) {
  if (is.null(value)) return("")
  switch(q$type,
    boolean = if (isTRUE(value)) "true" else "false",
    list = paste(unlist(value), collapse = "\n"),
    as.character(value)
  )
}

# An explicit change marks every dependent explicit answer for review instead
# of dropping it. Re-sending an identical value (for example after a re-render)
# changes nothing, so review status is only cleared deliberately.
app_answers_update <- function(answers, id, value, questions) {
  old <- answers[[id]]
  if (is.null(value)) {
    if (is.null(old) || is.null(old$value)) return(answers)
    answers[[id]] <- NULL
  } else {
    if (!is.null(old) && identical(old$value, value)) return(answers)
    answers[[id]] <- list(status = "answered", value = value)
  }
  for (dependent in app_question_dependents(id, questions)) {
    if (identical(answers[[dependent]]$status, "answered")) answers[[dependent]]$status <- "review"
  }
  answers
}

app_answers_confirm <- function(answers, id) {
  if (identical(answers[[id]]$status, "review")) answers[[id]]$status <- "answered"
  answers
}

app_answers_skip <- function(answers, ids) {
  for (id in ids) {
    if (is.null(answers[[id]])) answers[[id]] <- list(status = "skipped", value = NULL)
  }
  answers
}

app_set_path <- function(x, keys, value) {
  if (length(keys) == 1L) {
    x[keys] <- list(value)
    return(x)
  }
  child <- x[[keys[[1]]]]
  if (is.null(child)) child <- list()
  x[[keys[[1]]]] <- app_set_path(child, keys[-1], value)
  x
}

# Assemble explicit options from relevant, confirmed answers. Hidden or
# review-pending answers never reach the configuration.
app_answers_options <- function(answers, questions, base = NULL) {
  state <- app_question_state(answers, questions, base)
  config <- list()
  for (id in names(questions)) {
    q <- questions[[id]]
    entry <- answers[[id]]
    if (is.null(entry) || !identical(entry$status, "answered") || !isTRUE(state$relevant[[id]])) next
    keys <- strsplit(sub("^/", "", q$pointer), "/", fixed = TRUE)[[1]]
    if (!is.null(q$entries)) {
      fresh <- Filter(function(x) !x$existing, app_list_entries(q, entry$value, base))
      if (length(fresh)) config[[keys[[1]]]] <- c(config[[keys[[1]]]], lapply(fresh, function(x) x$entry))
      next
    }
    value <- if (q$type == "list") as.list(unlist(entry$value)) else entry$value
    index <- match(TRUE, grepl("^[0-9]+$", keys))
    if (is.na(index)) {
      config <- app_set_path(config, keys, value)
      next
    }
    array_key <- keys[seq_len(index - 1L)]
    target <- app_spec_value(base, paste0("/", paste(keys[seq_len(index)], collapse = "/")))
    if (is.null(target$id)) next
    rows <- if (length(array_key) == 1L) config[[array_key]] else NULL
    ids <- vapply(rows, function(x) x$id, character(1))
    at <- match(target$id, ids)
    if (is.na(at)) {
      rows <- c(list(list(id = target$id)), rows)
      at <- 1L
    }
    rows[[at]] <- app_set_path(rows[[at]], keys[-seq_len(index)], value)
    config[[array_key]] <- rows
  }
  config
}

app_answer_value_valid <- function(q, value, choices) {
  bounded <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x) && nchar(x, type = "bytes") <= 65536L && !grepl("[[:cntrl:]]", x)
  switch(q$type,
    choice = bounded(value) && (is.null(choices) || value %in% choices),
    text = bounded(value),
    boolean = is.logical(value) && length(value) == 1L && !is.na(value),
    list = is.list(value) && length(value) >= 1L && length(value) <= 50L && all(vapply(value, bounded, logical(1)))
  )
}

# Validate a stored answer set, for example from an imported draft.
app_answers_validate <- function(answers, questions) {
  fail <- function() abort_cttir("Draft answers are malformed or name unknown questions.", "cttir_schema_error", "invalid_draft_answers")
  if (is.null(answers) || (is.list(answers) && !length(answers))) return(stats::setNames(list(), character()))
  if (!is.list(answers) || is.null(names(answers)) || any(!nzchar(names(answers))) || anyDuplicated(names(answers))) fail()
  for (id in names(answers)) {
    q <- questions[[id]]
    entry <- answers[[id]]
    if (is.null(q) || !is.list(entry) || is.null(names(entry)) || !all(names(entry) %in% c("status", "value")) ||
        !is.character(entry$status) || length(entry$status) != 1L || !entry$status %in% app_answer_statuses) {
      fail()
    }
    if (identical(entry$status, "skipped")) {
      if (!is.null(entry$value)) fail()
    } else if (!app_answer_value_valid(q, entry$value, if (is.null(q$choices_from)) unlist(q[["choices"]]) else NULL)) {
      fail()
    }
  }
  answers
}

# Map a schema error location to the question that edits it.
app_pointer_question <- function(pointer, questions) {
  if (!is.character(pointer) || length(pointer) != 1L || !startsWith(pointer, "/")) return(NULL)
  candidates <- c(pointer, sub("/[0-9]+(/.*)?$", "", pointer))
  for (candidate in unique(candidates)) {
    for (q in questions) if (identical(q$pointer, candidate)) return(q$id)
  }
  NULL
}
