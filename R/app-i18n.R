# English/German message catalog for the local application. Static interface
# text carries a data-i18n key and is replaced in the browser on a language
# change; server-rendered output is translated directly. File names, IDs and
# domain error details stay unchanged.

.app_message_cache <- new.env(parent = emptyenv())

app_languages <- c(English = "en", Deutsch = "de")

app_messages <- function() {
  if (is.null(.app_message_cache$messages)) {
    doc <- jsonlite::fromJSON(resource_file("extdata", "app-messages.json"), simplifyVector = FALSE)
    if (!identical(doc$schema_version, 1L) || !setequal(names(doc$messages), unname(app_languages))) {
      abort_cttir("The application message catalog is malformed.", "cttir_schema_error", "invalid_message_catalog")
    }
    .app_message_cache$messages <- doc$messages
  }
  .app_message_cache$messages
}

app_lang <- function(lang) if (is.character(lang) && length(lang) == 1L && lang %in% app_languages) lang else "en"

app_has_message <- function(key) !is.null(app_messages()$en[[key]])

app_t <- function(key, lang = "en", ...) {
  messages <- app_messages()
  text <- messages[[app_lang(lang)]][[key]]
  if (is.null(text)) text <- messages$en[[key]]
  if (is.null(text)) text <- key
  args <- list(...)
  for (name in names(args)) {
    text <- gsub(paste0("{", name, "}"), paste(as.character(args[[name]]), collapse = ", "), text, fixed = TRUE)
  }
  text
}

# Static text node that the browser translates in place.
app_tr <- function(key) shiny::tags$span(`data-i18n` = key, app_t(key, "en"))

# Human label for an enum value or field name; unknown values stay literal.
app_value_label <- function(value, lang = "en", prefix = "value.") {
  if (is.null(value) || !length(value)) return("")
  value <- as.character(value)[[1]]
  key <- paste0(prefix, value)
  if (app_has_message(key)) app_t(key, lang) else value
}

app_choice_names <- function(values, lang = "en") {
  stats::setNames(values, vapply(values, app_value_label, character(1), lang = lang))
}
