# A bounded document subset for reviewed suggestions. Parse code and metadata
# without knitting, evaluating YAML tags, following includes or loading engines.
assistant_document_code <- function(path, text) {
  if (grepl("[.]R$", path)) return(list(code = text, prose = FALSE, problem = NULL))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  reject <- function(reason) list(code = character(), prose = TRUE, problem = reason)
  if (length(lines)) lines[[1]] <- sub("^\ufeff", "", lines[[1]])
  first <- which(nzchar(trimws(lines)))
  if (length(first) && first[[1]] > 1L) lines <- lines[seq.int(first[[1]], length(lines))]
  if (length(lines) && trimws(lines[[1]]) == "---") {
    end <- which(trimws(lines[-1L]) %in% c("---", "..."))
    if (!length(end)) return(reject("unterminated_document_metadata"))
    end <- end[[1]] + 1L
    metadata <- if (end > 2L) lines[seq.int(2L, end - 1L)] else character()
    if (any(grepl("(^|[[:space:]])!|(^|[[:space:]:])[*&][A-Za-z0-9_-]|`(r[[:space:]#]|\\{)|<[!/]?[A-Za-z]", metadata))) {
      return(reject("executable_document_metadata"))
    }
    header <- tryCatch(yaml::yaml.load(paste(metadata, collapse = "\n"), eval.expr = FALSE), error = function(e) FALSE)
    bounded <- tryCatch({check_tree(header); TRUE}, error = function(e) FALSE)
    if (!bounded) return(reject("invalid_document_metadata"))
    # Check decoded scalars as YAML quoted escapes can conceal raw TeX syntax.
    metadata_values <- as.character(unlist(header, recursive = TRUE, use.names = FALSE))
    if (any(grepl("<[!/]?[A-Za-z]", metadata_values))) return(reject("unsupported_document_markup"))
    if (any(grepl("\\\\[A-Za-z@]", metadata_values))) return(reject("unsupported_document_tex"))
    allowed <- c("title", "subtitle", "author", "date", "abstract", "keywords", "lang", "language", "output")
    inert <- function(x) is.null(x) || is.atomic(x) || (is.list(x) && all(vapply(x, inert, logical(1))))
    if (identical(header, FALSE) || (!is.null(header) && (!is.list(header) || is.null(names(header)) ||
        anyDuplicated(names(header)) || any(!names(header) %in% allowed) || !inert(header)))) {
      return(reject("unsupported_document_metadata"))
    }
    if (!is.null(header$output) && (!is.character(header$output) || length(header$output) != 1L ||
        !header$output %in% c("html_document", "pdf_document", "word_document"))) {
      return(reject("unsupported_document_output"))
    }
    lines <- if (end < length(lines)) lines[seq.int(end + 1L, length(lines))] else character()
  }
  code <- character(); fence <- NULL; engine <- NULL
  options <- c("echo", "eval", "include", "message", "warning", "error", "results", "fig.width", "fig.height",
    "fig.cap", "fig.align", "fig.show", "fig.keep", "dpi", "collapse", "comment")
  inline_code <- function(line) {
    engines <- regmatches(line, gregexpr("`\\{[^}]+\\}", line))[[1]]
    if (grepl("`r#", line) || any(engines != "`{r}")) return(list(problem = "unsupported_inline_engine"))
    inline <- regmatches(line, gregexpr("`(r|\\{r\\})[[:space:]]+[^`]+`", line))[[1]]
    remaining <- gsub("`(r|\\{r\\})[[:space:]]+[^`]+`", "", line)
    if (grepl("`(r|\\{r\\})[[:space:]]", remaining)) return(list(problem = "unterminated_inline_code"))
    list(code = sub("`$", "", sub("^`(r|\\{r\\})[[:space:]]+", "", inline)), problem = NULL)
  }
  for (line in lines) {
    if (!is.null(fence)) {
      closing <- regmatches(line, regexec("^[[:space:]>]*(`{3,}|~{3,})[[:space:]]*$", line))[[1]]
      if (length(closing) && substr(closing[[2]], 1L, 1L) == substr(fence, 1L, 1L) &&
          nchar(closing[[2]]) >= nchar(fence)) {
        fence <- NULL; engine <- NULL; next
      }
      if (grepl("^[[:space:]>]*(`{3,}|~{3,})[[:space:]]*\\{", line)) return(reject("nested_executable_fence"))
      if (identical(engine, "r")) {
        if (grepl("^[[:space:]]*#[|]", line)) return(reject("unsupported_document_options"))
        code <- c(code, line)
      } else {
        # Knitr evaluates inline R even inside ordinary Markdown code fences.
        inline <- inline_code(line)
        if (!is.null(inline$problem)) return(reject(inline$problem))
        code <- c(code, inline$code)
      }
      next
    }
    opening <- regmatches(line, regexec("^[[:space:]>]*(`{3,}|~{3,})(.*)$", line))[[1]]
    if (length(opening)) {
      fence <- opening[[2]]; info <- trimws(opening[[3]])
      if (startsWith(info, "{")) {
        chunk <- regmatches(info, regexec("^\\{[[:space:]]*([A-Za-z0-9_.]+)(.*)\\}$", info))[[1]]
        if (!length(chunk) || !identical(tolower(chunk[[2]]), "r")) return(reject("unsupported_document_engine"))
        engine <- "r"
        expression <- audit_chunk_options(chunk[[3]])
        if (length(expression)) {
          parsed <- tryCatch(parse(text = expression, keep.source = FALSE), error = function(e) NULL)
          if (is.null(parsed) || length(parsed) != 1L || !is.call(parsed[[1]]) ||
              !identical(parsed[[1]][[1]], as.name("list"))) return(reject("invalid_document_options"))
          args <- as.list(parsed[[1]])[-1L]
          if (is.null(names(args)) || any(!nzchar(names(args))) || anyDuplicated(names(args)) ||
              any(!names(args) %in% options) ||
              any(vapply(seq_along(args), function(i) empty_argument(args, i), logical(1)))) return(reject("unsupported_document_options"))
          code <- c(code, expression)
        }
      } else {
        if (!tolower(info) %in% c("", "r", "text", "plaintext", "markdown", "md")) return(reject("unsupported_document_engine"))
        engine <- if (tolower(info) == "r") "r" else "literal"
      }
      next
    }
    if (grepl("<[!/]?[A-Za-z]", line)) return(reject("unsupported_document_markup"))
    if (grepl("\\\\[A-Za-z@]", line)) return(reject("unsupported_document_tex"))
    inline <- inline_code(line)
    if (!is.null(inline$problem)) return(reject(inline$problem))
    code <- c(code, inline$code)
  }
  if (!is.null(fence)) return(reject("unterminated_document_fence"))
  list(code = code, prose = TRUE, problem = NULL)
}

assistant_validate_change <- function(change, catalog) {
  document <- assistant_document_code(change$path, change$content)
  if (!is.null(document$problem)) {
    return(structure(data.frame(call = "<document>", package = NA_character_, export = NA_character_,
      status = "rejected", reason = document$problem, stringsAsFactors = FALSE), valid = FALSE, static_complete = FALSE))
  }
  check <- validate_generated_code(document$code, catalog, approved_only = TRUE)
  # A prose-only document has no executable expressions to validate. This does
  # not claim factual review, rendering or permission to execute a document.
  if (document$prose && isTRUE(attr(check, "valid")) && nrow(check) == 0L) attr(check, "static_complete") <- TRUE
  check
}
