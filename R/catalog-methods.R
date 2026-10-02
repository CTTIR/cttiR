static_s3_methods <- function(namespace, functions, topics) {
  atom <- function(x) {
    if (is.symbol(x)) return(as.character(x))
    if (is.character(x) && length(x) == 1L && !is.na(x)) return(x)
    NULL
  }
  records <- list()
  for (expr in namespace) {
    if (!is.call(expr) || !identical(expr[[1]], as.name("S3method"))) next
    declaration <- paste(deparse(expr), collapse = " ")
    generic <- class <- implementation <- NULL
    parts <- as.list(expr)
    incomplete <- any(vapply(seq_along(parts), function(i) {
      identical(unname(parts[i]), unname(alist(x = )))
    }, logical(1)))
    if (length(expr) %in% c(3L, 4L) && !incomplete) {
      generic <- atom(expr[[2]])
      if (is.call(expr[[2]]) && length(expr[[2]]) == 3L && identical(expr[[2]][[1]], as.name("::"))) {
        owner <- atom(expr[[2]][[2]])
        symbol <- atom(expr[[2]][[3]])
        if (!is.null(owner) && !is.null(symbol)) generic <- paste0(owner, "::", symbol)
      }
      class <- atom(expr[[3]])
      if (!is.null(generic) && !is.null(class)) {
        implementation <- if (length(expr) == 4L) atom(expr[[4]]) else paste0(sub("^.*::", "", generic), ".", class)
      }
    }
    fn <- if (is.null(implementation)) NULL else functions[[implementation]]
    resolved <- !is.null(fn) && !startsWith(fn$signature, "unresolved")
    records[[length(records) + 1L]] <- list(
      declaration = declaration, generic = generic, class = class, implementation = implementation,
      signature = if (resolved) fn$signature else "unresolved",
      arguments = if (resolved) fn$arguments else list(),
      source_path = if (is.null(fn)) "NAMESPACE" else fn$source_path,
      documentation = if (is.null(implementation)) NULL else topics[[implementation]],
      verification = if (resolved) "static_method_verified" else "unknown", approved = FALSE)
  }
  keys <- vapply(records, function(x) paste(x$generic, x$class, sep = "\r"), character(1))
  duplicate <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  for (i in which(duplicate)) {
    records[[i]]$verification <- "unknown"
    records[[i]]$signature <- "unresolved: duplicate dispatch registration"
    records[[i]]$arguments <- list()
  }
  records
}

method_hits <- function(package, query) {
  q <- tolower(query)
  result <- list()
  for (method in package$s3_methods) {
    text <- paste(method$generic, method$class, method$implementation, method$declaration)
    symbol <- paste(package$name, "S3", method$generic, method$class, sep = "/")
    if (!grepl(q, tolower(paste(symbol, text, paste0(package$name, "::", method$generic))), fixed = TRUE)) next
    result[[length(result) + 1L]] <- list(
      id = content_hash(paste(package$source_hash, method$declaration)), symbol = symbol,
      snippet = paste("S3 declaration:", text, method$signature,
        "Installed dispatch and workflow approval not established."),
      source_path = method$source_path, verification = method$verification)
  }
  result
}
