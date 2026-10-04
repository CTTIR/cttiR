# Read a deliberately small literal Authors@R grammar without evaluating calls.
description_roles <- function(literal) {
  unknown <- list(status = "unresolved", people = list(), ownership = "not_inferred")
  if (!is.character(literal) || length(literal) != 1L || is.na(literal) ||
      !nzchar(literal) || nchar(literal, type = "bytes") > 100000L) return(unknown)
  tryCatch({
    fail <- function() stop("Unsupported Authors@R syntax", call. = FALSE)
    vector <- function(x) {
      if (is.null(x) || identical(x, quote(NULL))) return(character())
      if (is.character(x)) return(x)
      if (!is.call(x) || !identical(x[[1]], quote(c))) fail()
      parts <- as.list(x)[-1L]
      if (!all(vapply(parts, is.character, logical(1)))) fail()
      unname(unlist(parts, use.names = FALSE))
    }
    person <- function(x) {
      if (!is.call(x) || !(identical(x[[1]], quote(person)) ||
            identical(x[[1]], quote(utils::person)))) fail()
      args <- as.list(x)[-1L]
      labels <- names(args)
      if (is.null(labels)) labels <- rep("", length(args))
      fields <- c("given", "family", "middle", "email", "role", "comment")
      named <- labels[nzchar(labels)]
      if (anyDuplicated(named) || any(!named %in% fields)) fail()
      available <- setdiff(fields, named)
      if (sum(!nzchar(labels)) > length(available)) fail()
      labels[!nzchar(labels)] <- utils::head(available, sum(!nzchar(labels)))
      values <- stats::setNames(lapply(args, vector), labels)
      if (!length(c(values$given, values$family))) fail()
      list(given = values$given, family = values$family, middle = values$middle,
        email = values$email, roles = values$role)
    }
    parsed <- parse(text = literal, keep.source = FALSE)
    if (length(parsed) != 1L) fail()
    expr <- parsed[[1L]]
    entries <- if (is.call(expr) && identical(expr[[1L]], quote(c))) as.list(expr)[-1L] else list(expr)
    if (!length(entries) || length(entries) > 1000L) fail()
    list(status = "literal_roles_parsed", people = lapply(entries, person), ownership = "not_inferred")
  }, error = function(e) unknown)
}

description_has_maintainer <- function(roles) {
  identical(roles$status, "literal_roles_parsed") && any(vapply(roles$people, function(x) {
    "cre" %in% x$roles && length(x$email) == 1L &&
      !is.na(x$email) && grepl("^[^[:space:]@]+@[^[:space:]@]+$", x$email)
  }, logical(1)))
}
