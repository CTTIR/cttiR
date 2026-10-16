model_implementation_fingerprint <- function() {
  # Qualification covers the implementation that produced the evidence, not
  # just its entry points. Include helper bodies, defaults and static rule data;
  # mutable caches and namespace bookkeeping are not qualification inputs.
  namespace <- environment(model_implementation_fingerprint)
  binding_names <- sort(ls(namespace, all.names = TRUE), method = "radix")
  bindings <- lapply(binding_names, function(name) {
    value <- get(name, envir = namespace, inherits = FALSE)
    if (is.function(value)) {
      return(list(formals = paste(deparse(formals(value)), collapse = "\n"),
        body = paste(deparse(body(value)), collapse = "\n")))
    }
    inert <- function(x) is.null(x) || is.atomic(x) ||
      (is.list(x) && all(vapply(x, inert, logical(1))))
    if (inert(value)) return(list(value = value))
    NULL
  })
  names(bindings) <- binding_names
  bindings <- Filter(Negate(is.null), bindings)
  content_hash(json_text(list(binding_version = 1L, bindings = bindings,
    R = as.character(getRversion()), platform = R.version$platform,
    dependencies = model_dependency_versions())))
}

model_dependency_versions <- function() {
  dependencies <- function(package) {
    fields <- utils::packageDescription(package, fields = c("Imports", "Depends"))
    specs <- unlist(fields, use.names = FALSE)
    specs <- specs[!is.na(specs) & nzchar(specs)]
    if (!length(specs)) return(character())
    names <- trimws(sub("\\s*\\(.*$", "", unlist(strsplit(specs, ",", fixed = TRUE))))
    setdiff(names[nzchar(names)], "R")
  }
  pending <- dependencies("cttiR")
  seen <- character()
  while (length(pending)) {
    package <- pending[[1L]]
    seen <- c(seen, package)
    pending <- setdiff(unique(c(pending[-1L], dependencies(package))), seen)
  }
  seen <- sort(seen, method = "radix")
  loaded <- loadedNamespaces()
  stats::setNames(lapply(seen, function(package) as.character(
    if (package %in% loaded) getNamespaceVersion(package) else utils::packageVersion(package))), seen)
}
