# Static validation of generated R code against one catalog revision.
#
# The text is parsed for its syntax tree only and is never evaluated, sourced or
# deparsed back into executable form. A passing result means every namespaced
# call resolves to a statically verified (and, by default, workflow-approved)
# export of the supplied catalog revision with plausible argument names, and
# that no forbidden or dynamic construct was seen. It is not a semantic,
# scientific or safety proof of the code.

# Base functions that may be called without `::`. They neither evaluate text,
# load code, touch the file system or network, nor change global options.
safe_base_calls <- c(
  "abs", "all", "any", "anyNA", "append", "apply", "array", "as.character", "as.Date", "as.double", "as.factor",
  "as.integer", "as.list", "as.logical", "as.numeric", "as.vector", "attr", "c", "cbind", "ceiling",
  "character", "class", "colMeans", "colnames", "colSums", "cumsum", "cut", "data.frame", "diff", "dim",
  "do.call", "droplevels", "duplicated", "endsWith", "exp", "factor", "Filter", "floor", "format", "formatC",
  "identical", "ifelse", "inherits", "integer", "intersect", "invisible", "is.character",
  "is.data.frame", "is.element", "is.factor", "is.finite", "is.function", "is.list", "is.logical", "is.na",
  "is.null", "is.numeric", "isFALSE", "isTRUE", "lapply", "length", "levels", "list", "log", "log10",
  "log1p", "log2", "logical", "Map", "mapply", "match", "match.arg", "matrix", "max", "mean", "merge",
  "message", "min", "missing", "names", "nchar", "ncol", "nlevels", "nrow", "numeric", "nzchar", "order",
  "outer", "paste", "paste0", "pmax", "pmin", "print", "prod", "range", "rank", "rbind", "Reduce", "rep", "rep_len",
  "rev", "round", "rownames", "rowMeans", "rowSums", "sapply", "scale", "seq", "seq_along", "seq_len",
  "set.seed", "setdiff", "signif", "sort", "split", "sprintf", "sqrt", "startsWith", "stop", "stopifnot",
  "structure", "subset", "substr", "substring", "sum", "summary", "suppressMessages", "suppressWarnings",
  "switch", "Sys.Date", "Sys.time", "t", "table", "tabulate", "tapply", "tolower", "toupper", "trimws", "tryCatch",
  "typeof", "union", "unique", "unlist", "unname", "vapply", "vector", "warning", "which", "which.max",
  "which.min", "with", "xor"
)

# Language syntax that is walked but needs no evidence of its own.
syntax_calls <- c(
  "<-", "=", "{", "(", "if", "for", "while", "repeat", "break", "next", "function", "return",
  "+", "-", "*", "/", "^", "%%", "%/%", "==", "!=", "<", ">", "<=", ">=", "&", "&&", "|", "||", "!",
  "$", "[", "[[", ":", "~", "%in%"
)

# Model-formula specials resolved by the modelling function, never called directly.
formula_terms <- c("I", "offset", "log", "exp", "sqrt", "poly", "factor", "as.factor", "scale", "s", "te",
  "ti", "t2", "strata", "cluster", "frailty", "tt", "pspline", "ns", "bs", "interaction", "relevel", "cut")

# Constructs that evaluate text, load or attach code, touch processes/network or
# rebind namespaces. Rejected bare, namespaced or passed as a function value.
forbidden_calls <- c(
  "eval", "evalq", "eval.parent", "eval_tidy", "eval_bare", "exec", "inject", "source", "sys.source",
  "system", "system2", "shell", "shell.exec", "Sys.setenv", "Sys.unsetenv", "Sys.setlocale", "Sys.chmod",
  "library", "require", "requireNamespace", "loadNamespace", "attachNamespace", "attach", "install.packages",
  "remove.packages", "update.packages", "download.file", "download.packages", "get", "get0", "mget",
  "getFromNamespace", "getExportedValue", "getAnywhere", "match.fun", "parse", "str2lang", "str2expression",
  "parse_expr", "parse_exprs", "assign", "assignInNamespace", "assignInMyNamespace", "setwd", "unlink",
  "file.remove", "file.rename", "dyn.load", ".Call", ".External", ".Internal", ".Primitive", "reg.finalizer",
  "q", "quit", "<<-"
)

# Function-valued argument of base higher-order calls; it must be a symbol, a
# function literal or a namespaced reference, never a character name.
higher_order_formals <- list(
  lapply = c("X", "FUN"), sapply = c("X", "FUN"), vapply = c("X", "FUN"), mapply = "FUN", Map = "f",
  Reduce = "f", Filter = "f", apply = c("X", "MARGIN", "FUN"), tapply = c("X", "INDEX", "FUN"),
  outer = c("X", "Y", "FUN"), do.call = "what"
)

# An omitted argument such as the gap in `x[, 1]` is an empty symbol.
empty_argument <- function(parts, i) identical(unname(parts[i]), unname(alist(x = )))

#' Validate generated R code against a catalog revision without evaluating it
#'
#' @param text Character vector of R code (lines are joined).
#' @param catalog Catalog list (for example from `resolve_catalog()`); `NULL`
#'   selects the active catalog.
#' @param approved_only Require every namespaced export to be covered by a
#'   complete workflow approval of that revision.
#' @return A data frame `call, package, export, status, reason` with attribute
#'   `valid` that is `TRUE` only when every row has status `ok`.
#' @noRd
validate_generated_code <- function(text, catalog = NULL, approved_only = TRUE) {
  scalar_flag(approved_only, "approved_only")
  if (!is.character(text) || anyNA(text)) abort_cttir("text must be a character vector of R code.")
  text <- paste(text, collapse = "\n")
  if (nchar(text, type = "bytes") > 1048576L) abort_cttir("Generated code exceeds the one-megabyte bound.")
  acc <- new.env(parent = emptyenv())
  acc$rows <- data.frame(call = character(), package = character(), export = character(), status = character(),
    reason = character(), stringsAsFactors = FALSE)
  finish <- function(rows) structure(rows, valid = nrow(rows) == 0L || all(rows$status == "ok"))
  expressions <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) e)
  if (inherits(expressions, "error")) {
    message <- strsplit(conditionMessage(expressions), "\n", fixed = TRUE)[[1]][[1]]
    acc$rows[1L, ] <- list("<text>", NA_character_, NA_character_, "parse_error", paste("parse_error:", message))
    return(finish(acc$rows))
  }
  if (is.null(catalog)) catalog <- resolve_catalog()
  packages <- stats::setNames(catalog$packages, vapply(catalog$packages, function(x) x$name, character(1)))
  cache <- new.env(parent = emptyenv())
  approved <- function(package) {
    if (is.null(cache[[package$name]])) assign(package$name, approved_export_index(package), envir = cache)
    cache[[package$name]]
  }
  acc$local <- character()
  collect <- function(expr, depth = 0L) {
    if (!is.call(expr) || depth > 256L) return(invisible(NULL))
    if (is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("<-", "=") && length(expr) == 3L &&
        is.symbol(expr[[2]]) && is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) {
      acc$local <- c(acc$local, as.character(expr[[2]]))
    }
    parts <- as.list(expr)[-1]
    for (i in seq_along(parts)) if (!empty_argument(parts, i) && is.call(parts[[i]])) collect(parts[[i]], depth + 1L)
    invisible(NULL)
  }
  for (expr in expressions) collect(expr)
  local_functions <- setdiff(unique(acc$local), forbidden_calls)
  add <- function(expr, package, export, status, reason) {
    text <- substr(paste(deparse(expr, width.cutoff = 120L, nlines = 2L), collapse = " "), 1L, 200L)
    acc$rows[nrow(acc$rows) + 1L, ] <- list(text, package, export, status, reason)
  }
  namespaced <- function(x) {
    is.call(x) && length(x) == 3L && (identical(x[[1]], as.name("::")) || identical(x[[1]], as.name(":::")))
  }
  check_namespaced <- function(ref, call = NULL) {
    expr <- if (is.null(call)) ref else call
    package <- static_atom(ref[[2]])
    export <- static_atom(ref[[3]])
    if (is.null(package) || is.null(export)) return(add(expr, NA_character_, NA_character_, "rejected", "nonliteral_namespace_reference"))
    if (identical(ref[[1]], as.name(":::"))) return(add(expr, package, export, "rejected", "internal_triple_colon"))
    if (export %in% forbidden_calls) return(add(expr, package, export, "rejected", paste0("forbidden_call:", export)))
    p <- packages[[package]]
    if (is.null(p)) {
      if (identical(package, "base") && export %in% safe_base_calls) return(add(expr, package, export, "ok", "base_allowlist"))
      return(add(expr, package, export, "rejected", "package_not_in_catalog_revision"))
    }
    entry <- Filter(function(x) identical(x$name, export), p$exports)
    if (!length(entry)) return(add(expr, package, export, "rejected", "export_absent_from_catalog_revision"))
    entry <- entry[[1]]
    reason <- if (approved_only) "workflow_approved_export" else "static_api_verified_export"
    if (identical(entry$kind, "reexport")) {
      # A reexport is accepted only through its owner's verified (and approved)
      # export, following at most three reexport hops, and only when every
      # reexporting revision on the way carries an approval, because loading it
      # is what registers its methods.
      chain <- list(p)
      owner <- p
      owned <- entry
      for (hop in seq_len(3L)) {
        if (!identical(owned$kind, "reexport")) break
        owner <- packages[[if (is.null(owned$owner_package)) "" else owned$owner_package]]
        found <- if (is.null(owner)) list() else Filter(function(x) identical(x$name, export), owner$exports)
        if (!length(found)) {
          owned <- NULL
          break
        }
        owned <- found[[1]]
        if (identical(owned$kind, "reexport")) chain <- c(chain, list(owner))
      }
      valid_owner <- !is.null(owned) && identical(owned$kind, "function") &&
        owned$verification %in% c("static_api_verified", "installed_api_verified")
      has_approval <- function(x) any(vapply(effective_approvals(x), function(a) identical(a$status, "approved"), logical(1)))
      approved_route <- !approved_only || (valid_owner && !is.null(approved(owner)[[export]]) &&
        all(vapply(chain, has_approval, logical(1))))
      if (!valid_owner || !approved_route) {
        return(add(expr, package, export, "rejected", paste0("reexport_use_owner:", entry$owner_package)))
      }
      entry <- owned
      reason <- paste0(if (approved_only) "workflow_approved_reexport:" else "static_reexport:", owner$name)
    } else if (identical(entry$kind, "s4_generic") && !is.null(call)) {
      # S4 generics dispatch on argument classes; their declared signature is not
      # a complete argument list, so only approval is checked here.
      if (approved_only && is.null(approved(p)[[export]])) {
        return(add(expr, package, export, "rejected", "not_workflow_approved_for_revision"))
      }
      return(add(expr, package, export, "ok", if (approved_only) "workflow_approved_s4_generic" else "s4_generic_declared"))
    } else if (!identical(entry$kind, "function") || !entry$verification %in% c("static_api_verified", "installed_api_verified")) {
      # Exported data objects may be referenced (not called) when documented and approved.
      object_ok <- is.null(call) && !is.null(entry$documentation) && (!approved_only || !is.null(approved(p)[[export]]))
      if (object_ok) return(add(expr, package, export, "ok", if (approved_only) "workflow_approved_object" else "documented_object"))
      return(add(expr, package, export, "rejected", paste0("unverified_export:", entry$verification)))
    } else if (approved_only && is.null(approved(p)[[export]])) {
      return(add(expr, package, export, "rejected", "not_workflow_approved_for_revision"))
    }
    if (!is.null(call)) {
      formals <- approval_text(entry$arguments)
      supplied <- names(as.list(call)[-1])
      if (is.null(supplied)) supplied <- rep("", length(call) - 1L)
      named <- supplied[nzchar(supplied)]
      if (!"..." %in% formals) {
        unknown <- setdiff(named, formals)
        if (length(unknown)) return(add(expr, package, export, "rejected", paste0("unknown_argument:", paste(unknown, collapse = ","))))
        if (length(supplied) > length(formals)) return(add(expr, package, export, "rejected", "too_many_arguments"))
      }
      if (anyDuplicated(named)) return(add(expr, package, export, "rejected", "duplicate_argument"))
    }
    add(expr, package, export, "ok", reason)
  }
  check_function_value <- function(value, call, name) {
    if (is.symbol(value)) {
      symbol <- as.character(value)
      if (symbol %in% c(safe_base_calls, local_functions) && !symbol %in% forbidden_calls) return(TRUE)
      add(call, NA_character_, name, "rejected", paste0("unverified_function_value:", symbol))
      return(FALSE)
    }
    if (namespaced(value) || (is.call(value) && identical(value[[1]], as.name("function")))) return(TRUE)
    add(call, NA_character_, name, "rejected", "dynamic_function_value")
    FALSE
  }
  walk <- function(expr, depth = 0L, formula = FALSE) {
    if (depth > 256L) return(add(quote(nesting), NA_character_, NA_character_, "rejected", "nesting_exceeds_bound"))
    if (is.symbol(expr)) {
      name <- as.character(expr)
      if (nzchar(name) && name %in% forbidden_calls) add(expr, NA_character_, name, "rejected", paste0("forbidden_reference:", name))
      return(invisible(NULL))
    }
    if (is.pairlist(expr)) {
      parts <- as.list(expr)
      for (i in seq_along(parts)) if (!empty_argument(parts, i)) walk(parts[[i]], depth + 1L, formula)
      return(invisible(NULL))
    }
    if (!is.call(expr)) return(invisible(NULL))
    head <- expr[[1]]
    args <- as.list(expr)[-1]
    if (namespaced(expr)) return(check_namespaced(expr))
    name <- if (is.symbol(head)) as.character(head) else NULL
    if (namespaced(head)) {
      check_namespaced(head, expr)
      if (identical(static_atom(head[[2]]), "base")) name <- static_atom(head[[3]])
    } else if (is.symbol(head)) {
      if (name %in% forbidden_calls) {
        add(expr, NA_character_, name, "rejected", paste0("forbidden_call:", name))
      } else if (name %in% syntax_calls) {
        if (name == "$") args <- args[1]
        if (name %in% c("<-", "=") && is.symbol(args[[1]])) args <- args[-1]
        if (name == "~") formula <- TRUE
      } else if (formula && name %in% formula_terms) {
        add(expr, NA_character_, name, "ok", "formula_term")
      } else if (name %in% local_functions) {
        add(expr, NA_character_, name, "ok", "local_function")
      } else if (name %in% safe_base_calls) {
        add(expr, "base", name, "ok", "base_allowlist")
      } else {
        add(expr, NA_character_, name, "rejected", paste0("unsupported_call:", name))
      }
    } else {
      add(expr, NA_character_, NA_character_, "rejected", "computed_function_call")
      walk(head, depth + 1L, formula)
    }
    formals <- if (is.null(name)) NULL else higher_order_formals[[name]]
    if (!is.null(formals)) {
      value <- static_match_args(expr, formals)[[formals[[length(formals)]]]]
      if (!is.null(value)) check_function_value(value, expr, name)
    }
    for (i in seq_along(args)) if (!empty_argument(args, i)) walk(args[[i]], depth + 1L, formula)
    invisible(NULL)
  }
  for (expr in expressions) walk(expr)
  finish(acc$rows)
}
