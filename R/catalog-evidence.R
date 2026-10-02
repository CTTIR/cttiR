# Static object-system and reexport evidence (evidence_version 3).
#
# Everything here inspects parsed syntax only. No package code, NAMESPACE
# directive or declaration is evaluated. S4 and S7 records describe literal
# top-level declarations; they do not prove installed dispatch, inheritance,
# validity or constructor behaviour, so they carry `static_declaration_only`.

static_atom <- function(x) {
  if (is.symbol(x)) return(as.character(x))
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) return(x)
  NULL
}

static_literal <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) x else NULL
}

# Resolve `name(...)` or `pkg::name(...)` heads without evaluating anything.
static_call_head <- function(expr, packages = NULL) {
  if (!is.call(expr)) return(NULL)
  head <- expr[[1]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && length(head) == 3L && identical(head[[1]], as.name("::"))) {
    owner <- static_atom(head[[2]])
    symbol <- static_atom(head[[3]])
    if (!is.null(owner) && !is.null(symbol) && (is.null(packages) || owner %in% packages)) return(symbol)
  }
  NULL
}

# Exact-name then positional matching against known formals; partial names are
# never guessed and simply remain unmatched.
static_match_args <- function(expr, formals) {
  args <- as.list(expr)[-1]
  keys <- names(args)
  if (is.null(keys)) keys <- rep("", length(args))
  matched <- list()
  present <- !vapply(seq_along(args), function(i) empty_argument(args, i), logical(1))
  for (i in which(present)) {
    if (nzchar(keys[[i]]) && keys[[i]] %in% formals && is.null(matched[[keys[[i]]]])) matched[keys[[i]]] <- list(args[[i]])
  }
  remaining <- setdiff(formals, names(matched))
  for (i in which(!nzchar(keys) & present)) {
    if (!length(remaining)) break
    matched[remaining[[1]]] <- list(args[[i]])
    remaining <- remaining[-1]
  }
  matched
}

static_signature_values <- function(x) {
  literal <- static_literal(x)
  if (!is.null(literal)) return(stats::setNames(literal, ""))
  head <- static_call_head(x, "methods")
  if (is.null(head) || !head %in% c("c", "signature")) return(NULL)
  if (head == "c" && !is.symbol(x[[1]])) return(NULL)
  parts <- as.list(x)[-1]
  if (!length(parts)) return(NULL)
  values <- vapply(parts, function(p) if (is.null(static_literal(p))) NA_character_ else p, character(1))
  if (anyNA(values)) return(NULL)
  keys <- names(parts)
  stats::setNames(unname(values), if (is.null(keys)) rep("", length(values)) else keys)
}

static_function_text <- function(x) {
  if (!is.call(x) || !identical(x[[1]], as.name("function"))) return(NULL)
  args <- x[[2]]
  list(signature = sub(" NULL$", "", paste(deparse(as.call(list(as.name("function"), args, NULL)), width.cutoff = 500L), collapse = " ")),
    arguments = as.list(names(args)))
}

static_declaration_text <- function(expr) {
  text <- paste(deparse(expr, width.cutoff = 200L, nlines = 4L), collapse = " ")
  substr(text, 1L, 300L)
}

# Collect literal top-level S4 and S7 declarations from one parsed source file.
static_object_declarations <- function(text, source_path) {
  expressions <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) {
    abort_cttir("An R source file could not be parsed statically.", "cttir_source_unavailable", "source_parse")
  })
  out <- new.env(parent = emptyenv())
  slots <- c("s4_classes", "s4_generics", "s4_methods", "s4_validity", "s4_unresolved", "s7_classes",
    "s7_generics", "s7_unresolved")
  for (slot in slots) assign(slot, list(), envir = out)
  out$generators <- character()
  out$targets <- character()
  # Binding targets outside function bodies. A replacement call such as
  # `method(f, cls) <- value` rebinds only its innermost first argument.
  target_of <- function(lhs) {
    while (is.call(lhs) && length(lhs) >= 2L) lhs <- lhs[[2]]
    static_atom(lhs)
  }
  scan <- function(expr, depth = 0L) {
    if (!is.call(expr) || depth > 64L || identical(expr[[1]], as.name("function"))) return(invisible(NULL))
    head <- if (is.symbol(expr[[1]])) as.character(expr[[1]]) else ""
    parts <- as.list(expr)
    if (head %in% c("<-", "=", "<<-") && length(expr) == 3L) out$targets <- c(out$targets, target_of(expr[[2]]))
    if (head == "assign" && length(expr) >= 3L && !empty_argument(parts, 2L)) {
      out$targets <- c(out$targets, static_literal(expr[[2]]))
    }
    for (i in seq_along(parts)) if (!empty_argument(parts, i) && is.call(parts[[i]])) scan(parts[[i]], depth + 1L)
    invisible(NULL)
  }
  for (top in expressions) scan(top)
  add <- function(slot, value) out[[slot]][[length(out[[slot]]) + 1L]] <- value
  declared <- function(slot, ...) add(slot, list(..., source_path = source_path, verification = "static_declaration_only"))
  unresolved <- function(slot, expr, reason) {
    record <- list(declaration = static_declaration_text(expr), source_path = source_path,
      reason = reason, verification = "unknown")
    add(slot, record)
  }
  s4 <- c("setClass", "setClassUnion", "setRefClass", "setGeneric", "setMethod", "setReplaceMethod", "setValidity")
  for (top in expressions) {
    target <- NULL
    expr <- top
    if (is.call(top) && is.symbol(top[[1]]) && as.character(top[[1]]) %in% c("<-", "=") && length(top) == 3L) {
      target <- if (is.symbol(top[[2]])) as.character(top[[2]]) else NULL
      expr <- top[[3]]
    }
    head <- static_call_head(expr, c("methods", "S7"))
    if (is.null(head)) next
    if (head %in% s4) {
      if (is.call(expr[[1]]) && !identical(static_atom(expr[[1]][[2]]), "methods")) next
      if (head %in% c("setClass", "setRefClass", "setValidity", "setClassUnion")) {
        key <- if (head == "setClassUnion") "name" else "Class"
        name <- static_literal(static_match_args(expr, key)[[key]])
        if (is.null(name)) {
          unresolved("s4_unresolved", expr, "nonliteral_class_name")
        } else if (head == "setValidity") {
          declared("s4_validity", class = name)
        } else {
          declared("s4_classes", name = name, declaration = head)
          if (!is.null(target) && head != "setClassUnion") out$generators <- c(out$generators, target)
        }
      } else if (head == "setGeneric") {
        args <- static_match_args(expr, c("name", "def"))
        name <- static_literal(args[["name"]])
        def <- static_function_text(args[["def"]])
        if (is.null(name)) {
          unresolved("s4_unresolved", expr, "nonliteral_generic_name")
        } else if (is.null(def)) {
          declared("s4_generics", name = name, signature = "unresolved: implicit or nonliteral generic definition",
            arguments = list())
        } else {
          declared("s4_generics", name = name, signature = def$signature, arguments = def$arguments)
        }
      } else {
        args <- static_match_args(expr, c("f", "signature"))
        name <- static_literal(args[["f"]])
        signature <- if (is.null(args[["signature"]])) NULL else static_signature_values(args[["signature"]])
        if (is.null(name) || is.null(signature)) {
          unresolved("s4_unresolved", expr, if (is.null(name)) "nonliteral_generic_name" else "nonliteral_method_signature")
        } else {
          generic <- if (head == "setReplaceMethod") paste0(name, "<-") else name
          declared("s4_methods", generic = generic, signature = as.list(unname(signature)),
            signature_arguments = as.list(names(signature)), declaration = head)
        }
      }
    } else if (head %in% c("new_class", "new_generic")) {
      if (is.call(expr[[1]]) && !identical(static_atom(expr[[1]][[2]]), "S7")) next
      if (is.null(target)) {
        unresolved("s7_unresolved", expr, "unassigned_s7_declaration")
      } else if (head == "new_class") {
        args <- static_match_args(expr, c("name", "parent"))
        parent <- if (is.null(args[["parent"]])) NULL else static_declaration_text(args[["parent"]])
        declared("s7_classes", name = target, class_name = static_literal(args[["name"]]), parent = parent)
      } else {
        args <- static_match_args(expr, c("name", "dispatch_args"))
        dispatch <- if (is.null(args[["dispatch_args"]])) NULL else static_signature_values(args[["dispatch_args"]])
        dispatch <- if (is.null(dispatch)) "unresolved" else as.list(unname(dispatch))
        declared("s7_generics", name = target, generic_name = static_literal(args[["name"]]), dispatch_args = dispatch)
      }
    }
  }
  as.list(out)
}

namespace_directives <- function(ns) {
  names_of <- function(expr) {
    parts <- as.list(expr)[-1]
    values <- lapply(seq_along(parts), function(i) if (empty_argument(parts, i)) NULL else static_atom(parts[[i]]))
    if (any(vapply(values, is.null, logical(1)))) return(NULL)
    unlist(values, use.names = FALSE)
  }
  out <- list(imports_from = list(), imports_all = character(), export_classes = character(),
    export_methods = character(), export_class_patterns = character())
  for (expr in ns) {
    if (!is.call(expr) || !is.symbol(expr[[1]])) next
    head <- as.character(expr[[1]])
    if (head == "importFrom" && length(expr) >= 3L) {
      values <- names_of(expr)
      if (is.null(values)) next
      for (name in values[-1]) out$imports_from[[name]] <- unique(c(out$imports_from[[name]], values[[1]]))
    } else if (head == "import" && length(expr) >= 2L) {
      owner <- static_atom(expr[[2]])
      if (!is.null(owner)) out$imports_all <- unique(c(out$imports_all, owner))
    } else if (head == "exportClasses") {
      out$export_classes <- c(out$export_classes, names_of(expr))
    } else if (head == "exportMethods") {
      out$export_methods <- c(out$export_methods, names_of(expr))
    } else if (head == "exportClassPattern" && length(expr) == 2L && !is.null(static_literal(expr[[2]]))) {
      out$export_class_patterns <- c(out$export_class_patterns, expr[[2]])
    }
  }
  out
}

# Combine per-file declarations into package-level S4/S7 records.
object_evidence <- function(declarations, directives, exports) {
  pick <- function(slot) {
    values <- unlist(lapply(declarations, `[[`, slot), recursive = FALSE)
    if (is.null(values)) list() else values
  }
  classes <- pick("s4_classes")
  class_names <- vapply(classes, `[[`, character(1), "name")
  exported_class <- function(name) {
    name %in% directives$export_classes ||
      any(vapply(directives$export_class_patterns, function(p) grepl(p, name), logical(1)))
  }
  classes <- lapply(classes, function(x) c(x, list(exported = exported_class(x$name))))
  generics <- lapply(pick("s4_generics"), function(x) c(x, list(exported = x$name %in% c(exports, directives$export_methods))))
  methods <- lapply(pick("s4_methods"), function(x) c(x, list(exported = x$generic %in% c(exports, directives$export_methods))))
  duplicate <- class_names[duplicated(class_names)]
  for (i in seq_along(classes)) {
    if (classes[[i]]$name %in% duplicate) {
      classes[[i]]$verification <- "unknown"
      classes[[i]]$reason <- "duplicate_class_declaration"
    }
  }
  s7_classes <- lapply(pick("s7_classes"), function(x) c(x, list(exported = x$name %in% exports)))
  s7_generics <- lapply(pick("s7_generics"), function(x) c(x, list(exported = x$name %in% exports)))
  list(
    s4 = list(classes = classes, generics = generics, methods = methods, validity = pick("s4_validity"),
      unresolved = pick("s4_unresolved"),
      exports = list(classes = as.list(unique(directives$export_classes)),
        methods = as.list(unique(directives$export_methods)),
        class_patterns = as.list(unique(directives$export_class_patterns)),
        undeclared_classes = as.list(setdiff(unique(directives$export_classes), class_names))),
      verification = "static_declaration_only",
      limitation = "Literal top-level declarations only; installed dispatch, inheritance, validity and coercion are not established."),
    s7 = list(classes = s7_classes, generics = s7_generics, unresolved = pick("s7_unresolved"),
      verification = "static_declaration_only",
      limitation = "Literal top-level S7 assignments only; properties, constructors and dispatch are not established."),
    generators = unique(unlist(lapply(declarations, `[[`, "generators"))),
    targets = table(unlist(lapply(declarations, `[[`, "targets")))
  )
}

# Classify exports that have no statically verified local function.
classify_export <- function(name, fn, directives, objects) {
  single <- function(records) sum(vapply(records, function(x) identical(x$name, name), logical(1))) == 1L
  declared <- function(kind, signature) list(kind = kind, signature = signature, verification = "static_declaration_only")
  # The declaration must be the only binding of this name outside function bodies.
  bindings <- if (name %in% names(objects$targets)) as.integer(objects$targets[[name]]) else 0L
  nonliteral <- !is.null(fn) && startsWith(fn$signature, "unresolved") && identical(bindings, 1L)
  owners <- directives$imports_from[[name]]
  if (is.null(fn) && length(owners) == 1L) {
    reexport <- list(kind = "reexport", owner_package = owners, signature = paste0("reexport: ", owners, "::", name))
    return(c(reexport, verification = "reexport_declared"))
  }
  if (nonliteral && single(objects$s7$classes) && !single(objects$s7$generics)) {
    return(declared("s7_class", "unresolved: S7 class constructor (static declaration only)"))
  }
  if (nonliteral && single(objects$s7$generics) && !single(objects$s7$classes)) {
    return(declared("s7_generic", "unresolved: S7 generic (static declaration only)"))
  }
  if (nonliteral && name %in% objects$generators) {
    return(declared("s4_class_generator", "unresolved: S4 class generator (static declaration only)"))
  }
  if (is.null(fn) && single(objects$s4$generics)) {
    return(declared("s4_generic", "unresolved: S4 generic (static declaration only)"))
  }
  NULL
}

object_hits <- function(package, query) {
  q <- tolower(query)
  acc <- new.env(parent = emptyenv())
  acc$out <- list()
  add <- function(kind, symbol, text, source_path) {
    if (!grepl(q, tolower(paste(symbol, text)), fixed = TRUE)) return(invisible(NULL))
    hit <- list(id = content_hash(paste(package$source_hash, kind, symbol, source_path)),
      kind = kind, symbol = symbol, source_path = source_path,
      snippet = paste(text, "Static declaration only; installed dispatch and workflow approval not established."))
    acc$out[[length(acc$out) + 1L]] <- hit
  }
  for (x in package$s4$classes) add("s4_class", paste(package$name, "S4 class", x$name, sep = "/"), paste("S4", x$declaration, x$name), x$source_path)
  exported <- vapply(package$exports, function(x) x$name, character(1))
  for (x in package$s4$generics) {
    # Exported generics already appear as export records.
    if (!x$name %in% exported) add("s4_generic", paste(package$name, "S4 generic", x$name, sep = "/"), paste("S4 generic", x$name, x$signature), x$source_path)
  }
  for (x in package$s4$methods) {
    signature <- paste(unlist(x$signature), collapse = ",")
    add("s4_method", paste(package$name, "S4 method", x$generic, signature, sep = "/"), paste("S4 method", x$generic, "for", signature), x$source_path)
  }
  acc$out
}
