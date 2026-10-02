# Standard-workflow, resource and approval-coverage audit checks. All read
# metadata only: project control files, generated code text (parsed, never
# evaluated), the catalog snapshot and the resource database.

audit_standard_project <- function(context) {
  spec <- audit_spec(context)
  if (is.null(spec)) return("Project metadata could not be read; see PRJ-001.")
  if (!identical(spec$provenance$template_version, current_template_version)) {
    return(paste0("Template ", spec$provenance$template_version, " has no standard workflow stages."))
  }
  TRUE
}

# Project code audit ------------------------------------------------------------
#
# All R code of a project is parsed (never evaluated) and classified with the
# vocabulary of R/code-validation.R. Constructs that also occur in the reviewed
# code the package generates for the same file are not reported again.

# Process and network connection constructors (also in forbidden_calls), reported
# under their own reason.
audit_connection_calls <- c("pipe", "url", "socketConnection", "socketAccept", "serverSocket", "make.socket")

# Knitr engines that run a shell. Other non-R engines cannot be checked statically.
audit_shell_engines <- c("bash", "sh", "zsh", "shell", "system", "cmd", "powershell", "bat")

audit_default_packages <- c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")

# R scripts anywhere and R Markdown/Quarto documents, outside data/, renv/ and
# hidden directories.
audit_code_files <- function(root) {
  pattern <- "[.]([Rr]|[Rr]md|[Qq]md)$"
  top <- list.files(root, all.files = TRUE, no.. = TRUE)
  folder <- dir.exists(file.path(root, top))
  # Project libraries and data can be large, so they are not traversed at all.
  walk <- top[folder & !top %in% c("data", "renv") & !startsWith(top, ".")]
  files <- c(grep(pattern, top[!folder], value = TRUE), unlist(lapply(walk, function(dir) {
    found <- list.files(file.path(root, dir), pattern, recursive = TRUE, all.files = TRUE)
    if (length(found)) file.path(dir, found) else character()
  })))
  hidden <- vapply(strsplit(files, "/", fixed = TRUE), function(parts) any(startsWith(parts[-length(parts)], ".")), logical(1))
  sort(files[!hidden])
}

# R code of one file. Documents contribute R chunks, evaluated chunk options and
# inline R; the engines of other chunks are returned separately.
audit_code_text <- function(path, lines) {
  if (!grepl("[.]([Rr]md|[Qq]md)$", path)) return(list(code = lines, engines = character()))
  code <- character()
  engines <- character()
  inside <- NULL
  for (line in lines) {
    if (is.null(inside)) {
      open <- regmatches(line, regexec("^[[:space:]>]*```+[[:space:]]*\\{[[:space:]]*([A-Za-z0-9_.]+)(.*)\\}[[:space:]]*$", line))[[1]]
      if (length(open)) {
        inside <- tolower(open[[2]])
        if (inside == "r") code <- c(code, audit_chunk_options(open[[3]])) else engines <- c(engines, inside)
      } else {
        inline <- regmatches(line, gregexpr("`(r|\\{r\\})[[:space:]]+[^`]+`", line))[[1]]
        code <- c(code, sub("`$", "", sub("^`(r|\\{r\\})[[:space:]]+", "", inline)))
      }
    } else if (grepl("^[[:space:]>]*```+[[:space:]]*$", line)) {
      inside <- NULL
    } else if (inside == "r") {
      code <- c(code, line)
      # Quarto `#| key: !expr value` options are evaluated as R.
      if (grepl("^[[:space:]]*#[|].*!expr[[:space:]]", line)) {
        code <- c(code, gsub("^['\"]|['\"][[:space:]]*$", "", sub("^[[:space:]]*#[|].*!expr[[:space:]]+", "", line)))
      }
    }
  }
  list(code = code, engines = engines)
}

# Chunk header options after the engine, without the chunk label.
audit_chunk_options <- function(rest) {
  rest <- trimws(sub("^[[:space:]]*,", "", rest))
  label <- regmatches(rest, regexpr("^[^,=]*", rest))
  after <- substring(rest, nchar(label) + 1L)
  if (!startsWith(after, "=")) rest <- trimws(sub("^,", "", after))
  if (nzchar(rest)) paste0("list(", rest, ")") else character()
}

audit_parse_code <- function(code) {
  if (sum(nchar(code, type = "bytes")) > 1048576L) return(structure(list(), class = "audit_too_large"))
  tryCatch(parse(text = code, keep.source = FALSE, encoding = "UTF-8"), error = function(e) e)
}

audit_call_text <- function(expr) {
  substr(paste(deparse(expr, width.cutoff = 120L, nlines = 2L), collapse = " "), 1L, 200L)
}

audit_namespaced <- function(x) {
  is.call(x) && length(x) == 3L && (identical(x[[1]], as.name("::")) || identical(x[[1]], as.name(":::")))
}

# Names bound in the code: defined functions, other assigned or formal symbols,
# and packages attached by literal name.
audit_code_bindings <- function(exprs) {
  out <- new.env(parent = emptyenv())
  out$functions <- character()
  out$locals <- character()
  out$attached <- character()
  visit <- function(expr, depth = 0L) {
    if (depth > 256L) return(invisible(NULL))
    if (!is.call(expr) && !is.pairlist(expr)) return(invisible(NULL))
    parts <- as.list(expr)
    if (is.pairlist(expr)) {
      for (i in seq_along(parts)) if (!empty_argument(parts, i)) visit(parts[[i]], depth + 1L)
      return(invisible(NULL))
    }
    head <- expr[[1]]
    name <- if (is.symbol(head)) as.character(head) else if (audit_namespaced(head)) static_atom(head[[3]]) else ""
    if (length(name) != 1L) name <- ""
    if (name %in% c("<-", "=", "<<-") && length(expr) == 3L && is.symbol(expr[[2]])) {
      value <- expr[[3]]
      if (is.call(value) && identical(value[[1]], as.name("function"))) {
        out$functions <- c(out$functions, as.character(expr[[2]]))
      } else {
        out$locals <- c(out$locals, as.character(expr[[2]]))
      }
    } else if (name == "function" && !is.null(expr[[2]])) {
      out$locals <- c(out$locals, names(expr[[2]]))
    } else if (name == "for" && is.symbol(expr[[2]])) {
      out$locals <- c(out$locals, as.character(expr[[2]]))
    } else if (name == "setGeneric" && length(expr) > 1L && is.character(expr[[2]])) {
      out$functions <- c(out$functions, expr[[2]])
    } else if (name %in% c("library", "require") && length(expr) > 1L) {
      package <- static_match_args(expr, "package")$package
      dynamic <- isTRUE(as.list(expr)$character.only) && !is.character(package)
      if (!is.null(package) && !dynamic && !is.null(static_atom(package))) out$attached <- c(out$attached, static_atom(package))
    }
    for (i in seq_along(parts)) if (!empty_argument(parts, i)) visit(parts[[i]], depth + 1L)
    invisible(NULL)
  }
  for (expr in exprs) visit(expr)
  list(functions = unique(out$functions), locals = unique(out$locals), attached = unique(out$attached))
}

# A static relative path: a string literal or file.path() of string literals.
audit_static_path <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) return(x)
  if (!is.call(x) || !(identical(x[[1]], as.name("file.path")) || identical(x[[1]], quote(base::file.path)))) return(NULL)
  parts <- as.list(x)[-1]
  if (!is.null(names(parts)) && any(nzchar(names(parts)))) return(NULL)
  values <- lapply(parts, audit_static_path)
  if (!length(values) || any(vapply(values, is.null, logical(1)))) return(NULL)
  paste(unlist(values), collapse = "/")
}

# Project-relative target of a static source() path, if it is checked R code.
audit_source_target <- function(path, from, scanned) {
  if (is.null(path) || grepl("^(/|~|[A-Za-z]:|\\\\)", path)) return(NULL)
  for (base in unique(c("", dirname(from)))) {
    parts <- strsplit(gsub("\\\\", "/", if (base %in% c("", ".")) path else paste0(base, "/", path)), "/", fixed = TRUE)[[1]]
    kept <- character()
    for (part in parts[nzchar(parts) & parts != "."]) {
      if (part != "..") {
        kept <- c(kept, part)
      } else if (length(kept)) {
        kept <- kept[-length(kept)]
      } else {
        kept <- NULL
        break
      }
    }
    target <- paste(kept, collapse = "/")
    if (length(kept) && target %in% scanned && grepl("[.][Rr]$", target)) return(target)
  }
  NULL
}

# Status of a forbidden name used as a call: NULL when the reviewed generated
# code's use is matched (attaching by literal name, sourcing checked project
# code, quitting and superassignment), otherwise a finding.
audit_forbidden_use <- function(name, expr, path, scanned) {
  if (name %in% c("<<-", "q", "quit")) return(NULL)
  if (name %in% c("library", "require", "requireNamespace", "loadNamespace")) {
    package <- static_match_args(expr, "package")$package
    by_name <- name %in% c("library", "require") && !isTRUE(as.list(expr)$character.only)
    literal <- is.character(package) || (is.symbol(package) && by_name)
    if (is.null(package) || literal) return(NULL)
    return(c("warning", "dynamic_package_load"))
  }
  if (name %in% c("source", "sys.source")) {
    target <- audit_source_target(audit_static_path(static_match_args(expr, "file")$file), path, scanned)
    if (!is.null(target)) return(NULL)
    return(c("fail", "source_outside_checked_code"))
  }
  c("fail", if (name %in% audit_connection_calls) "connection_call" else "forbidden_call")
}

audit_no_findings <- function() {
  data.frame(file = character(), status = character(), reason = character(), name = character(),
    call = character(), stringsAsFactors = FALSE)
}

# Findings for one parsed file: forbidden or unsafe calls and function values,
# and non-namespaced calls that resolve to no default-attached or project
# function. Namespaced approvals are checked by validate_generated_code().
audit_code_findings <- function(exprs, path, context) {
  out <- new.env(parent = emptyenv())
  out$rows <- list()
  unsafe <- forbidden_calls
  loaders <- c("requireNamespace", "loadNamespace")
  locals <- audit_code_bindings(exprs)$locals
  resolved <- c(context$defined, locals, context$base, syntax_calls, "function", ":=")
  add <- function(status, reason, name, expr) {
    out$rows[[length(out$rows) + 1L]] <- data.frame(file = path, status = status, reason = reason, name = name,
      call = audit_call_text(expr), stringsAsFactors = FALSE)
  }
  resolve <- function(name, expr, formula) {
    if (name %in% resolved || paste0(name, "<-") %in% resolved || (formula && name %in% formula_terms)) return()
    if (name %in% context$attached_exports) return(add("warning", "attached_package_call", name, expr))
    if (context$unknown_attached) return(add("warning", "unresolved_call", name, expr))
    add("fail", "unresolved_call", name, expr)
  }
  call_name <- function(name, expr, formula) {
    if (!name %in% unsafe) return(resolve(name, expr, formula))
    finding <- audit_forbidden_use(name, expr, path, context$scanned)
    if (!is.null(finding)) add(finding[[1]], finding[[2]], name, expr)
  }
  # A function passed by name (or, to higher-order calls, as a string).
  value_name <- function(value, strings) {
    if (audit_namespaced(value)) return(static_atom(value[[3]]))
    if (is.symbol(value) && !as.character(value) %in% locals) return(as.character(value))
    if (strings) static_literal(value) else NULL
  }
  function_value <- function(name, value, expr, formula) {
    if (name %in% loaders) return(add("warning", "dynamic_package_load", name, expr))
    if (name %in% unsafe) return(add("fail", "forbidden_function_value", name, expr))
    if (!audit_namespaced(value)) resolve(name, expr, formula)
  }
  walk <- function(expr, depth = 0L, formula = FALSE, parent = expr) {
    if (depth > 256L) return(add("not_tested", "nesting_exceeds_bound", "", parent))
    if (is.symbol(expr)) {
      name <- as.character(expr)
      # Data columns may share these names, so a bare reference is advisory.
      if (!formula && nzchar(name) && name %in% unsafe && !name %in% locals) add("warning", "forbidden_name_reference", name, parent)
      return(invisible(NULL))
    }
    if (is.pairlist(expr)) {
      parts <- as.list(expr)
      for (i in seq_along(parts)) if (!empty_argument(parts, i)) walk(parts[[i]], depth + 1L, formula, parent)
      return(invisible(NULL))
    }
    if (!is.call(expr)) return(invisible(NULL))
    if (audit_namespaced(expr)) {
      name <- static_atom(expr[[3]])
      if (!is.null(name) && name %in% setdiff(unsafe, loaders)) add("fail", "forbidden_function_value", name, parent)
      return(invisible(NULL))
    }
    head <- expr[[1]]
    args <- as.list(expr)[-1]
    name <- NULL
    if (is.symbol(head)) {
      name <- as.character(head)
      call_name(name, expr, formula)
    } else if (audit_namespaced(head)) {
      export <- static_atom(head[[3]])
      base <- identical(static_atom(head[[2]]), "base")
      if (!is.null(export) && (base || export %in% unsafe)) call_name(export, expr, formula)
      if (base) name <- export
    } else {
      # Members such as baseenv()$system(...) reach the same functions.
      if (is.call(head) && length(head) == 3L && is.symbol(head[[1]]) && as.character(head[[1]]) %in% c("$", "@", "[[")) {
        member <- static_atom(head[[3]])
        if (!is.null(member) && member %in% unsafe) add("fail", "forbidden_member_call", member, expr)
      }
      walk(head, depth + 1L, formula, expr)
    }
    if (!is.null(name)) {
      if (name %in% c("$", "@")) args <- args[1]
      if (name %in% c("<-", "=", "<<-") && length(args) == 2L && is.symbol(args[[1]])) {
        args <- args[-1]
        alias <- value_name(args[[1]], strings = FALSE)
        if (!is.null(alias) && alias %in% setdiff(unsafe, loaders)) {
          add("fail", "forbidden_function_value", alias, expr)
          args <- list()
        }
      }
      if (name == "~") formula <- TRUE
      formals <- higher_order_formals[[name]]
      if (!is.null(formals)) {
        value <- static_match_args(expr, formals)[[formals[[length(formals)]]]]
        fun <- if (is.null(value)) NULL else value_name(value, strings = TRUE)
        if (!is.null(fun)) {
          function_value(fun, value, expr, formula)
          args <- Filter(function(x) !identical(x, value), args)
        }
      }
    }
    for (i in seq_along(args)) if (!empty_argument(args, i)) walk(args[[i]], depth + 1L, formula, expr)
    invisible(NULL)
  }
  for (expr in exprs) walk(expr)
  if (!length(out$rows)) return(audit_no_findings())
  do.call(rbind, out$rows)
}

# Removes findings that also occur (as often) in the reviewed version of a file.
audit_unreviewed <- function(findings, reviewed) {
  key <- function(x) paste(x$reason, x$name, x$call, sep = "\r")
  pool <- key(reviewed)
  keep <- logical(nrow(findings))
  for (i in seq_along(keep)) {
    at <- match(key(findings[i, ]), pool)
    keep[[i]] <- is.na(at)
    if (!is.na(at)) pool <- pool[-at]
  }
  findings[keep, , drop = FALSE]
}

# Reads, parses and classifies every project code file. `bundle` holds the
# package's regenerated files; byte-identical copies are its reviewed code.
audit_project_code <- function(p, catalog, bundle) {
  scanned <- audit_code_files(p$path)
  units <- lapply(scanned, function(path) {
    file <- file.path(p$path, path)
    assert_plain_path(file)
    unit <- audit_code_text(path, readLines(file, warn = FALSE, encoding = "UTF-8"))
    unit$path <- path
    unit$exprs <- audit_parse_code(unit$code)
    unit$reviewed <- bundle$files[[path]]
    unit$identical <- !is.null(unit$reviewed) && identical(content_hash(unit$reviewed), file_hash(file))
    unit
  })
  parsed <- Filter(function(u) is.expression(u$exprs), units)
  bindings <- lapply(parsed, function(u) audit_code_bindings(u$exprs))
  attached <- setdiff(unique(unlist(lapply(bindings, function(b) b$attached))), audit_default_packages)
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(x) x$name, character(1)))
  exports <- lapply(index[intersect(attached, names(index))], function(x) vapply(x$exports, function(e) e$name, character(1)))
  context <- list(scanned = scanned, base = unique(unlist(lapply(audit_default_packages, getNamespaceExports))),
    defined = unique(unlist(lapply(bindings, function(b) b$functions))),
    attached_exports = unique(unlist(exports)), unknown_attached = any(!attached %in% names(index)))
  findings <- lapply(units, function(u) {
    row <- function(status, reason, name = "") {
      data.frame(file = u$path, status = status, reason = reason, name = name, call = "", stringsAsFactors = FALSE)
    }
    found <- if (inherits(u$exprs, "audit_too_large")) {
      row("not_tested", "too_large_to_check")
    } else if (inherits(u$exprs, "error")) {
      row("fail", "parse_error")
    } else if (u$identical) {
      audit_no_findings()
    } else {
      current <- audit_code_findings(u$exprs, u$path, context)
      original <- NULL
      if (!is.null(u$reviewed)) {
        original <- audit_parse_code(audit_code_text(u$path, strsplit(u$reviewed, "\n", fixed = TRUE)[[1]])$code)
      }
      if (is.expression(original)) audit_unreviewed(current, audit_code_findings(original, u$path, context)) else current
    }
    engines <- lapply(u$engines, function(e) row(if (e %in% audit_shell_engines) "fail" else "warning", "non_r_chunk", e))
    do.call(rbind, c(list(found), engines))
  })
  list(units = units, findings = do.call(rbind, c(list(audit_no_findings()), findings)))
}

audit_std_routing <- function(context) {
  audit_with_project(context, function(p) {
    catalog <- catalog_snapshot(p$spec$provenance$catalog_id)
    route <- route_workflow(p$spec, p$spec$workflow$profile, catalog)
    summary <- route_summary(route)
    dependencies <- route_dependencies(route, catalog)
    consistent <- identical(json_text(summary), json_text(p$lock$workflow)) &&
      identical(json_text(dependencies), json_text(p$lock$dependencies))
    specialist_ok <- identical(route$profile, "standard_reflowR") ||
      any(vapply(route$stages, function(x) identical(x$family, "cttir") && identical(x$status, "approved"), logical(1)))
    evidence <- list(profile = route$profile, stages = length(route$stages), gaps = as.list(route$gaps),
      lock_consistent = consistent, specialist_evidence = specialist_ok)
    if (!consistent || !specialist_ok) {
      return(audit_result("fail", "The recorded workflow or dependencies differ from routing against the pinned catalog.", evidence))
    }
    audit_result("pass", paste0("Profile ", route$profile, " matches capability evidence in the pinned catalog."), evidence)
  })
}

audit_std_bundle <- function(context) {
  audit_with_project(context, function(p) {
    manifest_file <- file.path(p$path, "metadata", "workflow-template.json")
    if (is.na(file_hash(manifest_file))) return(audit_result("fail", "metadata/workflow-template.json is missing."))
    manifest <- read_document(manifest_file)
    bundled <- standard_bundle_manifest()
    baseline <- stats::setNames(vapply(p$manifest$files, function(x) x$baseline_sha256, character(1)),
      vapply(p$manifest$files, function(x) x$path, character(1)))
    code <- grep("^(code/|_targets[.]R$)", names(baseline), value = TRUE)
    edited <- code[vapply(code, function(path) !identical(file_hash(file.path(p$path, path)), unname(baseline[path])), logical(1))]
    evidence <- list(mode = manifest$mode, initializer_invoked = manifest$initializer_invoked,
      source_revision = manifest$source_revision, manifest_matches_bundle = identical(manifest$files, bundled$files),
      edited_code = as.list(edited))
    if (!identical(manifest$mode, "adapted_templates_with_reviewed_stage_library") || isTRUE(manifest$initializer_invoked)) {
      return(audit_result("fail", "The workflow template provenance is not the reviewed adapted reflowR layout.", evidence))
    }
    if (length(edited)) {
      return(audit_result("warning", "Reviewed stage code was edited locally; receipts from edited code do not count as evidence.", evidence))
    }
    audit_result("pass", "Adapted reflowR layout with the reviewed stage library; managed code matches its baseline.", evidence)
  })
}

audit_std_apis <- function(context) {
  audit_with_project(context, function(p) {
    catalog <- catalog_snapshot(p$spec$provenance$catalog_id)
    bundle <- audit_bundle(context)
    if (inherits(bundle, "error")) stop(bundle)
    code <- audit_project_code(p, catalog, bundle)
    rows <- list()
    for (unit in code$units) {
      if (!is.expression(unit$exprs)) next
      result <- validate_generated_code(unit$code, catalog)
      namespaced <- result[!is.na(result$package) & result$package != "base", , drop = FALSE]
      if (nrow(namespaced)) rows[[unit$path]] <- cbind(file = unit$path, namespaced, stringsAsFactors = FALSE)
    }
    calls <- if (length(rows)) do.call(rbind, rows) else data.frame(status = character(), package = character())
    # The generated validation script optionally calls the builder that created
    # the project; that self-check is reported separately, not as an approval.
    self_check <- calls$package == "cttiR" & calls$export %in% "validate_spec" & calls$file == "code/validate_project.R"
    unapproved <- calls[calls$status != "ok" & !self_check, , drop = FALSE]
    index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(x) x$name, character(1)))
    pins <- vapply(p$lock$dependencies, function(dep) {
      record <- index[[dep$package]]
      !is.null(record) && identical(record$version, dep$version) && identical(record$source_hash, dep$source_hash)
    }, logical(1))
    found <- code$findings
    listed <- function(keep) {
      x <- found[keep, , drop = FALSE]
      lapply(seq_len(nrow(x)), function(i) paste0(x$file[[i]], ": ", x$name[[i]], if (nzchar(x$name[[i]])) " ", "(", x$reason[[i]], ")"))
    }
    fail <- found$status == "fail"
    evidence <- list(files = length(code$units), namespaced_calls = nrow(calls),
      unapproved = lapply(seq_len(nrow(unapproved)), function(i) {
        paste0(unapproved$file[[i]], ": ", unapproved$package[[i]], "::", unapproved$export[[i]], " (", unapproved$reason[[i]], ")")
      }),
      unsafe = listed(fail & !found$reason %in% c("parse_error", "unresolved_call")),
      unparseable = as.list(found$file[found$reason == "parse_error"]),
      unresolved = listed(fail & found$reason == "unresolved_call"),
      advisory = listed(found$status == "warning"), not_checked = listed(found$status == "not_tested"),
      builder_self_checks = sum(self_check), dependencies = length(pins), pins_consistent = all(pins))
    counts <- c(unsafe = length(evidence$unsafe), unparseable = length(evidence$unparseable),
      unresolved = length(evidence$unresolved), unapproved = nrow(unapproved))
    if (any(counts > 0L) || !all(pins)) {
      message <- paste0("Project code has ", counts[["unsafe"]], " forbidden or unsafe constructs, ", counts[["unparseable"]],
        " unparseable files, ", counts[["unresolved"]], " unresolved calls and ", counts[["unapproved"]],
        " namespaced calls without an approval of the pinned revision; dependency pins ",
        if (all(pins)) "agree." else "differ.")
      return(audit_result("fail", message, evidence))
    }
    if (length(evidence$not_checked)) {
      return(audit_result("not_tested", paste0(length(evidence$not_checked), " code files could not be checked statically."), evidence))
    }
    if (length(evidence$advisory)) {
      message <- paste0(length(evidence$advisory), " calls or chunks need review: non-namespaced package calls, ",
        "dynamic package loads, forbidden names used as values or chunk engines that cannot be checked.")
      return(audit_result("warning", message, evidence))
    }
    message <- paste0(nrow(calls), " namespaced calls in ", length(code$units),
      " code files are covered by approvals of the pinned catalog revision; no unsafe or unresolved calls.")
    audit_result("pass", message, evidence)
  })
}

audit_std_apis_description <- paste("All project R code parses without forbidden, unsafe or unresolved calls,",
  "every namespaced call is approved for the pinned revision and dependency pins agree.")

audit_std_preconditions <- function(context) {
  audit_with_project(context, function(p) {
    analysis <- analysis_configuration(p$spec)
    evidence <- list(state = analysis$state, engine = analysis$candidate_engine,
      missing = analysis$missing_fields, gaps = analysis$capability_gaps)
    if (identical(analysis$state, "configuration_recorded")) {
      return(audit_result("pass", "Mappings, reviewed settings and approval are recorded; data checks run only on explicit data.", evidence))
    }
    message <- paste0("Analysis configuration incomplete: ", length(analysis$missing_fields), " missing fields, ",
      length(analysis$capability_gaps), " capability gaps.")
    audit_result("warning", message, evidence)
  })
}

audit_std_execution <- function(context) {
  audit_with_project(context, function(p) {
    route <- project_route(p$spec)
    prediction <- any(vapply(route$stages, function(x) identical(x$capability, "std.prediction.tidymodels") && isTRUE(x$enabled), logical(1)))
    receipt <- read_receipt(p, "output/workflow-receipt.json")
    receipt_ok <- is.null(receipt) || receipt_matches_code(p, receipt)
    evidence <- list(prediction_stage_enabled = prediction, workflow_receipt = !is.null(receipt),
      receipt_from_reviewed_code = receipt_ok, analysis_approved = isTRUE(p$spec$analysis$approved))
    if (prediction) return(audit_result("fail", "A prediction stage is enabled without a reviewed leakage-safe adapter.", evidence))
    if (!receipt_ok) return(audit_result("warning", "A study-data receipt was produced by edited stage code.", evidence))
    audit_result("pass", "No unapproved inferential stage is enabled; prediction remains a recorded gap.", evidence)
  })
}

audit_kb_approvals <- function(context) {
  catalog <- if (is.null(context$path)) resolve_catalog() else tryCatch(resolve_catalog(context$path), error = function(e) e)
  pin_note <- NULL
  if (inherits(catalog, "error")) {
    # Never substitute silently: name the pin and say which catalog was used.
    p <- audit_project(context)
    pin <- if (is.null(p) || inherits(p, "error") || !is.character(p$lock$catalog_id)) "unknown" else p$lock$catalog_id
    reason <- audit_condition_message(catalog)
    catalog <- resolve_catalog()
    pin_note <- list(pin = pin, reason = reason, used = catalog$content_id)
  }
  decisions <- 0L
  complete <- 0L
  incomplete <- character()
  packages <- 0L
  docs <- list()
  for (package in catalog$packages) {
    approvals <- Filter(function(x) identical(x$status, "approved"), package$approvals)
    if (!length(approvals)) next
    packages <- packages + 1L
    docs[[package$name]] <- documentation_counts(package$documentation_corpus)
    for (approval in approvals) {
      decisions <- decisions + 1L
      if (identical(approval_coverage(package, approval)$state, "complete")) {
        complete <- complete + 1L
      } else {
        incomplete <- c(incomplete, approval$approval_id)
      }
    }
  }
  total <- function(field) sum(vapply(docs, function(x) x[[field]], numeric(1)))
  standard <- standard_capability_approvals(catalog)
  pending <- Filter(function(x) !identical(x$status, "approved"), standard)
  pending_text <- vapply(names(pending), function(id) {
    paste0(id, " (", paste(unlist(pending[[id]]$missing), collapse = ", "), ")")
  }, character(1))
  evidence <- list(catalog_id = catalog$content_id, approved_packages = packages, decisions = decisions,
    complete = complete, incomplete = as.list(incomplete),
    pinned_catalog = if (!is.null(pin_note)) pin_note else if (is.null(context$path)) "not_applicable" else "used",
    standard_capabilities = length(standard), standard_without_approval = as.list(unname(pending_text)),
    documentation = list(reference_topics_stored = total("reference_topics_stored"),
      reference_topics_indexed = total("reference_topics_indexed"),
      vignette_sources_stored = total("vignette_sources_stored"), vignette_sources_indexed = total("vignette_sources_indexed"),
      vignette_files_stored = total("vignette_files_stored"), vignette_files_indexed = total("vignette_files_indexed"),
      packages = docs))
  prefix <- ""
  if (!is.null(pin_note)) {
    prefix <- paste0("The project's pinned catalog snapshot ", pin_note$pin, " is unavailable (", pin_note$reason,
      "); these results were computed against the active catalog ", pin_note$used, " instead. ")
  }
  if (!decisions) {
    return(audit_result("fail", paste0(prefix, "The catalog has no workflow approvals; supported-profile readiness cannot pass."), evidence))
  }
  if (length(incomplete)) {
    return(audit_result("fail", paste0(prefix, "Some approvals no longer have complete documentation or API coverage."), evidence))
  }
  if (length(pending)) {
    message <- paste0(prefix, "Standard workflow capabilities have no valid approval in this catalog, so their stages ",
      "are approval-pending: ", paste(pending_text, collapse = "; "), ".")
    return(audit_result("fail", message, evidence))
  }
  documentation <- evidence$documentation
  vignettes_missing <- documentation$vignette_sources_stored < documentation$vignette_sources_indexed
  message <- paste0(prefix, complete, " approvals across ", packages, " package revisions cover their required callables, ",
    "topics and documents. Their corpora store ", documentation$reference_topics_stored, " of ",
    documentation$reference_topics_indexed, " reference topics and ", documentation$vignette_sources_stored,
    " of ", documentation$vignette_sources_indexed, " vignette sources as text; the rest are indexed by hash only.")
  audit_result(if (is.null(pin_note) && !vignettes_missing) "pass" else "warning", message, evidence)
}

audit_res_separation <- function(context) {
  con <- DBI::dbConnect(RSQLite::SQLite(), resource_snapshot()$file, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT name, adapter_status FROM packages")
  approvals <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM workflow_approvals")$n
  catalog <- resolve_catalog()
  has_approval <- function(p) any(vapply(p$approvals, function(a) identical(a$status, "approved"), logical(1)))
  approved <- vapply(Filter(has_approval, catalog$packages), function(p) p$name, character(1))
  claims <- rows$name[grepl("tested|approved", rows$adapter_status) & !rows$name %in% approved]
  evidence <- list(resource_candidates = nrow(rows), resource_db_approvals = approvals,
    catalog_approved_packages = length(approved), unsupported_claims = as.list(claims))
  if (length(claims) || approvals > 0L) {
    return(audit_result("fail", "Resource metadata claims adapter testing or approval that the knowledge catalog does not evidence.", evidence))
  }
  audit_result("pass", "Resource metadata stays discovery-only; approvals live only in the knowledge catalog.", evidence)
}

audit_res_release <- function(context) {
  policy <- tryCatch(bioc_release_policy(NULL, catalog_store()), error = function(e) NULL)
  if (is.null(policy)) return(audit_result("not_tested", "No compatible Bioconductor release could be resolved for this R version."))
  evidence <- policy[intersect(names(policy), c("release", "source", "r_minor", "running_r", "compatible"))]
  if (!isTRUE(policy$compatible)) {
    return(audit_result("warning", "The recorded Bioconductor release does not match the running R version.", evidence))
  }
  audit_result("pass", paste0("Bioconductor ", policy$release, " is compatible with the running R."), evidence)
}

# Projects pinning Bioconductor revisions need the R minor version of that
# release; installed versions that differ from the pins are refused by the
# interop helpers when they are given the project pins.
audit_res_bioc_pins <- function(context) {
  audit_with_project(context, function(p) {
    pins <- Filter(function(d) is.character(d$bioc_release), p$lock$dependencies)
    if (!length(pins)) return(audit_result("not_applicable", "The project pins no Bioconductor package."))
    releases <- unique(vapply(pins, function(d) d$bioc_release, character(1)))
    table <- bioc_release_table()
    r_minor <- table$r_minor[match(releases, table$release)]
    running <- running_r_minor()
    installed <- vapply(pins, function(d) {
      tryCatch(as.character(utils::packageVersion(d$package)), error = function(e) NA_character_)
    }, character(1))
    differs <- vapply(seq_along(pins), function(i) {
      is.na(installed[[i]]) || package_version(installed[[i]]) != package_version(pins[[i]]$version)
    }, logical(1))
    mismatches <- character()
    if (any(differs)) {
      mismatches <- paste0(vapply(pins[differs], function(d) d$package, character(1)), " ",
        ifelse(is.na(installed[differs]), "not installed", installed[differs]), " (pinned ",
        vapply(pins[differs], function(d) d$version, character(1)), ")")
    }
    evidence <- list(releases = as.list(releases), r_minor = as.list(r_minor), running_r = running,
      version_mismatches = as.list(mismatches))
    if (length(releases) > 1L || anyNA(r_minor) || any(r_minor != running)) {
      message <- paste0("The project pins Bioconductor ", paste(releases, collapse = ", "), " (R ",
        paste(r_minor, collapse = ", "), ") but this session runs R ", running,
        "; those revisions were not verified for this R.")
      return(audit_result("fail", message, evidence))
    }
    if (length(mismatches)) {
      message <- paste0("Installed Bioconductor-family packages differ from the project pins: ",
        paste(mismatches, collapse = "; "), ". The interop helpers refuse them when given the project pins.")
      return(audit_result("warning", message, evidence))
    }
    audit_result("pass", paste0("Bioconductor ", releases, " pins match the running R ", running, " and the installed versions."),
      evidence)
  })
}

audit_res_pins <- function(context) {
  audit_with_project(context, function(p) {
    snapshot <- tryCatch(resource_snapshot(p$path), error = function(e) e)
    if (inherits(snapshot, "error")) return(audit_result("fail", "The project's pinned resource snapshot is unavailable or corrupt."))
    audit_result("pass", "The pinned resource snapshot is retained and verified.", list(resource_id = snapshot$id))
  })
}

audit_res_interop <- function(context) {
  registry <- capability_registry()
  catalog <- resolve_catalog()
  tested <- Filter(function(x) identical(x$status, "adapter_tested") && !identical(x$family, "standard") && !identical(x$family, "cttir"),
    registry$capabilities)
  unapproved <- character()
  for (cap in tested) {
    if (!identical(capability_approval(cap, catalog)$status, "approved")) unapproved <- c(unapproved, cap$id)
  }
  evidence <- list(adapter_tested = length(tested), without_approval = as.list(unapproved))
  if (length(unapproved)) {
    return(audit_result("warning", "Some interop capabilities are marked tested but lack approvals in the active catalog.", evidence))
  }
  audit_result("pass", "Every tested class/method/conversion capability has approved fixture evidence.", evidence)
}

audit_checks_standard <- function() {
  project_check <- function(id, description, run, ...) {
    audit_check(id, "project", description, run, applies = function(context) {
      has <- audit_has_path(context)
      if (!isTRUE(has)) has else audit_standard_project(context)
    }, ...)
  }
  list(
    project_check("STD-001", "Profile routing matches capability evidence and the recorded lock.",
      audit_std_routing, required = TRUE, read_effects = c("reads_project_metadata", "reads_catalog_store", "reads_installation"),
      evidence_schema = c("profile", "stages", "gaps", "lock_consistent", "specialist_evidence")),
    project_check("STD-002", "Adapted reflowR layout provenance and unmodified reviewed stage code.",
      audit_std_bundle, required = TRUE, read_effects = c("reads_project_metadata", "reads_installation"),
      evidence_schema = c("mode", "initializer_invoked", "source_revision", "manifest_matches_bundle", "edited_code")),
    project_check("STD-003", audit_std_apis_description,
      audit_std_apis, required = TRUE, read_effects = c("reads_project_metadata", "reads_catalog_store", "reads_installation"),
      evidence_schema = c("files", "namespaced_calls", "unapproved", "unsafe", "unparseable", "unresolved", "advisory",
        "not_checked", "builder_self_checks", "dependencies", "pins_consistent")),
    project_check("STD-004", "Model data and design preconditions are recorded (no data are opened).",
      audit_std_preconditions, required = function(context) audit_readiness_at_least(context, "analysis_ready"),
      severity = "warning", read_effects = "reads_project_metadata", evidence_schema = c("state", "engine", "missing", "gaps")),
    project_check("STD-005", "No unapproved inferential stage, prediction leakage path or receipt from edited code.",
      audit_std_execution, required = TRUE, read_effects = "reads_project_metadata",
      evidence_schema = c("prediction_stage_enabled", "workflow_receipt", "receipt_from_reviewed_code", "analysis_approved")),
    audit_check("KB-006", "knowledge", "Workflow approvals exist and keep complete documentation and API coverage.",
      audit_kb_approvals, required = TRUE, read_effects = c("reads_installation", "reads_catalog_store"),
      evidence_schema = c("catalog_id", "approved_packages", "decisions", "complete", "incomplete")),
    audit_check("RES-003", "knowledge", "Resource metadata never claims API, adapter or approval evidence.",
      audit_res_separation, required = TRUE, read_effects = c("reads_installation", "reads_catalog_store"),
      evidence_schema = c("resource_candidates", "resource_db_approvals", "catalog_approved_packages", "unsupported_claims")),
    audit_check("RES-004", "knowledge", "Selected Bioconductor release is compatible with the running R.",
      audit_res_release, required = FALSE, read_effects = c("reads_installation", "reads_catalog_store"),
      evidence_schema = c("release", "source", "r_minor", "running_r", "compatible")),
    audit_check("RES-007", "project", "Pinned Bioconductor revisions belong to one release compatible with the running R.",
      audit_res_bioc_pins, required = TRUE, applies = audit_has_path,
      read_effects = c("reads_project_metadata", "reads_installation"),
      evidence_schema = c("releases", "r_minor", "running_r", "version_mismatches")),
    audit_check("RES-005", "project", "The project's pinned resource snapshot is retained.",
      audit_res_pins, required = TRUE, applies = audit_has_path, read_effects = c("reads_project_metadata", "reads_catalog_store"),
      evidence_schema = "resource_id"),
    audit_check("RES-006", "knowledge", "Tested class, method and conversion capabilities carry approved fixture evidence.",
      audit_res_interop, required = FALSE, read_effects = c("reads_installation", "reads_catalog_store"),
      evidence_schema = c("adapter_tested", "without_approval"))
  )
}
