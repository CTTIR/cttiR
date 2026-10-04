read_source_text <- function(path, max_bytes = 1048576L) {
  assert_plain_path(path)
  if (!file.exists(path) || dir.exists(path) || file.info(path)$size > max_bytes) {
    abort_cttir("Source file is missing or exceeds the extraction size limit.", "cttir_source_unavailable")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

static_functions <- function(text, source_path) {
  expressions <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) {
    abort_cttir("An R source file could not be parsed statically.", "cttir_source_unavailable", "source_parse")
  })
  result <- new.env(parent = emptyenv())
  result$values <- list()
  record <- function(name, value) {
    if (name %in% names(result$values)) {
      value$signature <- "unresolved: multiple assignments"
      value$arguments <- list()
    }
    result$values[[name]] <- value
  }
  inspect <- function(expr, direct = FALSE, depth = 0L) {
    if (depth > 64L) abort_cttir("Static assignment nesting exceeds the bound.", "cttir_source_unavailable")
    if (!is.call(expr) || !is.symbol(expr[[1]])) return(invisible(NULL))
    head <- as.character(expr[[1]])
    # Function bodies have their own scope and are never traversed or evaluated.
    if (head == "function") return(invisible(NULL))
    if (head %in% c("<-", "=", "<<-") && length(expr) == 3L) {
      lhs <- expr[[2]]
      names <- if (is.symbol(lhs)) as.character(lhs) else all.names(lhs, functions = FALSE, unique = TRUE)
      literal <- direct && head != "<<-" && is.symbol(lhs) && is.call(expr[[3]]) &&
        identical(expr[[3]][[1]], as.name("function"))
      for (name in names) {
        args <- if (literal) expr[[3]][[2]] else NULL
        signature <- if (literal) {
          sub(" NULL$", "", paste(deparse(as.call(list(as.name("function"), args, NULL)), width.cutoff = 500L), collapse = " "))
        } else {
          "unresolved: nonliteral or conditional assignment"
        }
        record(name, list(signature = signature, arguments = as.list(names(args)), source_path = source_path))
      }
      inspect(expr[[3]], depth = depth + 1L)
      return(invisible(NULL))
    }
    if (head == "assign" && length(expr) >= 3L && is.character(expr[[2]]) && length(expr[[2]]) == 1L) {
      record(expr[[2]], list(signature = "unresolved: dynamic assignment", arguments = list(), source_path = source_path))
    }
    parts <- as.list(expr)[-1]
    for (i in seq_along(parts)) {
      if (identical(unname(parts[i]), unname(alist(x = )))) next
      if (is.call(parts[[i]])) inspect(parts[[i]], depth = depth + 1L)
    }
    invisible(NULL)
  }
  for (expr in expressions) inspect(expr, direct = TRUE)
  result$values
}

extract_source <- function(path, repository, revision, family = "local", documentation_rights = NULL, distribution_version = NULL) {
  scalar_text(path, "source path")
  scalar_text(repository, "repository")
  scalar_text(revision, "revision")
  assert_plain_path(path)
  description_file <- if (is.null(distribution_version)) "DESCRIPTION" else "DESCRIPTION.in"
  description <- read_source_text(file.path(path, description_file))
  if (!is.null(distribution_version)) {
    scalar_text(distribution_version, "R distribution version")
    if (!grepl("^[0-9]+[.][0-9]+[.][0-9]+$", distribution_version)) {
      abort_cttir("Only an exact released R distribution version is supported.", "cttir_source_unavailable")
    }
    description <- gsub("@VERSION@", distribution_version, description, fixed = TRUE)
    if (grepl("@[A-Za-z_]+@", description)) {
      abort_cttir("Unsupported distribution DESCRIPTION substitution.", "cttir_source_unavailable")
    }
  }
  # Pass the UTF-8 bytes through and mark them, so fields never depend on the
  # session locale (translating to a C locale would lose characters).
  input <- textConnection(description, encoding = "bytes")
  desc <- tryCatch(read.dcf(input), error = function(e) {
    abort_cttir("Invalid source DESCRIPTION.", "cttir_source_unavailable")
  }, finally = close(input))
  Encoding(desc) <- ifelse(!is.na(desc) & validUTF8(desc), "UTF-8", "unknown")
  value <- function(field) if (field %in% colnames(desc)) unname(desc[1, field]) else ""
  package <- value("Package")
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", package)) abort_cttir("Invalid source package name.", "cttir_source_unavailable")
  namespace <- read_source_text(file.path(path, "NAMESPACE"))
  ns <- tryCatch(parse(text = namespace), error = function(e) {
    abort_cttir("Invalid static NAMESPACE.", "cttir_source_unavailable")
  })
  source_files <- sort(list.files(file.path(path, "R"), "\\.[Rr]$", recursive = FALSE, full.names = TRUE))
  if (length(source_files) > 5000L) abort_cttir("Source file count exceeds the configured bound.", "cttir_source_unavailable")
  funcs <- list()
  declarations <- list()
  hashes <- list(NAMESPACE = digest::digest(file = file.path(path, "NAMESPACE"), algo = "sha256"))
  hashes[[description_file]] <- digest::digest(file = file.path(path, description_file), algo = "sha256")
  for (f in source_files) {
    text <- read_source_text(f)
    rel <- substring(f, nchar(path) + 2L)
    hashes[[rel]] <- digest::digest(file = f, algo = "sha256")
    found <- static_functions(text, rel)
    declarations[[length(declarations) + 1L]] <- static_object_declarations(text, rel)
    # Multiple assignments are deliberately unresolved, rather than evaluated.
    for (name in names(found)) {
      if (name %in% names(funcs)) {
        found[[name]]$signature <- "unresolved: multiple assignments"
        found[[name]]$arguments <- list()
      }
      funcs[[name]] <- found[[name]]
    }
  }
  exports <- character()
  methods <- list()
  for (expr in ns) {
    if (!is.call(expr) || !is.symbol(expr[[1]])) next
    head <- as.character(expr[[1]])
    if (head == "export") {
      args <- as.list(expr)[-1]
      if (all(vapply(args, function(x) is.character(x) || is.symbol(x), logical(1)))) {
        exports <- c(exports, vapply(args, as.character, character(1)))
      }
    } else if (head == "exportPattern" && length(expr) == 2L && is.character(expr[[2]])) {
      exports <- c(exports, grep(expr[[2]], names(funcs), value = TRUE))
    } else if (head %in% c("S3method", "exportMethods", "exportClasses")) {
      methods <- append(methods, list(paste(deparse(expr), collapse = " ")))
    }
  }
  topics <- list()
  docs <- sort(list.files(file.path(path, "man"), "\\.Rd$", recursive = FALSE, full.names = TRUE))
  for (f in docs) {
    text <- read_source_text(f)
    rel <- substring(f, nchar(path) + 2L)
    hashes[[rel]] <- digest::digest(file = f, algo = "sha256")
    # Only index the plain alias tags; no Rd macros or embedded expressions execute.
    hits <- regmatches(text, gregexpr("\\\\alias\\{[^{}]+\\}", text))[[1]]
    aliases <- sub("\\}$", "", sub("^\\\\alias\\{", "", hits))
    # Rd-escaped aliases such as `\%>\%` name the unescaped topic.
    aliases <- gsub("\\\\([%{}\\\\])", "\\1", aliases)
    for (alias in aliases) topics[[alias]] <- list(path = rel, sha256 = hashes[[rel]])
  }
  s3_methods <- static_s3_methods(ns, funcs, topics)
  directives <- namespace_directives(ns)
  objects <- object_evidence(declarations, directives, unique(exports))
  corpus <- document_inventory(path, documentation_rights)
  for (doc in corpus$documents) hashes[[doc$path]] <- doc$source_sha256
  revision_hash <- content_hash(json_text(hashes[sort(names(hashes), method = "radix")]))
  entries <- lapply(sort(unique(exports)), function(name) {
    fn <- funcs[[name]]
    topic <- topics[[name]]
    entry <- list(
      name = name, kind = if (is.null(fn) || startsWith(fn$signature, "unresolved")) "unresolved_export" else "function",
      signature = if (is.null(fn)) "unresolved" else fn$signature,
      arguments = if (is.null(fn)) list() else fn$arguments,
      verification = if (is.null(fn) || startsWith(fn$signature, "unresolved")) "unknown" else "static_api_verified",
      source_path = if (is.null(fn)) "NAMESPACE" else fn$source_path,
      documentation = topic, approved = FALSE
    )
    # Reexports, S4 and S7 objects keep non-callable labels; plain functions are unchanged.
    special <- if (entry$kind == "unresolved_export") classify_export(name, fn, directives, objects) else NULL
    if (!is.null(special)) {
      entry$kind <- special$kind
      entry$signature <- special$signature
      entry$verification <- special$verification
      if (!is.null(special$owner_package)) entry$owner_package <- special$owner_package
    }
    entry
  })
  kinds <- vapply(entries, function(x) x$kind, character(1))
  list(
    name = package, version = value("Version"), title = value("Title"),
    description = value("Description"), license = value("License"), repository = repository,
    family = family, revision = revision, source_hash = revision_hash,
    maintainer_evidence = list(source_hash = revision_hash,
      description_sha256 = hashes[[description_file]], description_file = description_file,
      author = value("Author"), authors_r_literal = value("Authors@R"),
      authors_r_roles = description_roles(value("Authors@R")),
      maintainer = value("Maintainer"), copyright = value("Copyright"),
      source_url = repository, extraction = "dcf_text_no_execution", ownership = "not_inferred"),
    exports = entries, methods = methods, s3_methods = s3_methods, source_files = hashes, documentation_corpus = corpus,
    coverage = list(
      exports = length(entries), documented = sum(vapply(entries, function(x) !is.null(x$documentation), logical(1))),
      resolved = sum(vapply(entries, function(x) x$verification == "static_api_verified", logical(1))),
      approved = 0L, s3_declared = length(s3_methods),
      s3_resolved = sum(vapply(s3_methods, function(x) x$verification == "static_method_verified", logical(1))),
      reexports = sum(kinds == "reexport"), s4_classes = length(objects$s4$classes),
      s4_generics = length(objects$s4$generics), s4_methods = length(objects$s4$methods),
      s4_unresolved = length(objects$s4$unresolved), s7_classes = length(objects$s7$classes),
      s7_generics = length(objects$s7$generics)
    ),
    s4 = objects$s4, s7 = objects$s7,
    freshness = "not_rechecked", extraction = "static_no_execution", static_assignment_version = 2L,
    evidence_version = 3L
  )
}

.package_json_cache <- new.env(parent = emptyenv())

# Byte-identical to json_text(catalog), assembled from per-package fragments that
# are cached by an in-memory hash of each record, so unchanged packages are
# serialized once per session. Content IDs are defined over these exact bytes.
package_json <- function(package) {
  key <- digest::digest(package, algo = "sha256")
  if (exists(key, envir = .package_json_cache, inherits = FALSE)) return(get(key, envir = .package_json_cache))
  if (length(ls(.package_json_cache)) >= 1000L) rm(list = ls(.package_json_cache), envir = .package_json_cache)
  text <- json_text(package)
  assign(key, text, envir = .package_json_cache)
  text
}

catalog_json <- function(catalog) {
  if (!is.list(catalog) || is.null(names(catalog)) || !is.list(catalog$packages) || !length(catalog$packages)) {
    return(json_text(catalog))
  }
  fields <- vapply(names(catalog), function(name) {
    value <- if (identical(name, "packages")) {
      paste0("[", paste(vapply(catalog$packages, package_json, character(1)), collapse = ","), "]")
    } else {
      json_text(catalog[[name]])
    }
    paste0(json_text(name), ":", value)
  }, character(1))
  paste0("{", paste(fields, collapse = ","), "}")
}

write_catalog <- function(packages, path, inventory = list(), tombstones = NULL) {
  names <- vapply(packages, function(p) p$name, character(1))
  if (anyDuplicated(names)) abort_cttir("Catalog package identities are duplicated.", "cttir_catalog_corrupt")
  packages <- packages[order(names, method = "radix")]
  catalog <- list(schema_version = 1L, packages = packages, inventory = inventory)
  if (length(tombstones)) catalog$tombstones <- tombstones
  catalog$content_id <- content_hash(catalog_json(catalog))
  con <- gzfile(path, open = "wb", compression = 9)
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(catalog_json(catalog)), con)
  catalog$content_id
}

.catalog_cache <- new.env(parent = emptyenv())

read_catalog <- function(file) {
  assert_plain_path(file)
  if (!file.exists(file)) abort_cttir("Catalog snapshot is unavailable.", "cttir_source_unavailable")
  if (dir.exists(file) || file.info(file)$size > 25000000L) {
    abort_cttir("Catalog file exceeds the read limit or is not regular.", "cttir_catalog_corrupt")
  }
  cache_key <- digest::digest(file = file, algo = "sha256")
  if (exists(cache_key, envir = .catalog_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .catalog_cache, inherits = FALSE))
  }
  con <- gzfile(file, "rb")
  on.exit(close(con), add = TRUE)
  text <- readBin(con, "raw", n = 25000001L)
  if (length(text) > 25000000L) abort_cttir("Catalog exceeds the read limit.", "cttir_catalog_corrupt")
  # Catalogs are UTF-8 bytes; an unmarked string would be translated from the
  # session locale (e.g. C) and no longer reproduce its content ID.
  text <- rawToChar(text)
  Encoding(text) <- "UTF-8"
  catalog <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) {
    abort_cttir("Catalog cannot be decoded.", "cttir_catalog_corrupt")
  })
  id <- catalog$content_id
  catalog$content_id <- NULL
  # write_catalog() stores the canonical serialization followed by the content
  # ID, so hashing the text without that final field verifies the ID without
  # re-encoding every package; any other layout is checked by re-serialization.
  suffix <- paste0(",\"content_id\":\"", if (is.character(id) && length(id) == 1L) id else "", "\"}")
  stored_canonical <- is.character(id) && length(id) == 1L && endsWith(text, suffix) &&
    identical(id, content_hash(paste0(substr(text, 1L, nchar(text) - nchar(suffix)), "}")))
  if (!identical(catalog$schema_version, 1L) || !(stored_canonical || identical(id, content_hash(catalog_json(catalog))))) {
    abort_cttir("Catalog schema or logical hash is invalid.", "cttir_catalog_corrupt")
  }
  catalog$content_id <- id
  for (package in catalog$packages) {
    validate_document_corpus(package$documentation_corpus)
    for (doc in package$documentation_corpus$documents) {
      if (!identical(doc$source_sha256, package$source_files[[doc$path]])) {
        abort_cttir("Documentation is not aligned with its source revision.", "cttir_catalog_corrupt")
      }
    }
  }
  if (length(ls(.catalog_cache)) >= 4L) rm(list = ls(.catalog_cache)[[1]], envir = .catalog_cache)
  assign(cache_key, catalog, envir = .catalog_cache)
  catalog
}
