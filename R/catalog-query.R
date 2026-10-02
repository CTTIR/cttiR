catalog_store <- function() {
  path <- getOption("cttiR.catalog_dir", tools::R_user_dir("cttiR", "data"))
  scalar_text(path, "catalog directory")
  assert_plain_path(path)
  path
}

resolve_catalog <- function(path = NULL) {
  bundled <- resource_file("extdata", "api-catalog.json.gz")
  if (!is.null(path)) {
    p <- read_project(path)
    id <- p$lock$catalog_id
    if (identical(id, "unavailable")) {
      abort_cttir("This project predates the API catalog and has no API snapshot pin.", "cttir_source_unavailable")
    }
    if (!grepl("^[a-f0-9]{64}$", id)) abort_cttir("Invalid catalog snapshot identifier.", "cttir_catalog_corrupt")
    return(catalog_snapshot(id))
  }
  active <- file.path(catalog_store(), "active.json")
  assert_plain_path(active)
  if (!file.exists(active)) {
    return(read_catalog(bundled))
  }
  id <- catalog_pointer()$content_id
  if (!is.character(id) || length(id) != 1L || !grepl("^[a-f0-9]{64}$", id)) {
    abort_cttir("Invalid active catalog pointer.", "cttir_catalog_corrupt")
  }
  result <- read_catalog(file.path(catalog_store(), "snapshots", id, "api-catalog.json.gz"))
  if (!identical(result$content_id, id)) abort_cttir("Active catalog identity mismatch.", "cttir_catalog_corrupt")
  result
}

#' Inspect cataloged package revisions
#' @param path Optional exact project root selecting its pinned API catalog.
#' @return A data frame of package identity, revision, extraction coverage and
#'   verification limits. S3 counts are missing for historical snapshots without
#'   a method index. `approved` counts callables covered by a complete reviewed
#'   workflow approval of that exact revision and `approved_roles` lists the
#'   approved roles (comma separated, empty when none). No namespaces are loaded
#'   to inspect installed versions.
#' @details Workflow approvals are reviewer decisions keyed to an exact package
#'   `source_hash`. [update()] attaches the bundled reviewed decisions and, when
#'   set, those in the JSON file named by `options(cttiR.approvals = path)` to
#'   the matching extracted revision inside the immutable snapshot, so project
#'   pins and [rollback_knowledge()] carry them. A decision counts only when
#'   that revision stores every required topic and document under a rights
#'   basis, every required callable is a statically verified documented export
#'   and a recorded fixture passed. Any source change, including documentation
#'   only, requires a new decision. Decisions never install or run anything.
#' @export
packages <- function(path = NULL) {
  catalog <- resolve_catalog(path)
  installed <- utils::installed.packages(fields = "Version")
  rows <- lapply(catalog$packages, function(p) {
    index <- match(p$name, installed[, "Package"])
    data.frame(
      package = p$name, version = p$version, revision = p$revision,
      source_hash = p$source_hash, repository = p$repository, provider = p$family,
      installed_version = if (is.na(index)) NA_character_ else installed[index, "Version"],
      pinned_version = if (is.null(path)) NA_character_ else p$version,
      exports = p$coverage$exports, resolved = p$coverage$resolved,
      documented = p$coverage$documented, approved = p$coverage$approved,
      approved_roles = paste(approval_text(p$approval_state$roles), collapse = ","),
      s3_declared = if (is.null(p$s3_methods)) NA_integer_ else length(p$s3_methods),
      s3_resolved = if (is.null(p$s3_methods)) NA_integer_ else sum(vapply(p$s3_methods, function(x) x$verification == "static_method_verified", logical(1))),
      documents_discovered = if (is.null(p$documentation_corpus)) NA_integer_ else p$documentation_corpus$coverage$discovered,
      documents_stored = if (is.null(p$documentation_corpus)) NA_integer_ else p$documentation_corpus$coverage$stored,
      freshness = p$freshness, stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

catalog_evidence_url <- function(package, path) {
  if (!startsWith(package$repository, "https://github.com/")) return(paste0(package$repository, "#", path))
  prefix <- if (is.null(package$source_subdir)) "" else sub("/+$", "", package$source_subdir)
  if (nzchar(prefix) && !startsWith(path, paste0(prefix, "/"))) path <- paste0(prefix, "/", path)
  paste0(package$repository, "/blob/", package$revision, "/", path)
}

#' Search revision-scoped APIs and stored documentation
#' @param query Nonempty literal search text, optionally `package::export`.
#' @param packages Optional character vector of exact package names.
#' @param path Optional exact project root selecting its pinned API catalog.
#' @param limit Positive integer result limit, at most 10000.
#' @return A data frame containing stable IDs, revision, snippet, score and source
#'   evidence. S3 declarations are labelled separately, including private
#'   implementations; static evidence does not establish installed dispatch or
#'   tested workflow approval. Reexports (`reexport_declared`) and S4/S7
#'   declarations (`static_declaration_only`) are never callable evidence.
#'   Only exports covered by a complete reviewed approval of the exact revision
#'   are labelled `workflow_approved` with `approved = TRUE`.
#' @export
search <- function(query, packages = NULL, path = NULL, limit = 20L) {
  scalar_text(query, "query")
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || !is.finite(limit) ||
      limit < 1 || limit > 10000 || limit != as.integer(limit)) {
    abort_cttir("Invalid search limit.")
  }
  if (!is.null(packages) && (!is.character(packages) || !length(packages) || anyNA(packages) || any(!nzchar(packages)))) {
    abort_cttir("packages must contain nonempty package names.")
  }
  catalog <- resolve_catalog(path)
  out <- data.frame(
    id = character(), package = character(), revision = character(), kind = character(),
    symbol = character(), snippet = character(), score = numeric(), evidence = character(),
    verification = character(), approved = logical(), stringsAsFactors = FALSE
  )
  q <- tolower(query)
  for (p in catalog$packages) {
    if (!is.null(packages) && !p$name %in% packages) next
    for (hit in if (grepl("::", query, fixed = TRUE)) list() else document_hits(p, query)) {
      source <- catalog_evidence_url(p, hit$path)
      out[nrow(out) + 1L, ] <- list(hit$id, p$name, p$revision, "document", "",
        hit$snippet, 5, source,
        "documentation_indexed", FALSE)
    }
    for (hit in method_hits(p, query)) {
      out[nrow(out) + 1L, ] <- list(hit$id, p$name, p$revision, "s3_method_declaration", hit$symbol,
        hit$snippet, 8, catalog_evidence_url(p, hit$source_path), hit$verification, FALSE)
    }
    for (hit in object_hits(p, query)) {
      out[nrow(out) + 1L, ] <- list(hit$id, p$name, p$revision, hit$kind, hit$symbol,
        hit$snippet, 8, catalog_evidence_url(p, hit$source_path), "static_declaration_only", FALSE)
    }
    approved <- approved_export_index(p)
    for (entry in p$exports) {
      symbol <- paste0(p$name, "::", entry$name)
      haystack <- tolower(paste(symbol, p$title, p$description))
      exact <- q %in% tolower(c(symbol, entry$name))
      if (!exact && !grepl(q, haystack, fixed = TRUE)) next
      evidence <- catalog_evidence_url(p, entry$source_path)
      covered <- !is.null(approved[[entry$name]]) && identical(entry$verification, "static_api_verified")
      out[nrow(out) + 1L, ] <- list(
        content_hash(paste(p$source_hash, symbol)), p$name, p$revision,
        entry$kind, symbol, paste(symbol, entry$signature), if (exact) 100 else 10,
        evidence, if (covered) "workflow_approved" else entry$verification, covered
      )
    }
  }
  out <- out[order(-out$score, out$package, out$symbol), , drop = FALSE]
  utils::head(out, as.integer(limit))
}

#' Retrieve grounded local API evidence
#'
#' Returns matching static API evidence and explicitly separates it from approved
#' workflow advice. It never evaluates code, calls a model or transmits a question.
#' @param question Nonempty question or exact package/export name.
#' @param path Optional exact project root selecting its pinned catalog.
#' @param verified_only Restrict results to resolved static APIs, including
#'   those covered by a reviewed workflow approval.
#' @return A `cttir_answer` with evidence, citations, limitations and no executable
#'   code when approved workflow evidence is unavailable.
#' @export
ask <- function(question, path = NULL, verified_only = TRUE) {
  scalar_flag(verified_only, "verified_only")
  hits <- search(question, path = path, limit = 10L)
  if (verified_only) hits <- hits[hits$verification %in% c("static_api_verified", "workflow_approved"), , drop = FALSE]
  structure(
    list(
      answer = if (nrow(hits)) {
        "Matching evidence records were found; inspect their revision and verification level."
      } else {
        "No matching verified API record was found. Try an exact package::export name."
      },
      steps = as.list(hits$snippet), code = "", citations = as.list(hits$evidence),
      verification_levels = unique(hits$verification), evidence = hits,
      limitations = c("Static signatures are not tested workflow approvals.", "Natural-language planning and local model integration remain pending.")
    ),
    class = "cttir_answer"
  )
}

#' @export
print.cttir_answer <- function(x, ...) {
  cat(x$answer, "\n")
  if (length(x$steps)) cat(paste(unlist(x$steps), collapse = "\n"), "\n")
  invisible(x)
}
