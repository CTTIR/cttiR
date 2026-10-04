# Reviewed workflow approvals keyed to exact package revisions.
#
# A decision names one package revision by its `source_hash`, a workflow role
# and profile, the adapter that was tested, the callables/topics/documents the
# role depends on and the fixture results. Decisions are reviewer input; they are
# attached to the package record inside the immutable catalog snapshot, so
# project pins, rollback and history carry the exact approval state atomically.
# A decision only counts when every requirement is evidenced by that same
# revision's statically verified exports and stored documentation. Decisions for
# any other revision are ignored, which makes every source change (including a
# documentation-only change with an unchanged version) require a new review.

approval_text <- function(x) as.character(unlist(x, use.names = FALSE))

# Read the bundled reviewed decisions plus an optional local decision file named
# by `getOption("cttiR.approvals")`. Both are validated strictly; unknown keys,
# malformed hashes and duplicate approval IDs are rejected.
approval_decisions <- function() {
  files <- resource_file("extdata", "approvals.json")
  local <- getOption("cttiR.approvals")
  if (!is.null(local)) {
    scalar_text(local, "cttiR.approvals")
    assert_plain_path(local)
    files <- c(files, local)
  }
  decisions <- list()
  for (file in files) decisions <- c(decisions, validate_approvals(read_document(file)))
  ids <- vapply(decisions, function(x) x$approval_id, character(1))
  if (anyDuplicated(ids)) {
    abort_cttir("Approval identifiers must be unique across decision files.", "cttir_schema_error", "duplicate_approval")
  }
  decisions
}

validate_approvals <- function(document) {
  document <- validate_document(document, "approvals")
  for (decision in document$decisions) {
    scope <- c(approval_text(decision$required_callables), approval_text(decision$required_methods),
      approval_text(decision$required_objects))
    if (!length(scope)) {
      abort_cttir("An approval decision must name the callables, methods or objects it covers.",
        "cttir_schema_error", "empty_approval_scope")
    }
    for (path in approval_text(decision$required_documents)) relative_file(path)
    for (fixture in decision$fixtures) relative_file(fixture$test_file)
  }
  document$decisions
}

# Rd aliases of stored reference documents only; unstored topics never count.
stored_topic_aliases <- function(stored) {
  aliases <- character()
  for (doc in stored) {
    if (!identical(doc$kind, "reference") || !grepl("[.]Rd$", doc$path)) next
    hits <- regmatches(doc$content, gregexpr("\\\\alias\\{[^{}]+\\}", doc$content))[[1]]
    found <- sub("\\}$", "", sub("^\\\\alias\\{", "", hits))
    aliases <- c(aliases, gsub("\\\\([%{}\\\\])", "\\1", found))
  }
  unique(aliases)
}

# Stored versus indexed reference topics (Rd files) and vignette files of one
# revision. A complete approval covers only what its decision requires; the rest
# of the corpus may be indexed by hash without stored text, and reports say so.
documentation_counts <- function(corpus) {
  docs <- if (is.null(corpus)) list() else corpus$documents
  kind <- vapply(docs, function(d) as.character(d$kind), character(1))
  paths <- vapply(docs, function(d) as.character(d$path), character(1))
  topic <- kind == "reference" & grepl("[.]Rd$", paths)
  stored <- vapply(docs, function(d) identical(d$storage, "source_text"), logical(1))
  reasons <- vapply(docs[(topic | kind == "vignette") & !stored], document_storage_reason, character(1))
  # A vignette is identified by its source document; inst/doc copies of the same
  # source, rendered outputs and build assets are not separate vignettes.
  source <- vignette_source_name(paths)
  vignettes <- unique(source[!is.na(source)])
  vignettes_stored <- unique(source[!is.na(source) & stored])
  list(reference_topics_indexed = sum(topic), reference_topics_stored = sum(topic & stored),
    vignette_sources_indexed = length(vignettes), vignette_sources_stored = length(vignettes_stored),
    vignette_files_indexed = sum(kind == "vignette"), vignette_files_stored = sum(kind == "vignette" & stored),
    not_stored_reasons = as.list(table(reasons)))
}

vignette_source_name <- function(paths) {
  pattern <- "^(vignettes|inst/doc)/([^/]+)[.](Rmd|Rnw|qmd|Rmarkdown|Rhtml|Rtex)$"
  out <- rep(NA_character_, length(paths))
  hit <- grepl(pattern, paths, ignore.case = TRUE)
  out[hit] <- sub(pattern, "\\2", paths[hit], ignore.case = TRUE)
  out
}

#' Coverage of one approval decision against one package revision
#'
#' Complete requires the exact package/version/source hash; every required
#' callable is a statically verified function export whose reference topic is
#' stored for this revision; every required topic is an alias in a stored Rd
#' file; DESCRIPTION (or DESCRIPTION.in), NAMESPACE and every required document
#' are stored as source text; the source document manifest is complete; at least
#' one fixture passed and none failed; and a rights basis is recorded. Vignettes
#' and reference topics a decision does not require are not part of `state`; their
#' stored and indexed counts are reported in `counts` so completeness is never
#' read as full documentation coverage.
#' @param package_record One package record from `extract_source()` or a catalog.
#' @param approval One validated decision.
#' @return A list with `state`, `missing` and `counts`.
#' @noRd
approval_coverage <- function(package_record, approval) {
  callables <- unique(approval_text(approval$required_callables))
  topics <- unique(approval_text(approval$required_topics))
  description <- if (is.null(package_record$distribution)) "DESCRIPTION" else "DESCRIPTION.in"
  documents <- unique(c(description, "NAMESPACE", approval_text(approval$required_documents)))
  corpus <- package_record$documentation_corpus
  stored <- list()
  for (doc in corpus$documents) if (identical(doc$storage, "source_text")) stored[[doc$path]] <- doc
  conditions <- character()
  if (package_record$name %in% c("SeuratDisk", "BPCells", "presto", "glmGamPoi") &&
      !companion_provenance_valid(package_record)) conditions <- c(conditions, "maintainer_provenance_missing_or_stale")
  if (!identical(approval$package, package_record$name)) conditions <- c(conditions, "package_mismatch")
  if (!identical(approval$version, package_record$version)) conditions <- c(conditions, "version_mismatch")
  if (!identical(approval$source_hash, package_record$source_hash)) conditions <- c(conditions, "source_hash_mismatch")
  if (is.null(corpus) || !isTRUE(corpus$coverage$source_manifest_complete)) {
    conditions <- c(conditions, "documentation_manifest_incomplete")
  }
  rights <- approval_text(approval$rights_basis)
  if (length(rights) != 1L || is.na(rights) || !nzchar(trimws(rights))) conditions <- c(conditions, "rights_basis_missing")
  methods <- unique(approval_text(approval$required_methods))
  objects <- unique(approval_text(approval$required_objects))
  if (!length(c(callables, methods, objects))) conditions <- c(conditions, "no_required_api")
  names <- vapply(package_record$exports, function(x) x$name, character(1))
  exports <- stats::setNames(package_record$exports, names)
  topic_stored <- function(topic) {
    !is.null(topic) && is.character(topic$path) && !is.null(stored[[topic$path]]) &&
      identical(stored[[topic$path]]$source_sha256, topic$sha256)
  }
  covered <- vapply(callables, function(name) approvable_callable(exports[[name]], topic_stored), logical(1))
  # S3 implementations reached through dispatch: one statically verified,
  # unduplicated registration whose own documentation topic is stored.
  method_covered <- vapply(methods, function(name) {
    hits <- Filter(function(x) identical(x$implementation, name), package_record$s3_methods)
    if (length(hits) != 1L || !identical(hits[[1]]$verification, "static_method_verified")) return(FALSE)
    # An undocumented method is covered by its generic's stored topic in the same revision.
    generic <- exports[[sub("^.*::", "", hits[[1]]$generic)]]
    topic_stored(hits[[1]]$documentation) || (!is.null(generic) && topic_stored(generic$documentation))
  }, logical(1))
  # Exported data objects cannot be verified as callables; they must be
  # exported locally (not reexported) and documented in a stored topic.
  object_covered <- vapply(objects, function(name) {
    entry <- exports[[name]]
    !is.null(entry) && !identical(entry$kind, "reexport") && topic_stored(entry$documentation)
  }, logical(1))
  aliases <- stored_topic_aliases(stored)
  results <- vapply(approval$fixtures, function(x) as.character(x$result), character(1))
  failing <- vapply(approval$fixtures[results %in% c("fail", "error")], function(x) {
    paste(x$test_file, x$test_name, sep = "::")
  }, character(1))
  missing <- list(
    callables = as.list(callables[!covered]),
    methods = as.list(methods[!method_covered]),
    objects = as.list(objects[!object_covered]),
    topics = as.list(setdiff(topics, aliases)),
    documents = as.list(documents[!documents %in% names(stored)]),
    fixtures = as.list(c(failing, if (!any(results == "pass")) "no_passing_fixture")),
    conditions = as.list(conditions)
  )
  list(
    state = if (all(lengths(missing) == 0L)) "complete" else "incomplete",
    missing = missing,
    counts = c(list(
      required_callables = length(callables), covered_callables = sum(covered),
      required_methods = length(methods), covered_methods = sum(method_covered),
      required_objects = length(objects), covered_objects = sum(object_covered),
      required_topics = length(topics), stored_topics = length(intersect(topics, aliases)),
      required_documents = length(documents), stored_documents = sum(documents %in% names(stored)),
      fixtures = length(results), passing_fixtures = sum(results == "pass")
    ), documentation_counts(corpus))
  )
}

# Callable evidence levels that can back an approval: statically resolved
# functions, functions whose formals were read from the identical installed
# version in an isolated process, and literal S4 generic declarations (whose
# dispatch must be exercised by the approval's fixtures). Each needs a stored topic.
approvable_callable <- function(entry, topic_stored) {
  if (is.null(entry) || !topic_stored(entry$documentation)) return(FALSE)
  (identical(entry$kind, "function") && entry$verification %in% c("static_api_verified", "installed_api_verified")) ||
    (identical(entry$kind, "s4_generic") && identical(entry$verification, "static_declaration_only"))
}

#' Attach reviewed decisions to a package record
#'
#' Only decisions for this package and exact `source_hash` are attached. An
#' approved decision that is not complete is kept as `pending` with its missing
#' evidence; revoked decisions stay revoked. Exports are marked approved only when
#' covered by a complete approved decision.
#' @param package_record One package record.
#' @param decisions Validated decisions, for example from `approval_decisions()`.
#' @return The record with `approvals`, `approval_state`, export `approved` flags
#'   and `coverage$approved` (number of approved callables).
#' @noRd
attach_approvals <- function(package_record, decisions) {
  ids <- function(x) vapply(x, function(d) d$approval_id, character(1))
  same <- Filter(function(d) identical(d$package, package_record$name), decisions)
  matching <- Filter(function(d) identical(d$source_hash, package_record$source_hash), same)
  attached <- lapply(matching, function(d) {
    coverage <- approval_coverage(package_record, d)
    status <- if (identical(d$status, "revoked")) {
      "revoked"
    } else if (identical(d$status, "approved") && identical(coverage$state, "complete")) {
      "approved"
    } else {
      "pending"
    }
    list(approval_id = d$approval_id, package = d$package, version = d$version, source_hash = d$source_hash,
      role = d$role, profile = d$profile, adapter_id = d$adapter_id, adapter_version = d$adapter_version,
      status = status, decision_status = d$status,
      required_callables = as.list(approval_text(d$required_callables)),
      required_methods = as.list(approval_text(d$required_methods)),
      required_objects = as.list(approval_text(d$required_objects)),
      required_topics = as.list(approval_text(d$required_topics)),
      required_documents = as.list(approval_text(d$required_documents)),
      fixtures = lapply(d$fixtures, function(x) x[c("test_file", "test_name", "result", "run_at")]),
      decided_at = d$decided_at, rights_basis = d$rights_basis, coverage = coverage)
  })
  attached <- attached[order(ids(attached), method = "radix")]
  status <- vapply(attached, function(x) x$status, character(1))
  approved <- attached[status == "approved"]
  covered <- unique(unlist(lapply(approved, function(x) c(approval_text(x$required_callables), approval_text(x$required_objects)))))
  package_record$exports <- lapply(package_record$exports, function(entry) {
    entry$approved <- entry$name %in% covered
    entry
  })
  package_record$coverage$approved <- sum(vapply(package_record$exports, function(x) isTRUE(x$approved), logical(1)))
  if (!is.null(package_record$documentation_corpus)) {
    package_record$documentation_corpus$coverage$approval <- if (length(approved)) "approved_for_roles" else "pending"
  }
  others <- setdiff(ids(same), ids(matching))
  package_record$approvals <- attached
  package_record$approval_state <- list(
    state = if (length(approved)) "approved" else if (length(attached) || length(others)) "pending" else "unapproved",
    decisions = length(attached), approved = length(approved), pending = sum(status == "pending"),
    revoked = sum(status == "revoked"),
    roles = as.list(sort(unique(vapply(approved, function(x) x$role, character(1))), method = "radix")),
    other_revision_decisions = as.list(sort(others, method = "radix"))
  )
  package_record
}

# Recompute approval at read time rather than trusting stored flags: name ->
# list of covering approvals for this exact revision.
approved_export_index <- function(package) {
  index <- list()
  for (approval in package$approvals) {
    if (!identical(approval$status, "approved")) next
    if (!identical(approval_coverage(package, approval)$state, "complete")) next
    covering <- list(approval_id = approval$approval_id, role = approval$role, profile = approval$profile)
    for (name in c(approval_text(approval$required_callables), approval_text(approval$required_objects))) {
      index[[name]] <- c(index[[name]], list(covering))
    }
  }
  index
}

#' Approved callables of a catalog revision
#' @param catalog A catalog list, or `NULL` for the active catalog.
#' @param packages Optional exact package names.
#' @return A data frame with one row per approved export and covering approval.
#' @noRd
approved_callables <- function(catalog = NULL, packages = NULL) {
  if (is.null(catalog)) catalog <- resolve_catalog()
  if (!is.null(packages) && (!is.character(packages) || anyNA(packages))) abort_cttir("packages must be exact package names.")
  out <- data.frame(package = character(), version = character(), source_hash = character(), export = character(),
    signature = character(), approval_id = character(), role = character(), stringsAsFactors = FALSE)
  for (p in catalog$packages) {
    if (!is.null(packages) && !p$name %in% packages) next
    index <- approved_export_index(p)
    for (entry in p$exports) {
      if (is.null(index[[entry$name]])) next
      for (approval in index[[entry$name]]) {
        out[nrow(out) + 1L, ] <- list(p$name, p$version, p$source_hash, entry$name, entry$signature,
          approval$approval_id, approval$role)
      }
    }
  }
  out[order(out$package, out$export, out$approval_id, method = "radix"), , drop = FALSE]
}

# Effective approval status per approval ID, recomputed against the record.
effective_approvals <- function(package) {
  result <- list()
  if (is.null(package)) return(result)
  for (approval in package$approvals) {
    complete <- identical(approval_coverage(package, approval)$state, "complete")
    status <- if (identical(approval$status, "approved") && !complete) "pending" else as.character(approval$status)
    result[[approval$approval_id]] <- list(role = approval$role, status = status)
  }
  result
}

#' Approval changes between two catalog snapshots
#' @return A data frame of approvals `added`, `removed` (same revision, decision
#'   withdrawn or revoked) and `invalidated` (package revision changed or removed).
#' @noRd
approval_diff <- function(before, after) {
  out <- data.frame(package = character(), approval_id = character(), role = character(), change = character(),
    previous_source_hash = character(), current_source_hash = character(), status = character(),
    stringsAsFactors = FALSE)
  index <- function(catalog) stats::setNames(catalog$packages, vapply(catalog$packages, function(x) x$name, character(1)))
  old <- index(before)
  new <- index(after)
  for (name in sort(as.character(union(names(old), names(new))), method = "radix")) {
    a <- old[[name]]
    b <- new[[name]]
    was <- effective_approvals(a)
    now <- effective_approvals(b)
    previous <- if (is.null(a)) NA_character_ else a$source_hash
    current <- if (is.null(b)) NA_character_ else b$source_hash
    for (id in sort(as.character(union(names(was), names(now))), method = "radix")) {
      before_ok <- identical(was[[id]]$status, "approved")
      after_ok <- identical(now[[id]]$status, "approved")
      if (before_ok == after_ok) next
      change <- if (after_ok) "added" else if (!identical(previous, current)) "invalidated" else "removed"
      role <- if (is.null(now[[id]])) was[[id]]$role else now[[id]]$role
      out[nrow(out) + 1L, ] <- list(name, id, role, change, previous, current,
        if (is.null(now[[id]])) "absent" else now[[id]]$status)
    }
  }
  out
}

# Approval state of every adapter-tested standard-workflow capability in one
# catalog, decided exactly as the router decides it. The time-to-event figure
# panel additionally needs survival approved for the figures adapter.
standard_capability_approvals <- function(catalog, registry = capability_registry()) {
  caps <- Filter(function(x) startsWith(x$id, "std.") && identical(x$status, "adapter_tested"), registry$capabilities)
  figures <- caps[["std.figures.accessible"]]
  if (!is.null(figures)) {
    figures$packages <- figure_packages(registry, list(outcome_family = "time_to_event"))
    caps[["std.figures.accessible:time_to_event"]] <- figures
  }
  lapply(caps, function(cap) {
    approval <- capability_approval(cap, catalog)
    list(status = approval$status, missing = as.list(approval$missing))
  })
}

# API comparisons ignore reviewer decisions, which are reported by approval_diff().
without_approvals <- function(package) {
  if (is.null(package)) return(NULL)
  package$approvals <- NULL
  package$approval_state <- NULL
  package$coverage$approved <- NULL
  if (!is.null(package$documentation_corpus)) package$documentation_corpus$coverage$approval <- NULL
  package$exports <- lapply(package$exports, function(entry) {
    entry$approved <- NULL
    entry
  })
  package
}
