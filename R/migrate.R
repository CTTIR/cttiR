# Schema 1 remains readable without changing its content hash. Synchronization
# previews the pure migration and preserves the original before applying it.
spec_schema_version <- function() 2L

ecosystem_policy <- function() {
  list(seurat_for_relevant_gaps = TRUE, require_role_approval = TRUE, modality = "unknown",
    policy_version = 1L, allowed_providers = list("bioconductor", "seurat"))
}

spec_migrations <- function() {
  list("1" = function(spec) {
    spec$schema_version <- 2L
    spec$ecosystem <- merge_config(ecosystem_policy(), spec$ecosystem)
    spec
  })
}

spec_version_of <- function(spec) {
  version <- spec$schema_version
  if (!is.numeric(version) || length(version) != 1L || is.na(version) || !is.finite(version) ||
      version != round(version) || version < 0) {
    abort_cttir("schema_version must be a nonnegative whole number.", "cttir_schema_error",
      "schema_validation", "/schema_version")
  }
  as.integer(version)
}

# Schema 1 remains readable under its retained contract; version 0 and future
# versions are refused. Only migrate_spec() transforms a resolved specification.
check_schema_version <- function(x, kind = "project specification", supported = spec_schema_version(),
  allow_older = FALSE) {
  if (!is.list(x) || is.null(x$schema_version)) return(invisible(x))
  version <- x$schema_version
  if (!is.numeric(version) || length(version) != 1L || is.na(version) || !is.finite(version) ||
      version != round(version) || version < 0) {
    message <- paste0("The ", kind, " has schema_version ", encodeString(format(version), quote = "'"),
      "; it must be a whole number (currently ", supported, ").")
    abort_cttir(message, "cttir_schema_error", "schema_validation", "/schema_version",
      remediation = paste0("Set schema_version to ", supported, "."))
  }
  if (version > supported) {
    abort_cttir(
      paste0("This ", kind, " uses schema version ", format(version), ", newer than the supported version ",
        supported, "; it is treated as read-only."),
      "cttir_schema_error", "unsupported_schema_version", "/schema_version",
      remediation = "Install a cttiR release that supports this schema version. No files were changed."
    )
  }
  if (version < supported && !allow_older && version != 1L) {
    abort_cttir(
      paste0("This ", kind, " uses schema version ", format(version), ", older than the supported version ",
        supported, "; no migration from version ", format(version), " is defined."),
      "cttir_schema_error", "unsupported_schema_version", "/schema_version",
      remediation = paste0("Recreate the document with this cttiR release (schema version ", supported,
        "). No files were changed.")
    )
  }
  invisible(x)
}

spec_leaves <- function(x, prefix = "") {
  if (is.list(x) && length(x)) {
    keys <- if (is.null(names(x))) as.character(seq_along(x) - 1L) else names(x)
    keys <- gsub("~", "~0", gsub("/", "~1", keys, fixed = TRUE), fixed = TRUE)
    out <- list()
    for (i in seq_along(x)) out <- c(out, spec_leaves(x[[i]], paste0(prefix, "/", keys[[i]])))
    return(out)
  }
  stats::setNames(list(json_text(x)), if (nzchar(prefix)) prefix else "/")
}

spec_changes <- function(before, after) {
  a <- spec_leaves(before)
  b <- spec_leaves(after)
  pointers <- sort(union(names(a), names(b)), method = "radix")
  change <- vapply(pointers, function(p) {
    if (is.null(a[[p]])) "added" else if (is.null(b[[p]])) "removed" else if (!identical(a[[p]], b[[p]])) "changed" else ""
  }, character(1))
  data.frame(pointer = pointers[nzchar(change)], change = unname(change[nzchar(change)]), stringsAsFactors = FALSE)
}

#' Migrate a ProjectSpec one schema version at a time
#'
#' Internal. Returns the migrated spec, one record per applied step (source and
#' target version, before/after content hashes and changed JSON pointers), and
#' the untouched original. Never reads or writes project files beyond an
#' explicitly supplied spec file path.
#' @param spec A specification list or a local JSON/YAML file path.
#' @return list(spec, steps, original).
#' @noRd
migrate_spec <- function(spec) {
  if (is.character(spec)) spec <- read_document(spec)
  if (!is.list(spec) || is.null(names(spec))) {
    abort_cttir("A project specification must be a named mapping.", "cttir_schema_error", "schema_validation")
  }
  check_tree(spec)
  original <- spec
  check_schema_version(spec, allow_older = TRUE)
  version <- spec_version_of(spec)
  current <- spec_schema_version()
  registry <- spec_migrations()
  steps <- list()
  while (version < current) {
    step <- registry[[as.character(version)]]
    if (!is.function(step)) {
      abort_cttir(paste0("No migration is defined from schema version ", version, "."),
        "cttir_schema_error", "no_migration_path", "/schema_version")
    }
    before <- spec
    after <- step(before)
    if (!is.list(after) || !identical(tryCatch(spec_version_of(after), error = function(e) NA), version + 1L)) {
      abort_cttir("A migration step must advance exactly one schema version.", "cttir_schema_error", "invalid_migration")
    }
    if (!identical(step(before), after) || !identical(before, spec)) {
      abort_cttir("Migration steps must be pure and deterministic.", "cttir_schema_error", "invalid_migration")
    }
    steps[[length(steps) + 1L]] <- list(
      from = version, to = version + 1L,
      before_sha256 = content_hash(json_text(before)), after_sha256 = content_hash(json_text(after)),
      changes = spec_changes(before, after)
    )
    spec <- after
    version <- version + 1L
  }
  list(spec = spec, steps = steps, original = original)
}
