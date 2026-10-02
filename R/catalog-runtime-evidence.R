# Trusted installed-package inspection for exports that static parsing cannot
# resolve (for example functions built by a constructor at build time).
#
# Loading a namespace runs its load hooks, so this is used only for packages the
# caller explicitly trusts and has installed, in a separate R process. Evidence is
# recorded only when the installed version equals the cataloged revision's
# version; identical version strings do not prove identical source bytes, which
# the recorded limitation states. Static evidence is never overwritten.

installed_export_evidence <- function(package, version, names, timeout = 120) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    abort_cttir("Installed-package inspection requires the suggested callr package.", "cttir_source_unavailable")
  }
  scalar_text(package, "package")
  scalar_text(version, "version")
  if (!is.character(names) || anyNA(names) || !length(names)) abort_cttir("names must list exports to inspect.")
  callr::r(function(package, version, names) {
    installed <- tryCatch(as.character(utils::packageVersion(package)), error = function(e) NA_character_)
    if (!identical(installed, version)) {
      return(list(status = "version_mismatch", installed = installed, records = list()))
    }
    exports <- getNamespaceExports(package)
    records <- lapply(names, function(name) {
      if (!name %in% exports) return(list(name = name, status = "not_exported"))
      value <- getExportedValue(package, name)
      if (!is.function(value)) return(list(name = name, status = "not_function", class = class(value)))
      signature <- sub("\\s*NULL$", "", paste(deparse(args(value), width.cutoff = 500L), collapse = " "))
      list(name = name, status = "function", arguments = names(formals(value)), signature = signature)
    })
    list(status = "inspected", installed = installed, r_version = as.character(getRversion()), records = records)
  }, args = list(package, version, names), timeout = timeout)
}

# Apply inspection results to unresolved exports of one package record.
apply_installed_evidence <- function(entry, evidence) {
  if (!identical(evidence$status, "inspected")) return(entry)
  found <- stats::setNames(evidence$records, vapply(evidence$records, function(x) x$name, character(1)))
  entry$exports <- lapply(entry$exports, function(export) {
    record <- found[[export$name]]
    if (is.null(record) || !identical(record$status, "function") ||
        identical(export$verification, "static_api_verified") || identical(export$kind, "reexport")) {
      return(export)
    }
    export$kind <- "function"
    export$verification <- "installed_api_verified"
    export$signature <- record$signature
    export$arguments <- as.list(record$arguments)
    export$runtime_inspection <- list(installed_version = evidence$installed, r_version = evidence$r_version,
      method = "formals of the exported value in an isolated R process",
      limitation = "Matching version strings do not prove the installed build equals the indexed source bytes.")
    export
  })
  entry$coverage$installed_verified <- sum(vapply(entry$exports, function(x) identical(x$verification, "installed_api_verified"), logical(1)))
  entry
}
