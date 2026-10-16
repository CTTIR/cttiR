# Read-only profile audit: no dependency loading, data access, render or Git action.
audit_workflowr_bundle <- function(p) {
  preset <- p$spec$workflow$reporting_preset
  if (is.null(preset)) preset <- "standard"
  expected <- workflowr_profile_files(preset, p$spec$workflow$pipeline)
  bundled <- read_document(resource_file("templates", "workflowr-0.4.0", "manifest.json"))
  manifest_file <- file.path(p$path, "metadata/workflow-template.json")
  manifest <- if (file.exists(manifest_file)) read_document(manifest_file) else list()
  matches <- identical(json_text(manifest), json_text(bundled))
  protected <- c(names(bundled$files), "research.Rproj", "_workflowr.yml", "analysis/_site.yml",
    "config/report-pages.json", "docs/.nojekyll")
  protected <- intersect(protected, names(expected))
  edited <- protected[vapply(protected, function(path) {
    !identical(file_hash(file.path(p$path, path)), content_hash(expected[[path]]))
  }, logical(1))]
  pages <- jsonlite::fromJSON(expected[["config/report-pages.json"]])$pages
  missing <- pages[!vapply(pages, function(path) {
    file <- file.path(p$path, path)
    isTRUE(file.exists(file) && !dir.exists(file) && !nzchar(Sys.readlink(file)))
  }, logical(1))]
  roots <- list.files(p$path, pattern = "[.]Rproj$", all.files = TRUE)
  git <- file.exists(file.path(p$path, ".git"))
  baseline <- stats::setNames(vapply(p$manifest$files, function(x) x$baseline_sha256, ""),
    vapply(p$manifest$files, function(x) x$path, ""))
  code <- grep("^(code/|_targets[.]R$)", names(baseline), value = TRUE)
  edited <- unique(c(edited, code[vapply(code, function(path) {
    !identical(file_hash(file.path(p$path, path)), unname(baseline[path]))
  }, logical(1))]))
  evidence <- list(mode = manifest$mode, initializer_invoked = manifest$initializer_invoked_during_project_creation,
    source_revision = manifest$reflowR_revision, manifest_matches_bundle = matches,
    edited_code = as.list(edited), missing_pages = as.list(missing), one_root = identical(roots, "research.Rproj"),
    git_present = git, preset = preset, rendering = "not_executed_by_audit")
  if (!matches || !identical(roots, "research.Rproj") || !git || length(missing)) {
    return(audit_result("fail", "Workflowr provenance, root structure or declared report pages differ from the selected profile.", evidence))
  }
  if (length(edited)) {
    return(audit_result("fail", "Workflowr code or managed report configuration differs from the reviewed profile.", evidence))
  }
  audit_result("pass", "Workflowr profile identities and root structure agree; this does not establish rendering or scientific validity.", evidence)
}
