r_distribution_source <- function(record) {
  scalar_text(record$package, "source package")
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", record$package)) {
    abort_cttir("Invalid distribution package identity.", "cttir_source_unavailable")
  }
  root <- record$r_distribution
  assert_plain_path(root)
  version_file <- file.path(root, "VERSION")
  version <- trimws(read_source_text(version_file))
  if (!grepl("^[0-9]+[.][0-9]+[.][0-9]+$", version)) {
    abort_cttir("Only exact released R distribution sources are supported.", "cttir_source_unavailable")
  }
  license_file <- file.path(root, "COPYING")
  read_source_text(license_file)
  path <- file.path(root, "src", "library", record$package)
  entry <- extract_source(path, paste0("local-source:", record$id), "local",
    "configured_r_distribution", record$documentation_rights, distribution_version = version)
  if (!identical(entry$version, version) || !identical(entry$name, record$package)) {
    abort_cttir("Distribution package identity or version does not match.", "cttir_source_unavailable")
  }
  entry$distribution <- list(name = "R", version = version,
    version_sha256 = digest::digest(file = version_file, algo = "sha256"),
    license_sha256 = digest::digest(file = license_file, algo = "sha256"),
    license_source = "COPYING",
    documentation_scope = "package_subtree_only_distribution_manuals_and_NEWS_not_indexed",
    description_source = "DESCRIPTION.in", substitution = "literal_VERSION_only",
    provenance = "user_registered_local_source_not_canonical_authentication")
  entry$source_hash <- content_hash(json_text(list(files = entry$source_files,
        distribution = entry$distribution)))
  entry
}
