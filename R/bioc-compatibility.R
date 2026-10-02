# Pinned Bioconductor release -> compatible R minor version table.
# Source: the official https://bioconductor.org/config.yaml (`r_ver_for_bioc_ver`,
# `release_version: "3.23"`, `devel_version: "3.24"`), retrieved 2026-10-02, and
# the release announcements at https://bioconductor.org/about/release-announcements/.
# Only released versions are listed; devel (3.24 at retrieval) and the mutable
# `release`/`devel` aliases are deliberately absent. Extend this table only after
# checking a new official release announcement.
bioc_release_table <- function() {
  data.frame(
    release = c("3.13", "3.14", "3.15", "3.16", "3.17", "3.18", "3.19", "3.20", "3.21", "3.22", "3.23"),
    r_minor = c("4.1", "4.1", "4.2", "4.2", "4.3", "4.3", "4.4", "4.4", "4.5", "4.5", "4.6"),
    stringsAsFactors = FALSE
  )
}

running_r_minor <- function() {
  paste(R.version$major, sub("[.].*$", "", R.version$minor), sep = ".")
}

validate_bioc_release <- function(x, field = "bioc_version") {
  scalar_text(x, field)
  table <- bioc_release_table()
  if (!x %in% table$release) {
    abort_cttir(
      paste0("Unknown Bioconductor release '", x, "'. Use an exact released version such as 3.23; aliases like 'release' or 'devel' are not accepted."),
      "cttir_input_error", "unknown_bioc_release", field,
      "Choose an exact release listed by cttiR's pinned Bioconductor compatibility table."
    )
  }
  invisible(x)
}

bioc_policy_file <- function(root = catalog_store()) {
  file <- file.path(root, "bioc-policy.json")
  assert_plain_path(file)
  file
}

read_bioc_policy <- function(root = catalog_store()) {
  file <- bioc_policy_file(root)
  if (!file.exists(file)) return(NULL)
  value <- read_document(file)
  if (!is.list(value) || !identical(value$schema_version, 1L) || !is.character(value$release) ||
      length(value$release) != 1L || !value$release %in% bioc_release_table()$release) {
    abort_cttir("The stored Bioconductor release policy is invalid.", "cttir_catalog_corrupt",
      "invalid_bioc_policy", "bioc-policy.json", "Inspect or remove the store policy file, then select an exact release.")
  }
  value
}

write_bioc_policy <- function(release, root = catalog_store()) {
  validate_bioc_release(release)
  file <- bioc_policy_file(root)
  staged <- tempfile("bioc-policy-", tmpdir = root, fileext = ".json")
  on.exit(unlink(staged), add = TRUE)
  table <- bioc_release_table()
  policy <- list(schema_version = 1L, release = release, r_minor = table$r_minor[table$release == release],
    recorded_at = utc_timestamp(), scope = "global_catalog_store_not_project_pins")
  write_bytes(paste0(json_text(policy), "\n"), staged)
  tryCatch(fs::file_move(staged, file), error = function(e) {
    abort_cttir("Could not record the Bioconductor release policy.", "cttir_transaction_conflict")
  })
  invisible(file)
}

utc_timestamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' Resolve the Bioconductor release used for metadata refresh
#'
#' Internal audit/router helper. Never contacts the network, never installs or
#' upgrades anything and never changes project pins.
#' @param bioc_version Optional exact release; `NULL` keeps the store policy or
#'   derives the latest release compatible with the running R minor version.
#' @param root Catalog store directory.
#' @return A list with `release`, `source`, `r_minor` (for the release),
#'   `running_r`, `compatible`, `stored` and `changes_policy`.
#' @noRd
bioc_release_policy <- function(bioc_version = NULL, root = catalog_store()) {
  if (!is.null(bioc_version)) validate_bioc_release(bioc_version)
  table <- bioc_release_table()
  running <- running_r_minor()
  stored <- read_bioc_policy(root)
  stored_release <- if (is.null(stored)) NA_character_ else stored$release
  if (!is.null(bioc_version)) {
    release <- bioc_version
    source <- "explicit"
  } else if (!is.na(stored_release)) {
    release <- stored_release
    source <- "store_policy"
  } else {
    candidates <- table$release[table$r_minor == running]
    release <- if (length(candidates)) candidates[[length(candidates)]] else NA_character_
    source <- if (length(candidates)) "derived_from_running_r" else "unresolved"
  }
  r_minor <- if (is.na(release)) NA_character_ else table$r_minor[table$release == release]
  list(
    release = release, source = source, r_minor = r_minor, running_r = running,
    compatible = !is.na(r_minor) && identical(r_minor, running),
    stored = stored_release,
    changes_policy = !is.null(bioc_version) && !identical(bioc_version, stored_release),
    evidence = "Pinned table from https://bioconductor.org/config.yaml retrieved 2026-10-02"
  )
}

require_bioc_release <- function(policy) {
  if (is.na(policy$release)) {
    abort_cttir(
      paste0("No known Bioconductor release is compatible with R ", policy$running_r, "."),
      "cttir_source_unavailable", "bioc_release_unresolved", "bioc_version",
      "Pass an exact bioc_version or record a store release policy."
    )
  }
  policy$release
}
