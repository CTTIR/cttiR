# Remote resource observation refresh from the official CRAN and selected
# Bioconductor release PACKAGES indices. Curated package fields (purpose, tiers,
# notes, verification and adapter states) are never rewritten here. Nothing is
# installed and no downloaded code is executed.

refresh_managed_lifecycle <- c("listed_in_selected_repository", "missing_from_selected_index", "unavailable_observation")

resource_refresh_policy <- function() {
  x <- getOption("cttiR.resource_refresh", list())
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!names(x) %in% c("optional", "exclude"))))) {
    abort_cttir("cttiR.resource_refresh may only contain `optional` and `exclude`.", field = "cttiR.resource_refresh")
  }
  optional <- if (is.null(x$optional)) character() else x$optional
  if (!is.character(optional) || anyNA(optional) || any(!optional %in% c("cran", "bioconductor"))) {
    abort_cttir("Optional resource indices must be `cran` and/or `bioconductor`.", field = "cttiR.resource_refresh")
  }
  exclude <- if (is.null(x$exclude)) character() else x$exclude
  if (!is.character(exclude) || anyNA(exclude) || any(!grepl(paste0("^", repository_package_pattern, "$"), exclude))) {
    abort_cttir("Excluded resources must be exact package names.", field = "cttiR.resource_refresh")
  }
  list(optional = optional, exclude = exclude)
}

discovery_policy <- function() {
  x <- getOption("cttiR.discovery", list(
    keywords = c("tidy", "cytometry", "spatial", "proteom", "microbiom", "singlecell"),
    biocViews = c("SingleCell", "FlowCytometry", "Spatial", "Proteomics", "Microbiome"),
    limit = 25L
  ))
  if (!is.list(x) || is.null(names(x)) || any(!names(x) %in% c("keywords", "biocViews", "limit"))) {
    abort_cttir("cttiR.discovery may only contain keywords, biocViews and limit.", field = "cttiR.discovery")
  }
  terms <- function(value) {
    value <- if (is.null(value)) character() else value
    if (!is.character(value) || anyNA(value) || length(value) > 50L || any(!nzchar(trimws(value))) ||
        any(nchar(value) > 64L) || any(grepl("[[:cntrl:]]", value))) {
      abort_cttir("Discovery terms must be at most 50 short nonempty strings.", field = "cttiR.discovery")
    }
    value
  }
  limit <- if (is.null(x$limit)) 25L else x$limit
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || limit < 1 || limit > 100 || limit != as.integer(limit)) {
    abort_cttir("The discovery limit must be an integer between 1 and 100.", field = "cttiR.discovery")
  }
  out <- list(keywords = terms(x$keywords), biocViews = terms(x$biocViews), limit = as.integer(limit))
  if (!length(out$keywords) && !length(out$biocViews)) abort_cttir("Discovery needs at least one keyword or biocView.", field = "cttiR.discovery")
  out
}

parse_dependency_field <- function(value, role) {
  empty <- data.frame(role = character(), dependency = character(), version_constraint = character(), stringsAsFactors = FALSE)
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) return(empty)
  parts <- trimws(strsplit(gsub("[[:space:]]+", " ", value), ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  name <- trimws(sub("[(].*$", "", parts))
  constraint <- ifelse(grepl("[(]", parts), trimws(sub("^[^(]*[(]([^)]*)[)].*$", "\\1", parts)), NA_character_)
  constraint <- ifelse(is.na(constraint), NA_character_, sub("^(>=|<=|==|!=|>|<)[[:space:]]*", "\\1 ", constraint))
  keep <- grepl(paste0("^", repository_package_pattern, "$"), name) & !duplicated(name)
  if (!any(keep)) return(empty)
  data.frame(role = role, dependency = name[keep], version_constraint = constraint[keep], stringsAsFactors = FALSE)
}

stanza_dependencies <- function(stanza) {
  roles <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  do.call(rbind, lapply(roles, function(role) parse_dependency_field(if (role %in% names(stanza)) stanza[[role]] else NA, role)))
}

constraint_floor <- function(x) {
  if (is.null(x) || !length(x) || is.na(x) || !grepl("^>=? ", x)) return(NULL)
  version <- sub("^>=? ", "", x)
  tryCatch(numeric_version(version), error = function(e) NULL)
}

r_dependency_text <- function(deps) {
  row <- deps[deps$role == "Depends" & deps$dependency == "R", , drop = FALSE]
  if (!nrow(row)) return(NA_character_)
  if (is.na(row$version_constraint[[1]])) "R" else paste0("R (", row$version_constraint[[1]], ")")
}

r_floor <- function(text) {
  if (is.null(text) || !length(text) || is.na(text)) return(NULL)
  constraint_floor(trimws(sub("^R[[:space:]]*[(]?([^)]*)[)]?$", "\\1", text)))
}

floor_increased <- function(old, new) {
  !is.null(new) && (is.null(old) || new > old)
}

refresh_index_targets <- function(con, targets, exclude) {
  rows <- DBI::dbGetQuery(con, paste(
    "SELECT p.package_id, p.name, p.lifecycle_status, o.observation_id, o.repository, o.subrepository,",
    "o.bioconductor_release, o.observed_version, o.title, o.license, o.r_dependency, o.system_requirements,",
    "o.documentation_url, o.upstream_urls, o.source_sha256, o.fetch_status, o.freshness, o.observed_at",
    "FROM packages p JOIN observations o USING(package_id)",
    "WHERE o.repository = 'CRAN' OR (o.repository = 'Bioconductor' AND (o.subrepository IS NULL OR o.subrepository = 'bioc'))",
    "ORDER BY p.name, o.observation_id"
  ))
  rows <- rows[!rows$name %in% exclude, , drop = FALSE]
  if (!is.null(targets)) rows <- rows[rows$name %in% targets, , drop = FALSE]
  rows
}

observation_record <- function(package, stanza, index, prior, secondary = FALSE) {
  deps <- stanza_dependencies(stanza)
  bioc <- identical(index$repository, "Bioconductor")
  fields <- stanza[sort(names(stanza), method = "radix")]
  hash <- content_hash(json_text(list(repository = index$repository, release = index$release, fields = as.list(fields))))
  same_version <- !is.null(prior) && identical(prior$observed_version, stanza[["Version"]])
  pick <- function(field) if (same_version && !is.na(prior[[field]])) prior[[field]] else NA_character_
  value <- function(field) if (field %in% names(stanza)) stanza[[field]] else NA_character_
  list(
    observation_id = paste0(if (bioc) paste0("bioc-", index$release) else "cran", ":", package$package_id),
    package_id = package$package_id, repository = index$repository,
    subrepository = if (bioc) "bioc" else NA_character_,
    bioconductor_release = if (bioc) index$release else NA_character_,
    observed_version = stanza[["Version"]], title = pick("title"), license = value("License"),
    r_dependency = r_dependency_text(deps), needs_compilation = value("NeedsCompilation"),
    system_requirements = pick("system_requirements"), source_url = index$url,
    documentation_url = if (bioc) {
      paste0("https://bioconductor.org/packages/", index$release, "/bioc/html/", package$name, ".html")
    } else {
      paste0("https://cran.r-project.org/package=", package$name)
    },
    upstream_urls = if (!is.null(prior) && !is.na(prior$upstream_urls)) prior$upstream_urls else NA_character_,
    date_publication = value("Published"), observed_at = index$retrieved_at,
    fetch_status = if (secondary) "secondary_repository_listing" else "listed_in_selected_index",
    source_sha256 = hash, freshness = "remote_index_checked", dependencies = deps
  )
}

insert_observation <- function(con, record) {
  fields <- setdiff(names(record), "dependencies")
  marks <- paste(rep("?", length(fields)), collapse = ", ")
  sql <- paste0("INSERT INTO observations (", paste(fields, collapse = ", "), ") VALUES (", marks, ")")
  DBI::dbExecute(con, sql, params = unname(record[fields]))
}

empty_resource_changes <- function() {
  data.frame(package = character(), repository = character(), change = character(),
    before = character(), after = character(), impact = character(), stringsAsFactors = FALSE)
}

insert_dependencies <- function(con, package_id, deps) {
  if (!nrow(deps)) return(invisible(0L))
  DBI::dbExecute(con, "INSERT INTO dependencies (package_id, role, dependency, version_constraint) VALUES (?, ?, ?, ?)",
    params = list(rep(package_id, nrow(deps)), deps$role, deps$dependency, deps$version_constraint))
}

upsert_evidence <- function(con, id, package_id, url, observed_at, sha256, status) {
  old <- DBI::dbGetQuery(con, "SELECT status, sha256 FROM evidence WHERE evidence_id = ?", params = list(id))
  if (nrow(old) && identical(old$status[[1]], status) && identical(old$sha256[[1]], sha256)) return(FALSE)
  DBI::dbExecute(con, "DELETE FROM evidence WHERE evidence_id = ?", params = list(id))
  DBI::dbExecute(con, "INSERT INTO evidence (evidence_id, package_id, url, observed_at, scope, sha256, status) VALUES (?, ?, ?, ?, 'repository_index', ?, ?)",
    params = list(id, package_id, url, observed_at, sha256, status))
  TRUE
}

set_managed_lifecycle <- function(con, package, state) {
  if (!package$lifecycle_status %in% refresh_managed_lifecycle || identical(package$lifecycle_status, state)) return(FALSE)
  DBI::dbExecute(con, "UPDATE packages SET lifecycle_status = ? WHERE package_id = ?", params = list(state, package$package_id))
  TRUE
}

index_label <- function(index) {
  if (identical(index$repository, "CRAN")) "CRAN" else paste("Bioconductor", index$release)
}

refresh_repository_resources <- function(file, context, release, targets = NULL, discovery = NULL,
  policy = resource_refresh_policy()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA foreign_keys=ON")
  log <- new.env(parent = emptyenv())
  log$changes <- empty_resource_changes()
  add_change <- function(package, repository, change, before, after, impact) {
    text <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)
    log$changes[nrow(log$changes) + 1L, ] <- list(package, repository, change, text(before), text(after), impact)
  }
  rows <- refresh_index_targets(con, targets, policy$exclude)
  needed <- unique(ifelse(rows$repository == "CRAN", "cran", "bioconductor"))
  if (!is.null(discovery)) needed <- c("cran", "bioconductor")
  if (!length(needed)) return(list(changes = log$changes, partial = FALSE, unavailable = character()))
  if ("bioconductor" %in% needed) require_bioc_release(list(release = release, running_r = running_r_minor()))
  indices <- list()
  log$unavailable <- character()
  for (family in c("cran", "bioconductor")) {
    if (!family %in% needed) next
    repository <- if (family == "cran") "CRAN" else "Bioconductor"
    indices[[family]] <- tryCatch(repository_index(context, repository, if (family == "cran") NULL else release),
      cttir_source_unavailable = function(e) {
        if (!family %in% policy$optional) stop(e)
        log$unavailable <- c(log$unavailable, family)
        NULL
      })
  }
  stale_sql <- paste("UPDATE observations SET fetch_status = 'unavailable_observation', freshness = 'source_unavailable'",
    "WHERE observation_id = ? AND (fetch_status != 'unavailable_observation' OR freshness != 'source_unavailable')")
  missing_sql <- paste("UPDATE observations SET fetch_status = 'missing_from_selected_index', freshness = 'missing_from_selected_index'",
    "WHERE observation_id = ? AND fetch_status != 'missing_from_selected_index'")
  DBI::dbWithTransaction(con, {
    packages <- unique(rows[, c("package_id", "name", "lifecycle_status")])
    for (i in seq_len(nrow(packages))) {
      package <- packages[i, ]
      mine <- rows[rows$package_id == package$package_id, , drop = FALSE]
      primary <- mine[mine$fetch_status != "secondary_repository_listing", , drop = FALSE]
      families <- unique(ifelse(primary$repository == "CRAN", "cran", "bioconductor"))
      for (family in families) {
        index <- indices[[family]]
        repository <- if (family == "cran") "CRAN" else "Bioconductor"
        same <- primary[primary$repository == repository & (repository == "CRAN" | primary$bioconductor_release %in% release), , drop = FALSE]
        anyrel <- primary[primary$repository == repository, , drop = FALSE]
        prior <- if (nrow(same)) same[nrow(same), ] else if (nrow(anyrel)) anyrel[nrow(anyrel), ] else NULL
        label <- if (family == "cran") "CRAN" else paste("Bioconductor", release)
        key <- if (family == "cran") "cran" else paste0("bioc-", release)
        if (is.null(index)) {
          url <- repository_index_url(repository, if (family == "cran") NULL else release)
          changed <- FALSE
          for (id in same$observation_id) {
            changed <- DBI::dbExecute(con, stale_sql, params = list(id)) > 0 || changed
          }
          changed <- upsert_evidence(con, paste0("index:", key, ":", package$package_id), package$package_id, url,
            utc_timestamp(), NA_character_, "unavailable_observation") || changed
          if (changed) add_change(package$name, label, "unavailable_observation", prior$observed_version, NULL,
            "Optional index unavailable; the previous observation is retained and marked stale, not retired.")
          next
        }
        stanza <- index_stanza(index, package$name)
        if (is.null(stanza)) {
          changed <- FALSE
          for (id in same$observation_id) {
            changed <- DBI::dbExecute(con, missing_sql, params = list(id)) > 0 || changed
          }
          changed <- upsert_evidence(con, paste0("index:", key, ":", package$package_id), package$package_id, index$url,
            index$retrieved_at, NA_character_, "missing_from_selected_index") || changed
          if (set_managed_lifecycle(con, package, "missing_from_selected_index")) {
            package$lifecycle_status <- "missing_from_selected_index"
            changed <- TRUE
          }
          if (changed) add_change(package$name, label, "missing_from_selected_index", prior$observed_version, NULL,
            "Absent from the selected index; this is lifecycle evidence, not proof of retirement.")
          next
        }
        record <- observation_record(package, stanza, index, prior)
        current <- same[same$observation_id == record$observation_id, , drop = FALSE]
        lifecycle <- set_managed_lifecycle(con, package, "listed_in_selected_repository")
        if (lifecycle) package$lifecycle_status <- "listed_in_selected_repository"
        if (nrow(same) == 1L && nrow(current) == 1L && identical(current$source_sha256, record$source_sha256) &&
            identical(current$fetch_status, "listed_in_selected_index")) {
          if (lifecycle) add_change(package$name, label, "listed_in_selected_index", NULL, record$observed_version, "Listed again in the selected index.")
          next
        }
        old_deps <- DBI::dbGetQuery(con, "SELECT role, dependency, version_constraint FROM dependencies WHERE package_id = ?",
          params = list(package$package_id))
        if (!is.null(prior)) {
          if (!identical(prior$observed_version, record$observed_version)) {
            add_change(package$name, label, "version_changed", prior$observed_version, record$observed_version,
              "Observed upstream version changed; existing project pins and installed packages are unchanged.")
          }
          if (!identical(prior$license, record$license)) {
            add_change(package$name, label, "license_changed", prior$license, record$license,
              "License changed; review redistribution and documentation rights before relying on this package.")
          }
          if (floor_increased(r_floor(prior$r_dependency), r_floor(record$r_dependency))) {
            add_change(package$name, label, "dependency_floor_increased", prior$r_dependency, record$r_dependency,
              "The R version floor increased; check compatibility with pinned projects.")
          }
        } else {
          add_change(package$name, label, "new_observation_set", NULL, record$observed_version,
            "First observation from this repository index or release.")
        }
        for (j in seq_len(nrow(record$dependencies))) {
          dep <- record$dependencies[j, ]
          if (dep$dependency == "R") next
          old <- old_deps[old_deps$role == dep$role & old_deps$dependency == dep$dependency, , drop = FALSE]
          if (nrow(old) && floor_increased(constraint_floor(old$version_constraint[[1]]), constraint_floor(dep$version_constraint))) {
            add_change(package$name, label, "dependency_floor_increased",
              paste(dep$dependency, if (is.na(old$version_constraint[[1]])) "(no version floor)" else old$version_constraint[[1]]),
              paste(dep$dependency, dep$version_constraint),
              "A dependency version floor increased; adapter verification must be repeated for this revision.")
          }
        }
        if (nrow(same)) {
          DBI::dbExecute(con, paste0("DELETE FROM observations WHERE observation_id IN (", paste(rep("?", nrow(same)), collapse = ", "), ")"),
            params = as.list(same$observation_id))
        }
        DBI::dbExecute(con, "DELETE FROM observations WHERE observation_id = ?", params = list(record$observation_id))
        insert_observation(con, record)
        if (identical(family, families[[1]])) {
          DBI::dbExecute(con, "DELETE FROM dependencies WHERE package_id = ?", params = list(package$package_id))
          insert_dependencies(con, package$package_id, record$dependencies)
        }
        upsert_evidence(con, paste0("index:", key, ":", package$package_id), package$package_id, index$url,
          index$retrieved_at, record$source_sha256, "listed_in_selected_index")
        if (!any(log$changes$package == package$name & log$changes$repository == label)) {
          add_change(package$name, label, "observation_refreshed", prior$observed_version, record$observed_version,
            "Repository metadata refreshed; curated fields are unchanged.")
        }
      }
      # Cross-repository conflicts are stored as separate observations, never merged.
      if (!is.null(indices$cran) && !is.null(indices$bioconductor) && length(families) == 1L) {
        other <- if (families == "cran") indices$bioconductor else indices$cran
        mine_index <- if (families == "cran") indices$cran else indices$bioconductor
        stanza <- index_stanza(other, package$name)
        primary_stanza <- index_stanza(mine_index, package$name)
        if (!is.null(stanza)) {
          record <- observation_record(package, stanza, other, NULL, secondary = TRUE)
          existing <- mine[mine$observation_id == record$observation_id, , drop = FALSE]
          if (!(nrow(existing) == 1L && identical(existing$source_sha256, record$source_sha256))) {
            DBI::dbExecute(con, "DELETE FROM observations WHERE observation_id = ?", params = list(record$observation_id))
            insert_observation(con, record)
          }
          if (!is.null(primary_stanza) && !identical(primary_stanza[["Version"]], stanza[["Version"]])) {
            add_change(package$name, paste(index_label(mine_index), "/", index_label(other)), "repository_conflict",
              primary_stanza[["Version"]], stanza[["Version"]],
              "The package is listed in both repositories with different versions; both observations are kept.")
          }
        }
      }
    }
    if (!is.null(discovery)) {
      found <- discover_candidates(con, indices, discovery, policy$exclude)
      for (candidate in found$selected) {
        DBI::dbExecute(con, paste(
          "INSERT INTO packages (package_id, name, category, purpose, selection_tier, tidy_alignment, interop_expectation,",
          "interop_verification, metadata_verification, api_verification, adapter_status, lifecycle_status, default_install,",
          "eligible_after_adapter_validation, notes_json, documentation_url) VALUES (?, ?, 'unreviewed_discovery', ?,",
          "'discovered_candidate', 'unreviewed', 'unreviewed', 'not_reviewed', 'repository_index_listed',",
          "'not_function_level_verified', 'not_implemented', 'listed_in_selected_repository', 0, 0, ?, ?)"
        ), params = list(candidate$package_id, candidate$name,
          paste0("Quarantined discovery candidate matching '", candidate$term, "' in the ", index_label(candidate$index), " index; not reviewed."),
          json_text(list("Discovered by bounded metadata matching. Requires curator review; never promoted, installed or used by generated code automatically.")),
          if (identical(candidate$index$repository, "CRAN")) paste0("https://cran.r-project.org/package=", candidate$name) else
            paste0("https://bioconductor.org/packages/", candidate$index$release, "/bioc/html/", candidate$name, ".html")))
        package <- list(package_id = candidate$package_id, name = candidate$name, lifecycle_status = "listed_in_selected_repository")
        record <- observation_record(package, candidate$stanza, candidate$index, NULL)
        insert_observation(con, record)
        insert_dependencies(con, candidate$package_id, record$dependencies)
        key <- if (identical(candidate$index$repository, "CRAN")) "cran" else paste0("bioc-", candidate$index$release)
        upsert_evidence(con, paste0("index:", key, ":", candidate$package_id), candidate$package_id, candidate$index$url,
          candidate$index$retrieved_at, record$source_sha256, "discovered_candidate")
        add_change(candidate$name, index_label(candidate$index), "discovered_candidate", NULL, record$observed_version,
          "Quarantined candidate; API, interoperability and adapter status remain unverified.")
      }
      if (found$truncated > 0L) {
        add_change(NA_character_, "discovery", "discovery_limit_reached", found$matched, discovery$limit,
          paste0(found$truncated, " further matching candidates were not added because of the per-run limit."))
      }
    }
  })
  if (!identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]], "ok") || nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check"))) {
    abort_cttir("Candidate resource integrity checks failed.", "cttir_catalog_corrupt")
  }
  list(changes = log$changes, partial = length(log$unavailable) > 0L, unavailable = log$unavailable)
}

discover_candidates <- function(con, indices, discovery, exclude) {
  known <- DBI::dbGetQuery(con, "SELECT name, package_id FROM packages")
  matches <- list()
  for (index in Filter(Negate(is.null), list(indices$cran, indices$bioconductor))) {
    fields <- index$fields
    names <- fields[, "Package"]
    title <- if ("Title" %in% colnames(fields)) ifelse(is.na(fields[, "Title"]), "", fields[, "Title"]) else rep("", length(names))
    text <- tolower(paste(names, title))
    term <- rep(NA_character_, length(names))
    for (keyword in rev(discovery$keywords)) term[grepl(tolower(keyword), text, fixed = TRUE)] <- keyword
    if ("biocViews" %in% colnames(fields) && length(discovery$biocViews)) {
      views <- strsplit(tolower(ifelse(is.na(fields[, "biocViews"]), "", fields[, "biocViews"])), "[[:space:]]*,[[:space:]]*")
      for (view in rev(discovery$biocViews)) {
        hit <- is.na(term) & vapply(views, function(x) tolower(view) %in% trimws(x), logical(1))
        term[hit] <- view
      }
    }
    rows <- which(!is.na(term) & !names %in% known$name & !paste0("r:", names) %in% known$package_id & !names %in% exclude)
    rows <- rows[order(names[rows], method = "radix")]
    for (row in rows) {
      name <- names[[row]]
      if (any(vapply(matches, function(x) identical(x$name, name), logical(1)))) next
      matches[[length(matches) + 1L]] <- list(name = name, package_id = paste0("r:", name), term = term[[row]],
        stanza = index_stanza(index, name), index = index)
    }
  }
  selected <- utils::head(matches, discovery$limit)
  list(selected = selected, matched = length(matches), truncated = max(0L, length(matches) - length(selected)))
}

index_reports <- function(context, optional = character()) {
  lapply(unname(context$indices), function(x) {
    family <- if (identical(x$repository, "CRAN")) "cran" else "bioconductor"
    list(repository = x$repository, release = x$release, url = x$url, status = x$status,
      optional = family %in% optional, bytes = if (is.null(x$bytes)) NA_real_ else x$bytes,
      sha256 = if (is.null(x$sha256)) NA_character_ else x$sha256, retrieved_at = x$retrieved_at,
      last_modified = if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      etag = if (is.null(x$etag)) NA_character_ else x$etag,
      malformed_rows = if (is.null(x$malformed_rows)) NA_integer_ else x$malformed_rows,
      message = if (is.null(x$message)) NA_character_ else x$message)
  })
}
