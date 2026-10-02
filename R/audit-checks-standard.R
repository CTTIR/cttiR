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

audit_project_code <- function(root) {
  files <- c(list.files(file.path(root, "code"), "\\.R$", recursive = TRUE, full.names = TRUE),
    if (file.exists(file.path(root, "_targets.R"))) file.path(root, "_targets.R"),
    list.files(file.path(root, "analysis"), "\\.Rmd$", full.names = TRUE))
  texts <- lapply(files, function(file) {
    assert_plain_path(file)
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    if (grepl("[.]Rmd$", file)) {
      inside <- FALSE
      keep <- character()
      for (line in lines) {
        if (grepl("^```\\{r", line)) {
          inside <- TRUE
        } else if (grepl("^```", line)) {
          inside <- FALSE
        } else if (inside) {
          keep <- c(keep, line)
        }
      }
      lines <- keep
    }
    lines
  })
  stats::setNames(texts, substring(files, nchar(root) + 2L))
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
    rows <- list()
    for (file in names(texts <- audit_project_code(p$path))) {
      result <- tryCatch(validate_generated_code(texts[[file]], catalog), error = function(e) NULL)
      if (is.null(result)) next
      namespaced <- result[!is.na(result$package) & result$package != "base", , drop = FALSE]
      if (nrow(namespaced)) rows[[file]] <- cbind(file = file, namespaced, stringsAsFactors = FALSE)
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
    evidence <- list(namespaced_calls = nrow(calls), unapproved = lapply(seq_len(nrow(unapproved)), function(i) {
      paste0(unapproved$file[[i]], ": ", unapproved$package[[i]], "::", unapproved$export[[i]], " (", unapproved$reason[[i]], ")")
    }), builder_self_checks = sum(self_check), dependencies = length(pins), pins_consistent = all(pins))
    if (nrow(unapproved) || !all(pins)) {
      return(audit_result("fail", "Project code calls APIs without an approval of the pinned revision, or dependency pins differ.", evidence))
    }
    audit_result("pass", paste0(nrow(calls), " namespaced calls are covered by approvals of the pinned catalog revision."), evidence)
  })
}

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
    receipt <- read_receipt(p, "reports/workflow/receipt.json")
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
  message <- paste0(prefix, complete, " approvals across ", packages, " package revisions cover their required callables, ",
    "topics and documents. Their corpora store ", evidence$documentation$reference_topics_stored, " of ",
    evidence$documentation$reference_topics_indexed, " reference topics and ", evidence$documentation$vignette_files_stored,
    " of ", evidence$documentation$vignette_files_indexed, " vignette files as text; the rest are indexed by hash only.")
  audit_result(if (is.null(pin_note)) "pass" else "warning", message, evidence)
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
    project_check("STD-003", "Every namespaced call in project code is approved for the pinned revision; dependency pins agree.",
      audit_std_apis, required = TRUE, read_effects = c("reads_project_metadata", "reads_catalog_store", "reads_installation"),
      evidence_schema = c("namespaced_calls", "unapproved", "builder_self_checks", "dependencies", "pins_consistent")),
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
    audit_check("RES-005", "project", "The project's pinned resource snapshot is retained.",
      audit_res_pins, required = TRUE, applies = audit_has_path, read_effects = c("reads_project_metadata", "reads_catalog_store"),
      evidence_schema = "resource_id"),
    audit_check("RES-006", "knowledge", "Tested class, method and conversion capabilities carry approved fixture evidence.",
      audit_res_interop, required = FALSE, read_effects = c("reads_installation", "reads_catalog_store"),
      evidence_schema = c("adapter_tested", "without_approval"))
  )
}
