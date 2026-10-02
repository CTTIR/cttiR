# Build the bundled knowledge catalog with the standard-workflow package family
# and reviewed revision approvals.
#
#   Rscript tools/build_standard_catalog.R fetch      # verified repository fetch + static extraction
#   Rscript tools/build_standard_catalog.R build      # fixtures, approvals, compact corpus, catalog
#   Rscript tools/build_standard_catalog.R approvals  # re-scope approvals.json only
#
# Run from the package root. `fetch` downloads public source archives through the
# package's own bounded repository backends (CRAN/Bioconductor MD5 checks; archived
# versions pinned to the MD5 of the earlier acquisition) and reads the local R 4.6.1
# distribution tree under admin/standard-sources. No source code is executed.
# `build` runs the adapter fixture tests, writes inst/extdata/approvals.json and the
# catalog, and keeps the previous bundled catalog under inst/extdata/history.
# `approvals` works offline: it re-derives the decisions from the bundled catalog's
# package records and the fixture results already recorded in approvals.json, and
# rewrites only approvals.json; the bundled catalog and its content ID are unchanged.
# CTTIR_BUILD_ARTIFACTS overrides the evidence directory (default artifacts/implementation).
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, args %in% c("fetch", "build", "approvals"), file.exists("DESCRIPTION"))
pkgload::load_all(quiet = TRUE)
ns <- asNamespace("cttiR")
evidence_dir <- Sys.getenv("CTTIR_BUILD_ARTIFACTS", file.path("artifacts", "implementation"))
cache_file <- file.path(evidence_dir, "standard-entries.rds")
sources_root <- file.path("admin", "standard-sources")

cran <- c(DescrTab2 = "2.1.16", dplyr = "1.2.1", tidyr = "1.3.2", tibble = "3.3.1", readr = "2.2.0",
  ggplot2 = "4.0.3", patchwork = "1.3.2", viridisLite = "0.4.3", RColorBrewer = "1.1-3", colorspace = "2.1-2",
  broom = "1.0.13", broom.mixed = "0.2.9.7", generics = "0.1.4", yaml = "2.3.12", jsonlite = "2.0.0",
  rmarkdown = "2.31", knitr = "1.51", targets = "1.12.0", renv = "1.2.3", nlme = "3.1-170",
  survival = "3.8-9", Matrix = "1.7-6", Seurat = "5.5.1", SeuratObject = "5.4.0")
bioc <- c(SummarizedExperiment = "1.42.0", SingleCellExperiment = "1.34.0")
bioc_archive <- c(S4Vectors = "0.50.2")
distribution <- c("stats", "utils", "grDevices", "tools", "methods")
family <- function(name) if (name %in% c("DescrTab2")) "imbi" else "standard"

rights <- function(license) {
  paste0("Upstream license '", license, "' (DESCRIPTION) permits redistributing unmodified documentation ",
    "with its notice; stored literally for revision-aligned retrieval, never rendered or executed.")
}

if (identical(args, "fetch")) {
  context <- ns$new_fetch_context(tempfile("cttir-standard-fetch-"), ns$bioc_release_policy("3.23", tempfile()), budget = 3600)
  entries <- list()
  for (name in names(cran)) {
    version <- cran[[name]]
    local <- file.path(sources_root, paste0(name, "_", version, ".tar.gz"))
    md5 <- if (file.exists(local)) unname(tools::md5sum(local)) else NULL
    record <- list(id = paste0("cran-", name), cran = name, version = version, md5 = md5)
    first <- ns$cran_source(record, context)
    record$documentation_rights <- rights(first$license)
    entries[[name]] <- ns$cran_source(record, context)
    cat(sprintf("%-22s %-9s %s\n", name, version, entries[[name]]$revision))
  }
  for (name in names(bioc)) {
    record <- list(id = paste0("bioc-", name), bioc = name, bioc_version = "3.23", version = bioc[[name]])
    first <- ns$bioc_source(record, context)
    record$documentation_rights <- rights(first$license)
    entries[[name]] <- ns$bioc_source(record, context)
    cat(sprintf("%-22s %-9s %s\n", name, bioc[[name]], entries[[name]]$revision))
  }
  for (name in names(bioc_archive)) {
    record <- list(id = paste0("bioc-", name), bioc = name, bioc_version = "3.23", version = bioc_archive[[name]])
    # No checksum is published for archived Bioconductor versions; the entry is
    # labelled accordingly rather than pinned to a self-computed MD5.
    first <- ns$bioc_source(record, context)
    record$documentation_rights <- rights(first$license)
    entries[[name]] <- ns$bioc_source(record, context)
    cat(sprintf("%-22s %-9s %s\n", name, bioc_archive[[name]], entries[[name]]$revision))
  }
  for (name in distribution) {
    record <- list(id = paste0("r-4.6.1-", name), package = name, r_distribution = file.path(sources_root, "R-4.6.1"),
      documentation_rights = rights("Part of R (GPL-2 | GPL-3)"))
    entry <- ns$r_distribution_source(record)
    entry$revision <- paste0("r-distribution:R-4.6.1:", name)
    entry$repository <- "https://cran.r-project.org/src/base/R-4/R-4.6.1.tar.gz"
    entries[[name]] <- entry
    cat(sprintf("%-22s %-9s %s\n", name, entry$version, entry$revision))
  }
  for (name in names(entries)) {
    entries[[name]]$family <- family(name)
    entries[[name]]$freshness <- "observed_at_build"
  }
  saveRDS(list(entries = entries, requests = context$calls, built_at = format(Sys.time(), tz = "UTC", usetz = TRUE)), cache_file)
  cat("Saved", length(entries), "entries to", cache_file, "\n")
}

# ---------------------------------------------------------------------------
# build: fixtures, approvals, compact corpus and catalog

template_root <- file.path("inst", "templates", "standard-0.3.0")

empty_usage <- function() data.frame(package = character(), name = character(), use = character(), stringsAsFactors = FALSE)

# Namespaced references (`pkg::name` called, or used as a value) in one expression.
namespaced_usage <- function(expr) {
  rows <- list()
  walk <- function(e) {
    if (is.pairlist(e)) {
      parts <- as.list(e)
      for (i in seq_along(parts)) if (!identical(unname(parts[i]), unname(alist(x = )))) walk(parts[[i]])
      return(invisible(NULL))
    }
    if (!is.call(e)) return(invisible(NULL))
    head <- e[[1]]
    if (is.call(head) && identical(head[[1]], as.name("::"))) {
      rows[[length(rows) + 1L]] <<- c(as.character(head[[2]]), as.character(head[[3]]), "call")
    } else if (identical(head, as.name("::"))) {
      rows[[length(rows) + 1L]] <<- c(as.character(e[[2]]), as.character(e[[3]]), "value")
    }
    for (i in seq_along(e)[-1]) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  }
  walk(expr)
  if (!length(rows)) return(empty_usage())
  usage <- unique(as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE))
  names(usage) <- c("package", "name", "use")
  usage
}

template_code <- function(file) {
  text <- readLines(file, warn = FALSE)
  if (grepl("Rmd$", file)) {
    inside <- FALSE
    keep <- character()
    for (line in text) {
      if (grepl("^```\\{r", line)) {
        inside <- TRUE
      } else if (grepl("^```", line)) {
        inside <- FALSE
      } else if (inside) {
        keep <- c(keep, line)
      }
    }
    text <- keep
  }
  parse(text = text, keep.source = FALSE)
}

# One unit per top-level template function, plus the remaining top-level code of
# each file as `<file>path`: its namespaced usage and every name it references.
template_units <- function(root = template_root) {
  units <- list()
  for (file in list.files(root, pattern = "\\.(R|Rmd)$", recursive = TRUE, full.names = TRUE)) {
    top <- list()
    for (e in template_code(file)) {
      if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && is.call(e[[3]]) &&
          identical(e[[3]][[1]], as.name("function"))) {
        units[[as.character(e[[2]])]] <- list(usage = namespaced_usage(e[[3]]), names = all.names(e[[3]]))
      } else {
        top[[length(top) + 1L]] <- e
      }
    }
    units[[paste0("<file>", substring(file, nchar(root) + 2L))]] <- list(
      usage = namespaced_usage(as.call(c(as.name("{"), top))), names = as.character(unlist(lapply(top, all.names))))
  }
  units
}

analysis_units <- function(units) grep("^<file>analysis/", names(units), value = TRUE)

# Units reachable from `start` through the names they reference. cw_run() only
# sequences the stages, so a role never inherits every stage through it.
unit_closure <- function(units, start, stop = "cw_run") {
  seen <- character()
  queue <- intersect(start, names(units))
  while (length(queue)) {
    unit <- queue[[1]]
    queue <- queue[-1]
    if (unit %in% seen) next
    seen <- c(seen, unit)
    queue <- c(queue, setdiff(intersect(units[[unit]]$names, names(units)), c(seen, stop)))
  }
  seen
}

closure_usage <- function(units, members) {
  unique(do.call(rbind, c(list(empty_usage()), lapply(members, function(u) units[[u]]$usage))))
}

# A reexport is approved through its owner, so the owner's decision must list it.
with_reexport_owners <- function(usage, entries) {
  for (i in seq_len(nrow(usage))) {
    entry <- entries[[usage$package[[i]]]]
    hit <- if (is.null(entry)) list() else Filter(function(x) identical(x$name, usage$name[[i]]) && identical(x$kind, "reexport"), entry$exports)
    while (length(hit)) {
      owner <- hit[[1]]$owner_package
      usage[nrow(usage) + 1L, ] <- list(owner, usage$name[[i]], usage$use[[i]])
      hit <- Filter(function(x) identical(x$name, usage$name[[i]]) && identical(x$kind, "reexport"), entries[[owner]]$exports)
    }
  }
  unique(usage)
}

template_usage <- function(units, entries) with_reexport_owners(closure_usage(units, names(units)), entries)

# Template entry points of each tested adapter; a role approves only what its own
# code reaches.
role_roots <- function(units) {
  report <- c("<file>code/render_report.R", analysis_units(units))
  model <- c("cw_model", "cw_diagnose")
  list(
    standard.reflowr_layout = report, standard.report_render = report,
    standard.import_delimited = c("cw_import", "cw_config"),
    standard.check_mapped = c("cw_check", "cw_requirements"),
    standard.tidy_roles = "cw_tidy",
    standard.describe_descrtab2 = c("cw_describe", "cw_write_csv"),
    standard.describe_base = c("cw_describe", "cw_write_csv"),
    standard.figures_accessible = c("cw_figures", grep("^cf_", names(units), value = TRUE)),
    standard.model_lm = model, standard.model_glm_binomial = model, standard.model_lme = model, standard.model_coxph = model,
    standard.effects_broom = "cw_effects",
    standard.demo_synthetic = c("<file>code/run_demo.R", "<file>analysis/synthetic_demo.Rmd", "cw_run_demo"),
    standard.pipeline_targets = "<file>_targets.R",
    interop.bioc_s4 = c("ci_validate_s4", "ci_profile", "ci_conversion_report"),
    interop.se_tidy_view = "ci_se_tidy_view",
    interop.seurat_v5 = c("ci_convert", "ci_seurat_layers", "ci_pseudobulk")
  )
}

# cw_model() and cw_diagnose() branch on the engine inside switch() and if(), which
# a static scan cannot separate. These reviewed lists keep each model role to its
# own branch; together they must cover every namespaced call of those functions
# (survival::Surv is built with call() in cw_formula()).
engine_calls <- list(
  standard.model_lm = c("stats::as.formula", "stats::model.matrix", "stats::lm", "stats::na.fail", "stats::coef",
    "stats::residuals", "stats::cooks.distance"),
  standard.model_glm_binomial = c("stats::as.formula", "stats::model.matrix", "stats::glm", "stats::binomial",
    "stats::na.fail", "stats::coef", "stats::fitted"),
  standard.model_lme = c("stats::as.formula", "stats::model.matrix", "nlme::lme", "stats::na.fail", "nlme::fixef",
    "nlme::getVarCov"),
  standard.model_coxph = c("stats::as.formula", "survival::Surv", "survival::coxph", "stats::na.fail", "stats::coef",
    "survival::cox.zph")
)
engine_classes <- list(standard.model_lm = "lm", standard.model_glm_binomial = "glm", standard.model_lme = "lme",
  standard.model_coxph = "coxph")

# Calls a template function builds with call() instead of writing them out, so a
# static scan cannot see them; every role reaching the function needs them.
constructed_calls <- list(cw_formula = "survival::Surv")

# Namespaced calls and referenced names of one adapter role.
role_usage <- function(adapter, units, entries) {
  members <- unit_closure(units, role_roots(units)[[adapter]])
  if (!length(members)) stop("Adapter ", adapter, " has no template entry point.")
  usage <- with_reexport_owners(closure_usage(units, members), entries)
  key <- paste(usage$package, usage$name, sep = "::")
  for (call in setdiff(unlist(constructed_calls[intersect(names(constructed_calls), members)]), key)) {
    usage[nrow(usage) + 1L, ] <- c(strsplit(call, "::", fixed = TRUE)[[1]], "call")
  }
  names <- unique(as.character(unlist(lapply(members, function(u) units[[u]]$names))))
  calls <- engine_calls[[adapter]]
  if (!is.null(calls)) {
    key <- paste(usage$package, usage$name, sep = "::")
    stray <- setdiff(key, unlist(engine_calls))
    if (length(stray)) stop("Model calls without a reviewed engine: ", paste(stray, collapse = ", "))
    usage <- usage[key %in% calls, , drop = FALSE]
    for (call in setdiff(calls, key)) usage[nrow(usage) + 1L, ] <- c(strsplit(call, "::", fixed = TRUE)[[1]], "call")
    names <- sub("^.*::", "", calls)
  }
  list(usage = unique(usage), names = names)
}

# Template code each fixture executes: template functions it calls by name, the
# scripts it runs, and its own namespaced calls.
fixture_usage <- function(test_file, test_name, units, entries) {
  body <- NULL
  for (e in parse(test_file, keep.source = FALSE)) {
    if (is.call(e) && identical(e[[1]], as.name("test_that")) && identical(e[[2]], test_name)) body <- e[[3]]
  }
  if (is.null(body)) stop("Fixture not found: ", test_file, " / ", test_name)
  text <- paste(deparse(body), collapse = "\n")
  start <- intersect(all.names(body), names(units))
  scripts <- c("run_demo.R" = "<file>code/run_demo.R", "run_workflow.R" = "<file>code/run_workflow.R",
    "render_report.R" = "<file>code/render_report.R", tar_make = "<file>_targets.R")
  for (pattern in names(scripts)) if (grepl(pattern, text, fixed = TRUE)) start <- c(start, scripts[[pattern]])
  # render_site() renders every analysis page.
  if ("<file>code/render_report.R" %in% start) start <- c(start, analysis_units(units))
  usage <- rbind(namespaced_usage(body), closure_usage(units, unit_closure(units, start, stop = character())))
  with_reexport_owners(unique(usage), entries)
}

run_fixtures <- function(files) {
  Sys.setenv(CTTIR_LIVE_TESTS = "true", NOT_CRAN = "true")
  results <- list()
  for (file in files) {
    reporter <- testthat::ListReporter$new()
    testthat::test_file(file.path("tests", "testthat", file), reporter = reporter, package = "cttiR", load_package = "none")
    df <- as.data.frame(reporter$get_results())
    for (i in seq_len(nrow(df))) {
      status <- if (df$error[[i]] || df$failed[[i]] > 0) "fail" else if (df$skipped[[i]]) "skip" else "pass"
      results[[length(results) + 1L]] <- list(test_file = file.path("tests", "testthat", file), test_name = df$test[[i]],
        result = status, run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }
  }
  results
}

# S3 implementations generated code reaches through dispatch: generic -> classes.
dispatch <- list(tidy = c("lm", "glm", "coxph", "lme"), print = "DescrList", filter = "data.frame",
  mutate = "data.frame", arrange = "data.frame", lme = "formula", fixef = "lme", getVarCov = "lme",
  logLik = "lme", coef = "default", residuals = c("lm", "glm"), fitted = "default",
  cooks.distance = c("lm", "glm"), model.matrix = c("default", "lm"), quantile = "default",
  na.fail = "default", survfit = "formula")

# One decision per adapter-tested capability and package, scoped to the calls that
# role's template code makes and to the fixtures that execute them.
standard_decisions <- function(entries, registry, units, adapter_fixtures, decided_at) {
  usage <- template_usage(units, entries)
  missing_packages <- setdiff(unique(usage$package), c(names(entries), "base"))
  if (length(missing_packages)) stop("Template calls packages absent from the build: ", paste(missing_packages, collapse = ", "))
  fixture_keys <- list()
  for (fixture in unique(unlist(adapter_fixtures, recursive = FALSE))) {
    id <- paste(fixture$test_file, fixture$test_name, sep = "::")
    if (is.null(fixture_keys[[id]])) {
      used <- fixture_usage(fixture$test_file, fixture$test_name, units, entries)
      fixture_keys[[id]] <- paste(used$package, used$name, sep = "::")
    }
  }
  decisions <- list()
  report <- list()
  notices <- character()
  for (cap in registry$capabilities) {
    if (!identical(cap$status, "adapter_tested") || is.null(cap$adapter)) next
    adapter <- cap$adapter$id
    if (is.null(adapter_fixtures[[adapter]])) stop("No fixtures declared for adapter ", adapter)
    role <- role_usage(adapter, units, entries)
    extra <- switch(adapter, standard.figures_accessible = "survival", standard.effects_broom = c("generics", "broom.mixed"))
    packages <- setdiff(unique(c(cap$packages, extra)), "base")
    for (name in packages) {
      entry <- entries[[name]]
      if (is.null(entry)) stop("Capability ", cap$id, " needs uncataloged package ", name)
      used <- role$usage[role$usage$package == name, , drop = FALSE]
      exports <- stats::setNames(entry$exports, vapply(entry$exports, function(x) x$name, character(1)))
      objects <- used$name[used$use == "value" & vapply(used$name, function(n) !is.null(exports[[n]]) && !identical(exports[[n]]$kind, "function"), logical(1))]
      callables <- setdiff(unique(used$name), objects)
      callables <- callables[vapply(callables, function(n) !identical(exports[[n]]$kind, "reexport"), logical(1))]
      if (name == "generics") callables <- intersect("tidy", role$names)
      classes <- engine_classes[[adapter]]
      methods <- unique(unlist(lapply(entry$s3_methods, function(m) {
        generic <- sub("^.*::", "", if (is.null(m$generic)) "" else m$generic)
        ok <- isTRUE(m$class %in% dispatch[[generic]]) && identical(m$verification, "static_method_verified") &&
          generic %in% role$names && (is.null(classes) || m$class %in% c(classes, "default", "formula"))
        if (ok) m$implementation
      })))
      if (!length(c(callables, methods, objects))) {
        notices <- c(notices, paste0(cap$id, " declares ", name, " but the ", adapter, " code calls none of its API"))
        next
      }
      topics <- unique(c(callables, objects))
      documents <- intersect(c("NEWS.md", "NEWS", "inst/NEWS.Rd", "README.md", "inst/CITATION"),
        vapply(entry$documentation_corpus$documents, function(d) d$path, character(1)))
      # Fixtures count only when they execute this package's calls for this role.
      keys <- paste(name, unique(c(used$name, if (length(methods)) sub("[.][^.]*$", "", methods))), sep = "::")
      relevant <- Filter(function(x) any(keys %in% fixture_keys[[paste(x$test_file, x$test_name, sep = "::")]]),
        adapter_fixtures[[adapter]])
      if (!length(relevant)) {
        notices <- c(notices, paste0("no ", adapter, " fixture executes ", name, "; keeping the adapter fixtures"))
        relevant <- adapter_fixtures[[adapter]]
      }
      decision <- list(
        approval_id = paste0(name, ":", entry$version, ":", sub("^[a-z]+[.]", "", adapter)),
        package = name, version = entry$version, source_hash = entry$source_hash,
        role = gsub("[^a-z0-9_]", "_", sub("^[a-z]+[.]", "", adapter)), profile = "standard_reflowR",
        adapter_id = adapter, adapter_version = cap$adapter$version, status = "approved",
        required_callables = as.list(callables), required_methods = as.list(methods),
        required_objects = as.list(objects), required_topics = as.list(intersect(topics, ns$stored_topic_aliases(Filter(function(d) identical(d$storage, "source_text"), entry$documentation_corpus$documents)))),
        required_documents = as.list(documents),
        fixtures = lapply(relevant, function(x) x[c("test_file", "test_name", "result", "run_at")]),
        decided_at = decided_at, rights_basis = entry$documentation_corpus$documents[[1]]$rights_basis)
      coverage <- ns$approval_coverage(entry, decision)
      report[[decision$approval_id]] <- list(state = coverage$state, missing = coverage$missing, counts = coverage$counts)
      if (!identical(coverage$state, "complete")) {
        decision$status <- "pending"
        cat("PENDING", decision$approval_id, ":", jsonlite::toJSON(Filter(length, coverage$missing), auto_unbox = TRUE), "\n")
      }
      decisions[[decision$approval_id]] <- decision
    }
  }
  # Every namespaced template call needs some role that approves it.
  approved <- unlist(lapply(decisions, function(d) paste(d$package, c(unlist(d$required_callables), unlist(d$required_objects)), sep = "::")))
  needed <- usage[usage$package != "base" & vapply(seq_len(nrow(usage)), function(i) {
    entry <- entries[[usage$package[[i]]]]
    hit <- Filter(function(x) identical(x$name, usage$name[[i]]), entry$exports)
    length(hit) && !identical(hit[[1]]$kind, "reexport")
  }, logical(1)), , drop = FALSE]
  uncovered <- setdiff(paste(needed$package, needed$name, sep = "::"), approved)
  for (call in uncovered) notices <- c(notices, paste0(call, " is called by template code that no approved role covers"))
  for (notice in unique(notices)) cat("NOTICE", notice, "\n")
  list(decisions = unname(decisions[order(names(decisions), method = "radix")]), report = report, notices = unique(notices))
}

write_approvals <- function(decisions) {
  file <- file.path("inst", "extdata", "approvals.json")
  writeLines(jsonlite::toJSON(list(schema_version = 1L, decisions = decisions), auto_unbox = TRUE, pretty = TRUE, null = "null"),
    file, useBytes = TRUE)
  ns$validate_approvals(ns$read_document(file))
  invisible(file)
}

if (identical(args, "approvals")) {
  # Offline: the bundled records are the build's entries after installed-evidence
  # and corpus compaction, and approvals.json holds the recorded fixture results.
  bundled <- ns$read_catalog(file.path("inst", "extdata", "api-catalog.json.gz"))
  entries <- stats::setNames(bundled$packages, vapply(bundled$packages, function(p) p$name, character(1)))
  previous <- ns$read_document(file.path("inst", "extdata", "approvals.json"))$decisions
  adapter_fixtures <- list()
  for (d in previous) {
    for (x in d$fixtures) {
      known <- vapply(adapter_fixtures[[d$adapter_id]], function(y) paste(y$test_file, y$test_name), character(1))
      if (!paste(x$test_file, x$test_name) %in% known) adapter_fixtures[[d$adapter_id]] <- c(adapter_fixtures[[d$adapter_id]], list(x))
    }
  }
  built <- standard_decisions(entries, ns$capability_registry(), template_units(), adapter_fixtures,
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (any(vapply(built$decisions, function(d) !identical(d$status, "approved"), logical(1)))) {
    stop("Re-scoped decisions must stay complete against the bundled revisions.")
  }
  # An unchanged decision keeps the time it was made.
  index <- stats::setNames(previous, vapply(previous, function(d) d$approval_id, character(1)))
  built$decisions <- lapply(built$decisions, function(d) {
    old <- index[[d$approval_id]]
    if (!is.null(old) && identical(ns$json_text(within(d, rm(decided_at))), ns$json_text(within(old, rm(decided_at))))) {
      d$decided_at <- old$decided_at
    }
    d
  })
  write_approvals(built$decisions)
  cat("Decisions", length(built$decisions), "(previously", length(previous), ")\n")
}

if (identical(args, "build")) {
  cache <- readRDS(cache_file)
  entries <- cache$entries
  registry <- ns$capability_registry()
  units <- template_units()
  usage <- template_usage(units, entries)

  # Trusted installed inspection only for statically unresolved callables.
  for (name in unique(usage$package)) {
    entry <- entries[[name]]
    if (is.null(entry)) next
    calls <- usage$name[usage$package == name & usage$use == "call"]
    unresolved <- Filter(function(x) {
      x$name %in% calls && !identical(x$verification, "static_api_verified") && !x$kind %in% c("reexport", "s4_generic")
    }, entry$exports)
    if (!length(unresolved)) next
    evidence <- ns$installed_export_evidence(name, entry$version, vapply(unresolved, function(x) x$name, character(1)))
    cat(sprintf("installed evidence %-14s %s (%d names)\n", name, evidence$status, length(unresolved)))
    entries[[name]] <- ns$apply_installed_evidence(entry, evidence)
  }

  fixture_files <- c("test-standard-workflow.R", "test-figures.R", "test-templates.R", "test-interop.R", "test-environment.R")
  fixtures <- run_fixtures(fixture_files)
  pick <- function(file, pattern) {
    hits <- Filter(function(x) basename(x$test_file) == file && grepl(pattern, x$test_name), fixtures)
    if (!length(hits)) stop("No fixture matched ", file, " / ", pattern)
    hits
  }
  adapter_fixtures <- list(
    standard.reflowr_layout = c(pick("test-standard-workflow.R", "^the standard bundle is pinned"),
      pick("test-templates.R", "^synthetic template renders")),
    standard.import_delimited = pick("test-standard-workflow.R", "^delimited import honours"),
    standard.check_mapped = pick("test-standard-workflow.R", "^stage checks agree"),
    standard.tidy_roles = pick("test-standard-workflow.R", "^tidy aliases keep"),
    standard.describe_descrtab2 = pick("test-standard-workflow.R", "^descriptive tables never test"),
    standard.describe_base = pick("test-standard-workflow.R", "^descriptive tables never test"),
    standard.figures_accessible = c(pick("test-figures.R", "."), pick("test-standard-workflow.R", "^the synthetic demo runs")),
    standard.model_lm = pick("test-standard-workflow.R", "^(reviewed engines match|blocked fits)"),
    standard.model_glm_binomial = pick("test-standard-workflow.R", "^(reviewed engines match|blocked fits)"),
    standard.model_lme = pick("test-standard-workflow.R", "^reviewed engines match"),
    standard.model_coxph = pick("test-standard-workflow.R", "^reviewed engines match"),
    standard.effects_broom = pick("test-standard-workflow.R", "^(reviewed engines match|tidy aliases keep)"),
    standard.demo_synthetic = pick("test-standard-workflow.R", "^the synthetic demo runs"),
    standard.report_render = c(pick("test-templates.R", "^synthetic template renders"), pick("test-standard-workflow.R", "^the synthetic demo runs")),
    standard.pipeline_targets = pick("test-environment.R", "^(a default tar_make|_targets.R is a pinned)"),
    interop.bioc_s4 = pick("test-interop.R", "."),
    interop.se_tidy_view = pick("test-interop.R", "^a bounded tidy view|lossy SummarizedExperiment|equal dimensions"),
    interop.seurat_v5 = pick("test-interop.R", "Seurat|pseudobulk|SingleCellExperiment to Seurat")
  )
  built <- standard_decisions(entries, registry, units, adapter_fixtures, format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  # A release catalog must not silently leave a routed stage or a template call
  # without approval; reconcile inst/extdata/capabilities with the template first.
  gaps <- grep("calls none of its API|no approved role covers", built$notices, value = TRUE)
  if (length(gaps)) stop("Capability registry and template calls disagree:\n", paste(gaps, collapse = "\n"))
  decisions <- built$decisions
  report <- built$report
  write_approvals(decisions)

  # Compact bundled corpus: keep approval evidence and package-level documents.
  for (name in names(entries)) {
    entry <- entries[[name]]
    mine <- Filter(function(d) identical(d$package, name), decisions)
    keep_topics <- unique(unlist(lapply(mine, function(d) c(d$required_callables, d$required_objects, d$required_methods, d$required_topics))))
    exports <- stats::setNames(entry$exports, vapply(entry$exports, function(x) x$name, character(1)))
    paths <- unique(unlist(c(
      lapply(keep_topics, function(n) exports[[n]]$documentation$path),
      lapply(entry$s3_methods, function(m) if (isTRUE(m$implementation %in% keep_topics)) m$documentation$path),
      lapply(keep_topics, function(n) {
        m <- Filter(function(x) identical(x$implementation, n), entry$s3_methods)
        if (length(m)) exports[[sub("^.*::", "", m[[1]]$generic)]]$documentation$path
      }))))
    all_paths <- vapply(entry$documentation_corpus$documents, function(d) d$path, character(1))
    package_docs <- grep("^(DESCRIPTION([.]in)?|NAMESPACE|README([.].*)?|NEWS([.].*)?|CHANGELOG([.].*)?|LICENSE([.].*)?|LICENCE([.].*)?|inst/CITATION|inst/NEWS[.]Rd)$",
      all_paths, value = TRUE)
    # Vignette sources of approved revisions are stored (spec 30); an inst/doc
    # copy is kept only when vignettes/ has no source of the same name.
    vignette <- ns$vignette_source_name(all_paths)
    in_vignettes <- !is.na(vignette) & startsWith(all_paths, "vignettes/")
    vignette_docs <- if (length(mine)) {
      all_paths[!is.na(vignette) & (in_vignettes | !vignette %in% vignette[in_vignettes])]
    } else {
      character()
    }
    entry$documentation_corpus <- ns$compact_document_corpus(entry$documentation_corpus, c(paths, package_docs, vignette_docs))
    entries[[name]] <- entry
  }

  decisions_all <- ns$approval_decisions()
  old_file <- file.path("inst", "extdata", "api-catalog.json.gz")
  before <- ns$read_catalog(old_file)
  # A rebuild replaces the standard family recorded by an earlier build; every
  # other package record is carried over unchanged apart from its approvals.
  previous_standard <- unlist(lapply(Filter(function(x) identical(x$family, "standard"), before$inventory),
    function(x) vapply(x$sources, function(s) s$package, character(1))))
  kept <- Filter(function(p) !p$name %in% previous_standard, before$packages)
  packages <- lapply(kept, function(p) ns$attach_approvals(p, decisions_all))
  names(packages) <- vapply(packages, function(p) p$name, character(1))
  for (name in names(entries)) {
    if (!is.null(packages[[name]])) stop("Standard package collides with a cataloged package: ", name)
    packages[[name]] <- ns$attach_approvals(entries[[name]], decisions_all)
  }
  sources <- lapply(entries, function(e) list(package = e$name, version = e$version, revision = e$revision, repository = e$repository))
  inventory <- c(Filter(function(x) !identical(x$family, "standard"), before$inventory),
    list(list(family = "standard", built_at = cache$built_at, sources = sources)))
  history <- file.path("inst", "extdata", "history", paste0(before$content_id, ".json.gz"))
  if (!file.exists(history)) stopifnot(file.copy(old_file, history))
  candidate <- tempfile(fileext = ".json.gz")
  id <- ns$write_catalog(unname(packages), candidate, inventory)
  built <- ns$read_catalog(candidate)
  approved <- ns$approved_callables(built)
  stopifnot(identical(built$content_id, id), nrow(approved) > 0L)
  stopifnot(file.copy(candidate, old_file, overwrite = TRUE))
  summary <- list(previous = before$content_id, current = id, packages = length(built$packages),
    standard_packages = length(entries), decisions = length(decisions),
    approved = sum(vapply(decisions, function(d) identical(d$status, "approved"), logical(1))),
    pending = names(Filter(function(r) !identical(r$state, "complete"), report)),
    approved_callables = nrow(approved), catalog_bytes = file.size(old_file),
    fixtures = fixtures, coverage = report)
  jsonlite::write_json(summary, file.path(evidence_dir, "standard-catalog-build.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  cat("Catalog", id, "| packages", length(built$packages), "| decisions", summary$decisions, "| approved", summary$approved,
    "| approved callables", nrow(approved), "| bytes", summary$catalog_bytes, "\n")
}
