# Build the bundled knowledge catalog with the standard-workflow package family
# and reviewed revision approvals.
#
#   Rscript tools/build_standard_catalog.R fetch    # verified repository fetch + static extraction
#   Rscript tools/build_standard_catalog.R build    # fixtures, approvals, compact corpus, catalog
#
# Run from the package root. `fetch` downloads public source archives through the
# package's own bounded repository backends (CRAN/Bioconductor MD5 checks; archived
# versions pinned to the MD5 of the earlier acquisition) and reads the local R 4.6.1
# distribution tree under admin/standard-sources. No source code is executed.
# `build` runs the adapter fixture tests, writes inst/extdata/approvals.json and the
# catalog, and keeps the previous bundled catalog under inst/extdata/history.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, args %in% c("fetch", "build"), file.exists("DESCRIPTION"))
pkgload::load_all(quiet = TRUE)
ns <- asNamespace("cttiR")
evidence_dir <- file.path("artifacts", "implementation")
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

template_usage <- function(root = file.path("inst", "templates", "standard-0.3.0")) {
  files <- list.files(root, pattern = "\\.(R|Rmd)$", recursive = TRUE, full.names = TRUE)
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
  for (file in files) {
    text <- readLines(file, warn = FALSE)
    if (grepl("Rmd$", file)) {
      inside <- FALSE
      keep <- character()
      for (line in text) {
        if (grepl("^```\\{r", line)) { inside <- TRUE; next }
        if (grepl("^```", line)) { inside <- FALSE; next }
        if (inside) keep <- c(keep, line)
      }
      text <- keep
    }
    for (e in parse(text = text, keep.source = FALSE)) walk(e)
  }
  usage <- unique(as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE))
  names(usage) <- c("package", "name", "use")
  usage
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

if (identical(args, "build")) {
  cache <- readRDS(cache_file)
  entries <- cache$entries
  registry <- ns$capability_registry()
  usage <- template_usage()
  # A reexport is approved through its owner, so the owner's decision must list it.
  for (i in seq_len(nrow(usage))) {
    entry <- entries[[usage$package[[i]]]]
    hit <- if (is.null(entry)) list() else Filter(function(x) identical(x$name, usage$name[[i]]) && identical(x$kind, "reexport"), entry$exports)
    while (length(hit)) {
      owner <- hit[[1]]$owner_package
      usage[nrow(usage) + 1L, ] <- list(owner, usage$name[[i]], usage$use[[i]])
      hit <- Filter(function(x) identical(x$name, usage$name[[i]]) && identical(x$kind, "reexport"), entries[[owner]]$exports)
    }
  }
  usage <- unique(usage)
  missing_packages <- setdiff(unique(usage$package), c(names(entries), "base"))
  if (length(missing_packages)) stop("Template calls packages absent from the build: ", paste(missing_packages, collapse = ", "))

  # Trusted installed inspection only for statically unresolved callables.
  for (name in unique(usage$package)) {
    entry <- entries[[name]]
    if (is.null(entry)) next
    calls <- usage$name[usage$package == name & usage$use == "call"]
    unresolved <- Filter(function(x) x$name %in% calls && !identical(x$verification, "static_api_verified") &&
      !x$kind %in% c("reexport", "s4_generic"), entry$exports)
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

  # S3 implementations generated code reaches through dispatch: generic -> classes.
  dispatch <- list(tidy = c("lm", "glm", "coxph", "lme"), print = "DescrList", filter = "data.frame",
    mutate = "data.frame", arrange = "data.frame", lme = "formula", fixef = "lme", getVarCov = "lme",
    logLik = "lme", coef = "default", residuals = c("lm", "glm"), fitted = "default",
    cooks.distance = c("lm", "glm"), model.matrix = c("default", "lm"), quantile = "default",
    na.fail = "default", survfit = "formula")
  decisions <- list()
  report <- list()
  for (cap in registry$capabilities) {
    if (!identical(cap$status, "adapter_tested") || is.null(cap$adapter)) next
    adapter <- cap$adapter$id
    if (is.null(adapter_fixtures[[adapter]])) stop("No fixtures declared for adapter ", adapter)
    packages <- setdiff(unique(c(cap$packages, if (adapter == "standard.figures_accessible") "survival",
      if (adapter == "standard.effects_broom") c("generics", "broom.mixed"))), "base")
    for (name in packages) {
      entry <- entries[[name]]
      if (is.null(entry)) stop("Capability ", cap$id, " needs uncataloged package ", name)
      used <- usage[usage$package == name, , drop = FALSE]
      exports <- stats::setNames(entry$exports, vapply(entry$exports, function(x) x$name, character(1)))
      objects <- used$name[used$use == "value" & vapply(used$name, function(n) !is.null(exports[[n]]) && !identical(exports[[n]]$kind, "function"), logical(1))]
      callables <- setdiff(unique(used$name), objects)
      callables <- callables[vapply(callables, function(n) !identical(exports[[n]]$kind, "reexport"), logical(1))]
      if (name == "generics") callables <- "tidy"
      methods <- unique(unlist(lapply(entry$s3_methods, function(m) {
        generic <- sub("^.*::", "", if (is.null(m$generic)) "" else m$generic)
        if (isTRUE(m$class %in% dispatch[[generic]]) && identical(m$verification, "static_method_verified")) m$implementation
      })))
      topics <- unique(c(callables, objects))
      documents <- intersect(c("NEWS.md", "NEWS", "inst/NEWS.Rd", "README.md", "inst/CITATION"),
        vapply(entry$documentation_corpus$documents, function(d) d$path, character(1)))
      fixtures_used <- lapply(adapter_fixtures[[adapter]], function(x) x[c("test_file", "test_name", "result", "run_at")])
      decision <- list(
        approval_id = paste0(name, ":", entry$version, ":", sub("^[a-z]+[.]", "", adapter)),
        package = name, version = entry$version, source_hash = entry$source_hash,
        role = gsub("[^a-z0-9_]", "_", sub("^[a-z]+[.]", "", adapter)), profile = "standard_reflowR",
        adapter_id = adapter, adapter_version = cap$adapter$version, status = "approved",
        required_callables = as.list(callables), required_methods = as.list(methods),
        required_objects = as.list(objects), required_topics = as.list(intersect(topics, ns$stored_topic_aliases(Filter(function(d) identical(d$storage, "source_text"), entry$documentation_corpus$documents)))),
        required_documents = as.list(documents), fixtures = fixtures_used,
        decided_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        rights_basis = entry$documentation_corpus$documents[[1]]$rights_basis)
      if (!length(c(callables, methods, objects))) next
      coverage <- ns$approval_coverage(entry, decision)
      report[[decision$approval_id]] <- list(state = coverage$state, missing = coverage$missing, counts = coverage$counts)
      if (!identical(coverage$state, "complete")) {
        decision$status <- "pending"
        cat("PENDING", decision$approval_id, ":", jsonlite::toJSON(Filter(length, coverage$missing), auto_unbox = TRUE), "\n")
      }
      decisions[[decision$approval_id]] <- decision
    }
  }
  decisions <- unname(decisions[order(names(decisions), method = "radix")])
  writeLines(jsonlite::toJSON(list(schema_version = 1L, decisions = decisions), auto_unbox = TRUE, pretty = TRUE, null = "null"),
    file.path("inst", "extdata", "approvals.json"), useBytes = TRUE)
  ns$validate_approvals(ns$read_document(file.path("inst", "extdata", "approvals.json")))

  # Compact bundled corpus: keep approval evidence and package-level documents.
  for (name in names(entries)) {
    entry <- entries[[name]]
    mine <- Filter(function(d) identical(d$package, name), decisions)
    keep_topics <- unique(unlist(lapply(mine, function(d) c(d$required_callables, d$required_objects, d$required_methods, d$required_topics))))
    exports <- stats::setNames(entry$exports, vapply(entry$exports, function(x) x$name, character(1)))
    paths <- unique(unlist(c(
      lapply(keep_topics, function(n) exports[[n]]$documentation$path),
      lapply(entry$s3_methods, function(m) if (isTRUE(m$implementation %in% keep_topics)) m$documentation$path),
      lapply(keep_topics, function(n) { m <- Filter(function(x) identical(x$implementation, n), entry$s3_methods); if (length(m)) exports[[sub("^.*::", "", m[[1]]$generic)]]$documentation$path }))))
    package_docs <- grep("^(DESCRIPTION([.]in)?|NAMESPACE|README([.].*)?|NEWS([.].*)?|CHANGELOG([.].*)?|LICENSE([.].*)?|LICENCE([.].*)?|inst/CITATION|inst/NEWS[.]Rd)$",
      vapply(entry$documentation_corpus$documents, function(d) d$path, character(1)), value = TRUE)
    entry$documentation_corpus <- ns$compact_document_corpus(entry$documentation_corpus, c(paths, package_docs))
    entries[[name]] <- entry
  }

  decisions_all <- ns$approval_decisions()
  old_file <- file.path("inst", "extdata", "api-catalog.json.gz")
  before <- ns$read_catalog(old_file)
  packages <- lapply(before$packages, function(p) ns$attach_approvals(p, decisions_all))
  names(packages) <- vapply(packages, function(p) p$name, character(1))
  for (name in names(entries)) {
    if (!is.null(packages[[name]])) stop("Standard package collides with a cataloged package: ", name)
    packages[[name]] <- ns$attach_approvals(entries[[name]], decisions_all)
  }
  inventory <- c(before$inventory, list(list(family = "standard", built_at = cache$built_at,
    sources = lapply(entries, function(e) list(package = e$name, version = e$version, revision = e$revision, repository = e$repository)))))
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
