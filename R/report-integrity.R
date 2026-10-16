# Content identities for explicit computation and rendering boundaries.
# These functions do not establish scientific validity or permission to publish.
cr_file <- function(root, path, must_exist = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || grepl("^[/\\\\]|^[A-Za-z]:|\\\\", path) ||
      any(strsplit(path, "/", fixed = TRUE)[[1]] %in% c("", ".", ".."))) {
    stop("Invalid report receipt path.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  current <- root
  for (part in strsplit(path, "/", fixed = TRUE)[[1]]) {
    current <- file.path(current, part)
    link <- Sys.readlink(current)
    if (!is.na(link) && nzchar(link)) stop("Report paths cannot contain symbolic links.", call. = FALSE)
  }
  if (!file.exists(current) && !must_exist) return(current)
  if (!file.exists(current) || dir.exists(current)) stop("Report input/output is missing: ", path, call. = FALSE)
  resolved <- normalizePath(current, winslash = "/", mustWork = TRUE)
  if (!startsWith(resolved, paste0(root, "/"))) stop("Report path escapes the project.", call. = FALSE)
  resolved
}

cr_identity <- function(root, paths) {
  if (!is.character(paths) || anyNA(paths) || anyDuplicated(paths)) {
    stop("Report identities require unique character paths.", call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) stop("Prepare the digest dependency explicitly.", call. = FALSE)
  paths <- sort(paths, method = "radix")
  lapply(paths, function(path) list(path = path,
    sha256 = digest::digest(file = cr_file(root, path), algo = "sha256")))
}

cr_binding_identity <- function(bindings = character()) {
  if (!is.character(bindings) || anyNA(bindings) ||
      (length(bindings) && (is.null(names(bindings)) || anyNA(names(bindings)) ||
      any(!nzchar(names(bindings))) || anyDuplicated(names(bindings))))) {
    stop("External inputs require unique dataset IDs and local file paths.", call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) stop("Prepare the digest dependency explicitly.", call. = FALSE)
  if (!length(bindings)) return(list())
  bindings <- bindings[order(names(bindings), method = "radix")]
  lapply(seq_along(bindings), function(i) {
    path <- bindings[[i]]
    if (!file.exists(path) || dir.exists(path)) stop("A bound dataset is missing or is a directory.", call. = FALSE)
    # Local paths remain in the ignored binding configuration. Receipts record
    # only stable dataset IDs and content hashes, including for external files.
    list(dataset_id = names(bindings)[[i]],
      sha256 = digest::digest(file = path, algo = "sha256"))
  })
}

cr_snapshot <- function(root, inputs, outputs, bindings = character()) {
  if (!length(inputs) || !length(outputs)) stop("A report receipt requires inputs and outputs.", call. = FALSE)
  list(schema_version = 1L, status = "computed_not_scientifically_validated",
    inputs = cr_identity(root, inputs), outputs = cr_identity(root, outputs),
    external_inputs = cr_binding_identity(bindings))
}

cr_assert_fresh <- function(root, receipt, inputs, outputs, bindings = character()) {
  current <- cr_snapshot(root, inputs, outputs, bindings)
  if (!identical(receipt, current)) {
    stop("Report inputs, code, configuration or outputs changed; rerun the reviewed workflow.", call. = FALSE)
  }
  invisible(TRUE)
}

cr_project_bindings <- function(root, ids) {
  if (!is.character(ids) || anyNA(ids) || anyDuplicated(ids)) stop("Invalid dataset identity list.")
  if (!length(ids)) return(character())
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Prepare the yaml dependency explicitly.")
  config <- yaml::read_yaml(cr_file(root, ".cttir/local.yml"), eval.expr = FALSE)
  result <- stats::setNames(rep(NA_character_, length(ids)), ids)
  seen <- character()
  for (binding in config$bindings) {
    if (!is.list(binding) || !is.character(binding$id) || length(binding$id) != 1L || is.na(binding$id)) {
      stop("Invalid local dataset binding.")
    }
    if (binding$id %in% seen) stop("Duplicate local dataset binding.")
    seen <- c(seen, binding$id)
    if (!binding$id %in% ids) next
    if (!is.character(binding$path) || length(binding$path) != 1L || is.na(binding$path) || !nzchar(binding$path)) {
      stop("Missing local dataset path.")
    }
    path <- path.expand(binding$path)
    if (!grepl("^[/\\\\]|^[A-Za-z]:[/\\\\]", path)) path <- file.path(root, path)
    result[[binding$id]] <- path
  }
  if (anyNA(result)) stop("A recorded dataset has no current local binding.")
  result
}

cr_execution_inputs <- function(root) {
  fixed <- c("cttir-project.yml", "cttir-lock.json", ".cttir/local.yml", "_targets.R", "renv.lock",
    "metadata/data-registry.yml", "metadata/data-dictionary.csv")
  paths <- fixed[file.exists(file.path(root, fixed))]
  for (directory in c("code", "config")) {
    found <- list.files(file.path(root, directory), recursive = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(found)) paths <- c(paths, paste0(directory, "/", found))
  }
  publication_files <- list.files(file.path(root, "publications"), recursive = TRUE,
    all.files = TRUE, no.. = TRUE)
  analysis_files <- publication_files[grepl("^[^/]+/analysis/", publication_files)]
  if (length(analysis_files)) paths <- c(paths, paste0("publications/", analysis_files))
  sort(unique(paths), method = "radix")
}

cr_begin <- function(root, bindings) {
  list(inputs = cr_identity(root, cr_execution_inputs(root)),
    external_inputs = cr_binding_identity(bindings))
}

cr_finish <- function(root, before, outputs, bindings) {
  receipt <- cr_snapshot(root, cr_execution_inputs(root), outputs, bindings)
  if (!identical(before$inputs, receipt$inputs) || !identical(before$external_inputs, receipt$external_inputs)) {
    stop("Inputs changed during computation; outputs cannot receive a freshness receipt.", call. = FALSE)
  }
  receipt
}

cr_report_table <- function(root, name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !name %in% c("descriptives", "effects")) stop("Unknown reviewed report table.")
  relative <- paste0("output/tables/", name, ".csv")
  if (!file.exists(file.path(root, relative))) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Prepare the jsonlite dependency explicitly.")
  receipt <- jsonlite::read_json(cr_file(root, ".cttir/report-receipt.json"), simplifyVector = FALSE)
  outputs <- list.files(file.path(root, "output"), recursive = TRUE, all.files = TRUE, no.. = TRUE)
  outputs <- if (length(outputs)) paste0("output/", outputs) else character()
  ids <- vapply(receipt$external_inputs, function(x) x$dataset_id, character(1))
  cr_assert_fresh(root, receipt, cr_execution_inputs(root), outputs, cr_project_bindings(root, ids))
  table <- utils::read.csv(cr_file(root, relative), check.names = FALSE, stringsAsFactors = FALSE)
  expected <- Filter(function(x) identical(x$path, relative), receipt$outputs)
  if (length(expected) != 1L || !identical(expected[[1]]$sha256,
      digest::digest(file = cr_file(root, relative), algo = "sha256"))) {
    stop("Report table changed while it was being read.")
  }
  table
}
