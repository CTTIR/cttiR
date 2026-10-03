interop_modalities <- function() {
  c("single_cell", "bulk_rna", "cytometry", "spatial", "proteomics", "multiomics", "microbiome", "imaging", "tabular")
}

interop_statuses <- function() c("candidate", "adapter_tested")

interop_stages <- function() {
  c("container", "bridge", "aggregation", "interchange", "normalization", "analysis", "annotation", "acceleration", "data_distribution")
}

# Packages whose capabilities must never be offered for generic tabular or
# clinical aims.
interop_seurat_packages <- function() {
  c("Seurat", "SeuratObject", "sctransform", "Signac", "SeuratData", "Azimuth", "SeuratWrappers",
    "SeuratDisk", "BPCells", "presto", "glmGamPoi")
}

#' Concrete reviewed interoperability adapters
#'
#' Each adapter is a set of functions in the static project template file
#' `code/R/cttir_interop.R`. Runtime-tested status in the capability registry
#' may only point at these exact adapter versions; their fixtures live in
#' `tests/testthat/test-interop.R`.
#' @return A data.frame with adapter id, version, template file and functions.
#' @noRd
interop_adapters <- function() {
  data.frame(
    id = c("interop.bioc_s4", "interop.se_tidy_view", "interop.seurat_v5"),
    version = c("1.2.1", "1.2.1", "1.2.1"),
    template = "standard-0.3.0/code/R/cttir_interop.R",
    functions = c("ci_validate_s4,ci_conversion_report", "ci_se_tidy_view",
      "ci_seurat_layers,ci_pseudobulk,ci_convert,ci_conversion_report"),
    stringsAsFactors = FALSE
  )
}

#' Modality routing rules for the Bioconductor/Seurat capability family
#'
#' Documents how the router may use `inst/extdata/capabilities/bioc.json`.
#' Generic clinical or tabular aims never route to Seurat-ecosystem
#' capabilities; omics capabilities require a detected omics modality.
#' @return A data.frame with columns rule, applies_to, effect and detail.
#' @noRd
modality_routing_rules <- function() {
  data.frame(
    rule = c("tabular_never_seurat", "modality_required", "unknown_modality_no_route",
      "infrastructure_not_specialist", "candidate_is_advice_only", "donor_level_inference", "no_dataset_download"),
    applies_to = c("modality:tabular", "family:bioconductor,seurat", "modality:unknown", "infrastructure:true",
      "status:candidate", "modality:single_cell", "all"),
    effect = c("exclude", "require_match", "exclude", "not_specialist_match", "not_executable", "require_replicates", "forbid"),
    detail = c(
      "Generic clinical or tabular aims (modality 'tabular') never route to Seurat or its companions; the standard workflow handles them.",
      "A capability is offered only when the detected modality is listed in its applies.modality.",
      "Without a detected omics modality no capability from this family is offered.",
      "Infrastructure entries (containers, bridges, file interchange, acceleration) do not count as a specialist workflow match.",
      "Candidate entries are catalog advice; only adapter_tested entries point at reviewed, tested adapters.",
      "Cells are not biological replicates; differential inference uses donor-level pseudobulk with at least 2 donors per group.",
      "No automatic dataset, reference or package download from any capability in this family."
    ),
    stringsAsFactors = FALSE
  )
}

#' Read and validate the Bioconductor/Seurat capability registry
#'
#' @param path Registry JSON; defaults to the bundled `extdata/capabilities/bioc.json`.
#' @return The validated registry as a list (schema_version, family, capabilities).
#' @noRd
interop_capabilities <- function(path = NULL) {
  if (is.null(path)) path <- resource_file("extdata", "capabilities", "bioc.json")
  validate_interop_registry(read_document(path))
}

#' Capabilities that the routing rules allow for one detected modality
#'
#' @param modality One modality id, or "unknown".
#' @param registry A validated registry from `interop_capabilities()`.
#' @return A list of capability entries (possibly empty).
#' @noRd
interop_capabilities_for <- function(modality, registry = interop_capabilities()) {
  scalar_text(modality, "modality")
  if (!modality %in% c(interop_modalities(), "unknown")) abort_cttir("Unknown modality.", field = "modality")
  if (modality %in% c("tabular", "unknown")) return(list())
  Filter(function(cap) modality %in% unlist(cap$applies$modality), registry$capabilities)
}

interop_registry_error <- function(message, field = NULL) {
  abort_cttir(message, "cttir_schema_error", "invalid_capability_registry", field = field,
    remediation = "Correct inst/extdata/capabilities/bioc.json against the documented registry contract.")
}

interop_exact_keys <- function(x, keys, where) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x))) interop_registry_error(paste(where, "must be an object."), where)
  unknown <- setdiff(names(x), keys)
  missing <- setdiff(keys, names(x))
  if (length(unknown)) interop_registry_error(paste0(where, " has unknown keys: ", paste(unknown, collapse = ", "), "."), where)
  if (length(missing)) interop_registry_error(paste0(where, " is missing keys: ", paste(missing, collapse = ", "), "."), where)
  invisible(x)
}

interop_strings <- function(x, where, nonempty = FALSE, allowed = NULL) {
  ok <- is.list(x) && is.null(names(x)) && all(vapply(x, function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(v)), logical(1)))
  if (!ok) interop_registry_error(paste(where, "must be an array of nonempty strings."), where)
  values <- as.character(unlist(x))
  if (nonempty && !length(values)) interop_registry_error(paste(where, "must not be empty."), where)
  if (anyDuplicated(values)) interop_registry_error(paste(where, "must not contain duplicates."), where)
  if (!is.null(allowed) && length(setdiff(values, allowed))) {
    interop_registry_error(paste0(where, " has unsupported values: ", paste(setdiff(values, allowed), collapse = ", "), "."), where)
  }
  values
}

interop_scalar <- function(x, where, allowed = NULL, pattern = NULL) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) interop_registry_error(paste(where, "must be a nonempty string."), where)
  if (!is.null(allowed) && !x %in% allowed) interop_registry_error(paste0(where, " must be one of: ", paste(allowed, collapse = ", "), "."), where)
  if (!is.null(pattern) && !grepl(pattern, x)) interop_registry_error(paste(where, "has an invalid format."), where)
  x
}

validate_interop_registry <- function(x) {
  interop_exact_keys(x, c("schema_version", "family", "capabilities"), "registry")
  if (!identical(x$schema_version, 1L) && !identical(x$schema_version, 1)) interop_registry_error("registry schema_version must be 1.", "schema_version")
  interop_scalar(x$family, "family", allowed = "bioc_seurat")
  if (!is.list(x$capabilities) || !is.null(names(x$capabilities)) || !length(x$capabilities)) {
    interop_registry_error("capabilities must be a nonempty array.", "capabilities")
  }
  keys <- c("id", "family", "stage", "title", "packages", "adapter", "applies", "keywords", "infrastructure", "specialist", "requirements", "status")
  adapters <- interop_adapters()
  ids <- character()
  for (i in seq_along(x$capabilities)) {
    cap <- x$capabilities[[i]]
    where <- paste0("capabilities[", i, "]")
    interop_exact_keys(cap, keys, where)
    id <- interop_scalar(cap$id, paste0(where, ".id"), pattern = "^(bioc|seurat)\\.[a-z0-9_]+\\.[a-z0-9_]+$")
    if (id %in% ids) interop_registry_error(paste0("Duplicate capability id: ", id, "."), paste0(where, ".id"))
    ids <- c(ids, id)
    family <- interop_scalar(cap$family, paste0(where, ".family"), allowed = c("bioconductor", "seurat"))
    if (!startsWith(id, if (family == "seurat") "seurat." else "bioc.")) interop_registry_error(paste0(id, ": id prefix must match its family."), paste0(where, ".id"))
    interop_scalar(cap$stage, paste0(where, ".stage"), allowed = interop_stages())
    interop_scalar(cap$title, paste0(where, ".title"))
    packages <- interop_strings(cap$packages, paste0(where, ".packages"), nonempty = TRUE)
    if (!all(grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$", packages))) interop_registry_error(paste0(id, ": invalid package name."), paste0(where, ".packages"))
    status <- interop_scalar(cap$status, paste0(where, ".status"), allowed = interop_statuses())
    if (is.null(cap$adapter)) {
      if (status == "adapter_tested") interop_registry_error(paste0(id, ": adapter_tested requires a concrete adapter."), paste0(where, ".adapter"))
    } else {
      interop_exact_keys(cap$adapter, c("id", "version"), paste0(where, ".adapter"))
      aid <- interop_scalar(cap$adapter$id, paste0(where, ".adapter.id"), pattern = "^interop\\.[a-z0-9_]+$")
      aver <- interop_scalar(cap$adapter$version, paste0(where, ".adapter.version"), pattern = "^[0-9]+\\.[0-9]+\\.[0-9]+$")
      if (status != "adapter_tested") interop_registry_error(paste0(id, ": a candidate must not name an adapter."), paste0(where, ".adapter"))
      if (!any(adapters$id == aid & adapters$version == aver)) interop_registry_error(paste0(id, ": adapter ", aid, " ", aver, " is not a reviewed tested adapter."), paste0(where, ".adapter"))
    }
    interop_exact_keys(cap$applies, c("modality", "aim", "outcome_family", "unit_structure"), paste0(where, ".applies"))
    modality <- interop_strings(cap$applies$modality, paste0(where, ".applies.modality"), nonempty = TRUE, allowed = interop_modalities())
    for (k in c("aim", "outcome_family", "unit_structure")) interop_strings(cap$applies[[k]], paste0(where, ".applies.", k))
    if ("tabular" %in% modality && (family == "seurat" || any(packages %in% interop_seurat_packages()))) {
      interop_registry_error(paste0(id, ": Seurat-ecosystem capabilities must never apply to the tabular modality."), paste0(where, ".applies.modality"))
    }
    if (identical(modality, "tabular")) interop_registry_error(paste0(id, ": generic tabular/clinical capabilities do not belong in this registry."), paste0(where, ".applies.modality"))
    interop_exact_keys(cap$keywords, c("en", "de"), paste0(where, ".keywords"))
    interop_strings(cap$keywords$en, paste0(where, ".keywords.en"), nonempty = TRUE)
    interop_strings(cap$keywords$de, paste0(where, ".keywords.de"), nonempty = TRUE)
    for (k in c("infrastructure", "specialist")) {
      if (!is.logical(cap[[k]]) || length(cap[[k]]) != 1L || is.na(cap[[k]])) interop_registry_error(paste0(where, ".", k, " must be true or false."), paste0(where, ".", k))
    }
    if (isTRUE(cap$infrastructure) && isTRUE(cap$specialist)) interop_registry_error(paste0(id, ": infrastructure entries cannot count as specialist matches."), where)
    interop_strings(cap$requirements, paste0(where, ".requirements"), nonempty = TRUE)
  }
  x
}
