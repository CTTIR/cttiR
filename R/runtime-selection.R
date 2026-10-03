# Read-only resource preflight. Unknown availability fails closed; these budgets
# are conservative admission screens, not reservations or process memory limits.
runtime_resources <- function(root) {
  ancestor <- root
  while (!dir.exists(ancestor) && !identical(dirname(ancestor), ancestor)) ancestor <- dirname(ancestor)
  read <- function(expr) tryCatch(force(expr), error = function(e) NA_real_)
  list(platform = Sys.info()[["sysname"]], arch = Sys.info()[["machine"]],
    physical_cores = read(ps::ps_cpu_count(logical = FALSE)),
    available_memory_bytes = read(ps::ps_system_memory()$avail),
    available_disk_bytes = read(ps::ps_disk_usage(ancestor)$available[[1]]))
}

runtime_select_model <- function(manifest, root, resources = runtime_resources(root)) {
  result <- list(model = "auto", digest = NULL, resources = resources,
    policy = "qualified-cpu-headroom-1", blockers = "No qualified model fits the tested CPU profile and resource headroom.")
  if (!identical(resources$platform, manifest$platform) || !identical(resources$arch, manifest$arch)) {
    result$blockers <- "Automatic model selection is not qualified for this platform."
    return(result)
  }
  fits <- function(entry) {
    if (!identical(planner_qualification(entry$tag, entry$digest, manifest), "qualified_for_planning")) return(FALSE)
    budget <- entry$resource_requirements
    if (is.null(budget)) return(FALSE)
    fields <- c(physical_cores = "minimum_physical_cores", available_memory_bytes = "available_memory_bytes",
      available_disk_bytes = "available_disk_bytes")
    all(vapply(names(fields), function(field) {
      available <- resources[[field]]
      required <- budget[[fields[[field]]]]
      is.numeric(available) && length(available) == 1L && is.finite(available) &&
        is.numeric(required) && length(required) == 1L && is.finite(required) && required > 0 && available >= required
    }, logical(1)))
  }
  candidates <- Filter(fits, manifest$tested_models)
  if (!length(candidates)) return(result)
  sizes <- vapply(candidates, function(x) x$size_bytes, numeric(1))
  if (any(!is.finite(sizes) | sizes <= 0)) return(result)
  selected <- candidates[[order(sizes, method = "radix")[[1]]]]
  result$model <- selected$tag
  result$digest <- selected$digest
  result$requirements <- selected$resource_requirements
  result$blockers <- character()
  result
}
