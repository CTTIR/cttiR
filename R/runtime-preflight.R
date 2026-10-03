# Offline observations only. Presence is not integrity or daemon readiness.
runtime_preflight <- function(root, endpoint, manifest, model, validation,
  install_ollama, offline,
  resources = runtime_resources(root),
  archive_tools = Sys.which(c("tar", "zstd"))) {
  executable <- file.path(root, paste0("ollama-", manifest$runtime_version), "bin", "ollama")
  assert_plain_path(executable)
  present <- file.exists(executable)
  acquisition <- character()
  if (!present) {
    if (offline || !install_ollama) {
      acquisition <- "runtime_absent_acquisition_disabled"
    } else {
      if (!identical(resources$platform, manifest$platform) || !identical(resources$arch, manifest$arch)) {
        acquisition <- c(acquisition, "platform_unverified")
      }
      if (!all(c("tar", "zstd") %in% names(archive_tools)) ||
          any(is.na(archive_tools[c("tar", "zstd")]) | !nzchar(archive_tools[c("tar", "zstd")]))) {
        acquisition <- c(acquisition, "archive_tools_missing")
      }
    }
  }
  entries <- Filter(function(x) identical(x$tag, model), manifest$tested_models)
  entry <- if (length(entries)) entries[[1]] else NULL
  available <- resources$available_disk_bytes
  required <- entry$resource_requirements$available_disk_bytes
  known <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  disk_state <- if (!known(available) || !known(required)) "unknown" else if (available < required) "insufficient" else "meets_screen"
  qualified <- identical(validation, "qualified_for_planning")
  owner <- tryCatch(runtime_owner(root, endpoint), error = function(e) NULL)
  list(
    scope = "offline_preflight_not_runtime_verification",
    platform = resources$platform, arch = resources$arch,
    executable_present = present,
    owner = if (is.null(owner)) NULL else list(pid = owner$pid, host = owner$host, endpoint = owner$endpoint),
    downloads = list(runtime_archive_bytes = manifest$archive_bytes,
      model_bytes = entry$size_bytes,
      scope = "published_full_sizes_not_incremental_downloads; local model presence not queried"),
    disk = list(admission_state = disk_state, available_bytes = available,
      admission_required_bytes = entry$resource_requirements$available_disk_bytes,
      scope = "recorded_admission_screen_not_reserved_space; unknown when no profile exists"),
    acquisition_blockers = acquisition,
    planner_blockers = if (qualified) character() else "workflow_model_not_qualified"
  )
}
