# Developer tool: live planner benchmark against the owned local runtime.
#
#   Rscript --vanilla tools/run_planner_benchmark.R --runtime-dir DIR \
#     --endpoint http://127.0.0.1:11436 --out results.json \
#     --models qwen2.5-coder:1.5b,qwen2.5:7b [--allow-pull] [--processor cpu]
#
# The runtime must already have been started by setup(). Each listed model is
# verified through setup(model = tag); without --allow-pull, setup runs offline
# and never downloads. --allow-pull authorizes exactly the listed model pulls.
# Ordinary package checks never run this script.
args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default = NULL) {
  hit <- match(paste0("--", name), args)
  if (is.na(hit)) default else args[[hit + 1L]]
}
flag <- function(name) paste0("--", name) %in% args
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
pkgload::load_all(root, quiet = TRUE, export_all = FALSE)
ns <- asNamespace("cttiR")
internal <- function(name) get(name, envir = ns, inherits = FALSE)

runtime_dir <- option("runtime-dir")
endpoint <- option("endpoint", "http://127.0.0.1:11434")
out <- option("out")
models <- strsplit(option("models", ""), ",", fixed = TRUE)[[1]]
models <- models[nzchar(models)]
processor <- option("processor", "cpu")
stopifnot(!is.null(runtime_dir), !is.null(out))
options(cttiR.runtime_dir = runtime_dir, cttiR.ollama_endpoint = endpoint, cttiR.planner_processor = processor)
runtime_request <- internal("runtime_request")
local_model <- internal("local_model")
corpus <- internal("planner_cases")(file.path(root, "inst", "benchmarks", "planner-cases.json"))
policy <- corpus$selection_policy
started <- Sys.time()

capture <- function(command, arguments) {
  if (!nzchar(Sys.which(command))) return(NULL)
  tryCatch(trimws(processx::run(command, arguments, error_on_status = FALSE, timeout = 20)$stdout),
    error = function(e) NULL)
}
cpu <- tryCatch(sub(".*:\\s*", "", grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[[1]]), error = function(e) NA)
hardware <- list(
  cpu = cpu, physical_cores = ps::ps_cpu_count(logical = FALSE), logical_cores = ps::ps_cpu_count(logical = TRUE),
  memory_bytes = ps::ps_system_memory()[["total"]],
  gpus = capture("nvidia-smi", c("--query-gpu=name,memory.total,memory.used,driver_version", "--format=csv,noheader")),
  gpu_note = "GPUs were shared with a resident service using most of their memory; the reference profile is CPU-only.",
  os = paste(Sys.info()[c("sysname", "release", "machine")], collapse = " "),
  r = R.version.string, ollama = runtime_request(endpoint, "version")$version,
  cttiR = unname(read.dcf(file.path(root, "DESCRIPTION"))[, "Version"]),
  commit = capture("git", c("-C", root, "rev-parse", "HEAD"))
)

progress <- function(label) {
  function(row) {
    state <- if (row$cold) "cold" else "warm"
    line <- sprintf("[%s] %s %s %.1fs mode=%s attempts=%d fields=%d/%d fp=%d fn=%d %s\n", label, row$id, state,
      row$latency_seconds, row$planner_mode, row$attempts, row$score$field_correct, row$score$field_scored,
      row$score$capability_fp, row$score$capability_fn, paste(row$errors, collapse = " | "))
    cat(line)
    flush(stdout())
  }
}

compact_rows <- function(rows) {
  lapply(rows, function(r) {
    r$proposal$capability_ids <- as.list(r$proposal$capability_ids)
    list(id = r$id, cold = r$cold, planner_mode = r$planner_mode, fallback_reason = r$fallback_reason,
      attempts = r$attempts, errors = as.list(r$errors), latency_seconds = round(r$latency_seconds, 3),
      load_seconds = round(r$load_seconds, 3), prompt_tokens = r$prompt_tokens, output_tokens = r$output_tokens,
      proposal = r$proposal[c("aim", "outcome_family", "unit_structure", "modality", "capability_ids", "rationale")],
      field_correct = r$score$field_correct, field_scored = r$score$field_scored,
      capability_fp = r$score$capability_fp, capability_fn = r$score$capability_fn,
      injection_violation = r$score$injection_violation)
  })
}

failures <- function(rows, cases, limit = 12L) {
  failed <- function(r) {
    !r$score$field_exact || r$score$capability_fp > 0L || isTRUE(r$score$injection_violation) ||
      (identical(r$planner_mode, "deterministic") && length(r$errors) > 0L)
  }
  bad <- Filter(failed, rows)
  lapply(utils::head(bad, limit), function(r) {
    case <- cases[[match(r$id, vapply(cases, function(x) x$id, character(1)))]]
    wrong <- Filter(function(x) !x$correct, r$score$fields)
    list(id = r$id, goal = case$inputs$goal, wrong_fields = lapply(wrong, function(x) x[c("gold", "predicted")]),
      proposed_capabilities = as.list(r$proposal$capability_ids), required_capabilities = case$expected$capabilities$required,
      planner_mode = r$planner_mode, errors = as.list(r$errors),
      first_raw_reply = if (length(r$raw)) substr(r$raw[[1]], 1L, 600L) else NULL)
  })
}

runner_memory <- function() {
  state <- tryCatch(jsonlite::fromJSON(file.path(runtime_dir, "runtime-state.json")), error = function(e) NULL)
  if (is.null(state)) return(NULL)
  tryCatch({
    children <- ps::ps_children(ps::ps_handle(as.integer(state$pid)), recursive = TRUE)
    rss <- vapply(children, function(p) ps::ps_memory_info(p)[["rss"]], numeric(1))
    list(runner_rss_bytes = sum(rss), processes = length(children),
      note = "Resident set size of the owned daemon's runner processes sampled right after the warm run; CPU-only, so weights are memory-mapped.")
  }, error = function(e) NULL)
}

cat("Deterministic baseline\n")
baseline <- internal("planner_benchmark")(corpus$cases, "deterministic", progress = progress("deterministic"))
results <- list(deterministic = list(
  summary = baseline$summary, rows = compact_rows(baseline$rows),
  representative_failures = failures(baseline$rows, corpus$cases)
))
cold_ids <- c("S01", "X01", "R01")
repeat_ids <- c("S02", "S05", "S07", "X02", "P01", "R03", "R05", "B01", "B06", "N01", "I01", "I03")
candidates <- list()
for (tag in models) {
  cat("Model", tag, "\n")
  prepared <- internal("setup")(model = tag, offline = !flag("allow-pull"))
  if (!identical(prepared$state, "runtime_ready")) {
    candidates[[tag]] <- list(tag = tag, state = prepared$state, blockers = as.list(prepared$blockers))
    next
  }
  entry <- local_model(endpoint, tag)
  shown <- runtime_request(endpoint, "show", list(model = tag), timeout = 30)
  info <- shown$model_info
  context <- info[[grep("context_length$", names(info), value = TRUE)[1]]]
  license <- strsplit(if (is.character(shown$license)) shown$license else "", "\n", fixed = TRUE)[[1]]
  license <- trimws(license[nzchar(trimws(license))])
  unload <- function() {
    runtime_request(endpoint, "chat", list(model = tag, messages = list(), keep_alive = 0), timeout = 60)
    Sys.sleep(1)
  }
  run <- internal("planner_benchmark")(corpus$cases, "local_llm", cold = cold_ids, unload = unload,
    repeats = repeat_ids, progress = progress(tag), model = tag, keep_raw = TRUE)
  memory <- runner_memory()
  checks <- internal("planner_threshold_checks")(run$summary, baseline$summary, policy)
  candidates[[tag]] <- list(
    tag = tag, state = "benchmarked", digest = entry$digest, size_bytes = entry$size,
    format = entry$details$format, family = entry$details$family, parameter_size = entry$details$parameter_size,
    quantization = entry$details$quantization_level, context_length = context,
    capabilities = shown$capabilities, license_first_line = if (length(license)) substr(license[[1]], 1L, 120L) else NULL,
    general_license = info[["general.license"]], options = internal("planner_options")(),
    probe = prepared$steps[[3]]$evidence, memory = memory,
    summary = run$summary, checks = checks, qualified = all(vapply(checks, function(x) isTRUE(x$pass), logical(1))),
    rows = compact_rows(run$rows), representative_failures = failures(run$rows, corpus$cases)
  )
}
qualified <- Filter(function(x) isTRUE(x$qualified), candidates)
selected <- NULL
if (length(qualified)) {
  ordered <- qualified[order(
    vapply(qualified, function(x) -x$summary$field_accuracy$heldout, numeric(1)),
    vapply(qualified, function(x) -x$summary$field_accuracy$overall, numeric(1)),
    vapply(qualified, function(x) x$size_bytes, numeric(1)),
    vapply(qualified, function(x) x$summary$latency_seconds$warm$p95, numeric(1)))]
  selected <- ordered[[1]]$tag
}
report <- list(
  schema_version = 1L, benchmark = "m22-planner", corpus_version = corpus$corpus_version,
  prompt_version = corpus$prompt_version,
  prompt_system_sha256 = internal("content_hash")(internal("planner_prompt")("x", "x", "x")$system),
  corpus_sha256 = digest::digest(file = file.path(root, "inst", "benchmarks", "planner-cases.json"), algo = "sha256"),
  schema_sha256 = digest::digest(file = file.path(root, "inst", "schema", "planner.schema.json"), algo = "sha256"),
  started = format(started, "%Y-%m-%dT%H:%M:%S%z"),
  finished = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), hardware = hardware, endpoint = endpoint,
  processor_profile = processor, selection_policy = policy,
  deterministic = results$deterministic, candidates = unname(candidates),
  selection = list(selected = selected, qualified = as.list(names(qualified)),
    default_planner = if (is.null(selected)) "deterministic" else paste0("deterministic (opt-in local_llm with ", selected, ")"))
)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
writeLines(jsonlite::toJSON(report, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA), out, useBytes = TRUE)
cat("Selected:", if (is.null(selected)) "none (deterministic remains default)" else selected, "\n")
