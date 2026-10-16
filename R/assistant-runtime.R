assistant_qualification_context <- function(context, request, kind, manifest) {
  body_hash <- function(fun) content_hash(paste(deparse(body(fun)), collapse = "\n"))
  list(prompt_version = assistant_prompt_version, catalog_id = context$catalog_id, kind = kind,
    policy_sha256 = content_hash(json_text(context$policy)),
    schema_sha256 = content_hash(json_text(read_document(resource_file("schema", paste0("assistant-", kind, ".schema.json"))))),
    runtime_binary_sha256 = manifest$binary_sha256, options = request$options,
    implementation_sha256 = model_implementation_fingerprint(),
    request_sha256 = body_hash(assistant_request), retrieval_sha256 = body_hash(assistant_context),
    retrieval_selection_sha256 = body_hash(assistant_reference_candidates),
    policy_retrieval_sha256 = body_hash(assistant_policy_reference),
    contract_retrieval_sha256 = body_hash(assistant_policy_contract_references),
    project_retrieval_sha256 = body_hash(assistant_project_references),
    project_guidance_sha256 = file_hash(resource_file("extdata", "assistant-guidance.json")),
    validation_sha256 = body_hash(assistant_validate_response),
    proposal_contract_sha256 = body_hash(validate_assistant_proposal),
    code_validation_sha256 = body_hash(validate_generated_code),
    transport_sha256 = body_hash(assistant_exchange))
}

assistant_model_qualification <- function(model, digest, context, request, kind) {
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  expected <- NULL
  for (entry in manifest$tested_models) {
    if (!identical(entry$tag, model) || !identical(entry$digest, digest)) next
    for (record in entry$assistant_qualifications) {
      if (!identical(record$status, "qualified_for_assistance")) next
      if (is.null(expected)) expected <- assistant_qualification_context(context, request, kind, manifest)
      if (identical(json_text(record$context), json_text(expected))) return("qualified_for_assistance")
    }
  }
  "not_qualified_for_assistance"
}

# One bounded local request. An unqualified development opt-in never changes the
# model's label. No runtime/model acquisition or project mutation occurs here.
assistant_exchange <- function(context, language = "en", kind = "answer", endpoint = NULL,
                               model = NULL, allow_unqualified = FALSE, timeout = 120) {
  kind <- match.arg(kind, c("answer", "suggestion"))
  scalar_flag(allow_unqualified, "allow_unqualified")
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || !is.finite(timeout) || timeout < 1 || timeout > 120) {
    abort_cttir("Response timeout must be between one and 120 seconds.")
  }
  started <- proc.time()[["elapsed"]]
  remaining <- function() max(0, timeout - (proc.time()[["elapsed"]] - started))
  qualification <- "not_qualified_for_assistance"
  fallback <- function(reason) list(state = "fallback", reason = reason, proposal = NULL,
    qualification = qualification, execution_authorized = FALSE, attempts = 0L)
  if (!length(context$references)) return(fallback("evidence_unavailable"))
  if (any(vapply(context$gaps, function(x) identical(x$reason, "instruction_request_rejected"), logical(1)))) {
    return(fallback("instruction_request_rejected"))
  }
  if (is.null(endpoint)) endpoint <- tryCatch(runtime_endpoint(), error = function(e) NULL)
  if (!planner_valid_endpoint(endpoint)) return(fallback("endpoint_rejected"))
  owner <- tryCatch(runtime_owner(runtime_directory(), endpoint), error = function(e) NULL)
  if (is.null(owner)) return(fallback("runtime_unverified"))
  if (is.null(model)) model <- owner$model
  if (!valid_model_tag(model)) return(fallback("model_refused"))
  digest <- planner_expected_digest(model, owner)
  if (!is.character(digest) || length(digest) != 1L || is.na(digest) || !grepl("^[a-f0-9]{64}$", digest)) {
    return(fallback("model_digest_unrecorded"))
  }
  request <- tryCatch(assistant_request(context, model, language, kind), error = function(e) NULL)
  if (is.null(request)) return(fallback("request_preflight_failed"))
  qualification <- assistant_model_qualification(model, digest, context, request, kind)
  if (!identical(qualification, "qualified_for_assistance") && !allow_unqualified) return(fallback("model_not_qualified"))
  if (remaining() <= 0) return(fallback("deadline_exceeded"))
  entry <- tryCatch(local_model(endpoint, model, timeout = min(10, remaining())), error = function(e) NULL)
  if (is.null(entry)) return(fallback("model_locality_unverified"))
  if (!identical(entry$digest, digest)) return(fallback("model_digest_mismatch"))
  if (remaining() <= 0) return(fallback("deadline_exceeded"))
  response <- tryCatch(runtime_request(endpoint, "chat", request, timeout = remaining()), error = function(e) NULL)
  rejected <- function(reason) {
    result <- fallback(reason); result$attempts <- 1L; result
  }
  if (!is.list(response)) return(rejected("runtime_request_failed"))
  if (!identical(response$model, model) || !is.null(response$remote_host) || !is.null(response$remote_model)) {
    return(rejected("model_identity_mismatch"))
  }
  tokens <- response$prompt_eval_count
  if (!is.numeric(tokens) || length(tokens) != 1L || !is.finite(tokens) || tokens < 1 || tokens != floor(tokens) ||
      tokens + request$options$num_predict > request$options$num_ctx) return(rejected("context_overflow"))
  if (!isTRUE(response$done) || !identical(response$done_reason, "stop")) return(rejected("incomplete_response"))
  message <- response$message
  if (!is.list(message) || !identical(message$role, "assistant") || length(message$tool_calls)) return(rejected("invalid_message"))
  # Never store or return a reasoning field, even when a backend ignores think=FALSE.
  if (length(message$thinking) && !identical(message$thinking, "")) return(rejected("unexpected_reasoning"))
  checked <- assistant_validate_response(message$content, context, language, kind)
  if (!checked$ok) return(rejected(checked$reason))
  # Detect model replacement during generation before accepting the reply.
  if (remaining() <= 0) return(rejected("deadline_exceeded"))
  fresh <- tryCatch(local_model(endpoint, model, timeout = min(10, remaining())), error = function(e) NULL)
  if (is.null(fresh) || !identical(fresh$digest, digest)) return(rejected("model_digest_changed"))
  if (remaining() <= 0) return(rejected("deadline_exceeded"))
  list(state = "proposal", reason = NULL, proposal = checked$proposal, qualification = qualification,
    validation_level = checked$validation_level, code_checks = checked$code_checks,
    execution_authorized = FALSE, attempts = 1L, provenance = list(model = model, digest = digest,
      catalog_id = context$catalog_id, prompt_version = assistant_prompt_version, prompt_tokens = tokens))
}
