# Grounded, deterministic answers from the pinned catalog revision.
#
# A question is read as untrusted text. It is matched to registered capabilities
# and exact package::export names; nothing in it is executed or sent anywhere.
# Code comes only from reviewed snippets and is returned only after it validates
# against the approved function table of the same catalog revision.

ask_stage_order <- c("project", "import", "check", "tidy", "describe", "figures", "model", "effects",
  "demo", "report", "pipeline", "environment")

# Instruction-shaped text: an imperative aimed at rules or instructions, role
# switches, chat-template and override markers, and requests to run code. A
# single word such as "disregard" or "system prompt" is not enough, so benign
# questions ("disregard rows with missing values") are answered normally.
injection_patterns <- c(
  paste0("\\b(ignore|ignoring|disregard|forget|override|overrule|bypass|skip|abandon)\\s+((all|any|the|your|my|these|those|",
    "every|of|previous|prior|above|earlier|preceding|former|existing|current|system|safety|original|initial|given)\\s+){0,4}",
    "(instructions?|rules?|guidelines?|guardrails?|constraints?|restrictions?|directives?|polic(y|ies)|prompts?|",
    "system\\s+prompt|commands?|context)\\b"),
  "\\b(ignore|disregard|forget)\\s+((all|everything)\\s+)?((of\\s+)?the\\s+)?(above|foregoing)\\b",
  "\\b(ignore|disregard|forget)\\s+everything\\b",
  paste0("\\b(ignorier\\w*|missacht\\w*|vergiss|vergesst|vergessen\\s+sie|umgeh\\w*)\\s+((alle\\w*|die|der|den|deine\\w*|",
    "ihre\\w*|s(\u00e4|ae)mtliche\\w*|bisherige\\w*|vorherige\\w*|obige\\w*|vorigen?|jede\\w*)\\s+){0,4}",
    "(anweisung|regel|vorgabe|richtlinie|instruktion|befehl|systemprompt|prompt)"),
  "\\bvergiss\\s+alles\\b",
  "\\byou\\s+are\\s+now\\b", "\\bfrom\\s+now\\s+on\\b", "\\bpretend\\s+(to\\s+be|you\\s+are)\\b",
  "\\bact\\s+as\\s+(an?\\s+)?(admin|administrator|root|developer|system|unrestricted|jailbroken)\\b",
  "\\bdu\\s+bist\\s+(jetzt|nun|ab\\s+sofort)\\b", "\\bab\\s+(jetzt|sofort)\\s+(bist|gilt|antworte|ignorier)",
  "\\bnew\\s+(instructions?|rules?|system\\s+prompt)\\b", "\\bneue\\s+(anweisung|regel)",
  "(^|\\s)#{1,6}\\s*(system|admin|developer|override|instructions?)\\b",
  "\\b(system|admin|developer|safety|security)\\s+(override|mode)\\b",
  "(^|\\n)\\s*(system|assistant|developer)\\s*:", "\\[/?(inst|system)\\]", "<\\|?(system|im_start|im_end|endoftext)\\|?>",
  "<<\\s*/?sys\\s*>>",
  "\\b(reveal|show|print|repeat|output|leak|change|replace)\\s+((the|your|this)\\s+)*system\\s+prompt\\b",
  "\\bjailbreak", "\\bdan\\s+mode\\b",
  "\\b(system2?|shell|shell\\.exec|eval|evalq|install\\.packages|download\\.file|unlink|file\\.remove|sys\\.setenv)\\(",
  "\\brm\\s+-[a-z]*[rf]", "\\b(curl|wget)\\s+\\S", "\\bsudo\\b",
  "\\b(delete|wipe|erase)\\s+(all|every|everything)\\b"
)

# Cyrillic and Greek lookalikes of Latin letters, by code point, and their
# Latin counterparts ("Ign\u043ere" reads as "Ignore").
confusable_from <- strtoi(c(
  "0430", "0435", "043E", "0440", "0441", "0443", "0445", "0456", "0458", "0455", "0501", "04CF", "04BB", "051B", "051D",
  "0410", "0412", "0415", "041A", "041C", "041D", "041E", "0420", "0421", "0422", "0425", "0406", "0408", "0405", "0423",
  "03B1", "03B5", "03B9", "03BA", "03BD", "03BF", "03C1", "03C4", "03C5", "03C7",
  "0391", "0392", "0395", "0396", "0397", "0399", "039A", "039C", "039D", "039F", "03A1", "03A4", "03A5", "03A7"), 16L)
confusable_to <- utf8ToInt("aeopcyxijsdlhqwABEKMHOPCTXIJSYaeikvoptuxABEZHIKMNOPTYX")

# Fold lookalike and fullwidth letters to ASCII and drop invisible format
# characters, so instruction-shaped text cannot hide behind them.
fold_confusables <- function(x) {
  codes <- utf8ToInt(enc2utf8(x))
  if (anyNA(codes)) return(x)
  invisible <- codes == 0xAD | (codes >= 0x200B & codes <= 0x200F) | (codes >= 0x2060 & codes <= 0x2064) | codes == 0xFEFF
  codes <- codes[!invisible]
  codes[codes == 0xA0 | (codes >= 0x2000 & codes <= 0x200A) | codes %in% c(0x202F, 0x205F, 0x3000)] <- 32L
  wide <- codes >= 0xFF01 & codes <= 0xFF5E
  codes[wide] <- codes[wide] - 0xFEE0L
  hit <- match(codes, confusable_from)
  codes[!is.na(hit)] <- confusable_to[hit[!is.na(hit)]]
  intToUtf8(codes)
}

# Shared by ask() and the local planner, which screens goals before any model
# sees them.
instruction_like <- function(text) {
  text <- tolower(fold_confusables(text))
  any(vapply(injection_patterns, function(p) grepl(p, text, perl = TRUE), logical(1)))
}

# Phrase patterns for questions (regular expressions on lower-case text). They
# complement registry keywords with common ways of naming outcome types,
# repeated measurements and tasks; they only propose capabilities to look up.
ask_patterns <- list(
  outcome_family = list(
    time_to_event = c("time (until|to|till) (the )?(death|relapse|event|failure|recurrence|progression|discharge)",
      "lost to follow", "censor", "zeit bis (zum|zur)", "zensiert"),
    binary = c("yes( or |/| vs\\.? )no", "\\bwhether\\b", "readmi", "dichotom", "ja( oder |/)nein",
      "eingetreten ist oder nicht", "occurred or not"),
    continuous = c("regress [a-z ]+ on ", "blood pressure", "glucose", "laborwert", "continuous")
  ),
  unit_structure = list(
    longitudinal = c("repeatedly", "each (participant|patient|subject)", "per (participant|patient|subject)",
      "several (visits|time ?points)", "over (the )?(visits|follow-up)", "wiederholt", "pro (patient|teilnehmer)")
  ),
  capability = list(
    std.describe.descrtab2 = c("overview table", "demographic", "characteristics", "summary of the (sample|population|patients|cohort)",
      "\u00fcbersichtstabelle", "uebersichtstabelle", "patientenmerkmale", "merkmale der"),
    std.import.delimited = c("(read|load|import)( in)? [a-z ]*(csv|tsv|delimited|text file)", "(csv|tsv)",
      "einlesen", "importieren"),
    std.figures.accessible = c("greyscale", "grayscale", "\\bchart\\b", "\\bplot", "visuali[sz]", "graustufen")
  )
)

ask_pattern_hit <- function(text, patterns) any(vapply(patterns, function(p) grepl(p, text, perl = TRUE), logical(1)))

ask_signals <- function(text, registry) {
  signals <- infer_goal(text, registry)
  for (field in c("outcome_family", "unit_structure")) {
    if (!identical(signals[[field]], "unknown")) next
    hits <- names(Filter(function(patterns) ask_pattern_hit(text, patterns), ask_patterns[[field]]))
    if (length(hits) == 1L) signals[[field]] <- hits
  }
  if (event_outcome_veto(text, registry, signals$outcome_family)) signals$outcome_family <- "unknown"
  if (identical(signals$aim, "unknown") && !identical(signals$outcome_family, "unknown")) signals$aim <- "explanatory"
  signals
}

# Method families without a reviewed adapter (EN and DE patterns on lower-case
# text). A question naming one gets a precise gap that names the family, the
# registry's candidate capabilities and, as context only, the nearest reviewed
# capabilities; no model code is returned for it.
ask_unsupported_methods <- list(
  competing_risks = list(label = "competing risks (Fine-Gray subdistribution hazards, cumulative incidence)",
    patterns = c("\\bcompeting[- ](risk|event)", "\\bfine[- ](and[- ])?gray\\b", "\\bcumulative incidence",
      "\\bsubdistribution", "\\bkonkurrierende[nr]? (risik|ereignis)", "\\bkumulative[nr]? inzidenz"),
    candidates = character(),
    nearest = c(std.model.coxph = "cause-specific hazards only; competing events are not handled")),
  ordinal_multinomial = list(label = "ordinal or multinomial regression (proportional odds, cumulative link)",
    patterns = c("\\bordinal(e[nmrs]?)? (logistic|logit|probit|regression|model|outcome|endpoint|response)",
      "\\bproportional(e)?[- ]odds", "\\bcumulative (link|logit)", "\\bpolr\\b", "\\bmultinomial", "\\bpolytom",
      "\\bordinale[nrs]? (regression|logistisch|modell|endpunkt|zielgr)", "\\bmehrkategorial"),
    candidates = character(),
    nearest = c(std.model.glm_binomial = "only for a genuinely binary outcome; collapsing categories discards information")),
  gee = list(label = "generalized estimating equations (GEE)",
    patterns = c("\\bgee\\b", "\\bgenerali[sz]ed estimating equation", "\\bgeeglm\\b", "\\bgeepack\\b",
      "\\b(verallgemeinerte|generalisierte)n? sch(\u00e4|ae)tzgleichung"),
    candidates = "std.model.lme4",
    nearest = c(std.model.lme = "subject-specific random-intercept model for continuous outcomes only")),
  count_models = list(label = "count and rate models (Poisson, negative binomial, zero-inflated)",
    patterns = c("\\bpoisson", "\\bnegative[- ]binomial", "\\bzero[- ]inflat", "\\bhurdle model", "\\bquasi-?poisson",
      "\\bcount (model|outcome|regression|data)", "\\b(incidence )?rate ratio", "\\boverdispers",
      "\\bnegativ[- ]binomial", "\\bnull[- ]?inflat", "z(\u00e4|ae)hldaten", "\\binzidenzratenverh"),
    candidates = "std.model.glm_count", nearest = character()),
  quantile_regression = list(label = "quantile regression",
    patterns = c("\\bquantile regression", "\\bmedian regression", "\\bquantreg\\b", "\\bquantils?regression",
      "\\bmedian-?regression"),
    candidates = character(), nearest = c(std.model.lm = "models the conditional mean, not quantiles")),
  propensity_weighting = list(label = "propensity scores, matching and inverse probability weighting",
    patterns = c("\\bpropensity", "\\binverse[- ]probability", "\\biptw\\b", "\\bipw\\b", "\\baipw\\b",
      "\\b(score|case|exact|nearest[- ]neighbou?r|optimal|coarsened) matching", "\\bmatchit\\b",
      "\\bmarginal structural", "\\bdoubly[- ]robust", "\\bg-?computation", "\\bg-?formula",
      "\\binverse wahrscheinlichkeit", "\\bneigungs(score|wert)"),
    candidates = character(), nearest = character()),
  bayesian = list(label = "Bayesian models",
    patterns = c("\\bbayes", "\\bbrms\\b", "\\brstanarm\\b", "\\bstan\\b", "\\bmcmc\\b", "\\binformative prior",
      "\\bprior distribution", "\\bposterior (distribution|probabilit|predictive|draw|sample)", "\\ba[- ]posteriori"),
    candidates = "std.model.bayesian",
    nearest = c(std.model.lm = "frequentist fit without priors", std.model.glm_binomial = "frequentist fit without priors",
      std.model.lme = "frequentist fit without priors", std.model.coxph = "frequentist fit without priors")),
  additive_models = list(label = "generalized additive models (smooth terms)",
    patterns = c("\\bgenerali[sz]ed additive", "\\badditive models?\\b", "\\bgam\\b", "\\bmgcv\\b",
      "\\bsmooth(ing)? (term|spline|function)", "\\bpenali[sz]ed (regression )?splines?",
      "\\bgeneralisierte[s]? additive", "\\badditive[s]? modell"),
    candidates = "std.model.gam", nearest = c(std.model.lm = "linear terms only")),
  machine_learning = list(label = "machine learning and prediction models",
    patterns = c("\\bmachine learning", "\\brandom (survival )?forest", "\\bsurvival forest", "\\bgradient[- ]boost",
      "\\bboost(ed|ing)\\b", "\\bxgboost\\b", "\\blightgbm\\b", "\\bneural net", "\\bdeep learning", "\\bsupport vector",
      "\\bsvm\\b", "\\blasso\\b", "\\belastic[- ]net", "\\bridge regression", "\\btidymodels\\b", "\\bclassifier",
      "\\b(prediction|predictive|prognostic|risk prediction) models?\\b", "\\brisk score",
      "\\bmaschinelle[sn]? lernen", "\\bneuronale[sn]? netz", "(vorhersage|prognose)modell", "\\bklassifikator"),
    candidates = c("std.prediction.tidymodels", "std.model.tree_ensembles"), nearest = character()),
  meta_analysis = list(label = "meta-analysis and evidence synthesis",
    patterns = c("\\bmeta-?analy", "\\bnetwork meta", "\\bmetafor\\b", "\\bpooled (effect|estimate)",
      "metaanaly", "\\bnetzwerk-?meta"),
    candidates = "std.synthesis.meta", nearest = character())
)

ask_unsupported <- function(text) {
  hits <- vapply(ask_unsupported_methods, function(m) {
    any(vapply(m$patterns, function(p) affirmed_hit(text, p), logical(1)))
  }, logical(1))
  names(ask_unsupported_methods)[hits]
}

ask_unsupported_gap <- function(id) {
  m <- ask_unsupported_methods[[id]]
  candidates <- if (length(m$candidates)) paste0("; registry candidates: ", paste(m$candidates, collapse = ", ")) else ""
  nearest <- if (length(m$nearest)) {
    paste0("; nearest reviewed alternatives (context only, not a substitute): ",
      paste0(names(m$nearest), " (", m$nearest, ")", collapse = ", "))
  } else {
    "; no reviewed alternative covers this method"
  }
  paste0("unsupported_method:", id, " (", m$label, "): no reviewed adapter in this release, so no code is returned",
    candidates, nearest)
}

ask_snippets <- function() {
  doc <- read_document(resource_file("extdata", "ask-snippets.json"))
  stats::setNames(doc$snippets, vapply(doc$snippets, function(x) x$capability, character(1)))
}

# Engine capability implied by explicit design words in the question.
ask_engine_capability <- function(signals) {
  analysis <- list(aim = "explanatory", outcome_family = signals$outcome_family, unit_structure = signals$unit_structure)
  if (identical(signals$unit_structure, "unknown") && !identical(signals$outcome_family, "unknown")) {
    analysis$unit_structure <- "independent"
  }
  # Repeated/clustered designs without a stated outcome type are suggested the
  # continuous mixed model; the answer lists the outcome type as a prerequisite.
  if (isTRUE(signals$unit_structure %in% c("longitudinal", "clustered")) && identical(signals$outcome_family, "unknown")) {
    analysis$outcome_family <- "continuous"
  }
  engine <- tryCatch(analysis_configuration(list(analysis = analysis))$candidate_engine, error = function(e) NULL)
  if (is.null(engine)) NULL else engine_capability[[engine]]
}

ask_matches <- function(text, registry, signals, methods = character()) {
  ids <- character()
  for (cap in registry$capabilities) {
    if (keyword_hit(text, cap$keywords) || grepl(tolower(cap$title), text, fixed = TRUE)) ids <- c(ids, cap$id)
  }
  for (id in names(ask_patterns$capability)) {
    if (ask_pattern_hit(text, ask_patterns$capability[[id]])) ids <- c(ids, id)
  }
  ids <- c(ids, unlist(lapply(ask_unsupported_methods[methods], function(m) m$candidates), use.names = FALSE))
  # A named but unsupported method must not be answered with a nearby engine,
  # whether the engine is implied by design words or named by a keyword.
  unsupported_method <- length(methods) > 0L || any(vapply(ids, function(id) {
    cap <- registry$capabilities[[id]]
    identical(cap$stage, "model") && identical(cap$status, "candidate")
  }, logical(1)))
  if (length(methods)) {
    ids <- Filter(function(id) {
      cap <- registry$capabilities[[id]]
      !(cap$stage %in% c("model", "effects") && identical(cap$status, "adapter_tested"))
    }, ids)
  }
  engine <- if (unsupported_method) NULL else ask_engine_capability(signals)
  if (!is.null(engine)) ids <- c(ids, engine)
  model_ids <- c("std.model.lm", "std.model.glm_binomial", "std.model.lme", "std.model.coxph")
  if (any(ids %in% model_ids)) ids <- c(ids, "std.effects.broom")
  requested <- unique(unlist(ids, use.names = FALSE))
  # Capabilities for the detected data modality are context, not the request.
  if (!signals$modality %in% c("unknown", "tabular")) {
    for (cap in registry$capabilities) {
      if (signals$modality %in% cap$applies$modality && !identical(cap$family, "standard")) ids <- c(ids, cap$id)
    }
  }
  structure(unique(unlist(ids, use.names = FALSE)), requested = requested)
}

.ask_export_cache <- new.env(parent = emptyenv())

# Export name -> packages exporting it, per catalog revision.
ask_export_index <- function(catalog) {
  key <- if (is.null(catalog$content_id)) "unidentified" else catalog$content_id
  if (!is.null(.ask_export_cache[[key]])) return(.ask_export_cache[[key]])
  owners <- list()
  for (p in catalog$packages) {
    for (entry in p$exports) {
      if (identical(entry$kind, "reexport")) next
      owners[[entry$name]] <- c(owners[[entry$name]], p$name)
    }
  }
  if (length(ls(.ask_export_cache)) >= 2L) rm(list = ls(.ask_export_cache), envir = .ask_export_cache)
  assign(key, owners, envir = .ask_export_cache)
  owners
}

# Function names written without a namespace: identifiers followed by "()" or
# containing an underscore, a dot or an inner capital (FindClusters, t.test).
# Known exports become exact symbols; an unknown name written as a call is
# reported absent. Base R functions and package names are not function claims.
ask_function_names <- function(question, catalog) {
  tokens <- regmatches(question, gregexpr("(?<![[:alnum:]._:])[A-Za-z][A-Za-z0-9._]*(\\(\\))?", question, perl = TRUE))[[1]]
  called <- endsWith(tokens, "()")
  words <- sub("[.]+$", "", sub("\\(\\)$", "", tokens))
  keep <- (called | grepl("_|[.]|[a-z][A-Z]", words)) & nzchar(words)
  packages <- vapply(catalog$packages, function(p) p$name, character(1))
  base <- vapply(words, function(w) exists(w, envir = baseenv(), inherits = FALSE), logical(1))
  keep <- keep & !words %in% packages & !base
  owners <- ask_export_index(catalog)
  symbols <- character()
  absent <- character()
  for (i in which(keep)) {
    found <- owners[[words[[i]]]]
    if (length(found)) {
      symbols <- c(symbols, paste0(found, "::", words[[i]]))
    } else if (called[[i]]) {
      absent <- c(absent, words[[i]])
    }
  }
  list(symbols = unique(symbols), absent = unique(absent))
}

ask_symbol <- function(symbol, catalog, verified_only) {
  parts <- strsplit(symbol, ":{2,3}")[[1]]
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  validation <- validate_generated_code(symbol, catalog, approved_only = verified_only)
  package <- index[[parts[[1]]]]
  entry <- if (is.null(package)) NULL else Filter(function(x) identical(x$name, parts[[2]]), package$exports)
  list(symbol = symbol, found = length(entry) > 0L, package = parts[[1]], export = parts[[2]],
    status = validation$status[[1]], reason = validation$reason[[1]],
    signature = if (length(entry)) entry[[1]]$signature else NA_character_,
    revision = if (is.null(package)) NA_character_ else package$revision,
    citation = if (length(entry) && !is.null(entry[[1]]$documentation)) catalog_evidence_url(package, entry[[1]]$documentation$path) else NA_character_)
}

ask_evidence <- function(validation, catalog, adapter = NULL) {
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  rows <- validation[!is.na(validation$package) & validation$package != "base" & validation$status == "ok", , drop = FALSE]
  out <- data.frame(id = character(), package = character(), version = character(), revision = character(),
    export = character(), verification = character(), approval = character(), citation = character(),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(rows))) {
    package <- index[[rows$package[[i]]]]
    entry <- Filter(function(x) identical(x$name, rows$export[[i]]), package$exports)[[1]]
    owner <- sub("^workflow_approved_reexport:", "", rows$reason[[i]])
    approving <- if (startsWith(rows$reason[[i]], "workflow_approved_reexport:")) index[[owner]] else package
    approvals <- approved_export_index(approving)[[entry$name]]
    role <- if (is.null(adapter)) NULL else sub("^[a-z]+[.]", "", adapter)
    preferred <- Filter(function(a) identical(a$role, role), approvals)
    if (length(preferred)) approvals <- preferred
    doc <- entry$documentation
    out[nrow(out) + 1L, ] <- list(content_hash(paste(package$source_hash, entry$name)), package$name, package$version,
      package$revision, entry$name, rows$reason[[i]],
      if (length(approvals)) approvals[[1]]$approval_id else NA_character_,
      if (is.null(doc)) NA_character_ else catalog_evidence_url(package, doc$path))
  }
  unique(out)
}

ask_prerequisites <- function(id) {
  mapping <- switch(id,
    std.model.lm = c("outcome", "predictors", "estimand", "missing_data"),
    std.model.glm_binomial = c("outcome", "predictors", "event_value", "non_event_value", "estimand", "missing_data"),
    std.model.lme = c("outcome (continuous)", "predictors", "subject", "time (longitudinal)", "estimand", "missing_data"),
    std.model.coxph = c("time", "event", "event_value", "non_event_value", "time_origin", "time_unit", "predictors"),
    character())
  if (length(mapping)) paste0("Map analysis$mapping fields: ", paste(mapping, collapse = ", "), ".") else character()
}

#' Ask for grounded workflow advice
#'
#' Reads the question as untrusted text and matches it to registered workflow
#' capabilities and exact `package::export` names in the pinned catalog
#' revision. Supported capabilities return ordered steps, prerequisites, the
#' approved package revisions and a reviewed, namespaced code snippet that is
#' validated against the approved function table before it is returned. Nothing
#' is executed, no model is called and no question text leaves the machine.
#' Unsupported procedures return a precise gap and the nearest approved
#' alternatives instead of invented code. Method families without a reviewed
#' adapter (competing risks, ordinal or multinomial models, GEE, count models,
#' quantile regression, propensity scores and weighting, Bayesian models,
#' generalized additive models, machine learning and meta-analysis) are named in
#' the gap and never answered with a nearby engine; such a question gets no
#' code for any part of it. A question that names an absent or unapproved
#' function gets no code. Instruction-shaped text,
#' including lookalike letters and override markers, is treated as data.
#' @param question Nonempty question, optionally naming `package::export`.
#' @param path Optional exact project root selecting its pinned catalog.
#' @param verified_only If `TRUE`, every returned API claim and code call must be
#'   covered by a complete workflow approval of the pinned revision. If `FALSE`,
#'   statically verified but unapproved APIs and candidate capabilities may be
#'   listed, clearly labelled; nonexistent exports are never presented as real.
#' @return A `cttir_answer` with `answer`, `steps`, `prerequisites`, `packages`,
#'   `code`, `citations`, `verification_levels`, `evidence`, `symbols`, `gaps`,
#'   `alternatives` (approved capabilities offered only as context when a named
#'   function does not exist or the method family is unsupported) and
#'   `limitations`. Citations name the pinned
#'   revision (versioned source archive or commit). Code uses placeholders
#'   (`<...>`) for mapped data and is illustrative; generated projects use the
#'   full reviewed stage library.
#' @export
ask <- function(question, path = NULL, verified_only = TRUE) {
  question <- utf8_input(question)
  scalar_text(question, "question")
  scalar_flag(verified_only, "verified_only")
  catalog <- resolve_catalog(path)
  registry <- capability_registry()
  symbol_pattern <- "[A-Za-z][A-Za-z0-9.]*:{2,3}[A-Za-z._][A-Za-z0-9._]*"
  # Package names inside exact symbols must not trigger capability keywords.
  unqualified <- gsub(symbol_pattern, " ", question)
  text <- tolower(enc2utf8(unqualified))
  injected <- instruction_like(unqualified)
  signals <- if (injected) {
    list(aim = "unknown", outcome_family = "unknown", unit_structure = "unknown", modality = "unknown")
  } else {
    ask_signals(text, registry)
  }
  methods <- if (injected) character() else ask_unsupported(text)
  named <- ask_function_names(unqualified, catalog)
  symbols <- unique(regmatches(question, gregexpr(symbol_pattern, question))[[1]])
  symbol_rows <- lapply(symbols, ask_symbol, catalog = catalog, verified_only = verified_only)
  # Bare names are reported only when they are not usable at this revision.
  for (row in lapply(setdiff(named$symbols, symbols), ask_symbol, catalog = catalog, verified_only = verified_only)) {
    if (!identical(row$status, "ok")) symbol_rows[[length(symbol_rows) + 1L]] <- row
  }
  for (name in named$absent) {
    symbol_rows[[length(symbol_rows) + 1L]] <- list(symbol = paste0(name, "()"), found = FALSE, package = NA_character_,
      export = name, status = "rejected", reason = "export_absent_from_catalog_revision", signature = NA_character_,
      revision = NA_character_, citation = NA_character_)
  }
  # A request that names an absent or unapproved function gets no code at all;
  # when the function does not exist, matched capabilities are only the nearest
  # alternatives, not support for what was asked.
  blocked <- any(vapply(symbol_rows, function(x) !identical(x$status, "ok"), logical(1)))
  absent <- any(vapply(symbol_rows, function(x) !isTRUE(x$found), logical(1)))
  ids <- if (injected) character() else ask_matches(text, registry, signals, methods)
  requested <- as.character(attr(ids, "requested"))
  ids <- as.character(ids)
  routed <- lapply(ids, function(id) route_stage(registry, catalog, registry$capabilities[[id]]$stage, id))
  approved <- Filter(function(x) identical(x$status, "approved"), routed)
  pending <- Filter(function(x) !identical(x$status, "approved"), routed)
  order <- order(match(vapply(approved, function(x) x$stage, character(1)), ask_stage_order), na.last = TRUE)
  approved <- approved[order]
  # A question whose method family has no reviewed adapter gets no snippet for
  # any part of it either; matched capabilities are only nearby context.
  withheld <- absent || length(methods) > 0L
  alternatives <- if (withheld) vapply(approved, function(x) x$capability, character(1)) else character()
  if (withheld) approved <- list()
  snippets <- ask_snippets()
  code_blocks <- character()
  evidence <- ask_evidence(data.frame(package = character(), status = character(), stringsAsFactors = FALSE), catalog)
  limitations <- c("Snippets are illustrative and use placeholders; review mappings, assumptions and diagnostics before use.",
    "Approval covers the pinned package revisions and adapter, not a scientific conclusion.",
    "No local model is used: the tested local models did not qualify for planning (see setup()).")
  for (stage in if (blocked) list() else approved) {
    snippet <- snippets[[stage$capability]]
    if (is.null(snippet)) next
    validation <- validate_generated_code(snippet$code, catalog, approved_only = verified_only)
    if (isTRUE(attr(validation, "valid"))) {
      code_blocks <- c(code_blocks, snippet$code)
      evidence <- rbind(evidence, ask_evidence(validation, catalog, registry$capabilities[[stage$capability]]$adapter$id))
    } else {
      limitations <- c(limitations, paste0("The snippet for ", stage$capability, " did not validate against this revision and was withheld."))
    }
  }
  if (!verified_only && !blocked && !length(methods)) {
    for (stage in pending) {
      snippet <- snippets[[stage$capability]]
      if (is.null(snippet)) next
      validation <- validate_generated_code(snippet$code, catalog, approved_only = FALSE)
      if (isTRUE(attr(validation, "valid"))) {
        code_blocks <- c(code_blocks, paste0("# UNAPPROVED (statically verified only)\n", snippet$code))
        evidence <- rbind(evidence, ask_evidence(validation, catalog))
      }
    }
  }
  for (row in symbol_rows) {
    if (identical(row$status, "ok")) {
      evidence[nrow(evidence) + 1L, ] <- list(content_hash(paste(row$revision, row$symbol)), row$package, NA_character_,
        row$revision, row$export, row$reason, NA_character_, row$citation)
    }
  }
  if (!verified_only) {
    # Unverified mode also cites literal documentation excerpts, labelled as such.
    documents <- search(question, path = path, limit = 10L)
    documents <- documents[documents$kind == "document", , drop = FALSE]
    for (i in seq_len(nrow(documents))) {
      evidence[nrow(evidence) + 1L, ] <- list(documents$id[[i]], documents$package[[i]], NA_character_,
        documents$revision[[i]], NA_character_, "documentation_indexed", NA_character_, documents$evidence[[i]])
    }
  }
  evidence <- unique(evidence)
  gaps <- vapply(methods, ask_unsupported_gap, character(1), USE.NAMES = FALSE)
  for (stage in pending) {
    cap <- registry$capabilities[[stage$capability]]
    same_stage <- Filter(function(x) {
      identical(x$stage, cap$stage) && identical(x$status, "adapter_tested") &&
        identical(capability_approval(x, catalog)$status, "approved")
    }, registry$capabilities)
    nearby <- vapply(same_stage, function(x) x$id, character(1))
    nearest <- if (length(nearby)) paste0("; nearest approved alternatives: ", paste(nearby, collapse = ", ")) else ""
    unapproved <- unlist(stage$unapproved)
    callables <- unapproved[grepl("::", unapproved, fixed = TRUE)]
    missing <- if (length(callables)) paste0(" (unapproved callables: ", paste(callables, collapse = ", "), ")") else ""
    gaps <- c(gaps, paste0(stage$capability, " (", cap$title, "): ", stage$status, missing, nearest))
  }
  for (row in symbol_rows) {
    if (!identical(row$status, "ok")) gaps <- c(gaps, paste0(row$symbol, ": ", if (row$found) row$reason else "not in the pinned catalog revision"))
  }
  if (injected) limitations <- c(limitations, "Instruction-like text in the question was treated as data; no capability was inferred from it.")
  if (blocked) {
    limitations <- c(limitations, "The question names a function that is absent or not approved at the pinned revision, so no code is returned for this request.")
  }
  palette <- if (any(vapply(approved, function(x) identical(x$capability, "std.figures.accessible"), logical(1)))) {
    ask_palette_note(text)
  } else {
    character()
  }
  limitations <- unique(c(limitations, palette))
  steps <- vapply(approved, function(x) {
    cap <- registry$capabilities[[x$capability]]
    paste0(x$stage, ": ", cap$title, " [", x$capability, "; ", paste(unlist(x$packages), collapse = ", "), "]")
  }, character(1))
  requirements <- lapply(approved, function(x) unlist(registry$capabilities[[x$capability]]$requirements))
  mapping_needs <- lapply(approved, function(x) ask_prerequisites(x$capability))
  prerequisites <- unique(unlist(c(requirements, mapping_needs)))
  index <- stats::setNames(catalog$packages, vapply(catalog$packages, function(p) p$name, character(1)))
  used <- unique(unlist(lapply(approved, function(x) setdiff(unlist(x$packages), "base"))))
  packages <- data.frame(package = used,
    version = vapply(used, function(n) if (is.null(index[[n]])) NA_character_ else index[[n]]$version, character(1)),
    revision = vapply(used, function(n) if (is.null(index[[n]])) NA_character_ else index[[n]]$revision, character(1)),
    stringsAsFactors = FALSE, row.names = NULL)
  supported <- paste(vapply(approved, function(x) x$capability, character(1)), collapse = ", ")
  # Approved infrastructure or data-type context does not answer a request for
  # an analysis that itself has no approved adapter.
  analyses <- Filter(function(id) !isTRUE(registry$capabilities[[id]]$infrastructure), requested)
  approved_ids <- vapply(approved, function(x) x$capability, character(1))
  unanswered <- length(analyses) > 0L && !any(analyses %in% approved_ids)
  answer <- if (length(methods)) {
    paste0("Unsupported method: ", paste(vapply(ask_unsupported_methods[methods], function(m) m$label, character(1)), collapse = "; "),
      ". No reviewed adapter exists, so no code is returned for it; see gaps for registry candidates and the nearest ",
      "reviewed alternatives (context only).", if (length(approved)) paste0(" Other steps have approved adapters: ", supported, "."))
  } else if (absent) {
    paste0("A named function is not in the pinned catalog revision, so no code is returned; see gaps.",
      if (length(alternatives)) paste0(" Nearest approved alternatives (context only): ", paste(alternatives, collapse = ", "), "."))
  } else if (unanswered) {
    paste0("No approved adapter covers the requested analysis (", paste(setdiff(analyses, approved_ids), collapse = ", "),
      "); see gaps.", if (length(approved)) paste0(" Approved adapters for related steps or this data type (context only): ",
        supported, "."))
  } else if (length(approved) && blocked) {
    paste0("Approved adapters exist for: ", supported, ", but a named function is absent or not approved at the pinned ",
      "revision; see gaps. No code is returned.")
  } else if (length(approved)) {
    paste0("Supported with approved adapters: ", supported, ".")
  } else if (length(symbol_rows) && any(vapply(symbol_rows, function(x) identical(x$status, "ok"), logical(1)))) {
    "The named API is present in the pinned revision; see evidence for its verification level."
  } else if (length(pending) || blocked) {
    "No approved adapter covers this request; see gaps for candidates and the nearest approved alternatives."
  } else {
    "No supported capability or exact API matched. Ask about import, checks, tidy roles, descriptive tables, figures, lm/glm/mixed/Cox models, effects, reports or pipelines."
  }
  if (length(palette) > 1L) answer <- paste(answer, palette[[2]])
  structure(list(
    answer = answer, steps = as.list(steps), prerequisites = as.list(prerequisites), packages = packages,
    code = paste(code_blocks, collapse = "\n\n"), citations = as.list(unique(stats::na.omit(evidence$citation))),
    verification_levels = unique(evidence$verification), evidence = evidence,
    symbols = symbol_rows, gaps = as.list(gaps), capabilities = as.list(ids),
    approved_capabilities = as.list(vapply(approved, function(x) x$capability, character(1))),
    alternatives = as.list(alternatives), limitations = limitations, catalog_id = catalog$content_id
  ), class = "cttir_answer")
}

# Qualitative palette capacity from the pinned RColorBrewer table, stated with
# the figure policy; a question naming more groups than the palette holds is
# told so explicitly.
ask_palette_note <- function(text) {
  table <- brewer_palette_table()
  name <- figure_policy_defaults()$categorical_palette
  limit <- table$maxcolors[table$name == name]
  policy <- paste0("The colour-blind-flagged qualitative palette ", name, " holds at most ", limit, " colours in RColorBrewer ",
    brewer_pin$version, "; qualitative palettes are never interpolated or recycled, so more than ", limit,
    " categories need a redundant non-colour encoding (shape, line type, direct labels) or labelled facets.")
  pattern <- "\\b[0-9]+(?=\\s*(groups?|categor|colou?rs?|levels|classes|clusters|gruppen|kategorien|farben|stufen))"
  wanted <- max(c(0, as.numeric(regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]])))
  if (wanted <= limit) return(policy)
  advice <- paste0("The question asks for ", wanted, " categories, more than the ", limit, " colours of ", name,
    "; use labelled facets or direct labels instead of more colours.")
  c(policy, advice)
}

#' @export
print.cttir_answer <- function(x, ...) {
  cat(x$answer, "\n")
  if (length(x$steps)) cat(paste0("- ", unlist(x$steps), collapse = "\n"), "\n")
  if (length(x$gaps)) cat("Gaps:\n", paste0("- ", unlist(x$gaps), collapse = "\n"), "\n")
  if (nzchar(x$code)) cat("\n", x$code, "\n", sep = "")
  if (length(x$citations)) cat(length(x$citations), "citations; see $evidence\n")
  invisible(x)
}
