# The external declaration is the trust boundary. Project files cannot supply it.
study_require <- function() {
  for (package in c("reflowR", "workflowr", "callr", "git2r"))
    if (!requireNamespace(package, quietly = TRUE)) study_fail(paste("Provision the external R library first:", package))
}

study_repository <- function(directory) {
  root <- normalizePath(directory, mustWork = TRUE)
  metadata <- file.path(root, ".git")
  assert_plain_path(metadata)
  if (!dir.exists(metadata)) study_fail("A study-owned Git repository is required; ancestor discovery is forbidden.")
  repo <- tryCatch(git2r::repository(root, discover = FALSE), error = function(e) NULL)
  if (is.null(repo) || !identical(normalizePath(git2r::workdir(repo), mustWork = TRUE), root))
    study_fail("Git repository work directory must equal the study root.")
  invisible(TRUE)
}

study_report_git_status <- function(directory, files) {
  study_repository(directory)
  repo <- git2r::repository(directory, discover = FALSE)
  status <- git2r::status(repo, ignored = TRUE)
  explicit <- unlist(status, use.names = FALSE)
  head <- tryCatch(git2r::ls_tree(repo = repo), error = function(e) NULL)
  committed <- if (is.null(head)) character() else paste0(head$path[head$type == "blob"], head$name[head$type == "blob"])
  if (!all(files %in% c(explicit, committed)))
    study_fail("Report source Git status is ambiguous; stage the generated analysis Rmd files before rendering.")
  invisible(TRUE)
}

study_json <- function(path) jsonlite::read_json(path, simplifyVector = FALSE)
study_write_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_bytes(paste0(json_text(value, pretty = TRUE), "\n"), path)
}

study_inventory <- function(root) {
  assert_plain_path(root)
  visit <- function(path) {
    type <- as.character(fs::file_info(path, follow = FALSE)$type)
    if (!type %in% c("file", "directory")) study_fail("Only regular files and directories are admitted.")
    if (type == "file") return(path)
    unlist(lapply(list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE), visit), use.names = FALSE)
  }
  paths <- visit(root)
  paths <- sort(paths, method = "radix")
  stats::setNames(lapply(paths, file_hash), substring(paths, nchar(root) + 2L))
}

study_installed_runtime <- function() {
  root <- find.package("cttiR", quiet = TRUE)
  if (length(root) != 1L || !file.exists(file.path(root, "Meta/package.rds")) ||
      !identical(normalizePath(getNamespaceInfo("cttiR", "path")), normalizePath(root)))
    study_fail("Compile with an installed cttiR package in a prepared external library.")
  list(package = "cttiR", version = as.character(utils::packageVersion("cttiR")),
       writer = "study_stage", files = study_inventory(root))
}

#' Snapshot the Prepared Study Environment
#' @return A dependency lock containing the installed R and transitive package
#'   versions. This only reads the external library; it installs nothing and
#'   does not assert a download origin or provide a restoration manifest.
#' @export
study_dependency_lock <- function() {
  study_require()
  installed <- utils::installed.packages()
  roots <- c("cttiR", "reflowR", "workflowr", "callr", "git2r")
  dependencies <- tools::package_dependencies(roots, db = installed,
    which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)
  packages <- sort(unique(c(roots, unlist(dependencies, use.names = FALSE))))
  packages <- setdiff(packages, "R")
  if (!all(packages %in% rownames(installed))) study_fail("The prepared dependency closure is incomplete.")
  list(R = list(Version = as.character(getRversion())), Packages = stats::setNames(lapply(packages,
    function(p) list(Package = p, Version = as.character(utils::packageVersion(p)))), packages))
}

study_environment <- function(lock) {
  if (!identical(as.character(getRversion()), lock$R$Version)) study_fail("R version differs from the external lock.")
  required <- names(study_dependency_lock()$Packages)
  if (!all(required %in% names(lock$Packages)))
    study_fail("The external lock must include the complete installed study dependency closure.")
  for (p in names(lock$Packages)) {
    version <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "unavailable")
    if (!identical(version, lock$Packages[[p]]$Version)) study_fail(paste("Dependency version differs:", p))
  }
  invisible(TRUE)
}

#' Compile a Synthetic Study into a Strict Workflow Project
#' @inheritParams study_validate
#' @param directory New project directory whose parent already exists.
#' @param dependency_lock An externally prepared dependency lock, for example
#'   from [study_dependency_lock()]. No packages are restored automatically.
#' @param author Plain-text author name.
#' @param seed Fixed nonnegative integer seed for the workflow.
#' @return A list with directory and an external trusted declaration. Retain the
#'   declaration outside the project and pass successive updated declarations to
#'   [study_run()], [study_render()] and [study_validate_project()]. Creation
#'   initializes a study-owned Git repository and stages only generated analysis
#'   Rmd files. It does not commit, publish or approve a real study.
#' @export
study_project <- function(directory, spec, data, dependency_lock, author, units, seed = 1L) {
  study_require(); study_validate(spec, data, units); study_environment(dependency_lock)
  runtime <- study_installed_runtime()
  if (!all(c("cttiR", "reflowR", "workflowr", "callr", "fs") %in% names(dependency_lock$Packages)))
    study_fail("The lock must include the study compiler and rendering runtime.")
  root <- as.character(fs::path_abs(directory))
  assert_plain_path(root)
  if (file.exists(root) || dir.exists(root) || !dir.exists(dirname(root)))
    study_fail("Use a new project path with an existing parent directory.")
  layout <- study_data_layout(spec)
  variables <- layout$variables
  columns <- layout$columns
  projected <- data[, columns, drop = FALSE]
  projected[[layout$id]] <- as.character(projected[[layout$id]])
  scratch <- tempfile("study-input-", tmpdir = tempdir(), fileext = ".csv")
  on.exit(unlink(scratch), add = TRUE)
  old <- options(digits = 17L); on.exit(options(old), add = TRUE)
  utils::write.csv(projected, scratch, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  classes <- layout$classes
  paired <- study_is_paired(spec)
  recipe <- list(schema_version = 1L, id = spec$recipe_id,
    engine = if (paired) "stats::t.test" else "stats::lm",
    interpretation = if (paired) "noncausal_paired_mean_change" else "noncausal_association",
    input = list(path = "data/raw/observations.csv",
      sha256 = file_hash(scratch), rows = nrow(data), columns = as.list(columns), classes = as.list(classes),
      units = as.list(units), projection = "Only the declared ID and analysis columns are copied; no rows are excluded."),
    outputs = as.list(c("output/analysis/results.json", "output/analysis/coefficients.csv",
      "output/analysis/diagnostics.csv", "output/analysis/descriptives.csv", "output/analysis/frequencies.csv",
      "output/analysis/diagnostics.png")), scientific_approval = FALSE)
  stages <- list(sensitivity = list(applicable = FALSE,
    reason = "No sensitivity estimand is prespecified for this bounded synthetic recipe."),
    exploratory = list(applicable = FALSE, reason = "Exploratory analyses are outside this prespecified recipe."))
  profile <- reflowR::reflow_profile(spec$research$domain, spec$research$data_origin, spec$research$design,
    seed = seed, stages = stages, profile_version = "strict-cttir-v1", data_access = "synthetic",
    jurisdiction = "Synthetic development fixture; no human participant data",
    legal_basis = "Locally supplied synthetic values; no ethics or legal approval asserted")
  composition <- list(schema_version = 1L, study = spec, recipe = recipe, runtime = runtime)
  compiled <- reflowR::reflow_init_strict(root, profile, spec$name, author, dependency_lock,
    git = TRUE, composition = composition)
  analysis_files <- c("analysis/index.Rmd", vapply(profile$stages, `[[`, "", "analysis"))
  git2r::add(git2r::repository(root, discover = FALSE), analysis_files, force = TRUE)
  raw <- file.path(root, "data/raw")
  Sys.chmod(raw, "0755")
  on.exit(Sys.chmod(raw, "0555"), add = TRUE)
  if (!file.copy(scratch, file.path(root, recipe$input$path))) study_fail("Unable to retain the declared input.")
  Sys.chmod(file.path(root, recipe$input$path), "0444"); Sys.chmod(raw, "0555")
  declaration <- list(schema_version = 1L, structure = compiled$declaration, run = NULL, render = NULL)
  validation <- study_validate_project(root, declaration)
  if (!isTRUE(validation$valid)) study_fail(paste(validation$reason, collapse = "; "))
  list(directory = root, declaration = declaration)
}

study_controls <- c("cttir-study.json", "cttir-recipe.json", "cttir-runtime.json", "renv.lock", "reflow-profile.json")

study_hashes <- function(directory, paths) {
  if (!is.character(paths) || anyNA(paths) || anyDuplicated(paths) ||
      any(!grepl("^[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)*$", paths)) ||
      any(grepl("(^|/)\\.\\.?(/|$)", paths))) study_fail("Invalid study artifact paths.")
  stats::setNames(lapply(paths, function(p) {
    full <- file.path(directory, p); assert_plain_path(full)
    if (!identical(as.character(fs::file_info(full, follow = FALSE)$type), "file")) study_fail(paste("Missing regular file:", p))
    file_hash(full)
  }), paths)
}

study_assert_run <- function(directory, declaration) {
  pin <- declaration$run
  if (is.null(pin)) study_fail("An externally pinned analysis run is required.")
  study_keys(pin, c("path", "sha256"), "External run pin")
  if (!identical(pin$path, "output/provenance/study-run.json") ||
      !identical(study_hashes(directory, pin$path)[[1L]], pin$sha256)) study_fail("Analysis receipt differs from the external pin.")
  receipt <- study_json(file.path(directory, pin$path))
  recipe <- declaration$structure$composition$recipe
  paths <- unlist(recipe$outputs, use.names = FALSE)
  expected <- list(schema_version = 1L, status = "computed_not_scientifically_validated",
    controls = study_hashes(directory, study_controls),
    inputs = study_hashes(directory, recipe$input$path), outputs = study_hashes(directory, paths))
  if (!identical(json_text(receipt), json_text(expected))) study_fail("Analysis inputs, configuration or outputs are stale.")
  invisible(receipt)
}

#' Validate a Compiled Study and Its Externally Pinned Results
#' @param directory Existing study project.
#' @param declaration Trusted external declaration returned by the compiler or
#'   the latest successful run/render. Never obtain this from project files.
#' @param require_run Require current externally pinned numerical results.
#' @param require_render Require current externally pinned HTML and all applicable stages.
#' @return A list with valid, reason, structural validation and scientific_approval
#'   set to FALSE. Stale or unsupported material yields valid = FALSE.
#' @export
study_validate_project <- function(directory, declaration, require_run = FALSE, require_render = FALSE) {
  tryCatch({
    study_require()
    study_keys(declaration, c("schema_version", "structure", "run", "render"), "External study declaration")
    if (!isTRUE(declaration$schema_version == 1)) study_fail("Unsupported external declaration version.")
    structure <- declaration$structure
    if (!identical(structure$profile$profile_version, "strict-cttir-v1")) study_fail("A composed strict profile is required.")
    study_validate(structure$composition$study)
    study_repository(directory)
    study_environment(structure$dependency_lock)
    checked <- reflowR::reflow_validate_strict(directory, structure,
      execution_receipts = if (!is.null(declaration$render)) declaration$render$stages else NULL)
    if (!isTRUE(checked$valid) || !isTRUE(checked$execution_ready)) study_fail("Strict project structure or installed runtime differs from its declaration.")
    recipe <- structure$composition$recipe
    if (!identical(recipe$id, structure$composition$study$recipe_id) || !identical(recipe$input$path, "data/raw/observations.csv") ||
        !identical(study_hashes(directory, recipe$input$path)[[1L]], recipe$input$sha256)) study_fail("Raw input differs from its declared identity.")
    raw <- study_inventory(file.path(as.character(fs::path_abs(directory)), "data/raw"))
    if (!setequal(names(raw), c("README.md", "observations.csv"))) study_fail("Unexpected raw input inventory.")
    if (require_run || require_render || !is.null(declaration$run)) study_assert_run(directory, declaration)
    if (require_render || !is.null(declaration$render)) {
      render <- declaration$render
      if (is.null(render) || !isTRUE(checked$completed)) study_fail("Applicable report stages lack current external receipts.")
      if (!identical(json_text(study_hashes(directory, names(render$files))), json_text(render$files)))
        study_fail("Rendered report files differ from the external pins.")
    }
    list(valid = TRUE, reason = "Current declared structure and requested evidence verified.",
      structural = checked, analysis_current = !is.null(declaration$run),
      rendered_current = !is.null(declaration$render), scientific_approval = FALSE)
  }, error = function(e) list(valid = FALSE, reason = conditionMessage(e), scientific_approval = FALSE))
}

study_admit <- function(directory, declaration, require_run = FALSE) {
  checked <- study_validate_project(directory, declaration, require_run = require_run)
  if (!isTRUE(checked$valid)) study_fail(checked$reason, "stale_or_unapproved_project")
  invisible(checked)
}

#' Run the Fixed Synthetic Study Recipe
#' @inheritParams study_validate_project
#' @return A list with directory, numerical result and an updated external
#'   declaration pinning the exact run receipt. Preserve the updated declaration.
#' @export
study_run <- function(directory, declaration) {
  study_admit(directory, declaration)
  if (!is.null(declaration$render)) study_fail("Use a new compiled project for a new analysis after rendering.")
  spec <- declaration$structure$composition$study
  recipe <- declaration$structure$composition$recipe
  before <- study_hashes(directory, c(study_controls, recipe$input$path))
  data <- utils::read.csv(file.path(directory, recipe$input$path), check.names = FALSE,
    colClasses = unlist(recipe$input$classes, use.names = FALSE), stringsAsFactors = FALSE,
    fileEncoding = "UTF-8", na.strings = "")
  if (!identical(names(data), unlist(recipe$input$columns, use.names = FALSE)) || nrow(data) != recipe$input$rows)
    study_fail("Imported shape differs from the compiler declaration.")
  result <- study_analyze(spec, data, recipe$input$units)
  variables <- study_data_layout(spec)$variables
  continuous <- Filter(function(x) x$type == "continuous", variables)
  descriptives <- if (study_is_paired(spec)) do.call(rbind, lapply(c("before", "after", "difference"), function(key) {
    value <- result$diagnostics[[key]]
    data.frame(variable = key, unit = spec$outcome$unit, n = length(value), mean = mean(value),
      sd = stats::sd(value), min = min(value), median = stats::median(value), max = max(value))
  })) else do.call(rbind, lapply(continuous, function(x) {
    value <- data[[x$name]]
    data.frame(variable = x$name, unit = x$unit, n = length(value), mean = mean(value),
      sd = stats::sd(value), min = min(value), median = stats::median(value), max = max(value))
  }))
  frequencies <- data.frame(variable = character(), level = character(), n = integer())
  for (x in Filter(function(x) x$type == "categorical", variables)) {
    levels <- unlist(x$levels, use.names = FALSE)
    frequencies <- rbind(frequencies, data.frame(variable = x$name, level = levels,
      n = vapply(levels, function(level) sum(data[[x$name]] == level), integer(1))))
  }
  output <- file.path(directory, "output/analysis")
  if (dir.exists(output)) study_fail("Analysis output already exists; preserve it and compile a new project for another run.")
  dir.create(output)
  study_write_json(result, file.path(output, "results.json"))
  for (entry in c("coefficients", "diagnostics")) utils::write.csv(result[[entry]], file.path(output, paste0(entry, ".csv")), row.names = FALSE)
  utils::write.csv(descriptives, file.path(output, "descriptives.csv"), row.names = FALSE)
  utils::write.csv(frequencies, file.path(output, "frequencies.csv"), row.names = FALSE)
  grDevices::png(file.path(output, "diagnostics.png"), width = 1200L, height = 600L, res = 120L)
  device <- grDevices::dev.cur(); on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  graphics::par(mfrow = c(1L, 2L), mar = c(4, 4, 3, 1))
  color <- declaration$structure$profile$primary_color
  outcome_label <- paste0(spec$outcome$name, " [", spec$outcome$unit, "]")
  if (study_is_paired(spec)) {
    graphics::plot(seq_len(result$n_pairs), result$diagnostics$difference, pch = 19L, col = color,
      xlab = "Pair index (sorted ID; not time)", ylab = paste("After minus before:", outcome_label),
      main = "Recorded within-pair differences")
    graphics::abline(h = 0, col = "grey50", lty = 2L)
    stats::qqnorm(result$diagnostics$difference, pch = 19L, col = color,
      ylab = paste("After minus before:", outcome_label), main = "Difference normal quantiles")
    stats::qqline(result$diagnostics$difference, col = "grey50", lty = 2L)
  } else {
    graphics::plot(result$diagnostics$fitted, result$diagnostics$residual, pch = 19L, col = color,
      xlab = paste("Fitted", outcome_label), ylab = paste("Residual:", outcome_label), main = "Residuals versus fitted")
    graphics::abline(h = 0, col = "grey50", lty = 2L)
    stats::qqnorm(result$diagnostics$residual, pch = 19L, col = color,
      ylab = paste("Residual:", outcome_label), main = "Residual normal quantiles")
    stats::qqline(result$diagnostics$residual, col = "grey50", lty = 2L)
  }
  grDevices::dev.off(device)
  if (!identical(before, study_hashes(directory, names(before)))) study_fail("Declared inputs changed during computation.")
  study_admit(directory, declaration)
  receipt <- list(schema_version = 1L, status = "computed_not_scientifically_validated",
    controls = study_hashes(directory, study_controls), inputs = study_hashes(directory, recipe$input$path),
    outputs = study_hashes(directory, unlist(recipe$outputs, use.names = FALSE)))
  path <- "output/provenance/study-run.json"
  study_write_json(receipt, file.path(directory, path))
  declaration$run <- list(path = path, sha256 = file_hash(file.path(directory, path)))
  list(directory = as.character(fs::path_abs(directory)), declaration = declaration, result = result)
}

study_html <- function(x) {
  for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;"), c("'", "&#39;")))
    x <- gsub(pair[[1L]], pair[[2L]], x, fixed = TRUE)
  x
}

#' Present a Verified Recorded Study Stage
#' @param stage Fixed stage identifier from the compiled profile.
#' @param directory Study directory. Used by canonical stage files only.
#' @return Invisibly the stage view receipt. Requires the external trusted
#'   declaration installed by [study_render()] in a clean child R session.
#' @export
study_stage <- function(stage, directory = ".") {
  declaration <- getOption("cttir.study.external_declaration")
  if (is.null(declaration)) study_fail("Render through study_render with an external declaration.")
  study_admit(directory, declaration, require_run = TRUE)
  stages <- declaration$structure$profile$stages
  allowed <- vapply(Filter(function(x) x$applicable, stages), `[[`, "", "id")
  if (!is.character(stage) || length(stage) != 1L || !stage %in% allowed) study_fail("Unsupported report stage.")
  spec <- declaration$structure$composition$study
  recipe <- declaration$structure$composition$recipe
  result <- jsonlite::read_json(file.path(directory, "output/analysis/results.json"), simplifyVector = TRUE)
  paragraph <- function(x) cat("<p>", study_html(x), "</p>\n", sep = "")
  table <- function(x) cat(knitr::kable(x, format = "html", escape = TRUE, digits = 6L), "\n")
  paragraph(paste("Synthetic study:", spec$name))
  paragraph(paste("Scientific review:", result$scientific_review))
  if (study_is_paired(spec)) {
    study_paired_stage(stage, spec, recipe, result, directory, declaration, paragraph, table)
  } else {
  if (stage == "protocol") {
    paragraph(spec$question); paragraph(paste("Population:", spec$population))
    paragraph(paste("Estimand:", spec$estimand, "; independent observations; no causal interpretation."))
    paragraph(paste("Outcome:", spec$outcome$name, "[", spec$outcome$unit, "]; exposure:", spec$exposure$name, "[", spec$exposure$unit, "]"))
    if (length(spec$covariates)) table(data.frame(variable = vapply(spec$covariates, `[[`, "", "name"),
      type = vapply(spec$covariates, `[[`, "", "type"), reference_or_unit = vapply(spec$covariates,
        function(x) if (x$type == "continuous") x$unit else x$reference, "")))
    paragraph("Evidence declarations are retained as supplied; this compiler does not establish citation support.")
  } else if (stage == "import") {
    paragraph(paste("Imported rows:", result$n, "; raw SHA-256:", recipe$input$sha256)); paragraph(recipe$input$projection)
  } else if (stage == "validation") {
    paragraph(paste("Passed:", result$n, "unique complete observation IDs; explicit matching units; finite nonmissing analysis values; declared factor levels; full design rank", result$rank, "; residual degrees of freedom", result$df_residual))
    paragraph("Independence is a prespecified study assumption; unique IDs alone do not prove independence.")
  } else if (stage == "cleaning") {
    paragraph("No row exclusions, imputation, winsorization or outcome-driven covariate selection. Declared categorical reference levels were applied; raw inputs remain read-only.")
  } else if (stage == "descriptive") {
    table(utils::read.csv(file.path(directory, "output/analysis/descriptives.csv")))
    frequencies <- utils::read.csv(file.path(directory, "output/analysis/frequencies.csv"))
    if (nrow(frequencies)) table(frequencies)
  } else if (stage == "primary") {
    table(result$coefficients)
    paragraph(paste("Confidence coverage:", result$confidence, "; residual standard deviation:", signif(result$sigma, 7L)))
    paragraph(result$interpretation)
  } else if (stage == "diagnostics") {
    path <- file.path(directory, "output/analysis/diagnostics.png")
    encoded <- jsonlite::base64_enc(readBin(path, "raw", n = file.info(path)$size))
    cat('<img alt="Recorded residual versus fitted and normal quantile diagnostic plots" style="max-width:100%" src="data:image/png;base64,', encoded, '"/>\n', sep = "")
    paragraph(paste("Leverage review flags:", sum(result$diagnostics$high_leverage), "; influence review flags:", sum(result$diagnostics$high_influence)))
    paragraph("Plots and thresholds prompt scientific review; they do not automatically pass assumptions or remove observations.")
  } else if (stage == "report") {
    table(result$coefficients); paragraph(result$interpretation)
    for (limitation in result$limitations) paragraph(limitation)
  } else if (stage == "reproducibility") {
    paragraph(paste("Recipe:", recipe$id, "; fixed seed:", declaration$structure$profile$seed))
    paragraph(paste("External analysis receipt SHA-256:", declaration$run$sha256))
    paragraph("Current raw inputs, fixed compiler controls, complete installed writer inventory, R/package versions and recorded outputs were checked before this view. Rendering consumes recorded results and performs no model fit.")
  }
  }
  study_assert_run(directory, declaration)
  view <- list(schema_version = 1L, stage = stage, run_receipt_sha256 = declaration$run$sha256,
    status = "recorded_results_presented_not_scientifically_approved")
  study_write_json(view, file.path(directory, "output/report-views", paste0(stage, ".json")))
  invisible(view)
}

#' Render All Stages from Pinned Study Results
#' @inheritParams study_validate_project
#' @param timeout Maximum child R rendering time in seconds, between 30 and 3600.
#' @return A list with directory, validation and the updated external declaration
#'   pinning stage receipts and all generated report files. No Git commit or
#'   publication occurs. Rendering uses a clean child R session in the prepared
#'   external library and never reruns statistical estimation.
#' @export
study_render <- function(directory, declaration, timeout = 600) {
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout < 30 || timeout > 3600)
    study_fail("Rendering timeout must be between 30 and 3600 seconds.")
  study_admit(directory, declaration, require_run = TRUE)
  if (!is.null(declaration$render)) study_fail("This external declaration already pins a render; preserve it and its artifacts.")
  root <- as.character(fs::path_abs(directory))
  files <- c("analysis/index.Rmd", vapply(declaration$structure$profile$stages, `[[`, "", "analysis"))
  study_report_git_status(root, files)
  callr::r(function(root, declaration) {
    options(cttir.study.external_declaration = declaration)
    setwd(root)
    files <- c("analysis/index.Rmd", vapply(declaration$structure$profile$stages, `[[`, "", "analysis"))
    workflowr::wflow_build(files = files, view = FALSE, verbose = FALSE, local = TRUE)
    invisible(TRUE)
  }, args = list(root, declaration), libpath = .libPaths(), timeout = timeout,
    user_profile = FALSE, system_profile = FALSE, env = c(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"), show = FALSE)
  applicable <- Filter(function(x) x$applicable, declaration$structure$profile$stages)
  ids <- vapply(applicable, `[[`, "", "id")
  receipts <- stats::setNames(lapply(ids, function(id) file_hash(file.path(root, "output/provenance", paste0(id, ".json")))), ids)
  files <- study_inventory(file.path(root, "docs"))
  names(files) <- paste0("docs/", names(files))
  expected_html <- c("docs/index.html", vapply(declaration$structure$profile$stages,
    function(x) paste0("docs/", tools::file_path_sans_ext(basename(x$analysis)), ".html"), ""))
  if (!all(expected_html %in% names(files))) study_fail("Rendering did not produce all declared report pages.")
  declaration$render <- list(stages = receipts, files = files)
  validation <- study_validate_project(root, declaration, require_run = TRUE, require_render = TRUE)
  if (!isTRUE(validation$valid)) study_fail(validation$reason)
  list(directory = root, declaration = declaration, validation = validation)
}
