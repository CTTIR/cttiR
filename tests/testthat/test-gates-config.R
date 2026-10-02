# Gates G03 (replay), G04 (scientific metadata), G05 (publications) and G06
# (configuration and schema migration).

fixed_identity <- function(env = parent.frame()) {
  real <- default_spec
  local_mocked_bindings(default_spec = function(...) {
    spec <- real(...)
    spec$project$id <- "00000000-0000-4000-8000-000000000001"
    spec$project$created_at <- "2026-01-01T00:00:00Z"
    spec
  }, .env = env)
}

data_source <- function(id, label, uri = NULL) {
  list(id = id, label = label, logical_uri = uri, format = NULL, access_class = "unknown",
    checksum = NULL, checksum_status = "unknown", schema_ref = NULL)
}

text_files <- function(root) {
  files <- list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  stats::setNames(vapply(files, function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), character(1)),
    substring(files, nchar(root) + 2L))
}

# G03 -------------------------------------------------------------------------

test_that("G03 an accepted spec renders identical content hashes on every replay", {
  spec <- project("Replay", "methods", "Goal", tempdir(), dry_run = TRUE)$spec
  one <- project_bundle(spec)
  two <- project_bundle(spec)
  expect_identical(vapply(one$files, content_hash, character(1)), vapply(two$files, content_hash, character(1)))
  expect_identical(one$manifest, two$manifest)
  fixed_identity()
  first <- project("Replay", "methods", "Goal", new_parent())
  second <- project("Replay", "methods", "Goal", new_parent())
  expect_false(identical(dirname(first$path), dirname(second$path)))
  expect_identical(first$spec, second$spec)
  expect_identical(tree_state(first$path), tree_state(second$path))
  expect_identical(first$plan$sha256, unname(vapply(project_bundle(first$spec)$files, content_hash, character(1))))
})

test_that("G03 repeating creation or synchronization never replans", {
  parent <- new_parent()
  p <- project("No replan", "primary_research", "Goal", parent)
  state <- tree_state(p$path)
  again <- project("No replan", "primary_research", "Goal", parent)
  expect_identical(again$spec, p$spec)
  expect_true(all(again$plan$action == "skip"))
  s <- sync(p$path, dry_run = FALSE)
  expect_true(all(s$actions$action == "skip"))
  expect_length(s$changed_files, 0L)
  expect_identical(read_project(p$path)$spec$decisions, p$spec$decisions)
  expect_identical(tree_state(p$path), state)
})

# G04 -------------------------------------------------------------------------

test_that("G04 review and secondary research never invent ethics, consent or data", {
  parent <- new_parent()
  invented <- "(ethics|irb)[^\n]{0,40}(approved|granted|obtained)|consent[^\n]{0,40}(obtained|given|signed)|participants were"
  for (type in c("review", "secondary_research")) {
    p <- project(paste("Evidence", type), type, "Synthesize published evidence", parent)
    expect_equal(p$spec$research$ethics_status, "unknown")
    expect_equal(p$spec$research$analysis_role, "unknown")
    expect_equal(p$spec$research$data_origin, "unknown")
    expect_length(p$spec$data_sources, 0L)
    expect_false(p$spec$analysis$approved)
    expect_length(read_document(file.path(p$path, "metadata/data-registry.yml"))$datasets, 0L)
    expect_false(any(grepl(invented, text_files(p$path), ignore.case = TRUE, perl = TRUE)))
    pub <- p$spec$publications[[1]]
    # `review` is proposed as secondary research; no review subtype is guessed.
    expect_equal(pub$research_class, "secondary_research")
    expect_equal(pub$type, "other")
  }
})

test_that("G04 analysis role and data origin stay separate from the research class", {
  parent <- new_parent()
  publication <- list(id = "pub01", type = "original_article",
    analysis_role = "secondary_analysis", data_origin = "existing_dataset")
  options <- list(
    research = list(analysis_role = "secondary_analysis", data_origin = "existing_dataset"),
    publications = list(publication)
  )
  p <- project("Cohort reuse", "primary_research", "Reanalyse an existing cohort", parent, options = options)
  expect_equal(p$spec$project$type, "primary_research")
  expect_equal(p$spec$research$analysis_role, "secondary_analysis")
  expect_equal(p$spec$research$data_origin, "existing_dataset")
  pub <- p$spec$publications[[1]]
  expect_equal(pub[c("research_class", "type", "analysis_role", "data_origin")],
    list(research_class = "primary_research", type = "original_article",
      analysis_role = "secondary_analysis", data_origin = "existing_dataset"))
  expect_identical(read_document(file.path(p$path, "publications", pub$slug, "publication.yml")), pub)
  expect_equal(p$spec$research$ethics_status, "unknown")
})

# G05 -------------------------------------------------------------------------

test_that("G05 primary and secondary publications get independent trees and shared data by ID", {
  parent <- new_parent()
  publications <- list(
    list(id = "pub01", title = "Primary outcomes", type = "original_article", research_class = "primary_research",
      analysis_role = "primary_analysis", data_origin = "new_collection"),
    list(id = "pub02", title = "Meta-analysis", slug = "pub02_meta_analysis", type = "meta_analysis",
      research_class = "secondary_research", analysis_role = "unknown", data_origin = "literature")
  )
  shared <- data_source("cohort2024", "Shared cohort", "registry:cohort2024")
  p <- project("Two papers", "mixed", "Report primary results and a meta-analysis", parent,
    options = list(publications = publications, data_sources = list(shared)))
  pubs <- p$spec$publications
  expect_equal(vapply(pubs, function(x) x$id, character(1)), c("pub01", "pub02"))
  expect_false(identical(pubs[[1]]$slug, pubs[[2]]$slug))
  for (pub in pubs) {
    root <- file.path(p$path, "publications", pub$slug)
    for (dir in c("analysis", "manuscript", "figures", "tables", "supplement", "submission")) {
      expect_true(dir.exists(file.path(root, dir)))
    }
    expect_identical(read_document(file.path(root, "publication.yml")), pub)
    expect_true(file.exists(file.path(root, "README.md")))
  }
  expect_equal(pubs[[2]][c("type", "research_class")], list(type = "meta_analysis", research_class = "secondary_research"))
  registry <- read_document(file.path(p$path, "metadata/data-registry.yml"))
  expect_equal(vapply(registry$datasets, function(x) x$id, character(1)), "cohort2024")
  publication_text <- text_files(file.path(p$path, "publications"))
  expect_false(any(grepl("registry:cohort2024|data/raw|data/processed", publication_text)))
  expect_true(all(grepl("registry ID", publication_text[grepl("^pub[0-9]+_[^/]+/README.md$", names(publication_text))], fixed = TRUE)))
  report <- audit(p$path, scope = "project")
  expect_equal(report$checks$status[report$checks$id == "PRJ-003"], "pass")
  expect_equal(report$checks$status[report$checks$id == "PRJ-005"], "pass")
  # Publications declare the registry IDs they use; unknown IDs are refused.
  expect_silent(validate_config(list(publications = list(list(id = "pub01", data_source_ids = list("cohort2024"))))))
  expect_error(sync(p$path, options = list(publications = list(list(id = "pub01", data_source_ids = list("missing"))))),
    class = "cttir_schema_error")
})

# G06 -------------------------------------------------------------------------

test_that("G06 precedence: required arguments, then options, then config, then defaults", {
  config <- list(
    project = list(name = "Config name", language = "de"),
    research = list(design = "cohort", domain = "cardiology")
  )
  options <- list(research = list(design = "randomized trial"))
  p <- project("Argument name", "primary_research", "Goal", tempdir(), config = config, options = options, dry_run = TRUE)
  expect_equal(p$spec$project$name, "Argument name")
  expect_equal(p$spec$project$language, "de")
  expect_equal(p$spec$research$design, "randomized trial")
  expect_equal(p$spec$research$domain, "cardiology")
  expect_equal(p$spec$research$ethics_status, "unknown")
  origins <- function(field) vapply(Filter(function(d) d$field == field, p$spec$decisions), function(d) d$origin, character(1))
  expect_setequal(origins("/research/design"), c("config", "explicit"))
  expect_contains(origins("/project/name"), "explicit")
  file <- tempfile(fileext = ".yml")
  withr::defer(unlink(file))
  writeLines(yaml::as.yaml(config), file)
  from_file <- project("Argument name", "primary_research", "Goal", tempdir(), config = file, options = options, dry_run = TRUE)
  expect_identical(from_file$spec[c("research", "publications", "workflow")], p$spec[c("research", "publications", "workflow")])
  for (field in c("name", "type", "goal")) {
    override <- stats::setNames(list(if (field == "type") "methods" else "Override"), field)
    attempt <- list(project = override)
    expect_error(project("Argument name", "primary_research", "Goal", tempdir(), options = attempt, dry_run = TRUE),
      class = "cttir_input_error")
  }
})

test_that("G06 unknown keys are rejected at every level", {
  unknown <- list(
    list(unknown = 1), list(project = list(unknown = 1)), list(research = list(unknown = 1)),
    list(workflow = list(unknown = TRUE)), list(publications = list(list(id = "pub01", unknown = 1))),
    list(data_sources = list(list(id = "d1", unknown = 1))), list(analysis = list(mapping = list(unknown = "x"))),
    list(analysis = list(model = list(unknown = TRUE))), list(knowledge = list(unknown = TRUE)),
    list(figures = list(unknown = 1)), list(ecosystem = list(unknown = 1))
  )
  for (config in unknown) {
    expect_error(validate_config(config), class = "cttir_schema_error")
    expect_error(project("Unknown", "methods", "Goal", tempdir(), options = config, dry_run = TRUE), class = "cttir_schema_error")
  }
  expect_silent(validate_config(list(extensions = list(org.example = list(anything = list(1, "a"))))))
})

test_that("G06 explicit nulls clear nullable fields only, and absence leaves values unchanged", {
  p <- project("Nulls", "methods", "Goal", tempdir(), dry_run = TRUE,
    config = list(research = list(design = "cohort")), options = list(research = list(design = NULL)))
  expect_true("design" %in% names(p$spec$research))
  expect_null(p$spec$research$design)
  required_null <- list(research = list(ethics_status = NULL))
  expect_error(project("Nulls", "methods", "Goal", tempdir(), options = required_null, dry_run = TRUE),
    class = "cttir_schema_error")
  parent <- new_parent()
  saved <- project("Nulls", "methods", "Goal", parent, config = list(research = list(design = "cohort")))
  sync(saved$path, options = list(research = list(domain = "epidemiology")), dry_run = FALSE)
  spec <- read_project(saved$path)$spec
  expect_equal(spec$research$design, "cohort")
  expect_equal(spec$research$domain, "epidemiology")
  sync(saved$path, options = list(research = list(design = NULL)), dry_run = FALSE)
  spec <- read_project(saved$path)$spec
  expect_null(spec$research$design)
  expect_equal(spec$research$domain, "epidemiology")
})

test_that("G06 arrays merge by stable ID, not position", {
  parent <- new_parent()
  config <- list(
    publications = list(
      list(id = "pub01", title = "Main"),
      list(id = "pub02", title = "Review", slug = "pub02_review", type = "systematic_review",
        research_class = "secondary_research", analysis_role = "unknown", data_origin = "literature")
    ),
    data_sources = list(data_source("ds_a", "A"), data_source("ds_b", "B"))
  )
  options <- list(
    publications = list(list(id = "pub02", title = "Updated review"), list(id = "pub01", type = "original_article")),
    data_sources = list(list(id = "ds_b", label = "B revised"))
  )
  p <- project("Arrays", "mixed", "Goal", parent, config = config, options = options)
  expect_equal(vapply(p$spec$publications, function(x) x$id, character(1)), c("pub01", "pub02"))
  expect_equal(p$spec$publications[[1]][c("title", "type")], list(title = "Main", type = "original_article"))
  expect_equal(p$spec$publications[[2]]$title, "Updated review")
  expect_equal(vapply(p$spec$data_sources, function(x) x$label, character(1)), c("A", "B revised"))
  sync(p$path, options = list(data_sources = list(list(id = "ds_a", access_class = "restricted"))), dry_run = FALSE)
  spec <- read_project(p$path)$spec
  expect_equal(vapply(spec$data_sources, function(x) x$access_class, character(1)), c("restricted", "unknown"))
  expect_equal(spec$data_sources[[2]]$label, "B revised")
  expect_error(validate_config(list(data_sources = list(list(id = "x"), list(id = "X")))), class = "cttir_schema_error")
})

test_that("G06 specs and configs round trip through YAML and JSON with unicode", {
  parent <- new_parent()
  p <- project("\u00dcn\u00efcode \u2013 \u7814\u7a76", "mixed",
    "Goal \u2713 \u201cquotes\u201d, 'single': colon # hash\nsecond line", parent)
  spec <- p$spec
  yaml_file <- file.path(parent, "spec.yml")
  json_file <- file.path(parent, "spec.json")
  write_bytes(yaml::as.yaml(spec), yaml_file)
  write_bytes(json_text(spec, TRUE), json_file)
  expect_identical(validate_spec(yaml_file), spec)
  expect_identical(validate_spec(json_file), spec)
  expect_identical(validate_spec(file.path(p$path, "cttir-project.yml")), spec)
  config <- list(research = list(notes = "Z\u00fcrich \u6771\u4eac", design = NULL),
    publications = list(list(id = "pub01", title = "R\u00e9sum\u00e9")))
  write_bytes(yaml::as.yaml(config), yaml_file)
  write_bytes(json_text(config, TRUE), json_file)
  expect_identical(validate_config(yaml_file), validate_config(config))
  expect_identical(validate_config(json_file), validate_config(config))
})

test_that("G06 migration applies pure steps one version at a time and preserves the original", {
  current <- project("Migrate", "methods", "Goal", tempdir(), dry_run = TRUE)$spec
  legacy <- current
  legacy$schema_version <- 0L
  legacy$research$remarks <- "legacy notes"
  legacy$research$notes <- NULL
  calls <- 0L
  rename_notes <- function(spec) {
    calls <<- calls + 1L
    spec$research$notes <- spec$research$remarks
    spec$research$remarks <- NULL
    spec$schema_version <- 1L
    spec
  }
  local_mocked_bindings(spec_migrations = function() list("0" = rename_notes))
  result <- migrate_spec(legacy)
  expect_identical(result$original, legacy)
  expect_length(result$steps, 1L)
  expect_equal(result$steps[[1]][c("from", "to")], list(from = 0L, to = 1L))
  expect_setequal(result$steps[[1]]$changes$pointer, c("/schema_version", "/research/notes", "/research/remarks"))
  expect_equal(result$steps[[1]]$before_sha256, content_hash(json_text(legacy)))
  expect_identical(validate_spec(result$spec)$research$notes, "legacy notes")
  expect_gte(calls, 1L)
  expect_identical(migrate_spec(current)$steps, list())
  order <- character()
  step <- function(from) {
    force(from)
    function(spec) {
      order <<- c(order, as.character(from))
      spec$schema_version <- from + 1L
      spec
    }
  }
  local_mocked_bindings(spec_schema_version = function() 2L, spec_migrations = function() list("0" = step(0L), "1" = step(1L)))
  chained <- migrate_spec(legacy)
  expect_equal(vapply(chained$steps, function(x) x$from, integer(1)), c(0L, 1L))
  expect_equal(unique(order), c("0", "1"))
  expect_equal(chained$spec$schema_version, 2L)
  expect_identical(chained$original, legacy)
  code <- function(expr) tryCatch(expr, error = function(e) e$code)
  skip_version <- function(spec) {
    spec$schema_version <- 2L
    spec
  }
  local_mocked_bindings(spec_schema_version = function() 2L, spec_migrations = function() list("0" = skip_version))
  expect_equal(code(migrate_spec(legacy)), "invalid_migration")
  impure <- function(spec) {
    spec$schema_version <- 1L
    spec$project$id <- uuid::UUIDgenerate()
    spec
  }
  local_mocked_bindings(spec_schema_version = function() 1L, spec_migrations = function() list("0" = impure))
  expect_equal(code(migrate_spec(legacy)), "invalid_migration")
  local_mocked_bindings(spec_migrations = function() list())
  expect_equal(code(migrate_spec(legacy)), "no_migration_path")
  broken <- legacy
  broken$schema_version <- "zero"
  expect_error(migrate_spec(broken), class = "cttir_schema_error")
})

test_that("G06 newer schema versions are refused read-only with a typed error", {
  parent <- new_parent()
  p <- project("Future", "methods", "Goal", parent)
  file <- file.path(p$path, "cttir-project.yml")
  lines <- readLines(file)
  expect_true("schema_version: 1" %in% lines)
  writeLines(sub("^schema_version: 1$", "schema_version: 3", lines), file)
  before <- tree_state(p$path)
  code <- function(expr) tryCatch(expr, error = function(e) c(class(e)[[1]], e$code))
  unsupported <- c("cttir_schema_error", "unsupported_schema_version")
  expect_equal(code(read_project(p$path)), unsupported)
  expect_equal(code(validate_spec(file)), unsupported)
  expect_equal(code(migrate_spec(file)), unsupported)
  expect_equal(code(sync(p$path)), unsupported)
  expect_equal(code(sync(p$path, options = list(project = list(goal = "x")), dry_run = FALSE)), unsupported)
  expect_equal(code(project("Future", "methods", "Goal", parent)), unsupported)
  expect_equal(code(validate_config(list(schema_version = 2L))), unsupported)
  report <- audit(p$path, scope = "project", repair = TRUE)
  expect_equal(report$checks$status[report$checks$id == "PRJ-001"], "fail")
  expect_match(report$checks$message[report$checks$id == "PRJ-001"], "newer than the supported version", fixed = TRUE)
  expect_identical(tree_state(p$path), before)
})
