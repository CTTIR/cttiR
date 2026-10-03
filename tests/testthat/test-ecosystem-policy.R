test_that("ecosystem schemas constrain modality, providers and policy version", {
  for (kind in c("config", "project-spec")) {
    schema <- read_document(resource_file("schema", paste0(kind, ".schema.json")))
    expect_setequal(unlist(schema$properties$ecosystem$properties$modality$enum), c(interop_modalities(), "unknown"))
  }
  for (ecosystem in list(list(modality = "clinical"), list(modality = "zzz"),
      list(policy_version = 2L), list(allowed_providers = list("Seurat")),
      list(allowed_providers = list("seurat", "seurat")))) {
    expect_error(validate_config(list(ecosystem = ecosystem)), class = "cttir_schema_error")
  }
  expect_equal(validate_config(list(schema_version = 1L))$schema_version, 2L)
  expect_error(validate_config(list(schema_version = 1L, ecosystem = list(modality = "zzz"))), class = "cttir_schema_error")
  p <- project("Policy", "methods", "Single-cell RNA-seq clustering", tempdir(), dry_run = TRUE,
    options = list(ecosystem = list(allowed_providers = list("bioconductor"))))
  route <- route_workflow(p$spec)
  expect_gt(length(route$ecosystem), 0L)
  expect_true(all(vapply(route$ecosystem, function(x) x$family == "bioconductor", logical(1))))
  p$spec$ecosystem$allowed_providers <- list()
  expect_length(route_workflow(validate_spec(p$spec))$ecosystem, 0L)
  expect_equal(p$spec$ecosystem$policy_version, 1L)
})

legacy_policy_project <- function(parent) {
  p <- project("Migration ü", "methods", "Single-cell RNA-seq clustering", parent)
  spec <- p$spec
  spec$schema_version <- 1L
  spec$ecosystem$policy_version <- NULL
  spec$ecosystem$allowed_providers <- NULL
  bundle <- project_bundle(spec)
  for (path in names(bundle$files)) write_bytes(bundle$files[[path]], file.path(p$path, path))
  read_project(p$path)
}

test_that("schema migration previews without writes and preserves originals and pins on apply", {
  p <- legacy_policy_project(new_parent())
  original_hash <- file_hash(file.path(p$path, "cttir-project.yml"))
  before <- tree_hashes(p$path)
  preview <- sync(p$path)
  expect_identical(tree_hashes(p$path), before)
  expect_equal(preview$migration$steps[[1]][c("from", "to")], list(from = 1L, to = 2L))
  expect_contains(preview$migration$steps[[1]]$changes$pointer, "/schema_version")
  expect_true(preview$migration$original %in% preview$actions$path)
  result <- sync(p$path, dry_run = FALSE)
  expect_equal(result$state, "applied")
  expect_identical(file_hash(file.path(p$path, result$migration$original)), original_hash)
  current <- read_project(p$path)
  expect_equal(current$spec$schema_version, 2L)
  expect_identical(current$spec$project, p$spec$project)
  expect_identical(current$spec$provenance, p$spec$provenance)
  expect_identical(current$lock$dependencies, p$lock$dependencies)
  expect_identical(current$lock$catalog_id, p$lock$catalog_id)
  expect_null(sync(p$path)$migration)
  expect_equal(validate_spec(current$spec)$ecosystem$policy_version, 1L)
})

test_that("schema migration does not overwrite edited managed files or preexisting backups", {
  p <- legacy_policy_project(new_parent())
  preview <- sync(p$path)
  write_bytes("existing unrelated file\n", file.path(p$path, preview$migration$original))
  before <- tree_hashes(p$path)
  result <- sync(p$path, dry_run = FALSE)
  expect_equal(result$state, "conflict")
  expect_true(preview$migration$original %in% result$conflicts)
  expect_identical(tree_hashes(p$path), before)
})

test_that("extension pin validation uses catalog evidence instead of installed versions", {
  catalog <- resolve_catalog()
  seurat <- Filter(function(x) x$name == "Seurat", catalog$packages)[[1]]
  good <- list(name = "Seurat", version = seurat$version, revision = seurat$revision)
  expect_silent(validate_config(list(packages = list(good))))
  bad <- good
  bad$version <- "999.0.0"
  expect_error(validate_config(list(packages = list(bad))), class = "cttir_schema_error")
  bad <- good
  bad$revision <- "unreviewed-revision"
  expect_error(validate_config(list(packages = list(bad))), "does not match the selected catalog pin")
  expect_silent(validate_extension_pins(list(good), catalog))
})

test_that("failed migration writes restore the accepted project", {
  p <- legacy_policy_project(new_parent())
  before <- tree_hashes(p$path)
  real <- replace_file
  count <- 0L
  local_mocked_bindings(replace_file = function(from, to) {
    count <<- count + 1L
    if (count == 2L) stop("migration write failure")
    real(from, to)
  })
  expect_error(sync(p$path, dry_run = FALSE), "migration write failure")
  after <- tree_hashes(p$path)
  expect_identical(after[names(before)], before)
  expect_equal(read_project(p$path)$spec$schema_version, 1L)
  expect_identical(read_project(p$path)$lock, p$lock)
})

test_that("legacy configuration round trips without rewriting its source", {
  for (extension in c("json", "yml")) {
    file <- tempfile(fileext = paste0(".", extension))
    original <- list(schema_version = 1L, project = list(name = "Analyse ü"), ecosystem = list(modality = "single_cell"))
    write_bytes(if (extension == "json") json_text(original) else yaml::as.yaml(original), file)
    before <- file_hash(file)
    result <- validate_config(file)
    expect_equal(result$schema_version, 2L)
    expect_identical(result$project, original$project)
    expect_identical(file_hash(file), before)
    unlink(file)
  }
})
