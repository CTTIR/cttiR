app_study_test_parent <- function() {
  path <- tempfile("cttir-app-study-")
  dir.create(path)
  withr::defer({
    raw_dirs <- file.path(path, c("association_study", "paired_study"), "data", "raw")
    Sys.chmod(raw_dirs[dir.exists(raw_dirs)], mode = "0755")
    unlink(path, recursive = TRUE)
  }, envir = parent.frame())
  path
}

app_study_test_request <- function(parent, recipe = "association") {
  list(recipe = recipe, directory = file.path(parent, paste0(recipe, "_study")), author = "Test group",
    synthetic_acknowledged = TRUE, independent_pairs = identical(recipe, "paired"))
}

test_that("fixed study examples restore RNG state and reject unsupported requests", {
  withr::local_preserve_seed()
  set.seed(429)
  before <- .Random.seed
  kind <- RNGkind()
  a <- app_study_fixture("association")
  b <- app_study_fixture("paired")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
  expect_equal(nrow(a$data), 40L)
  expect_equal(nrow(b$data), 48L)
  expect_equal(length(unique(b$data$pair_id)), 24L)
  expect_equal(a$seed, 20261016L)
  expect_equal(b$seed, 20261017L)
  expect_identical(app_study_fixture("association"), a)
  expect_identical(app_study_fixture("paired"), b)
  expect_error(app_study_fixture("survival"), "Unsupported")
  request <- app_study_test_request(app_study_test_parent(), "paired")
  request$independent_pairs <- FALSE
  expect_error(app_study_request(request), "independent")
  request$independent_pairs <- TRUE
  request$synthetic_acknowledged <- FALSE
  expect_error(app_study_request(request), "Acknowledge")
  request$synthetic_acknowledged <- TRUE
  request$data <- b$data
  expect_error(app_study_request(request), "exactly")
  expect_false(dir.exists(request$directory))
})

test_that("strict preview is read-only and compilation binds its current fingerprint", {
  skip_if_not_installed("reflowR")
  parent <- app_study_test_parent(); request <- app_study_test_request(parent)
  before <- tree_hashes(parent)
  preview <- app_worker_run("study_preview", list(request = request))
  expect_true(preview$ok)
  expect_identical(tree_hashes(parent), before)
  expect_equal(preview$value$rows, 40L)
  expect_null(preview$value$declaration)
  again <- app_worker_run("study_preview", list(request = request))
  expect_identical(preview$value$fingerprint, again$value$fingerprint)
  changed <- request; changed$author <- "Different author"
  refused <- app_worker_run("study_compile", list(request = changed, expected_fingerprint = preview$value$fingerprint))
  expect_false(refused$ok)
  expect_match(refused$message, "stale")
  expect_false(dir.exists(request$directory))
  expect_false(app_worker_run("study_compile", list(request = request, expected_fingerprint = "wrong"))$ok)
  expect_false(app_worker_run("study_preview", list(request = request, data = data.frame(x = 1)))$ok)
})

test_that("study module keeps explicit phases and exports only its retained declaration", {
  skip_if_not_installed("shiny"); skip_if_not_installed("reflowR")
  parent <- app_study_test_parent(); values <- app_study_test_request(parent)
  shiny::testServer(app_study_server, args = app_test_args(), {
    do.call(session$setInputs, values); session$flushReact()
    session$setInputs(compile = 1); session$flushReact()
    expect_null(state$record)
    expect_false(dir.exists(values$directory))
    session$setInputs(preview = 1); session$flushReact()
    expect_identical(state$phase, "previewed")
    expect_false(dir.exists(values$directory))
    session$setInputs(author = "Changed author"); session$flushReact()
    session$setInputs(compile = 2); session$flushReact()
    expect_null(state$record)
    expect_false(dir.exists(values$directory))
    session$setInputs(author = values$author); session$flushReact()
    session$setInputs(preview = 2); session$flushReact()
    session$setInputs(compile = 3); session$flushReact()
    expect_identical(state$phase, "compiled")
    expect_null(state$record$declaration$run)
    expect_true(dir.exists(file.path(values$directory, ".git")))
    expect_false(dir.exists(file.path(values$directory, "output/analysis")))
    original <- state$record
    session$setInputs(recipe = "paired", independent_pairs = TRUE); session$flushReact()
    session$setInputs(run = 1); session$flushReact()
    expect_identical(state$record, original)
    file <- file.path(parent, "trusted-declaration.rds")
    export_declaration(file)
    expect_identical(readRDS(file), original$declaration)
    session$setInputs(recipe = values$recipe, independent_pairs = values$independent_pairs); session$flushReact()
    session$setInputs(render = 1); session$flushReact()
    expect_identical(state$record, original)
    session$setInputs(run = 2); session$flushReact()
    expect_identical(state$phase, "analyzed")
    expect_false(is.null(state$record$declaration$run))
    expect_null(state$record$declaration$render)
    expect_identical(state$record$result$scientific_review, "pending")
    recorded <- state$record
    session$setInputs(run = 3); session$flushReact()
    expect_identical(state$record, recorded)
    expect_true(!identical(state$exported_sha256, app_study_hash(recorded$declaration)))
    export_declaration(file)
    expect_identical(readRDS(file), recorded$declaration)
    output_file <- file.path(values$directory, "output/analysis/results.json")
    original_bytes <- readBin(output_file, "raw", n = file.info(output_file)$size)
    writeLines("{\"changed\":true}", output_file)
    session$setInputs(render = 2); session$flushReact()
    expect_true(state$failed_write)
    expect_identical(state$record, recorded)
    export_declaration(file)
    expect_identical(readRDS(file), recorded$declaration)
    writeBin(original_bytes, output_file)
    session$setInputs(render = 3); session$flushReact()
    expect_true(state$failed_write)
    expect_identical(state$record, recorded)
  })
})

test_that("pending study jobs refuse duplicates and ignore stale preview completion", {
  skip_if_not_installed("shiny"); skip_if_not_installed("reflowR")
  parent <- app_study_test_parent(); values <- app_study_test_request(parent)
  pending <- app_pending_worker()
  preview <- app_worker_run("study_preview", list(request = values))
  shiny::testServer(app_study_server, args = app_test_args(worker = pending$worker), {
    do.call(session$setInputs, values); session$flushReact()
    session$setInputs(preview = 1); session$flushReact()
    expect_equal(pending$control$count, 1L)
    session$setInputs(preview = 2); session$flushReact()
    expect_equal(pending$control$count, 1L)
    expect_identical(state$status$key, "status.duplicate")
    session$setInputs(author = "New selection"); session$flushReact()
    pending$control$result <- preview; pending$control$alive <- FALSE
    session$elapse(200); session$flushReact()
    expect_null(state$accepted)
    expect_null(state$record)
    expect_identical(state$status$key, "status.changed_while_planning")
  })
  expect_false(dir.exists(values$directory))
})

test_that("both fixed worker pipelines render current synthetic results without implicit fitting", {
  skip_if_not_installed("reflowR"); skip_if_not_installed("workflowr")
  skip_if_not(rmarkdown::pandoc_available())
  worker <- function(operation, args) {
    process <- app_worker(operation, args)
    process$wait(timeout = 660000)
    if (process$is_alive()) {
      process$kill()
      stop("The test-owned installed worker exceeded its deadline")
    }
    process$get_result()
  }
  for (recipe in c("association", "paired")) {
    parent <- app_study_test_parent(); request <- app_study_test_request(parent, recipe)
    p <- worker("study_preview", list(request = request))
    expect_true(p$ok, info = paste(p$message, p$code, p$class))
    if (!isTRUE(p$ok)) next
    c <- worker("study_compile", list(request = request, expected_fingerprint = p$value$fingerprint))
    expect_true(c$ok, info = paste(c$message, c$code, c$class))
    if (!isTRUE(c$ok)) next
    expect_identical(c$value$phase, "compiled")
    bad <- c$value; bad$request$recipe <- if (recipe == "paired") "association" else "paired"
    expect_false(app_worker_run("study_run", list(record = bad))$ok)
    a <- worker("study_run", list(record = c$value))
    expect_true(a$ok, info = paste(a$message, a$code, a$class))
    if (!isTRUE(a$ok)) next
    expect_identical(a$value$phase, "analyzed")
    numerical <- tree_hashes(file.path(request$directory, "output/analysis"))
    r <- worker("study_render", list(record = a$value))
    expect_true(r$ok, info = paste(r$message, r$code, r$class))
    if (!isTRUE(r$ok)) next
    expect_identical(r$value$phase, "rendered")
    expect_true(r$value$validation$valid)
    expect_false(r$value$validation$scientific_approval)
    expect_identical(tree_hashes(file.path(request$directory, "output/analysis")), numerical)
    expect_length(list.files(file.path(request$directory, "docs"), pattern = "\\.html$"), 12L)
    expect_false(app_worker_run("study_render", list(record = r$value))$ok)
  }
})


test_that("direct example records bind fixed raw values units seed and preview", {
  skip_if_not_installed("reflowR")
  for (variant in c("values", "seed", "units", "fingerprint")) {
    parent <- app_study_test_parent()
    request <- app_study_test_request(parent)
    preview <- app_study_preview(request)
    fixture <- app_study_fixture("association")
    if (variant == "values") fixture$data$pressure[[1L]] <- fixture$data$pressure[[1L]] + 99
    seed <- if (variant == "seed") fixture$seed + 1L else fixture$seed
    project <- study_project(request$directory, fixture$spec, fixture$data,
      preview$dependency_lock, request$author, fixture$units, seed)
    record <- list(schema_version = 1L, phase = "compiled", request = request,
      request_sha256 = preview$request_sha256, preview_fingerprint = preview$fingerprint,
      recipe_id = preview$recipe_id, directory = project$directory, declaration = project$declaration,
      result = NULL, validation = NULL)
    if (variant %in% c("values", "seed")) expect_true(study_validate_project(project$directory, project$declaration)$valid)
    if (variant == "units") record$declaration$structure$composition$recipe$input$units$pressure <- "kPa"
    if (variant == "fingerprint") record$preview_fingerprint <- "untrusted-nonempty"
    before <- tree_hashes(request$directory)
    expect_error(app_study_record(record, "compiled"), "fixed fixture|fixed preview")
    expect_false(app_worker_run("study_run", list(record = record))$ok)
    expect_identical(tree_hashes(request$directory), before)
    expect_false(dir.exists(file.path(request$directory, "output/analysis")))
  }
})
