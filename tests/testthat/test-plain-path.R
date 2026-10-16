test_that("plain paths admit regular and prospective files without changing bytes", {
  parent <- normalizePath(new_parent(), winslash = "/", mustWork = TRUE)
  path <- file.path(parent, "ordinary.txt")
  writeLines("unchanged", path)
  before <- tree_hashes(parent)
  expect_silent(assert_plain_path(path))
  expect_silent(assert_plain_path(file.path(parent, "new-directory", "new-file.txt")))
  expect_identical(tree_hashes(parent), before)
  expect_error(assert_plain_path(file.path(parent, "..", "elsewhere")), "Parent traversal")
})

test_that("links and linked ancestors reject even without Sys.readlink support", {
  parent <- normalizePath(new_parent(), winslash = "/", mustWork = TRUE)
  target <- file.path(parent, "target")
  dir.create(target)
  file <- file.path(target, "keep.txt"); writeLines("unchanged target", file)
  direct <- file.path(parent, "file-link")
  linked <- file.path(parent, "directory-link")
  made <- tryCatch({
    fs::link_create(file, direct)
    fs::link_create(target, linked)
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(made, "This runner cannot create test links.")
  before <- tree_hashes(target)
  guard <- assert_plain_path
  environment(guard) <- list2env(list(Sys.readlink = function(paths) rep("", length(paths))),
    parent = environment(assert_plain_path))
  for (path in c(direct, linked, file.path(linked, "keep.txt"), file.path(linked, "not-created", "child"))) {
    expect_error(guard(path), class = "cttir_path_conflict")
  }
  expect_identical(tree_hashes(target), before)
  expect_identical(readLines(file), "unchanged target")
  expect_false(file.exists(file.path(target, "not-created")))
})

test_that("only exact Darwin system aliases remain permitted", {
  aliases <- c("/var", "/tmp", "/etc")
  local_mocked_bindings(is_link = function(path) stats::setNames(path %in% c(aliases, "/var/user-link"), path), .package = "fs")
  correct_alias <- TRUE
  guard <- assert_plain_path
  environment(guard) <- list2env(list(
    Sys.info = function() c(sysname = "Darwin"),
    normalizePath = function(path, ...) if (path %in% aliases && correct_alias) paste0("/private", path) else path
  ), parent = environment(assert_plain_path))
  for (path in aliases) expect_silent(guard(file.path(path, "new-study")))
  expect_error(guard("/var/user-link/new-study"), class = "cttir_path_conflict")
  correct_alias <- FALSE
  expect_error(guard("/var/new-study"), class = "cttir_path_conflict")
})
