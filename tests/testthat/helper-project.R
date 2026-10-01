new_parent <- function() {
  p <- tempfile("cttir-test-")
  dir.create(p)
  withr::defer(unlink(p, recursive = TRUE), envir = parent.frame())
  p
}

tree_hashes <- function(path) {
  files <- list.files(path, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  stats::setNames(
    vapply(files, function(f) digest::digest(file = f, algo = "sha256"), character(1)),
    substring(files, nchar(path) + 2L)
  )
}
