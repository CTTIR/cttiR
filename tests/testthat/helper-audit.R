# Byte-level state of a tree: file hashes plus directory entries (empty
# directories included), so added or removed directories are also detected.
tree_state <- function(path) {
  if (!file.exists(path)) return(character())
  entries <- list.files(path, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE, full.names = TRUE)
  stats::setNames(
    vapply(entries, function(f) if (dir.exists(f)) "<dir>" else digest::digest(file = f, algo = "sha256"), character(1)),
    substring(entries, nchar(path) + 2L)
  )
}

# Synthetic interrupted project write over the given managed files, recorded by
# a stopped writer process.
interrupted_write <- function(root, pid, paths) {
  dir <- file.path(root, ".cttir/transactions/interrupted-fixture")
  dir.create(file.path(dir, "backup"), recursive = TRUE)
  rows <- list()
  for (i in seq_along(paths)) {
    dest <- file.path(root, paths[[i]])
    before <- file_hash(dest)
    file.copy(dest, file.path(dir, "backup", as.character(i)))
    write_bytes(paste0("# Interrupted replacement ", i, "\n"), dest)
    rows[[i]] <- data.frame(path = paths[[i]], action = "update", old_hash = before, new_hash = file_hash(dest))
  }
  actions <- do.call(rbind, rows)
  write_bytes(json_text(list(schema_version = 1L, status = "applying", actions = actions)), file.path(dir, "journal.json"))
  dir.create(file.path(root, ".cttir/write-lock"), showWarnings = FALSE)
  write_bytes(json_text(list(pid = pid, host = Sys.info()[["nodename"]])), file.path(root, ".cttir/write-lock/owner.json"))
  invisible(actions)
}

dead_pid <- function() callr::r(function() Sys.getpid())
