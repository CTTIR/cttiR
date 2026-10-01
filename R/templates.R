reflowr_templates <- function() {
  root <- system.file("templates", "reflowr-0.2.0", package = "cttiR")
  if (!nzchar(root)) abort_cttir("The bundled template is missing.", "cttir_api_mismatch")
  manifest <- read_document(file.path(root, "manifest.json"))
  files <- lapply(names(manifest$files), function(path) {
    file <- file.path(root, path)
    if (!file.exists(file)) abort_cttir("A bundled template is missing.", "cttir_api_mismatch")
    text <- paste0(paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n")
    if (!identical(content_hash(text), manifest$files[[path]])) {
      abort_cttir("A bundled template failed its integrity check.", "cttir_api_mismatch")
    }
    text
  })
  names(files) <- names(manifest$files)
  files[["metadata/reflowr-template.json"]] <- paste0(json_text(manifest, TRUE), "\n")
  files
}
