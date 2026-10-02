# Explicit local rendering; review all analysis/*.Rmd edits before running.
required <- c("cttir-project.yml", "analysis/_site.yml")
if (!all(file.exists(required))) stop("Run from the project root.")
for (package in c("rmarkdown", "knitr")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(paste("Install the rendering dependency explicitly:", package))
  }
}
if (!rmarkdown::pandoc_available()) stop("Install Pandoc explicitly before rendering.")
rmarkdown::render_site("analysis", envir = new.env(parent = globalenv()))
