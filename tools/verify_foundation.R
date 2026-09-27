# Run after installation: Rscript --vanilla tools/verify_foundation.R LIB OUTPUT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
lib <- normalizePath(args[[1]], mustWork = TRUE)
.libPaths(c(lib, .libPaths()))
stopifnot(requireNamespace("cttiR", quietly = TRUE))
stopifnot(normalizePath(find.package("cttiR")) == file.path(lib, "cttiR"))
out <- args[[2]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, mustWork = TRUE)
stopifnot(all(c("project", "validate_spec", "validate_config", "resources") %in%
                getNamespaceExports("cttiR")))

primary <- cttiR::project("Primary Example", "primary_research", "Plan an original research project", out)
mixed <- cttiR::project("Mixed Example", "mixed", "Plan distinct research publications", out,
  config = list(publications = list(list(id = "pub02", title = "Evidence review",
    slug = "pub02_review", type = "systematic_review", research_class = "secondary_research",
    analysis_role = "unknown", data_origin = "literature"))))
standard <- cttiR::project("Standard Example", "other", "Plan a tabular comparison", out)
stopifnot(standard$spec$workflow$profile == "standard_reflowR")
stopifnot("reflowR_integration_pending" %in% standard$readiness$blockers)
for (p in list(primary, mixed, standard)) {
  cttiR::validate_spec(file.path(p$path, "cttir-project.yml"))
  stopifnot(nrow(cttiR::resources("Seurat", path = p$path, limit = 1)) == 1L)
  previous <- getwd()
  tryCatch({
    setwd(p$path)
    # This fixed template is reviewed and only validates scaffold metadata.
    source("code/validate_project.R", local = new.env(parent = baseenv()))
  }, finally = setwd(previous))
}
cat("Installed foundation smoke passed; three scaffold examples validated.\n")
cat("R:", R.version.string, "\nPlatform:", R.version$platform, "\n")
print(utils::packageVersion("cttiR"))
