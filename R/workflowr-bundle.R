# Declarative workflowr profile for explicit project creation. Existing projects
# retain their reporting backend until an explicit migration is supported.
workflowr_profile_files <- function(preset = c("standard", "minimal"), pipeline = "none") {
  preset <- match.arg(preset)
  profile <- read_document(resource_file("extdata", "workflow-profile-v1.json"))
  stages <- unlist(profile$presets[[preset]], use.names = FALSE)
  pages <- c("index", stages)
  files <- list(
    "research.Rproj" = paste0("Version: 1.0\nRestoreWorkspace: No\nSaveWorkspace: No\n",
      "AlwaysSaveHistory: No\nEncoding: UTF-8\nUseSpacesForTab: Yes\nNumSpacesForTab: 2\n"),
    "_workflowr.yml" = yaml::as.yaml(list(seed = 12345L, knit_root_dir = ".")),
    "analysis/_site.yml" = yaml::as.yaml(list(name = "research", output_dir = "../docs",
      navbar = list(title = "Research project", left = lapply(pages, function(page)
        list(text = gsub("_", " ", page, fixed = TRUE), href = paste0(page, ".html")))),
      output = list("workflowr::wflow_html" = list(toc = TRUE, toc_float = TRUE)))),
    "docs/.nojekyll" = "",
    "assets/README.md" = "# Site assets\n\nOnly reviewed public assets belong here.\n",
    "config/report-pages.json" = paste0(json_text(list(schema_version = 1L, preset = preset,
      pages = as.list(paste0("analysis/", pages, ".Rmd"))), TRUE), "\n")
  )
  for (page in pages) {
    title <- if (page == "index") "Research project" else gsub("_", " ", page, fixed = TRUE)
    files[[paste0("analysis/", page, ".Rmd")]] <- paste0("---\n", yaml::as.yaml(list(title = title)), "---\n\n",
      "Creating this page does not run an analysis or establish scientific review.\n\n",
      "Review `protocol/analysis-plan.md` and the pinned workflow before adding calculations.\n",
      "Keep raw inputs read-only. Use explicit package namespaces and record unresolved design decisions.\n\n",
      if (page == "index") paste0("Publication units remain under `publications/<slug>/`; their stable IDs remain in project metadata.\n",
        "This project has one root workflowr site. Rendering previews pages; it does not commit or publish them.\n") else
        "This stage remains a placeholder until its inputs and reviewed implementation are available.\n")
  }
  table_page <- if (preset == "minimal") "analysis/03_report.Rmd" else "analysis/04_descriptive.Rmd"
  files[[table_page]] <- sub("This stage remains a placeholder until its inputs and reviewed implementation are available.",
    "Review the descriptive output and document its limitations.", files[[table_page]], fixed = TRUE)
  files[[table_page]] <- paste0(files[[table_page]],
    "\n## Recorded descriptive results\n\n",
    "The table below is shown only when input, code, configuration and output content identities remain current.\n",
    "A freshness check does not establish scientific validity.\n\n",
    "```{r reviewed-description}\n",
    "source('code/R/report_integrity.R', local = TRUE)\n",
    "descriptive <- cr_report_table('.', 'descriptives')\n",
    "if (is.null(descriptive)) {\n",
    "  cat('No completed study-data descriptive output is available.\\n')\n",
    "} else {\n",
    "  knitr::kable(descriptive, format = 'html', escape = TRUE)\n",
    "}\n```\n")
  root <- system.file("templates", "workflowr-0.4.0", package = "cttiR")
  manifest <- read_document(file.path(root, "manifest.json"))
  for (path in names(manifest$files)) {
    if (path == "_targets.R" && !identical(pipeline, "targets")) next
    relative_file(path)
    text <- paste0(paste(readLines(file.path(root, path), warn = FALSE), collapse = "\n"), "\n")
    if (!identical(content_hash(text), manifest$files[[path]])) {
      abort_cttir("The workflowr template failed its integrity check.", "cttir_api_mismatch")
    }
    files[[path]] <- text
  }
  files[["metadata/workflow-template.json"]] <- paste0(json_text(manifest, TRUE), "\n")
  files
}
