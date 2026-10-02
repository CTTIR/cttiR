# Synthetic demonstration of every reviewed stage. Constructed data only; run
# from the project root with `Rscript code/run_demo.R`. Outputs go to demo/.
required <- c("cttir-project.yml", "config/workflow.yml", "code/R/cttir_workflow.R", "code/R/cttir_figures.R")
if (!all(file.exists(required))) stop("Run from the project root.", call. = FALSE)
packages <- c("yaml", "jsonlite", "dplyr", "ggplot2", "patchwork", "viridisLite", "RColorBrewer",
  "colorspace", "nlme", "survival", "broom", "broom.mixed")
absent <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(absent)) stop("Install the workflow dependencies explicitly: ", paste(absent, collapse = ", "), call. = FALSE)
source("code/R/cttir_figures.R", local = TRUE)
source("code/R/cttir_workflow.R", local = TRUE)
spec <- cw_read_yaml("cttir-project.yml")
workflow <- cw_read_yaml("config/workflow.yml")
receipt <- cw_run_demo(root = ".", figures_policy = spec$figures, backend = workflow$table_backend)
message("Synthetic demonstration ", receipt$status, ". See demo/receipt.json; values are not study results.")
if (!identical(receipt$status, "passed")) quit(status = 1L)
