# Run: Rscript therapy-workflow.R /absolute/new/project "Study author"
# Add --with-adviser to retain an actual local model consultation before building.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop('Supply a new project directory and the study author.')
target <- sub('/+$', '', path.expand(args[[1]]))
record <- paste0(target, '-declaration.rds')
consultation <- paste0(target, '-adviser-consultation.rds')
if (any(file.exists(c(target, record, consultation))))
  stop('Project or external continuation record already exists; choose a new project path.')
suppressPackageStartupMessages(library(cttiR))
question <- paste('We plan an individually randomized parallel trial comparing a new therapy with standard care.',
  'The continuous score is measured at 12 weeks; lower is better. The meaningful difference is 5 points, planning SD 10 points,',
  'allocation 1:1, two-sided alpha 0.05, power 80%, and expected unavailable outcomes 10%.',
  'Explain the analysis, assumptions and sample-size calculation, with evaluable and randomized counts.')
if ('--with-adviser' %in% args) {
  advice <- advise(question, acknowledge_experimental = TRUE)
  saveRDS(advice, consultation)
  print(advice)
}
planning <- list(delta = 5, sd = 10, power = 0.8, alpha = 0.05, dropout = 0.1)
counts <- do.call(study_two_group_power, planning)
print(counts)
spec <- study_two_group_spec('therapy_comparison', 'Synthetic therapy versus standard care',
  question, 'Independently generated synthetic continuous outcomes; no patient data',
  'id', 'arm', list(name = 'score', type = 'continuous', unit = 'points'),
  'standard', 'therapy', independent_observations = TRUE, planning = planning)
set.seed(20261104)
n <- counts$evaluable_per_group
data <- data.frame(id = sprintf('synthetic_%05d', seq_len(2 * n)),
  arm = rep(c('standard', 'therapy'), each = n),
  score = c(stats::rnorm(n, 100, 10), stats::rnorm(n, 95, 10)))
compiled <- study_project(target, spec, data, study_dependency_lock(), args[[2]],
  units = c(score = 'points'), seed = 20261104)
saveRDS(compiled$declaration, record)
run <- study_run(compiled$directory, compiled$declaration)
saveRDS(run$declaration, record)
rendered <- study_render(compiled$directory, run$declaration)
saveRDS(rendered$declaration, record)
stopifnot(study_validate_project(compiled$directory, rendered$declaration,
  require_run = TRUE, require_render = TRUE)$valid)
cat('Complete local report:', file.path(compiled$directory, 'docs/index.html'), '\n')
