#' cttiR: Reproducible Research Project Builder
#'
#' Creates deterministic research projects from a name, research type and
#' goal, routes them to a reviewed standard or specialist workflow, and keeps
#' a revision-scoped knowledge catalog of the package APIs that workflow may
#' call.
#'
#' @section Entry points:
#' * Projects: [project()] creates or previews a project, [sync()] previews
#'   and applies configuration changes, [validate_config()] and
#'   [validate_spec()] check customization input and specifications, and
#'   [check_analysis_data()] checks explicitly supplied data.
#' * Synthetic study workflow: [study_spec()] and [study_paired_spec()]
#'   declare a study, [study_project()] compiles it, [study_run()] analyses
#'   it and [study_render()] renders the reports.
#' * Knowledge and resources: [packages()], [search()], [ask()] and
#'   [resources()] read the catalog; [update()] and [rollback_knowledge()]
#'   change the active snapshot.
#' * Audit and runtime: [audit()] and [doctor()] inspect an installation or a
#'   project; [setup()], [setup_app()] and [configure()] prepare the optional
#'   local runtime and the local application.
#'
#' @section Masked functions:
#' Two exports share their names with functions of the base distribution:
#' [search()] with [base::search()] and [update()] with [stats::update()].
#' After `library(cttiR)` the unqualified names refer to cttiR, so
#' `update(fit, ...)` no longer updates a fitted model. Either call cttiR
#' without attaching it (`cttiR::project()`, `cttiR::search()`), as the
#' README and vignettes do, or qualify the masked functions
#' (`base::search()`, `stats::update()`). [update_knowledge()] is the same
#' function as [update()] under a name that masks nothing.
#'
#' @keywords internal
"_PACKAGE"
