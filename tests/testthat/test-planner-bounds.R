test_that("overbroad deterministic matches abstain within the proposal schema", {
  registry <- capability_registry()
  aliases <- vapply(planner_capabilities(registry), function(cap)
    if (length(cap$keywords$en)) cap$keywords$en[[1L]] else "", "")
  goal <- paste(aliases[nzchar(aliases)], collapse = "; ")
  proposal <- deterministic_proposal(goal, registry)
  expect_length(proposal$capability_ids, 0L)
  expect_true(any(grepl("Too many capabilities", proposal$unresolved, fixed = TRUE)))
  expect_true(planner_validate(planner_proposal_json(proposal), registry = registry)$ok)
  # Fallback must have the same safe bounds as direct deterministic requests.
  fallback <- plan_goal("Broad plan", "methods", goal, mode = "local_llm",
    endpoint = "https://invalid.example")
  expect_identical(fallback$proposal, proposal)
})

test_that("candidate proposal notes preserve the distinction from workflow approval", {
  registry <- capability_registry()
  candidate <- Filter(function(cap) identical(cap$status, "candidate"),
    planner_capabilities(registry))[[1L]]
  proposal <- list(aim = "unknown", outcome_family = "unknown", unit_structure = "unknown",
    modality = "unknown", capability_ids = candidate$id, rationale = "Registered candidate.",
    unresolved = rep("Design details remain unresolved.", planner_limits$notes))
  bounded <- planner_bound_proposal(proposal, registry)
  expect_identical(bounded$capability_ids, candidate$id)
  expect_length(bounded$unresolved, planner_limits$notes)
  expect_match(tail(bounded$unresolved, 1L), "unapproved candidates", fixed = TRUE)
  expect_true(planner_validate(planner_proposal_json(bounded), registry = registry)$ok)
  approved <- proposal; approved$capability_ids <- "std.model.lm"
  expect_identical(planner_bound_proposal(approved, registry), approved)
})

test_that("planner qualification is invalidated by a changed grounding helper", {
  manifest <- read_document(resource_file("runtime", "manifest.json"))
  entry <- manifest$tested_models[[1L]]
  entry$qualification <- "qualified_for_planning"
  entry$qualification_context <- planner_qualification_context(manifest)
  manifest$tested_models <- list(entry)
  expect_identical(planner_qualification(entry$tag, entry$digest, manifest), "qualified_for_planning")
  testthat::local_mocked_bindings(planner_bound_proposal = function(proposal, registry) proposal)
  expect_identical(planner_qualification(entry$tag, entry$digest, manifest), "not_qualified_for_planning")
})
