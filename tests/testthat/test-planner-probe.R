test_that("live audit requires a grounded approved planner response beyond JSON smoke", {
  local_mocked_bindings(runtime_owner = function(...) list(model = "fixture:local", model_digest = "digest"),
    local_model = function(...) list(digest = "digest"), runtime_probe = function(...) list(state = "pass"))
  goal <- "Fit linear regression of blood pressure on age in independent adults."
  plan <- planner_ground(list(proposal = deterministic_proposal(goal),
    provenance = list(planner_mode = "local_llm", prompt_version = planner_prompt_version, options = planner_options())), goal)
  local_mocked_bindings(plan_goal = function(name, type, goal, mode, endpoint, model, allow_unqualified) {
    expect_identical(name, "Synthetic planner probe")
    expect_true(allow_unqualified)
    plan
  })
  result <- audit_int_live(list(live = TRUE))
  expect_identical(result$status, "pass")
  plan$provenance$planner_mode <- "deterministic"
  expect_error(audit_int_live(list(live = TRUE)), class = "cttir_runtime_unavailable")
  plan$provenance$planner_mode <- "local_llm"
  plan$proposal$capability_ids <- character()
  expect_error(audit_int_live(list(live = TRUE)), "synthetic grounded planner probe")
  plan$proposal$capability_ids <- "std.model.lm"
  local_mocked_bindings(capability_approval = function(...) list(status = "pending"))
  expect_error(audit_int_live(list(live = TRUE)), "workflow approval evidence")
})
