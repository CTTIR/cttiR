test_that("benchmark exposes removed claims without changing post-grounding thresholds", {
  case <- planner_cases()$cases[[1]]
  case$inputs$goal <- "Fit linear regression for blood pressure."
  case$expected$capabilities <- list(required = list("std.model.lm"), optional = list())
  runner <- function(...) {
    proposal <- deterministic_proposal(case$inputs$goal)
    proposal$capability_ids <- c("std.model.lm", "std.import.spreadsheet")
    output <- list(proposal = proposal, provenance = list(planner_mode = "local_llm"),
      latency_seconds = 0, attempts = list(list(accepted = FALSE, errors = "invalid_schema",
        raw = '{"capability_ids":["invented.capability"]}'),
        list(accepted = TRUE, errors = character(), raw = json_text(proposal))))
    planner_ground(output, case$inputs$goal)
  }
  result <- planner_benchmark(list(case), "local_llm", runner = runner)
  expect_equal(result$summary$capability$precision, 1)
  expect_equal(result$summary$pre_grounding_capability$precision, 0.5)
  expect_equal(result$summary$pre_grounding_capability$unsupported_share, 0.5)
  expect_equal(result$summary$grounding_removed, 1L)
  expect_equal(result$summary$rejection_rate, 1)
  expect_equal(result$summary$pre_validation_claims$scored_attempts, 2L)
  expect_equal(result$summary$pre_validation_claims$unsupported_claims, 2L)
  expect_setequal(result$rows[[1]]$pre_grounding_ids, c("std.model.lm", "std.import.spreadsheet"))
  baseline <- planner_benchmark(list(case))
  expect_equal(baseline$summary$pre_grounding_capability$precision, baseline$summary$capability$precision)
  expect_null(baseline$summary$rejection_rate)
})

test_that("unavailable and malformed diagnostic output is not scored as zero unsupported claims", {
  case <- planner_cases()$cases[[1]]
  attempts <- lapply(list(NULL, NA_character_, "not JSON", '{"capability_ids":null}',
    '{"capability_ids":[1]}', '{"capability_ids":[],"capability_ids":[]}'), function(raw) list(raw = raw))
  result <- planner_raw_claims(attempts, case)
  expect_equal(result$attempts, 6L)
  expect_equal(result$scored_attempts, 0L)
  empty <- planner_raw_claims(list(list(raw = '{"capability_ids":[]}')), case)
  expect_equal(empty$scored_attempts, 1L)
  expect_equal(empty$proposed_ids, 0L)
})
