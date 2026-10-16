# Explicit offline review of the cellspec structural conversion recipe.
# Usage: Rscript tools/verify-cellspec-structure.R PINNED_LIBRARY RECEIPT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, dir.exists(args[[1L]]), !file.exists(args[[2L]]))
.libPaths(c(normalizePath(args[[1L]]), .libPaths()))
for (package in c("cellspecR", "gatekeepR")) {
  stopifnot(normalizePath(find.package(package)) == normalizePath(file.path(args[[1L]], package)),
    identical(as.character(utils::packageVersion(package)), "1.0.0"))
}
results <- list()
check <- function(name, code) {
  force(code)
  results[[length(results) + 1L]] <<- list(test_name = name, result = "pass",
    run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}
expect_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))
cells <- data.frame(cell_id = c("c1", "c2", "c3"), image_id = "img1",
  x = c(10, 20, 30), y = c(5, 5, 5))
measurements <- cbind("cell:CD3e:mean" = c(1, 50, 3), "nucleus:FOXP3:mean" = c(0, 12, 1))
input <- serialize(list(cells, measurements), NULL)
x <- gatekeepR::gk_cellspec(cells, measurements)
check("producer preserves input and leaves unknown calibration explicit", {
  stopifnot(identical(serialize(list(cells, measurements), NULL), input),
    all(is.na(x$images$pixel_size)), identical(x$measurements, measurements))
})
check("producer rejects incomplete tables and invalid measurement shape", {
  expect_error(gatekeepR::gk_cellspec(cells[c("cell_id", "image_id", "x")], measurements))
  expect_error(gatekeepR::gk_cellspec(cells, measurements[-1L, , drop = FALSE]))
  expect_error(gatekeepR::gk_cellspec(cells, as.data.frame(measurements)))
})
check("shared class does not establish structural interchangeability", {
  report <- cellspecR::cs_validate(x, level = "structure")
  stopifnot(inherits(x, "cellspec"),
    setequal(report$check[report$status == "fail"], c("images_pixel_size", "provenance_fields")))
})
repair <- function(input, calibration) {
  images <- input$images
  images$pixel_size <- calibration
  cellspecR::cs_new(cells = input$cells, measurements = input$measurements,
    dictionary = input$dictionary, images = images, channels = input$channels,
    provenance = input$provenance)
}
before <- serialize(x, NULL)
repaired <- repair(x, 0.5)
check("explicit synthetic calibration yields valid structure without input mutation", {
  report <- cellspecR::cs_validate(repaired, level = "structure")
  stopifnot(identical(serialize(x, NULL), before), nrow(report) == 45L,
    all(report$status == "pass"), identical(repaired$measurements, measurements),
    identical(repaired$provenance$producer, x$provenance$producer))
})
check("constructor refuses absent zero and negative calibration", {
  for (calibration in c(NA_real_, 0, -0.5)) expect_error(repair(x, calibration))
})
check("constructor refuses duplicate cell keys and row misalignment", {
  duplicate <- x; duplicate$cells$cell_id[2L] <- duplicate$cells$cell_id[1L]
  expect_error(repair(duplicate, 0.5))
  mismatch <- x; mismatch$measurements <- mismatch$measurements[-1L, , drop = FALSE]
  expect_error(repair(mismatch, 0.5))
})
check("validator reports corrupted keys without repairing data", {
  bad <- repaired; bad$cells$cell_id[2L] <- bad$cells$cell_id[1L]
  frozen <- serialize(bad, NULL)
  report <- cellspecR::cs_validate(bad, level = "structure")
  stopifnot(any(report$status == "fail"), identical(serialize(bad, NULL), frozen),
    all(c("check", "level", "status", "n", "message") %in% names(report)))
})
check("nonfinite normalization is disclosed in provenance and preserves the source", {
  bad <- x; bad$measurements[1L, 1L] <- Inf
  frozen <- serialize(bad, NULL)
  converted <- repair(bad, 0.5)
  stopifnot(is.na(converted$measurements[1L, 1L]),
    identical(converted$provenance$counts$nonfinite_converted, 1L),
    identical(serialize(bad, NULL), frozen))
})
jsonlite::write_json(list(status = "structural_recipe_review_passed", fixtures = results,
  fixture_count = length(results), structure_checks = 45L, scientific_validity = "not_assessed",
  calibration_scope = "Explicitly supplied synthetic fixture only",
  recipe_scope = "gk_cellspec construction; cs_new normalization and structural validation; cs_validate report",
  script_sha256 = digest::digest(file = "tools/verify-cellspec-structure.R", algo = "sha256")),
  args[[2L]], pretty = TRUE, auto_unbox = TRUE)
