# Regression for approval closure through executable default expressions.
# Run from the repository root: Rscript tools/test_template_defaults.R
units <- new.env(parent = baseenv())
needed <- c("empty_usage", "namespaced_usage", "template_names", "unit_closure", "closure_usage")
for (expr in parse("tools/build_standard_catalog.R", keep.source = FALSE)) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.name(expr[[2]]) &&
      as.character(expr[[2]]) %in% needed) eval(expr, envir = units)
}
entry <- quote(function(pins = reader()) validator(pins))
reader <- quote(function() jsonlite::fromJSON("metadata.json"))
validator <- quote(function(x) is.list(x))
functions <- list(entry = entry, reader = reader, validator = validator)
registry <- lapply(functions, function(expr) list(names = units$template_names(expr), usage = units$namespaced_usage(expr)))
closure <- units$unit_closure(registry, "entry")
stopifnot(setequal(closure, c("entry", "reader", "validator")))
usage <- units$closure_usage(registry, closure)
stopifnot(any(usage$package == "jsonlite" & usage$name == "fromJSON"))
nested <- quote(function(x = function(y = reader()) y, z) x())
stopifnot("reader" %in% units$template_names(nested))
cat("Default-expression approval closure passed.\n")
