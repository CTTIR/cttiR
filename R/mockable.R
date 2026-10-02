# Namespace bindings for base functions used in fault-injection tests.
# Calls inside the package skip these non-function bindings and resolve to base,
# but testthat::local_mocked_bindings() can replace them temporarily (the
# namespace is locked under R CMD check, so the bindings must already exist).
file.rename <- NULL
writeBin <- NULL
system.file <- NULL
