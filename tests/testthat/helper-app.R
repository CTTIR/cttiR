app_test_worker <- function(operation, args) {
  if (isFALSE(args$dry_run) && operation == "project") expect_type(attr(args, "cttir_catalog_fingerprint"), "character")
  if (isFALSE(args$dry_run) && operation == "sync") expect_type(attr(args, "cttir_sync_plan"), "character")
  result <- tryCatch(list(ok = TRUE, value = do.call(getExportedValue("cttiR", operation), args)),
    error = function(e) list(ok = FALSE, message = conditionMessage(e))
  )
  list(is_alive = function() FALSE, get_result = function() result, kill = function() TRUE)
}
