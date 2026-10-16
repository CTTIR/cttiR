release_fixture <- function(parent = NULL, env = parent.frame()) {
  if (is.null(parent)) {
    parent <- tempfile("cttir-release-")
    dir.create(parent)
    withr::defer(unlink(parent, recursive = TRUE), envir = env)
  }
  model <- file.path(parent, "synthetic.gguf")
  writeBin(charToRaw("GGUFsynthetic transport fixture; not a model"), model)
  runtime <- read_document(resource_file("runtime", "manifest.json"))
  manifest <- list(schema_version = 1L, release_id = "synthetic-transport-1",
    package_version = as.character(utils::packageVersion("cttiR")), catalog_id = resolve_catalog()$content_id,
    policy_sha256 = file_hash(resource_file("extdata", "code-policy-v1.json")),
    prompt_version = assistant_prompt_version, runtime_version = runtime$runtime_version,
    platform = "Linux", arch = "x86_64", base_model = list(repository = "synthetic/fixture",
      revision = strrep("a", 40L), adapter_sha256 = strrep("b", 64L), license = "Synthetic test text"),
    model = list(tag = "synthetic:transport", file = "synthetic.gguf", sha256 = file_hash(model),
      bytes = file.info(model)$size, url = NULL, digest = strrep("c", 64L), template = "{{ .Prompt }}",
      template_sha256 = content_hash("{{ .Prompt }}"), import_template = "{{ .Prompt }}",
      parameters = list(num_ctx = 4096L, num_predict = 1024L, temperature = 0, stop = list("</s>"))),
    resources = list(minimum_memory_bytes = 1073741824, required_disk_bytes = 2147483648))
  path <- file.path(parent, "release.json")
  write_bytes(paste0(json_text(manifest, TRUE), "\n"), path)
  list(parent = parent, path = path, hash = file_hash(path), model = model, manifest = manifest)
}

release_test_resources <- function(...) list(platform = "Linux", arch = "x86_64",
  available_memory_bytes = 1e12, available_disk_bytes = 1e12)
