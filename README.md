# cttiR

CTTIR Project Builder creates research project scaffolds from three inputs:
a name, research type and goal.

This is development milestone `0.0.4`. It provides an offline
builder, schema validation and a searchable resource snapshot. It does not yet
provide the complete research workflow described in the development plan.

```r
p <- cttiR::project(
  "Example Study",
  "primary_research",
  "Describe longitudinal measurements",
  path = tempdir()
)
print(p)

cttiR::resources("cytometry", repository = "Bioconductor", limit = 5)
```

`path` is an existing **parent directory**. This example creates its
`example_study/` child. Use `dry_run = TRUE` for a read-only preview. Repeating
the same request preserves accepted identity and edited files. Different inputs,
unsafe paths and conflicting destinations produce typed errors.

Each project contains a YAML specification, a separate build lock, file ownership
metadata, a data registry and dictionary, an analysis plan, publication folders
and a read-only validation script. Data, scientific decisions and approvals remain
unknown until supplied. No packages are installed, no model is called, no remote
requests are made and no study code is executed during creation. ReflowR
integration and dependency preparation are reported as pending. The lock records
scaffold provenance; it is not an `renv.lock`.

Configuration accepts named lists or local YAML/JSON files. Required arguments
take precedence over configuration; `options` overrides other configuration
fields. Unknown keys are rejected. Publication and dataset arrays merge by ID.
New publication entries need all fields of the resolved publication schema.
Unsupported integration requests fail explicitly. Validate documents with
`cttiR::validate_config()` and `cttiR::validate_spec()`.

`cttiR::packages()` and `cttiR::search()` inspect a separate static API catalog
from 30 public package roots across 40 enumerated CTTIR repositories. It contains
1,340 export records, with 1,323 statically resolved function signatures.
`cttiR::ask()` returns matching source evidence; it does not yet perform
natural-language planning or generate approved workflow code. Project pins
prevent silent substitution of another API snapshot.

`cttiR::update()` refreshes explicitly registered local source directories into
immutable API and resource snapshots, then activates both through one pointer.
It never installs packages or changes existing project pins. Use `dry_run = TRUE`
for a disposable preview and `cttiR::rollback_knowledge(id)` for a rollback preview.
Removed exports stay removed in the active revision. Remote refresh, discovery,
pruning and Bioconductor release migration remain unsupported and fail explicitly.
See `help("update", package = "cttiR")` for local source registration.

The bundled resource snapshot contains **229 research package candidates** from
2026-09-26. It preserves source URLs, observed versions, lifecycle information and
verification limitations. These are dated observations, not current availability
claims or approved executable adapters. Querying does not install packages.
Project-scoped queries honor the recorded resource pin and fail if unavailable.

`cttiR::sync()` previews explicit configuration changes and preserves edited files.
`cttiR::audit()` inspects local integrity; `cttiR::doctor()` provides brief diagnostics.
Repairs restore missing managed files whose baseline can be verified and recover
interrupted transactions only when the writer has stopped and hashes match.

`cttiR::setup(dry_run = TRUE)` previews local runtime setup. Explicit setup can
acquire the pinned portable Ollama runtime on Linux x86_64, verify a local model
and run a bounded CPU structured-output probe. It refuses unmanaged daemons and
cloud-backed models. Runtime/model files stay outside the package. Offline mode
reuses verified artifacts; no model is pulled. The candidate model has passed a
real inference smoke test but is not yet qualified for workflow planning.

Still pending: reflowR and analysis adapters, verified API/documentation catalog,
remote catalog refresh, workflow-model qualification, grounded planning, Shiny,
the complete workflow lifecycle and full audit coverage. Unimplemented
APIs are not exported as placeholders. See [CURRENT_STATE.md](CURRENT_STATE.md).

To build and test locally with the declared dependencies installed:

```sh
R CMD build .
R CMD check --no-manual cttiR_0.0.4.tar.gz
R CMD INSTALL cttiR_0.0.4.tar.gz
```

MIT licensed. Local validation currently covers Linux with R 4.6.1; other
platforms and the full product acceptance gates remain unverified.
