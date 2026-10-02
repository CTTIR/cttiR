# cttiR

CTTIR Project Builder creates research project scaffolds from three inputs:
a name, research type and goal.

This is development milestone `0.0.11`. It provides an offline
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
requests are made and no study code is executed during creation. New projects include a hashed adaptation of the reflowR minimal layout.
Explicit `Rscript code/render_report.R` uses rmarkdown to render placeholder
pages and a labelled synthetic example. The pinned provenance and MIT notice
are included; reflow_init and workflowr are not invoked. Full reflowR analysis
integration and dependency preparation remain pending. The lock records
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
The pinned reflowR revision additionally includes 31 licensed documentation texts,
with image omissions recorded. Search returns bounded literal excerpts. Local
source registration can declare a reviewed `documentation_rights` basis; otherwise
only documentation inventory and hashes are retained. Documentation-only changes
and removed documents participate in immutable updates and rollback. Previous
bundled catalog pins remain available. Indexed documents do not grant workflow
approval.

`cttiR::ask()` returns matching source evidence; it does not yet perform
natural-language planning or generate approved workflow code. Project pins
prevent silent substitution of another API snapshot.

`cttiR::update()` refreshes explicitly registered sources into
immutable API and resource snapshots, then activates both through one pointer.
It never installs packages or changes existing project pins. Use `dry_run = TRUE`
for a disposable preview and `cttiR::rollback_knowledge(id)` for a rollback preview.
Removed exports stay removed in the active revision. `mode = "remote"` supports
registered public GitHub repositories, resolves one commit, and verifies bounded
source downloads against Git blob hashes. Local mode never fetches remote records.
Discovery, pruning, non-GitHub fetch backends and Bioconductor release migration
remain pending. See `help("update", package = "cttiR")` for registration details.
Applied updates can recover a verified stopped local catalog writer after checking
the active snapshot and any activation journal. Writer evidence is retained;
previews never recover locks. Unknown/live writers and corrupt snapshots require
manual review.

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

With the suggested `shiny` and `callr` packages installed, `cttiR::setup_app()`
opens a local Fast/Detailed builder. `cttiR::configure(path)` previews and applies
explicit changes through the same synchronization engine. Read-only tools run in
background workers; stale previews are refused. Detailed configuration currently
uses validated JSON. Export and restore an unfinished draft as a local JSON file;
restoring requires a fresh preview and keeps the current parent directory.
Configure drafts are bound to their project identity. Exported answers may include
sensitive user text, so review the file before sharing. The full questionnaire
and remaining workflow views are still pending. This interface is for local use, not remote multi-user hosting.

Still pending: complete standard analysis adapters, approved documentation for all
workflow dependencies, non-GitHub source refresh, workflow-model qualification,
grounded planning, the complete application workflow,
the complete workflow lifecycle and full audit coverage. Unimplemented
APIs are not exported as placeholders. See [CURRENT_STATE.md](CURRENT_STATE.md).

To build and test locally with the declared dependencies installed:

```sh
R CMD build .
R CMD check --no-manual cttiR_0.0.11.tar.gz
R CMD INSTALL cttiR_0.0.11.tar.gz
```

MIT licensed. Milestone 0.0.5 passed hosted checks on Linux release/oldrel/devel,
Windows and macOS. Full product acceptance gates remain incomplete.
