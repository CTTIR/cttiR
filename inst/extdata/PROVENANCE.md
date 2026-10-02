# Resource seed provenance

The resource database, JSON mirror and manifest were imported unchanged from the
user-supplied CTTIR project specification v7 resource snapshot dated 2026-09-26.
`file-hashes.json` records their imported SHA-256 values. The JSON mirror is
stored gzip-compressed (`package-resources.json.gz`); its decompressed bytes
equal the imported file (hash in `mirror-hashes.json`), which `audit()` check RES-001 verifies together with
SQLite/JSON table parity. Source observations and
retrieval hashes are retained in the SQLite/JSON records; no package installation
or API approval is implied. The database contains original research summaries
and metadata, not a complete redistributed upstream documentation corpus.

Upstream package licenses describe their respective upstream software. This
metadata does not grant rights to that software, documentation or website assets.
Workflow approvals and verified function records remain empty in this seed.
