"""Add reviewed, independently sourced DESCRIPTION observations without changing seed hashes.

Usage: python3 tools/build_supplemental_provenance.py reviewed-records.json
Inputs include local DESCRIPTION paths and reviewed literal roles. No source code
is executed. Existing snapshot bytes are retained before migration.
"""
import gzip
import hashlib
import json
import pathlib
import sqlite3
import sys

root = pathlib.Path('inst/extdata')
records = json.loads(pathlib.Path(sys.argv[1]).read_text())
verified = {}
for record in records:
    raw = pathlib.Path(record['path']).read_bytes()
    assert hashlib.sha256(raw).hexdigest() == record['description_sha256']
    dcf = {}
    key = None
    for line in raw.decode('utf-8').splitlines():
        if line.startswith((' ', '\t')) and key:
            dcf[key] += '\n' + line.strip()
        elif ':' in line:
            key, value = line.split(':', 1)
            dcf[key] = value.strip()
    assert (dcf['Package'], dcf['Version']) == (record['package'], record['version'])
    item = {k: v for k, v in record.items() if k != 'path'}
    item.update(author=dcf.get('Author'), maintainer=dcf.get('Maintainer'),
                authors_r_literal=dcf.get('Authors@R'))
    identity = (record['package'], record['version'])
    assert identity not in verified, 'Duplicate source identity'
    verified[identity] = item
mirror = json.loads(gzip.decompress((root / 'package-resources.json.gz').read_bytes()))
names = {x['package_id']: x['name'] for x in mirror['packages']}
targets = {(names[x['package_id']], x['observed_version']): x for x in mirror['observations']}
for identity, item in verified.items():
    assert identity in targets, 'No exact package/version observation'
    same = item['description_sha256'] == targets[identity]['source_sha256']
    assert same == (item['relation_to_seed'] == 'description_hash_matches')
db = root / 'package-resources.sqlite'
prior_hash = hashlib.sha256(db.read_bytes()).hexdigest()
prior_id = mirror['manifest']['content_id']
history_file = root / 'reshist/index.json'
history = json.loads(history_file.read_text())
retained = root / ('reshist/' + prior_hash + '.sqlite')
if retained.exists():
    assert retained.read_bytes() == db.read_bytes()
else:
    retained.write_bytes(db.read_bytes())
history['snapshots'][prior_id] = {'sha256': prior_hash, 'scope': 'Retained resource snapshot before supplemental DESCRIPTION observations'}
history_file.write_text(json.dumps(history, indent=2) + '\n')
field = 'supplemental_maintainer_evidence_json'
con = sqlite3.connect(db)
matched = []
with con:
    columns = [row[1] for row in con.execute('PRAGMA table_info(observations)')]
    if field not in columns:
        con.execute('ALTER TABLE observations ADD COLUMN ' + field + ' TEXT')
    for obs in mirror['observations']:
        item = verified.get((names[obs['package_id']], obs['observed_version']))
        if item is None:
            continue
        value = None
        if item:
            same = item['description_sha256'] == obs['source_sha256']
            assert same == (item['relation_to_seed'] == 'description_hash_matches')
            value = json.dumps(item, ensure_ascii=False, sort_keys=True)
            matched.append(obs['observation_id'])
        obs[field.removesuffix('_json')] = item
        con.execute('UPDATE observations SET ' + field + '=? WHERE observation_id=?', (value, obs['observation_id']))
    assert len(matched) == len(verified)
    mirror['manifest'].pop('content_id')
    raw = json.dumps(mirror, sort_keys=True, separators=(',', ':'), ensure_ascii=False).encode()
    mirror['manifest']['content_id'] = 'sha256:' + hashlib.sha256(raw).hexdigest()
    con.execute('UPDATE catalog_metadata SET value_json=? WHERE key=?', (json.dumps(mirror['manifest']['content_id']), 'content_id'))
con.close()
raw = (json.dumps(mirror, ensure_ascii=False, indent=2) + '\n').encode()
(root / 'package-resources.json.gz').write_bytes(gzip.compress(raw, mtime=0))
(root / 'resource-manifest.json').write_text(json.dumps(mirror['manifest'], ensure_ascii=False, indent=2) + '\n')
schema_file = root / 'resource-catalog.schema.json'
schema = json.loads(schema_file.read_text())
schema['properties']['observations']['items']['properties'][field.removesuffix('_json')] = {'type': ['object', 'null']}
schema_file.write_text(json.dumps(schema, indent=2) + '\n')
(root / 'mirror-hashes.json').write_text(json.dumps({'package-resources.json': hashlib.sha256(raw).hexdigest(), 'note': 'SHA-256 of the decompressed current resource mirror.'}, indent=2) + '\n')
hashes_file = root / 'file-hashes.json'
hashes = json.loads(hashes_file.read_text())
hashes[str(retained.relative_to(root))] = prior_hash
for name in hashes:
    hashes[name] = hashlib.sha256((root / name).read_bytes()).hexdigest()
hashes_file.write_text(json.dumps(hashes, indent=2) + '\n')
print(json.dumps({'previous_id': prior_id, 'previous_sha256': prior_hash, 'current_id': mirror['manifest']['content_id'], 'matched': matched}))
