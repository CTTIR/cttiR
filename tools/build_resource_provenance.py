"""Add metadata only from cached DESCRIPTION bytes matching recorded observations.

Usage: python3 tools/build_resource_provenance.py INVENTORY.json
The inventory supplies local paths only. No Authors@R expressions are evaluated.
Run after retaining the current database in reshist/index.json.
"""
import gzip
import hashlib
import json
import pathlib
import sqlite3
import sys

root = pathlib.Path('inst/extdata')
inventory = json.loads(pathlib.Path(sys.argv[1]).read_text())
database = root / 'package-resources.sqlite'
prior_hash = hashlib.sha256(database.read_bytes()).hexdigest()
history = json.loads((root / 'reshist/index.json').read_text())['snapshots']
assert prior_hash in [x['sha256'] for x in history.values()], 'Retain the old snapshot first'
mirror = json.loads(gzip.decompress((root / 'package-resources.json.gz').read_bytes()))
fields = ['author', 'authors_r_literal', 'maintainer', 'maintainer_description_sha256', 'maintainer_evidence_status']
verified = {}
for item in inventory['records']:
    source = pathlib.Path(item['path'])
    raw = source.read_bytes()
    digest = hashlib.sha256(raw).hexdigest()
    assert digest == item['description_sha256'], 'Cached source changed'
    dcf = {}
    key = None
    for line in raw.decode('utf-8').splitlines():
        if line.startswith((' ', '\t')) and key:
            dcf[key] += '\n' + line.strip()
        elif ':' in line:
            key, value = line.split(':', 1)
            dcf[key] = value.strip()
    verified[(dcf.get('Package'), dcf.get('Version'), digest)] = dcf
names = {p['package_id']: p['name'] for p in mirror['packages']}
matched = []
con = sqlite3.connect(database)
with con:
    for field in fields:
        con.execute('ALTER TABLE observations ADD COLUMN ' + field + ' TEXT')
    for obs in mirror['observations']:
        dcf = verified.get((names[obs['package_id']], obs['observed_version'], obs['source_sha256']))
        values = dict.fromkeys(fields)
        if dcf is not None:
            values.update(author=dcf.get('Author'), authors_r_literal=dcf.get('Authors@R'),
                          maintainer=dcf.get('Maintainer'), maintainer_description_sha256=obs['source_sha256'],
                          maintainer_evidence_status='description_hash_matched_not_ownership_verification')
            matched.append(obs['observation_id'])
        obs.update(values)
        con.execute('UPDATE observations SET ' + ', '.join(f + '=?' for f in fields) + ' WHERE observation_id=?',
                    [values[f] for f in fields] + [obs['observation_id']])
    mirror['manifest'].pop('content_id', None)
    # Semantic snapshot identity excludes its own identifier; file hashes are separate.
    canonical = json.dumps(mirror, sort_keys=True, separators=(',', ':'), ensure_ascii=False).encode()
    mirror['manifest']['content_id'] = 'sha256:' + hashlib.sha256(canonical).hexdigest()
    con.execute('UPDATE catalog_metadata SET value_json=? WHERE key=?',
                (json.dumps(mirror['manifest']['content_id']), 'content_id'))
con.close()
raw = (json.dumps(mirror, ensure_ascii=False, indent=2) + '\n').encode()
(root / 'package-resources.json.gz').write_bytes(gzip.compress(raw, mtime=0))
(root / 'resource-manifest.json').write_text(json.dumps(mirror['manifest'], ensure_ascii=False, indent=2) + '\n')
schema = json.loads((root / 'resource-catalog.schema.json').read_text())
for field in fields:
    schema['properties']['observations']['items']['properties'][field] = {'type': ['string', 'null']}
(root / 'resource-catalog.schema.json').write_text(json.dumps(schema, indent=2) + '\n')
(root / 'mirror-hashes.json').write_text(json.dumps({'package-resources.json': hashlib.sha256(raw).hexdigest(),
    'note': 'SHA-256 of the decompressed current resource mirror.'}, indent=2) + '\n')
hashes = json.loads((root / 'file-hashes.json').read_text())
for path in [*hashes, 'reshist/index.json', 'reshist/' + prior_hash + '.sqlite']:
    hashes[path] = hashlib.sha256((root / path).read_bytes()).hexdigest()
(root / 'file-hashes.json').write_text(json.dumps(hashes, indent=2) + '\n')
print(json.dumps({'matched_observations': matched, 'count': len(matched), 'content_id': mirror['manifest']['content_id']}))
