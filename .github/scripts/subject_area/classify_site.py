"""Keep cases/subjects.json current: one subject-area label per docket that has
questions presented on the site.

Incremental. A docket is (re)classified only when its input changed -- the
cleaned questions presented or the caption -- or when the model or the area
definitions did (areas.py; see classify.DEFS). Everything else is carried over,
so a daily run costs a handful of requests.

Input, as the pages see it: the site's three QP caches, merged in the order
render_dockets_for() merges them (conferences, arguments, dashboards; later
wins), and the captions in cases/search.json.

Output, cases/subjects.json:
  {"_meta": {"model", "defs", "updated"},
   "<docket>": {"area", "confidence", "second", "hash", "by"}          -- classified
   "<docket>": {"area": null, "unreadable": "<reason>", "hash"}       -- skipped}
The raw confidence is stored; the display threshold is applied at render
(SUBJECT_MIN_CONFIDENCE in R/subject_area.R), so moving it needs no API call.

Never fatal to the caller's pipeline: without a key it says so and exits 0,
leaving the file as it was.

Usage: python classify_site.py --site site [--max-new 5000]
"""
import argparse
import asyncio
import datetime as dt
import hashlib
import json
import os
from pathlib import Path

try:
    from classify import DEFAULT_MODEL, DEFS, api_key, case_text, classify
except ModuleNotFoundError:
    # Say where this interpreter looked. An import that works in the install
    # step and fails here means the environment moved it, not that the package
    # is missing -- and the traceback alone cannot tell those apart.
    import sys
    print(f'executable {sys.executable}\nprefix {sys.prefix}\npath {sys.path}\n'
          f'LD_LIBRARY_PATH {os.environ.get("LD_LIBRARY_PATH")}', file=sys.stderr)
    raise
from readable import readable

CHUNK = 500   # requests between checkpoints


def load_json(p: Path):
    try:
        return json.loads(p.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return {}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--site', default='site')
    ap.add_argument('--model', default=DEFAULT_MODEL)
    ap.add_argument('--max-new', type=int, default=5000,
                    help='cap on API requests this run; the rest wait for the next')
    ap.add_argument('--dry-run', action='store_true', help='count what would be classified, call nothing')
    a = ap.parse_args()
    site = Path(a.site)

    qp = {}
    for sec in ('conferences', 'arguments', 'dashboards'):
        for k, v in load_json(site / sec / 'qp_cache.json').items():
            q = v.get('qp') if isinstance(v, dict) else None
            if q and q != '-':
                qp[k] = q
    captions = load_json(site / 'cases' / 'search.json')
    out_path = site / 'cases' / 'subjects.json'
    prev = load_json(out_path)
    prev.pop('_meta', None)
    by = f'{a.model}|{DEFS}'   # which classifier produced an entry

    out, todo, skipped = {}, {}, 0
    for dkt, q in qp.items():
        text = case_text(captions.get(dkt, dkt), q)
        h = hashlib.sha256(text.encode('utf-8')).hexdigest()[:16]
        old = prev.get(dkt)
        if old and old.get('hash') == h and (old.get('by') == by or old.get('unreadable')):
            out[dkt] = old
            continue
        ok, why = readable(q)
        if not ok:
            out[dkt] = {'area': None, 'unreadable': why, 'hash': h}
            skipped += 1
        else:
            todo[dkt] = (text, h)

    batch = dict(list(todo.items())[:a.max_new])
    print(f'{len(qp)} dockets with QPs: {len(out) - skipped} unchanged, {skipped} newly unreadable, '
          f'{len(todo)} to classify ({len(batch)} this run)')
    if a.dry_run:
        return
    if batch and not (os.getenv('TYPESAFE_API_KEY') or (Path.home() / '.typesafe_key').exists()
                      or (Path.home() / '.typesafe_key.txt').exists()):
        print('no TYPESAFE_API_KEY: leaving subjects.json unchanged')
        return

    def write(done: int) -> None:
        # Dockets whose QP left the caches keep their label (a page already
        # shows it), and entries not yet re-classified keep their old label and
        # "by" until a run reaches them.
        full = dict(out)
        for dkt, v in prev.items():
            full.setdefault(dkt, v)
        meta = {'model': a.model, 'defs': DEFS, 'pending': len(todo) - done,
                'updated': dt.datetime.now(dt.timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')}
        out_path.parent.mkdir(parents=True, exist_ok=True)
        tmp = out_path.with_suffix('.tmp')
        tmp.write_text(json.dumps({'_meta': meta, **dict(sorted(full.items()))}, separators=(',', ':')),
                       encoding='utf-8')
        tmp.replace(out_path)   # never a half-written file, even if the job is killed mid-write
        n_lab = sum(1 for v in full.values() if v.get('area'))
        print(f'wrote {out_path}: {n_lab} labelled, {len(full) - n_lab} unreadable, {len(todo) - done} pending')

    if batch:
        api_key()  # fail here, not per request, on an unreadable key file
        # In chunks, writing after each: a bulk fill killed by a timeout keeps
        # everything it finished, and the next run starts where it stopped.
        items, done = list(batch.items()), 0
        for i in range(0, len(items), CHUNK):
            chunk = dict(items[i:i + CHUNK])
            res = asyncio.run(classify({k: t for k, (t, _) in chunk.items()}, None, a.model))
            for dkt, (_, h) in chunk.items():
                r = res.get(dkt)
                if not r:
                    continue  # a failed request: retried next run
                probs = r.get('probabilities') or {}
                second = sorted(probs, key=probs.get, reverse=True)[1:2]
                out[dkt] = {'area': r['area'], 'confidence': round(float(r['confidence']), 3),
                            'second': second[0] if second else None, 'hash': h, 'by': by}
            done += len(chunk)
            write(done)
    else:
        write(0)


if __name__ == '__main__':
    main()
