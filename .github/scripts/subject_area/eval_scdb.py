"""Score Jev's subject-area picks against the Supreme Court Database.

The labelled set is every decided case the Database codes for OT2017 on whose
docket we have cached questions presented. Two splits, by the Term of decision:

  dev   OT2017-OT2021   tune areas.py against this, as often as needed
  test  OT2022-OT2025   run once the definitions are settled; report, don't tune

Caveat that no number here removes: the Database codes only cases the Court
decided, which are mostly granted cases with polished questions presented. The
pages will mostly show denied petitions, many pro se. Accuracy here is an upper
bound for them, to be checked against a hand-labelled sample.

Usage:
  python eval_scdb.py --scdb SCDB_2026_01_caseCentered_Docket.csv \
      --qp qp_arguments.json qp_conferences.json qp_dashboards.json \
      --out out/ --split dev [--limit 20] [--baseline]
"""
import argparse
import asyncio
import collections
import csv
import json
import math
from pathlib import Path

from classify import AREAS, DEFAULT_MODEL, case_text, classify

DEV_TERMS = range(2017, 2022)
TEST_TERMS = range(2022, 2026)


def load_qp(paths):
    qp = {}
    for p in paths:
        for k, v in json.loads(Path(p).read_text(encoding='utf-8')).items():
            q = v.get('qp') if isinstance(v, dict) else v
            if q and k not in qp:
                qp[k] = q
    return qp


def load_labelled(scdb_path, qp):
    rows = []
    with open(scdb_path, encoding='latin-1', newline='') as fh:
        for r in csv.DictReader(fh):
            term, area, dkt = int(r['term']), r['issueArea'].strip(), r['docket'].strip()
            if term < 2017 or not area or dkt not in qp:
                continue
            rows.append({'id': dkt, 'term': term, 'label': AREAS[int(area) - 1],
                         'text': case_text(r['caseName'], qp[dkt])})
    # A docket the Database lists twice (rare) keeps its first coding.
    seen, out = set(), []
    for r in rows:
        if r['id'] not in seen:
            seen.add(r['id'])
            out.append(r)
    return out


def metrics(rows, pred):
    """rows: labelled cases; pred: id -> {'area', 'confidence', 'probabilities'}."""
    scored = [(r, pred[r['id']]) for r in rows if r['id'] in pred]
    n = len(scored)
    if n == 0:
        return {'n': 0}
    hit = [p['area'] == r['label'] for r, p in scored]
    out = {'n': n, 'accuracy': sum(hit) / n}

    # Macro-F1 over the areas that occur, so a 3-case area counts as much as a 150-case one.
    f1s, per = [], {}
    for a in AREAS:
        tp = sum(1 for r, p in scored if p['area'] == a and r['label'] == a)
        fp = sum(1 for r, p in scored if p['area'] == a and r['label'] != a)
        fn = sum(1 for r, p in scored if p['area'] != a and r['label'] == a)
        if tp + fn == 0 and fp == 0:
            continue
        prec = tp / (tp + fp) if tp + fp else 0.0
        rec = tp / (tp + fn) if tp + fn else 0.0
        f1 = 2 * prec * rec / (prec + rec) if prec + rec else 0.0
        per[a] = {'support': tp + fn, 'predicted': tp + fp, 'precision': round(prec, 3),
                  'recall': round(rec, 3), 'f1': round(f1, 3)}
        if tp + fn:
            f1s.append(f1)
    out['macro_f1'] = sum(f1s) / len(f1s)
    out['per_area'] = per
    out['confusions'] = collections.Counter(
        f"{r['label']} -> {p['area']}" for r, p in scored if p['area'] != r['label']).most_common(12)

    # Second pick, where the distribution came back.
    top2 = [r['label'] in sorted(p['probabilities'], key=p['probabilities'].get, reverse=True)[:2]
            for r, p in scored if isinstance(p.get('probabilities'), dict)]
    if top2:
        out['top2_accuracy'] = sum(top2) / len(top2)

    # Calibration: does a 0.8 mean right 80% of the time? Ten equal-width bins,
    # and the expected calibration error over them.
    conf = [(float(p['confidence']), h) for (r, p), h in zip(scored, hit)
            if isinstance(p.get('confidence'), (int, float))]
    if conf:
        bins = collections.defaultdict(list)
        for c, h in conf:
            bins[min(int(c * 10), 9)].append((c, h))
        table, ece = [], 0.0
        for b in sorted(bins):
            cs = bins[b]
            mc = sum(c for c, _ in cs) / len(cs)
            acc = sum(h for _, h in cs) / len(cs)
            ece += len(cs) / len(conf) * abs(mc - acc)
            table.append({'bin': f'{b / 10:.1f}-{(b + 1) / 10:.1f}', 'n': len(cs),
                          'mean_confidence': round(mc, 3), 'accuracy': round(acc, 3)})
        out['calibration'] = table
        out['ece'] = ece
        # What a display threshold buys: label only picks at or above t.
        out['coverage'] = []
        for t in (0.0, 0.5, 0.6, 0.7, 0.8, 0.9):
            kept = [h for c, h in conf if c >= t]
            out['coverage'].append({'threshold': t, 'coverage': round(len(kept) / len(conf), 3),
                                    'accuracy': round(sum(kept) / len(kept), 3) if kept else None})
    return out


def baseline(dev, test):
    """Majority class, and TF-IDF + logistic regression trained on dev. A floor,
    not a rival: ~300 training cases is thin for 14 classes."""
    out = {}
    maj = collections.Counter(r['label'] for r in dev).most_common(1)[0][0]
    out['majority'] = {'label': maj, 'accuracy': sum(r['label'] == maj for r in test) / len(test)}
    try:
        from sklearn.feature_extraction.text import TfidfVectorizer
        from sklearn.linear_model import LogisticRegression
    except ImportError:
        out['tfidf_lr'] = 'scikit-learn not installed'
        return out
    vec = TfidfVectorizer(ngram_range=(1, 2), min_df=2, sublinear_tf=True)
    X = vec.fit_transform([r['text'] for r in dev])
    clf = LogisticRegression(max_iter=2000, C=5.0).fit(X, [r['label'] for r in dev])
    proba = clf.predict_proba(vec.transform([r['text'] for r in test]))
    pred = {}
    for r, pr in zip(test, proba):
        dist = dict(zip(clf.classes_, pr))
        best = max(dist, key=dist.get)
        pred[r['id']] = {'area': best, 'confidence': dist[best], 'probabilities': dist}
    m = metrics(test, pred)
    out['tfidf_lr'] = {k: m[k] for k in ('n', 'accuracy', 'macro_f1', 'top2_accuracy', 'ece') if k in m}
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--scdb', required=True)
    ap.add_argument('--qp', nargs='+', required=True)
    ap.add_argument('--out', default='out')
    ap.add_argument('--split', choices=['dev', 'test'], default='dev')
    ap.add_argument('--model', default=DEFAULT_MODEL)
    ap.add_argument('--limit', type=int, default=0, help='first N cases only, for a smoke test')
    ap.add_argument('--baseline', action='store_true')
    a = ap.parse_args()

    rows = load_labelled(a.scdb, load_qp(a.qp))
    dev = [r for r in rows if r['term'] in DEV_TERMS]
    test = [r for r in rows if r['term'] in TEST_TERMS]
    use = dev if a.split == 'dev' else test
    if a.limit:
        use = use[:a.limit]
    print(f'labelled: {len(rows)} (dev {len(dev)}, test {len(test)}); scoring {len(use)} from {a.split}')

    out = Path(a.out)
    safe = a.model.replace(':', '_')
    pred = asyncio.run(classify({r['id']: r['text'] for r in use}, out / f'pred_{safe}.jsonl', a.model))
    m = metrics(use, pred)
    m['split'], m['model'] = a.split, a.model
    if a.baseline:
        m['baseline'] = baseline(dev, test if a.split == 'test' else dev)
    (out / f'metrics_{a.split}_{safe}.json').write_text(json.dumps(m, indent=2), encoding='utf-8')

    print(f"n={m['n']}  accuracy={m.get('accuracy', math.nan):.3f}  macro-F1={m.get('macro_f1', math.nan):.3f}"
          f"  top-2={m.get('top2_accuracy', math.nan):.3f}  ECE={m.get('ece', math.nan):.3f}")
    for c in m.get('coverage', []):
        print(f"  conf >= {c['threshold']:.1f}: coverage {c['coverage']:.0%}, accuracy {c['accuracy']}")
    for pair, k in m.get('confusions', []):
        print(f'  {k:3d}  {pair}')
    if a.baseline:
        print('baseline:', json.dumps(m['baseline'], default=str))


if __name__ == '__main__':
    main()
