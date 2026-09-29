"""Score Jev against a hand-labelled sample of ungranted petitions, and write a
review sheet.

The Database check (eval_scdb.py) covers decided cases only. This covers what
the pages mostly show: denied and pending petitions, paid and IFP, many pro se.
The sample is a JSON list of {dkt, type, caption, qp, label}, where `label` is
the hand label and null means the questions presented are unreadable (OCR
noise, a cover page) -- those are counted separately, not scored, because the
site should show no label for them at all.

The review sheet (CSV) lists every case with the hand label, Jev's pick and
confidence, sorted disagreements first, with a blank `reviewed_label` column.
Fill it in and re-run with --reviewed to score against the corrected labels.

Usage:
  python eval_sample.py --sample sample_labelled.json --label-field claude_label --out out/
  python eval_sample.py --sample sample_labelled.json --reviewed out/review_sample.csv --out out/
"""
import argparse
import asyncio
import csv
import json
from pathlib import Path

from classify import DEFAULT_MODEL, case_text, classify
from eval_scdb import metrics


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--sample', required=True)
    ap.add_argument('--label-field', default='label')
    ap.add_argument('--reviewed', help='review CSV with a filled-in reviewed_label column')
    ap.add_argument('--out', default='out')
    ap.add_argument('--model', default=DEFAULT_MODEL)
    a = ap.parse_args()

    rows = json.loads(Path(a.sample).read_text(encoding='utf-8'))
    for r in rows:
        r['id'] = r['dkt']
        r['label'] = r.get(a.label_field)
    if a.reviewed:
        rev = {x['dkt']: x['reviewed_label'].strip() for x in csv.DictReader(open(a.reviewed, encoding='utf-8-sig'))}
        for r in rows:
            v = rev.get(r['dkt'], '')
            if v:
                r['label'] = None if v.upper() in ('NA', 'NONE', 'UNREADABLE') else v

    out = Path(a.out)
    safe = a.model.replace(':', '_')
    pred = asyncio.run(classify({r['id']: case_text(r['caption'], r['qp']) for r in rows},
                                out / f'pred_sample_{safe}.jsonl', a.model))

    readable = [r for r in rows if r['label']]
    print(f'{len(rows)} cases; {len(rows) - len(readable)} with unreadable questions presented (not scored)')
    report = {}
    for name, sub in (('all', readable), ('paid', [r for r in readable if r['type'] == 'paid']),
                      ('ifp', [r for r in readable if r['type'] == 'ifp'])):
        m = metrics(sub, pred)
        report[name] = m
        print(f"\n{name}: n={m['n']}  accuracy={m['accuracy']:.3f}  macro-F1={m['macro_f1']:.3f}"
              f"  top-2={m.get('top2_accuracy', float('nan')):.3f}  ECE={m.get('ece', float('nan')):.3f}")
        for c in m.get('coverage', []):
            print(f"  conf >= {c['threshold']:.1f}: coverage {c['coverage']:.0%}, accuracy {c['accuracy']}")
        if name == 'all':
            for pair, k in m['confusions']:
                print(f'  {k:3d}  {pair}')
    # What Jev says about the unreadable ones: if it is confident on noise, the
    # site needs its own readability check rather than a confidence threshold.
    junk = [pred[r['id']] for r in rows if not r['label'] and r['id'] in pred]
    if junk:
        report['unreadable_confidence'] = sorted(round(p['confidence'], 3) for p in junk)
        print('\nconfidence on unreadable inputs:', report['unreadable_confidence'])
    tag = 'reviewed' if a.reviewed else a.label_field
    (out / f'metrics_sample_{tag}_{safe}.json').write_text(json.dumps(report, indent=2), encoding='utf-8')

    if not a.reviewed:
        sheet = out / 'review_sample.csv'
        def key(r):
            p = pred.get(r['id'], {})
            return (p.get('area') == r['label'], r['type'], r['n'] if 'n' in r else 0)
        with sheet.open('w', encoding='utf-8-sig', newline='') as fh:
            w = csv.writer(fh)
            w.writerow(['n', 'dkt', 'type', 'caption', 'questions_presented', 'hand_label', 'hand_unsure',
                        'jev_label', 'jev_confidence', 'jev_second', 'agree', 'reviewed_label'])
            for r in sorted(rows, key=key):
                p = pred.get(r['id'], {})
                probs = p.get('probabilities') or {}
                second = sorted(probs, key=probs.get, reverse=True)[1:2]
                w.writerow([r.get('n'), r['dkt'], r['type'], r['caption'], ' '.join(r['qp'].split())[:1500],
                            r['label'] or 'UNREADABLE', 'yes' if r.get('claude_unsure') else '',
                            p.get('area'), round(p.get('confidence') or 0, 3), second[0] if second else '',
                            'yes' if p.get('area') == r['label'] else 'NO', ''])
        print(f'\nreview sheet: {sheet}')


if __name__ == '__main__':
    main()
