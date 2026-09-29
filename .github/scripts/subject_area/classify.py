"""Classify cases into subject-matter areas with Jev (TypeSafe).

One request per case. Every result -- the pick, its confidence, and the full
distribution over the 14 areas -- is appended to a JSONL file as it arrives, so
an interrupted run resumes where it stopped rather than paying twice.

The input is the text a reader of the case page sees first: the caption and the
questions presented. Evaluation and production must build it the same way
(case_text), or the accuracy measured is for a different input.

The key comes from TYPESAFE_API_KEY, or else from the file ~/.typesafe_key (or
~/.typesafe_key.txt), so
it never has to be typed into a command line.
"""
import asyncio
import hashlib
import json
import os
from pathlib import Path

os.environ.setdefault('PYDANTIC_AI_NO_BANNER', '1')  # before the import, which prints it

from pydantic_ai import Agent  # noqa: E402

from areas import Area, Classification  # noqa: E402
from readable import clean_qp  # noqa: E402

# The definitions are part of the model: a result from an earlier wording of
# areas.py is a different classifier's answer, so the cache keys on this too.
DEFS = hashlib.sha256((Path(__file__).parent / 'areas.py').read_bytes().replace(b'\r\n', b'\n')).hexdigest()[:12]

# Pinned, not jev-latest: the alias moves when TypeSafe ships, and a confidence
# threshold tuned against one version means nothing against the next.
DEFAULT_MODEL = 'typesafe:jev-1.13.0'


def api_key() -> str:
    key = os.getenv('TYPESAFE_API_KEY')
    # Notepad saves "~/.typesafe_key" as "~/.typesafe_key.txt" unless told not to.
    for name in ('.typesafe_key', '.typesafe_key.txt'):
        f = Path.home() / name
        if not key and f.exists():
            key = f.read_text(encoding='utf-8-sig').strip()
    if not key:
        raise SystemExit('No Jev key: set TYPESAFE_API_KEY or save the key in ~/.typesafe_key')
    return key


def case_text(caption: str, qp: str) -> str:
    caption = ' '.join((caption or '').split())
    return f'Case: {caption}\n\nQuestions presented:\n{clean_qp(qp)}'


def _details(result) -> dict:
    """The pick's confidence and distribution from provider_details.

    Kept defensive and raw alongside: the shape is the client's, not ours, and a
    silent KeyError here would record every case as unscored.
    """
    pd = dict(result.response.provider_details or {})
    conf = pd.get('confidence')
    if isinstance(conf, dict):
        conf = conf.get('area')
    probs = pd.get('probabilities')
    if isinstance(probs, dict) and 'area' in probs and isinstance(probs['area'], dict):
        probs = probs['area']
    return {'confidence': conf, 'probabilities': probs, 'provider_details': pd}


async def classify(cases: dict[str, str], out_path: Path | None, model: str = DEFAULT_MODEL,
                   concurrency: int = 8) -> dict[str, dict]:
    """`cases` maps an id to its case_text(). Returns id -> result, including
    results already in `out_path` from an earlier run. With out_path None the
    caller keeps its own cache and nothing is written here."""
    done: dict[str, dict] = {}
    if out_path is not None and out_path.exists():
        for line in out_path.read_text(encoding='utf-8').splitlines():
            if line.strip():
                r = json.loads(line)
                if r.get('model') == model and r.get('defs') == DEFS and r.get('area'):
                    done[r['id']] = r
    todo = {k: v for k, v in cases.items() if k not in done}
    if not todo:
        return done

    os.environ.setdefault('TYPESAFE_API_KEY', api_key())
    agent = Agent(model, output_type=Classification)
    sem = asyncio.Semaphore(concurrency)
    fh = None
    if out_path is not None:
        out_path.parent.mkdir(parents=True, exist_ok=True)
        fh = out_path.open('a', encoding='utf-8')
    failures = 0

    async def one(k: str, text: str) -> None:
        nonlocal failures
        async with sem:
            try:
                res = await agent.run(text)
            except Exception as e:  # recorded, not raised: one bad case must not sink a run
                failures += 1
                rec = {'id': k, 'model': model, 'defs': DEFS, 'area': None, 'error': repr(e)[:500]}
            else:
                rec = {'id': k, 'model': model, 'defs': DEFS, 'area': res.output.area.value, **_details(res)}
                done[k] = rec
            if fh:
                fh.write(json.dumps(rec, default=str) + '\n')
                fh.flush()

    await asyncio.gather(*(one(k, v) for k, v in todo.items()))
    if fh:
        fh.close()
    if failures:
        print(f'{failures} of {len(todo)} requests failed')
    return done


AREAS = [a.value for a in Area]
