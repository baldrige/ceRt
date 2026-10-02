"""Align oral-argument transcripts to the Court's recordings.

For each argument page's transcript (arguments/<yyyy>/<dkt>.json, written by
R/argument_transcript.R), download the Court's MP3, run a small speech-
recognition model for word-level timestamps, match those words to the official
transcript, and write a start time (seconds) into every turn as "t". The
recognised text is never published -- it exists only to anchor the Court's own
words to the clock. reader.js uses these times where present and its even-rate
estimate where not.

    python .github/scripts/align_arguments.py SITE_DIR [--max N] [--shard I/K]
                                              [--model tiny.en] [--only KEY ...]
                                              [--out DIR] [--budget-min M] [--count]

Writes each aligned JSON in place under SITE_DIR/arguments, or with --out to
DIR/<yyyy>/<dkt>.json (the workflow's shards hand their files to one publish
job that way). --count prints how many transcripts are waiting, and exits. Order: the
current and previous Terms first, then back through the catalogue, newest
argument first within each. A transcript is skipped once aligned under
ALIGN_VERSION, or once it has failed under it (so a bad recording is not
retried every run). See docs/argument-transcripts.md.
"""
import argparse, bisect, datetime, difflib, glob, json, os, re, sys, tempfile, time, urllib.request

ALIGN_VERSION = "a1"
UA = "Mozilla/5.0 (ceRt SCOTUS research; +https://supremecourt.report)"
WPS = 2.6          # words per second, only to extrapolate past the last anchor
MIN_MATCH = 0.30   # below this share of transcript words matched, call it failed
# Below this, transcribe again without the silence filter and keep the better.
# In-person arguments match 88-94%; the remote ones of May 2020 and OT2020 came
# in as low as 32% filtered (19-351: 26% filtered, 89% not), so 0.80 catches the
# telephone audio and costs an in-person argument nothing.
RETRY_BELOW = 0.80


def mp3_url(dkt, nth=1):
    """The Court's MP3 for a docket's nth argument. Original actions are filed
    under "141-Orig", not the API's 22O141; a docket argued again has a file of
    its own for each later argument -- 24-109.mp3 (OT2024) and 24-109_2.mp3
    (Louisiana v. Callais, reargued OT2025), 141-Orig and 141-Orig_2. The first
    version used the bare docket for every argument, so the reargument was
    matched against the first argument's recording (3% of words) and every
    original action against a file that does not exist."""
    m = re.match(r"^\d{2}O(\d+)$", dkt or "")
    stem = f"{m.group(1)}-Orig" if m else dkt
    return f"https://www.supremecourt.gov/media/audio/mp3files/{stem}{'' if nth <= 1 else f'_{nth}'}.mp3"


def argument_term(today=None):
    d = today or datetime.date.today()
    return d.year if d.month >= 10 else d.year - 1


def norm(w):
    return re.sub(r"[^a-z0-9]", "", w.lower())


def candidates(site):
    """Every transcript JSON, with what the queue needs to order it."""
    out = []
    for p in glob.glob(os.path.join(site, "arguments", "[0-9][0-9][0-9][0-9]", "*.json")):
        try:
            d = json.load(open(p, encoding="utf-8"))
        except Exception:
            continue
        if not isinstance(d, dict) or "turns" not in d:
            continue
        out.append({"path": p, "dkt": d.get("dkt"), "term": int(d.get("term") or 0),
                    "posted": d.get("posted") or "", "align": d.get("align") or {}})
    # Which argument of its docket each transcript is (first, second...), for
    # the MP3 name. Counted from the transcripts on file, which start in OT2017,
    # so a case first argued earlier (or whose first argument the feeds miss) is
    # numbered one too low; main() then tries the docket's other recordings.
    by_dkt = {}
    for c in out:
        by_dkt.setdefault(c["dkt"], []).append(c["term"])
    for c in out:
        c["nth"] = sorted(by_dkt[c["dkt"]]).index(c["term"]) + 1
        c["url"] = mp3_url(c["dkt"], c["nth"])
    return out


def queue(site, max_n, shard=(0, 1), only=None):
    cs = candidates(site)
    if only:
        keys = set(only)
        cs = [c for c in cs if f"{c['term']}/{c['dkt']}" in keys]
    else:
        # Not yet aligned under this version -- or failed against a recording
        # other than the one this version would use (the original actions and
        # the reargument, re-queued when the MP3 naming was fixed), without
        # re-running every argument that aligned.
        # And a weak alignment made before the unfiltered retry existed (no
        # "vad" recorded): about 60 telephone arguments matched 32-80%.
        cs = [c for c in cs if c["align"].get("v") != ALIGN_VERSION
              or (not c["align"].get("ok") and (c["align"].get("url") != c["url"] or not c["align"].get("alts")))
              or (c["align"].get("ok") and "vad" not in c["align"]
                  and (c["align"].get("matched") or 0) < RETRY_BELOW)]
    now = argument_term()
    # Current and previous Terms first, then newest Term first; newest argument first.
    cs = sorted(cs, key=lambda c: (0 if c["term"] >= now - 1 else 1, -c["term"],
                                   "".join(chr(255 - ord(ch)) for ch in c["posted"])))
    i, k = shard
    cs = [c for j, c in enumerate(cs) if j % k == i]
    return cs[:max_n]


def download(url, dest):
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=300) as r, open(dest, "wb") as f:
        while True:
            b = r.read(1 << 20)
            if not b:
                break
            f.write(b)


def _anchors(t_tok, a_tok, n=4):
    """Pairs (i, j): an n-word phrase that occurs exactly once in the transcript
    and once in the recognised words, at i and j -- then the longest chain of
    them that runs forward in both, so a phrase the Court repeats (or the model
    mishears into a repeat) cannot pull the clock backwards."""
    def uniq(tok):
        seen, dup = {}, set()
        for i in range(len(tok) - n + 1):
            g = tuple(tok[i:i + n])
            if g in seen:
                dup.add(g)
            else:
                seen[g] = i
        return {g: i for g, i in seen.items() if g not in dup}
    ut, ua = uniq(t_tok), uniq(a_tok)
    pairs = sorted((i, ua[g]) for g, i in ut.items() if g in ua)
    # Longest increasing subsequence on j (pairs already ordered by i).
    tails, tails_idx, prev = [], [], [-1] * len(pairs)
    for k, (_, j) in enumerate(pairs):
        p = bisect.bisect_left(tails, j)
        if p == len(tails):
            tails.append(j); tails_idx.append(k)
        else:
            tails[p] = j; tails_idx[p] = k
        prev[k] = tails_idx[p - 1] if p else -1
    chain, k = [], tails_idx[-1] if tails_idx else -1
    while k >= 0:
        chain.append(pairs[k]); k = prev[k]
    return chain[::-1]


def _match(t_tok, a_tok, a_t, n=4):
    """A time for every transcript word the recognised words agree with: the
    anchor phrases themselves, then the stretches between consecutive anchors
    matched word by word (short spans, so the quadratic matcher is cheap)."""
    tok_time = [None] * len(t_tok)
    chain = _anchors(t_tok, a_tok, n)
    for i, j in chain:
        for q in range(n):
            tok_time[i + q] = a_t[j + q]
    bounds = [(-n, -n)] + chain + [(len(t_tok), len(a_tok))]
    for (i0, j0), (i1, j1) in zip(bounds, bounds[1:]):
        lo_t, hi_t, lo_a, hi_a = i0 + n, i1, j0 + n, j1
        if hi_t - lo_t <= 0 or hi_a - lo_a <= 0 or (hi_t - lo_t) * (hi_a - lo_a) > 4_000_000:
            continue
        sm = difflib.SequenceMatcher(None, t_tok[lo_t:hi_t], a_tok[lo_a:hi_a], autojunk=False)
        for m in sm.get_matching_blocks():
            for q in range(m.size):
                tok_time[lo_t + m.a + q] = a_t[lo_a + m.b + q]
    return tok_time


def align_turns(turns, asr):
    """turns: [{"x": text, ...}]; asr: [(word, start_seconds)]. Returns (starts, matched_share)."""
    t_tok, t_turn = [], []
    for i, t in enumerate(turns):
        for w in (t.get("x") or "").split():
            n = norm(w)
            if n:
                t_tok.append(n); t_turn.append(i)
    a_tok = [norm(w) for w, _ in asr]
    a_t = [s for _, s in asr]
    if not t_tok or not a_tok:
        return None, 0.0
    tok_time = _match(t_tok, a_tok, a_t)
    matched = sum(x is not None for x in tok_time)
    share = matched / len(t_tok)
    # Enforce order: an anchor earlier than one before it is a false match.
    best = -1.0
    for j in range(len(tok_time)):
        if tok_time[j] is not None:
            if tok_time[j] < best:
                tok_time[j] = None
            else:
                best = tok_time[j]
    # Interpolate between anchors; extrapolate at speaking pace at the ends.
    idx = [j for j, x in enumerate(tok_time) if x is not None]
    if not idx:
        return None, share
    filled = [0.0] * len(tok_time)
    for j in range(len(tok_time)):
        p = bisect.bisect_right(idx, j) - 1
        if p < 0:
            filled[j] = max(0.0, tok_time[idx[0]] - (idx[0] - j) / WPS)
        elif p == len(idx) - 1:
            filled[j] = tok_time[idx[-1]] + (j - idx[-1]) / WPS
        else:
            a, b = idx[p], idx[p + 1]
            filled[j] = tok_time[a] + (tok_time[b] - tok_time[a]) * (j - a) / (b - a)
    starts = [None] * len(turns)
    for j, ti in enumerate(t_turn):
        if starts[ti] is None:
            starts[ti] = round(filled[j], 1)
    # A turn with no words (rare) takes the next turn's start.
    nxt = None
    for i in range(len(starts) - 1, -1, -1):
        if starts[i] is None:
            starts[i] = nxt if nxt is not None else (starts[i - 1] if i else 0.0)
        nxt = starts[i]
    for i in range(1, len(starts)):
        starts[i] = max(starts[i], starts[i - 1])
    return starts, share


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("site")
    ap.add_argument("--max", type=int, default=4)
    ap.add_argument("--shard", default="0/1")
    ap.add_argument("--model", default="tiny.en")
    ap.add_argument("--threads", type=int, default=os.cpu_count() or 4)
    ap.add_argument("--budget-min", type=float, default=0, help="stop starting new work after this many minutes")
    ap.add_argument("--only", nargs="*")
    ap.add_argument("--out")
    ap.add_argument("--count", action="store_true")
    a = ap.parse_args()
    if a.count:
        print(len(queue(a.site, 10**9)))
        return
    i, k = (int(x) for x in a.shard.split("/"))
    todo = queue(a.site, a.max, (i, k), a.only)
    print(f"align: {len(todo)} transcript(s) this run (shard {i}/{k}, model {a.model})", flush=True)
    if not todo:
        return
    from faster_whisper import WhisperModel
    model = WhisperModel(a.model, device="cpu", compute_type="int8", cpu_threads=a.threads)
    t_start = time.time()
    tmp = tempfile.mkdtemp()
    for c in todo:
        if a.budget_min and (time.time() - t_start) / 60 > a.budget_min:
            print("align: time budget spent; the rest wait for the next run", flush=True)
            break
        key = f"{c['term']}/{c['dkt']}"
        mp3 = os.path.join(tmp, c["dkt"] + ".mp3")
        t0 = time.time()
        if os.path.exists(mp3):
            os.remove(mp3)
        d = json.load(open(c["path"], encoding="utf-8"))
        vad = True
        used = c["url"]

        def run(vad_filter):
            segs, info = model.transcribe(mp3, word_timestamps=True, vad_filter=vad_filter, language="en",
                                          beam_size=1, condition_on_previous_text=False)
            asr = [(w.word, w.start) for s in segs for w in (s.words or [])]
            return align_turns(d["turns"], asr) + (info.duration,)

        def attempt(url):
            download(url, mp3)
            try:
                st, sh, du = run(True)
                v = True
                # The silence filter discards much of the OT2020 telephone audio
                # as non-speech: 19-351 kept 4,065 words of 14,082 and matched
                # 26%; without it, 89%. Retry the weak ones unfiltered.
                if sh < RETRY_BELOW:
                    s2, sh2, du2 = run(False)
                    if sh2 > sh:
                        st, sh, du, v = s2, sh2, du2, False
                return st, sh, du, v
            finally:
                if os.path.exists(mp3):
                    os.remove(mp3)

        starts, share, dur = None, 0.0, None
        try:
            starts, share, dur, vad = attempt(c["url"])
        except Exception as e:
            print(f"  {key}: {c['url'].rsplit('/', 1)[1]}: {e}", flush=True)
        # Still no match: the docket's other recordings. Counting arguments from
        # the transcripts on file numbers a reargued case from OT2017, so one first
        # argued in OT2016 (15-1204, 15-1498), or whose first argument the feed
        # lists under the other Term (17-647), was matched against the wrong
        # recording. Its own is "_2" in the Court's current naming, or -- for
        # reargued cases of OT2017-OT2018 -- "rearg" ("15-1498rearg.mp3", as the
        # Court's own audio index links it). Try the others and keep the best.
        tried_alts = False
        if starts is None or share < MIN_MATCH:
            tried_alts = True
            for alt in [mp3_url(c["dkt"], n) for n in (1, 2, 3)] + [mp3_url(c["dkt"]).replace(".mp3", "rearg.mp3")]:
                if alt == c["url"]:
                    continue
                try:
                    st, sh, du, v = attempt(alt)
                except Exception:
                    continue          # no such recording
                if sh > share:
                    starts, share, dur, vad, used = st, sh, du, v, alt
                if share >= RETRY_BELOW:
                    break
        if starts is None or share < MIN_MATCH:
            d["align"] = {"v": ALIGN_VERSION, "ok": False, "matched": round(share, 3), "url": c["url"],
                          "alts": tried_alts}
            for t in d["turns"]:
                t.pop("t", None)
            print(f"  {key}: not aligned (matched {share:.0%})", flush=True)
        else:
            for t, s in zip(d["turns"], starts):
                t["t"] = s
            d["align"] = {"v": ALIGN_VERSION, "ok": True, "model": a.model, "matched": round(share, 3),
                          "duration": round(dur or 0, 1), "url": used, "vad": vad}
            print(f"  {key}: aligned, {share:.0%} of words matched, {dur/60:.0f} min of audio in "
                  f"{(time.time() - t0)/60:.1f} min", flush=True)
        dest = c["path"]
        if a.out:
            dest = os.path.join(a.out, str(c["term"]), c["dkt"] + ".json")
            os.makedirs(os.path.dirname(dest), exist_ok=True)
        with open(dest, "w", encoding="utf-8") as f:
            json.dump(d, f, ensure_ascii=False, separators=(",", ":"))


if __name__ == "__main__":
    main()
