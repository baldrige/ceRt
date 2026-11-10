#!/usr/bin/env python3
"""A Supreme Court opinion PDF -> structured JSON (docs/opinions.md).

    python extract_opinion.py <file.pdf> [-o out.json] [--pages A-B]

The Court publishes an opinion as a PDF and nothing else. This reads the
PDF's own text layer, with the geometry and the fonts that the layer carries,
and writes the opinion as sections (syllabus, the Court's opinion, each
separate writing), each a list of blocks (paragraph, heading, block quote,
separator) of styled runs, with the footnotes attached to the section that
cites them and the original page breaks kept as markers.

Everything downstream -- the HTML reader, the Markdown file, the EPUB -- is
a rendering of this JSON, so a fix to the reading of the PDF is made here once.

Two layouts are read:

  slip   /opinions/25pdf/25-365_xxxx.pdf   one writing per page run, each with
         its own masthead and its own page numbers; footnotes under an
         em-dash rule.
  print  /opinions/25pdf/609us1r58_xxxx.pdf (the "R" file that replaces a
         slip) and the preliminary prints: U.S. Reports pagination, a writing
         can begin mid-page, footnotes set smaller with no rule.

What decides each thing, all of it measured on real files (docs/opinions.md):

  running head  the lines above the body, in the smaller head size
  footnotes     the trailing run of lines set smaller than the body
  paragraph     a first line indented by one em; a quotation by two or more
  heading       a short centred line (I, A, 1, * * *)
  italics       the font ("...-Italic"); small caps the font ("...SC-...") or,
                in a slip, capitals set two points smaller than their line
  note ref      a superscript run of digits
  word break    the PDF's own soft hyphen, so "self-\ndefense" keeps its hyphen
                and "guaran­\ntees" loses it

The print layout's fonts map the "fi" and "fl" ligatures to a bare "f":
"frearm", "fled a concurring opinion", "refect". The glyph is still there and
still wide (0.56 em against 0.33), so a wide "f" is a ligature, and the word
list beside this file (fi_fl_words.json, counted from clean text: the site's
QP corpus and argument transcripts) says which of the two.
"""
import argparse
import collections
import io
import json
import os
import re
import sys

import pymupdf

EXTRACT_VERSION = "x1"

LIGATURES = {"ﬀ": "ff", "ﬁ": "fi", "ﬂ": "fl", "ﬃ": "ffi", "ﬄ": "ffl"}
SOFT = "­"
WATERMARK = {"Page", "Proof", "Pending", "Publication"}

BYLINE_LEAD = re.compile(
    r"^((?:The )?(?:Chief )?Justice [A-Z][A-Za-z'’]+(?: and (?:Chief )?Justice [A-Z][A-Za-z'’]+)*"
    r"[^.]{0,260}?(?:delivered the opinion of the Court|announced the judgment of the Court)[^.]{0,300}\.)\s*",
    re.I)
BYLINE_SEP = re.compile(
    r"^((?:Chief )?Justice [A-Z][A-Za-z'’]+(?:, with whom [^.]{0,300}?)?, "
    r"(?:concurring|dissenting)[^.]{0,160}\.)\s*", re.I)
BYLINE_OTHER = re.compile(r"^((?:Opinion|Statement) of (?:Chief )?Justice [A-Z][A-Za-z'’]+[^.]{0,200}\.)\s*", re.I)
PER_CURIAM = re.compile(r"^(Per Curiam\.?)\s*$", re.I)
# ("De-" / "cided": the line wraps when several cases are argued together.)
DECIDED_LINE = re.compile(r"(?:\bDecided|^cided)\s+[A-Z][a-z]+\.?\s+\d{1,2},\s+\d{4}")
DATE_LINE = re.compile(r"^\[[A-Z][a-z]+ \d{1,2}, \d{4}\]$")
HEADING = re.compile(r"^(?:[IVX]{1,6}|[A-Z]|\d{1,2}|[a-z]|\(\d{1,2}\)|\([a-z]\))$")
STARS = re.compile(r"^\*\s*\*\s*\*$")
RULE = re.compile(r"^[—–_\-\s]{6,}$")


# ---- pages -> lines ---------------------------------------------------------------

def load_words(path):
    try:
        with io.open(path, encoding="utf-8") as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


class Ligatures:
    """Restore "fi"/"fl" where the font's map dropped the second letter."""

    def __init__(self, words):
        self.words = words
        self.fixed = collections.Counter()
        self.guessed = collections.Counter()
        self._part = {}

    def part(self, frag):
        if frag not in self._part:
            self._part[frag] = sum(c for w, c in self.words.items() if w.startswith(frag) or w.endswith(frag))
        return self._part[frag]

    def resolve(self, word, idx):
        """`word` with a wide f at each position in idx -> the repaired word."""
        out = word
        for i in sorted(idx, reverse=True):
            a = out[:i] + "fi" + out[i + 1:]
            b = out[:i] + "fl" + out[i + 1:]
            ca = self.words.get(a.lower(), 0)
            cb = self.words.get(b.lower(), 0)
            if not (ca or cb) and len(word) >= 3:
                # Half of a word broken over a line ("signi-" / "ficant"): the
                # list's words that begin or end with it vote instead.
                ca, cb = self.part(a.lower()), self.part(b.lower())
            if ca or cb:
                out = a if ca >= cb else b
                self.fixed[out] += 1
            else:
                # Unknown to the list. Before a vowel it is "fl" far more often
                # ("flaunt", "flour", "fluid", "flight"); otherwise "fi", which
                # is four times as common overall ("fit", "fix", "first").
                out = b if out[i + 1:i + 2].lower() in ("a", "o", "u", "i", "y") else a
                self.guessed[out] += 1
        return out


def page_lines(page, lig):
    """One page -> lines, top to bottom: dict(x0, x1, y0, y1, size, runs).

    A run is dict(t, i, sc, sup, size). MuPDF's own lines are the start, but
    it splits one printed line where the type changes baseline (a running
    head's page number and its caption; a superscript), so lines whose
    vertical extents overlap are merged and their runs read left to right.
    """
    raw = page.get_text("rawdict", flags=pymupdf.TEXT_PRESERVE_LIGATURES | pymupdf.TEXT_PRESERVE_WHITESPACE)
    rows = []
    for block in raw["blocks"]:
        for ln in block.get("lines", []):
            # A preliminary print's watermark is set on a slant.
            if abs(ln.get("dir", (1, 0))[1]) > 0.05:
                continue
            spans = []
            for s in ln["spans"]:
                chars = s["chars"]
                if not chars:
                    continue
                text = "".join(c["c"] for c in chars)
                if s["size"] > 17 and text.strip() in WATERMARK | {"Page Proof Pending Publication"}:
                    continue
                if s["font"].startswith("Arial") and s["size"] > 17:
                    continue
                font = s["font"]
                wide = set()
                # (A small-caps face sets its plain f that wide, and maps it properly.)
                if "f" in text and not any(k in text for k in LIGATURES) and not re.search(r"SC[-_]|SmallCap", font):
                    for k, c in enumerate(chars):
                        if c["c"] == "f" and (c["bbox"][2] - c["bbox"][0]) / s["size"] > 0.46:
                            wide.add(k)
                # An indent arrives as leading space characters, so the span's
                # edges are those of its first and last printed characters.
                ink = [c for c in chars if c["c"].strip()]
                if not ink:
                    ink = chars
                spans.append({
                    "t": text, "wide": wide, "x0": ink[0]["bbox"][0], "x1": ink[-1]["bbox"][2],
                    "y0": s["bbox"][1], "y1": s["bbox"][3], "size": round(s["size"], 1),
                    "i": bool(s["flags"] & 2) or "Italic" in font,
                    "b": bool(s["flags"] & 16) or "Bold" in font,
                    "scfont": bool(re.search(r"SC[-_]|SmallCap", font)),
                    "sup": bool(s["flags"] & 1),
                })
            if spans:
                rows.append(spans)
    # Merge rows that share a baseline band.
    rows.sort(key=lambda sp: (min(s["y0"] for s in sp), min(s["x0"] for s in sp)))
    merged = []
    for sp in rows:
        y0 = min(s["y0"] for s in sp); y1 = max(s["y1"] for s in sp)
        big = max(s["size"] for s in sp)
        if merged:
            m = merged[-1]
            overlap = min(m["y1"], y1) - max(m["y0"], y0)
            if overlap > 0.5 * min(m["y1"] - m["y0"], y1 - y0) and overlap > 0.35 * max(big, m["big"]):
                m["spans"].extend(sp); m["y0"] = min(m["y0"], y0); m["y1"] = max(m["y1"], y1)
                m["big"] = max(m["big"], big)
                continue
        merged.append({"spans": list(sp), "y0": y0, "y1": y1, "big": big})
    lines = []
    for m in merged:
        spans = sorted(m["spans"], key=lambda s: s["x0"])
        inked = [s for s in spans if s["t"].strip()] or spans
        chars_by_size = collections.Counter()
        for s in spans:
            chars_by_size[s["size"]] += len(s["t"].strip())
        if not sum(chars_by_size.values()):
            continue
        # The line's size is its LARGEST type with two characters or more, not
        # its commonest: a slip's byline is mostly small caps set two points
        # down ("JUSTICE JACKSON, with whom JUSTICE SOTOMAYOR joins ..."), and
        # read by its commonest size it was a line of footnote.
        big = [sz for sz, n in chars_by_size.items() if n >= 2]
        size = max(big) if big else max(chars_by_size)
        line = {"x0": min(s["x0"] for s in inked), "x1": max(s["x1"] for s in inked), "y0": m["y0"], "y1": m["y1"],
                "size": size, "spans": spans}
        line["runs"] = line_runs(line, lig)
        line["text"] = "".join(r["t"] for r in line["runs"]).strip()
        if line["text"]:
            lines.append(line)
    return lines


def line_runs(line, lig):
    """Styled runs for one line: ligatures repaired, small caps and note refs found."""
    size = line["size"]
    # Repair words over the whole line, since a word can cross a span boundary.
    flat = []
    for si, s in enumerate(line["spans"]):
        for k, ch in enumerate(s["t"]):
            flat.append([ch, si, k in s["wide"]])
    text = "".join(c[0] for c in flat)
    pieces = []          # (char, span index)
    pos = 0
    for m in re.finditer(r"[A-Za-z]+", text):
        for j in range(pos, m.start()):
            pieces.append((flat[j][0], flat[j][1]))
        idx = [j - m.start() for j in range(m.start(), m.end()) if flat[j][2]]
        word = m.group()
        if idx:
            fixed = lig.resolve(word, idx)
            # Hand the inserted letter to the span of the f it follows.
            k = 0
            for j in range(m.start(), m.end()):
                pieces.append((fixed[k], flat[j][1])); k += 1
                if flat[j][2]:
                    pieces.append((fixed[k], flat[j][1])); k += 1
        else:
            for j in range(m.start(), m.end()):
                pieces.append((flat[j][0], flat[j][1]))
        pos = m.end()
    for j in range(pos, len(flat)):
        pieces.append((flat[j][0], flat[j][1]))
    by_span = collections.defaultdict(list)
    for ch, si in pieces:
        by_span[si].append(ch)
    runs = []
    for si, s in enumerate(line["spans"]):
        t = "".join(by_span.get(si, []))
        for a, b in LIGATURES.items():
            t = t.replace(a, b)
        if not t:
            continue
        stripped = t.strip()
        small = s["size"] <= size - 1.5
        sup = small and (s["sup"] or s["y1"] < line["y1"] - 0.18 * size) and bool(re.fullmatch(r"[0-9*†‡]{1,3}", stripped))
        sc = s["scfont"] or (small and not sup and len(stripped) >= 2 and stripped.upper() == stripped
                             and bool(re.search(r"[A-Z]", stripped)) and size >= 8.5)
        if sc and not s["scfont"]:
            t = t.lower()
        run = {"t": t, "i": s["i"], "sc": sc, "sup": sup, "size": s["size"]}
        if runs and all(runs[-1][k] == run[k] for k in ("i", "sc", "sup")):
            runs[-1]["t"] += t
        else:
            runs.append(run)
    return runs


# ---- one page: head, body, notes -----------------------------------------------------

def head_section(head_text):
    """The section a running head names, or None."""
    h = head_text
    if re.search(r"\bSyllabus\b", h):
        return "syllabus"
    if re.search(r"Opinion of the Court", h, re.I):
        return "court"
    if re.search(r"\bPer Curiam\b", h, re.I):
        return "percuriam"
    m = re.search(r"([A-Z][A-Za-z'’]+), (?:C\. ?)?J\., "
                  r"(concurring in the judgment in part and dissenting in part|concurring in part and dissenting in part|"
                  r"concurring in the judgment|concurring in judgment|concurring in part|dissenting in part|concurring|dissenting)", h)
    if m:
        return m.group(1).lower() + ":" + m.group(2).lower()
    m = re.search(r"(?:Opinion|Statement) of ([A-Z][A-Za-z'’]+), (?:C\. ?)?J\.", h)
    if m:
        return m.group(1).lower() + ":opinion"
    return None


def split_page(lines, body_size, page_h):
    """lines -> (head lines, body lines, note lines, flags)."""
    flags = {"masthead": False, "cover": False, "reporter": False}
    top = [ln for ln in lines if ln["y0"] < 0.21 * page_h]
    joined = " | ".join(ln["text"] for ln in lines[:8])
    if re.search(r"PRELIMINARY PRINT", joined) and re.search(r"OFFICIAL REPORTS|Volume \d+ U\. ?S\.", joined):
        flags["cover"] = True
    if re.search(r"^Reporter.s Note", " ".join(ln["text"] for ln in lines[:3])):
        flags["reporter"] = True
    head = []
    for ln in top:
        t = ln["text"]
        if ln["size"] <= body_size - 1.5 and (
                re.fullmatch(r"\d{1,4}", t) or re.match(r"Cite as:", t) or head_section(t)
                or re.fullmatch(r"[A-Z0-9 .,'’&\-:()]+ v\. [A-Z0-9 .,'’&\-:()]+", t)
                or re.fullmatch(r"(?:In re|Ex parte) [A-Z0-9 .,'’&\-]+", t, re.I)
                or re.fullmatch(r"\(Slip Opinion\)|OCTOBER TERM, \d{4}", t)
                or re.fullmatch(r"\d{1,4}\s+.*", t) and len(t) < 70 and t.upper() == t
                or re.fullmatch(r"[A-Z0-9 .,'’&\-:()]{4,70}", t) and ln["y0"] < 0.195 * page_h
                or t == "Counsel" and ln["y0"] < 0.195 * page_h):
            head.append(ln)
    head_ids = {id(ln) for ln in head}
    rest = [ln for ln in lines if id(ln) not in head_ids]
    for ln in rest:
        if ln["text"] == "SUPREME COURT OF THE UNITED STATES" and any(r.get("size", 0) >= 13 for r in ln["runs"]):
            flags["masthead"] = True
    sec = head_section(" | ".join(ln["text"] for ln in head))
    # Footnotes: below the rule if there is one, else the trailing smaller run.
    rule = next((k for k, ln in enumerate(rest) if RULE.match(ln["text"]) and ln["text"].count("—") >= 4), None)
    if rule is not None:
        body, notes = rest[:rule], rest[rule + 1:]
    else:
        k = len(rest)
        while k > 0 and rest[k - 1]["size"] <= body_size - 1.5:
            k -= 1
        # A page that is small type throughout is a syllabus or an appendix.
        body, notes = (rest, []) if k == 0 else (rest[:k], rest[k:])
        if sec == "syllabus" and notes:
            # The syllabus IS small type. Its trailing small lines are notes
            # only when they open with a marker under full-size text low on
            # the page: the amici note under a print's counsel paragraphs.
            low = body[-1]["y1"] > 0.5 * page_h
            if not (low and re.match(r"^(?:[*†]|\d{1,2}\s?[A-Z])", notes[0]["text"])):
                body, notes = rest, []
    return head, body, notes, flags, sec


# ---- lines -> blocks -------------------------------------------------------------------

class Hyphens:
    """Does a hyphen at the end of a line belong to the word?

    The PDF marks a word broken for the line with a soft hyphen, and that is
    the first evidence. It is not enough: a slip sets "not-so-strange" and
    "self-deport" with the soft hyphen at the break, and "Con-gress" with a
    real one. So the joined word is looked up -- in the general word list
    beside this file and in this document's own words -- and a compound the
    document writes with its hyphen elsewhere keeps it.
    """

    def __init__(self, vocab):
        self.vocab = vocab
        self.doc = set()
        self.compounds = set()
        self.kept = collections.Counter()

    def learn(self, text):
        for m in re.finditer(r"[A-Za-z]+(?:-[A-Za-z]+)+", text):
            self.compounds.add(m.group().lower())
        for w in re.findall(r"[A-Za-z]{3,}", text):
            self.doc.add(w.lower())

    def keep(self, left, right, end):
        if not left or not right or re.search(r"\d", left):
            return end == "hard"              # "19th-century", "243-250"
        l, r = left.lower(), right.lower()
        joined = l + r
        if joined in self.vocab or joined in self.doc:
            return False
        if l + "-" + r in self.compounds:
            return True
        # Two words either side of the break, the joined form unknown: a
        # compound ("self-deport", "law-abiding"). Short halves are left out,
        # because a surname breaks into them ("East-man", "Wolf-son").
        if len(l) >= 3 and len(r) >= 4 and l in self.vocab and r in self.vocab:
            return True
        # A real hyphen stays, unless it only breaks a capitalised name.
        return end == "hard" and not (left[0].isupper() and right[0].islower() and (len(l) < 4 or len(r) < 4))


HYPH = Hyphens(set())


def join_runs(acc, runs, end):
    """Append a line's runs to a paragraph's, mending the word at the seam.

    `end` says how the paragraph's last line ended: None (a space), "soft"
    (the PDF's soft hyphen, already removed) or "hard" (a hyphen or dash,
    still there).
    """
    runs = [dict(r) for r in runs]
    last = next((r for r in reversed(acc) if "t" in r), None)
    if last is not None and runs:
        runs[0]["t"] = runs[0]["t"].lstrip()
        if end is None:
            last["t"] = last["t"].rstrip() + " "
        elif not last["t"].endswith("–"):
            lm = re.search(r"([A-Za-z0-9]+)-?$", last["t"])
            rm = re.match(r"[A-Za-z]+", "".join(r["t"] for r in runs))
            keep = HYPH.keep(lm.group(1) if lm else "", rm.group() if rm else "", end)
            has = last["t"].endswith("-")
            if keep and not has:
                last["t"] += "-"
            elif not keep and has:
                last["t"] = last["t"][:-1]
            if keep and lm and rm:
                HYPH.kept[lm.group(1) + "-" + rm.group()] += 1
    for r in runs:
        if acc and all(acc[-1].get(k) == r.get(k) for k in ("i", "sc", "sup")) and "pb" not in acc[-1] and "pb" not in r:
            acc[-1]["t"] += r["t"]
        else:
            acc.append(r)
    return acc


def line_end(runs):
    """How a line ends: "soft" (the PDF's soft hyphen), "hard" (a hyphen or dash), or None."""
    t = "".join(r["t"] for r in runs).rstrip()
    if t.endswith(SOFT):
        return "soft"
    return "hard" if re.search(r"[A-Za-z0-9][\-–]$", t) else None


def strip_end(runs):
    """Drop a trailing soft hyphen and trailing space from a line's runs."""
    runs = [dict(r) for r in runs]
    while runs and not runs[-1]["t"].strip():
        runs.pop()
    if runs:
        runs[-1]["t"] = runs[-1]["t"].rstrip()
        if runs[-1]["t"].endswith(SOFT):
            runs[-1]["t"] = runs[-1]["t"][:-1]
    return runs


class Flow:
    """Body lines, page after page, assembled into blocks."""

    def __init__(self):
        self.blocks = []
        self.open = None          # the block still taking lines
        self.pending_pb = None

    def page_break(self, label, pdf_page):
        self.pending_pb = {"pb": label, "p": pdf_page}

    def close(self):
        self.open = None

    def add_page(self, body, left_of, right, pdf_page, syllabus=False, col_left=None):
        """`left_of(line)` is the left edge of that line's column of type;
        `col_left` the page's own (a syllabus is set in from it, but a line
        centred on the page is centred on the page)."""
        for ln in body:
            t = ln["text"]
            left = left_of(ln)
            page_left = left if col_left is None else col_left
            centre = (page_left + right) / 2.0
            ind = ln["x0"] - left
            mid = (ln["x0"] + ln["x1"]) / 2.0
            centred = abs(mid - centre) < 9 and (ln["x0"] - page_left) > 15 and (right - ln["x1"]) > 15
            kind = None
            if centred and (HEADING.match(t) or STARS.match(t)):
                kind = "sep" if STARS.match(t) else "h"
            elif centred and len(t) < 64 and not re.search(r"[.;,]$", t):
                kind = "h"
            if kind:
                self._emit_pb(pdf_page)
                self.blocks.append({"type": kind, "runs": clean_runs(ln["runs"]), "page": pdf_page})
                self.open = None
            else:
                # The open block survives a page turn, so a paragraph (or a
                # quotation) that runs over the page simply takes the next
                # page's first line: only an indent starts a new one.
                quote = ind >= 17.5
                starts = (5.0 <= ind < 17.5)
                if syllabus:
                    # A syllabus nests its points -- (a) one em in, (1) two --
                    # and sets "Held:" out to the left; none of it is quotation.
                    quote, starts = False, False
                cur = self.open
                if cur is not None and cur["type"] in ("p", "quote") and abs(cur.get("size", ln["size"]) - ln["size"]) > 1.2:
                    cur = self.open = None        # the type changed size: not the same paragraph
                if syllabus and cur is not None and cur["type"] == "p":
                    # No fixed margin to measure from here: "Held:" hangs, the
                    # line-up paragraph is set out, each point is set in. So a
                    # paragraph's own second line says where its lines sit, and
                    # a later line anywhere else begins the next one.
                    if cur["n"] == 1:
                        starts = cur["last_x1"] < right - 25
                        if not starts:
                            cur["cont_x"] = ln["x0"]
                    else:
                        starts = abs(ln["x0"] - cur.get("cont_x", ln["x0"])) >= 5.0
                if quote:
                    if cur is not None and cur["type"] == "quote":
                        # A quotation's first line is set a further em in, so
                        # its left edge is known from the second line on; a
                        # later line at that deeper indent is a new paragraph
                        # of the same quotation.
                        if cur["n"] >= 2 and ln["x0"] - cur["q_left"] > 6.5:
                            self._start("quote", ln, pdf_page)
                        else:
                            cur["q_left"] = min(cur["q_left"], ln["x0"])
                            self._extend(ln, pdf_page)
                    else:
                        self._start("quote", ln, pdf_page)
                elif starts:
                    self._start("p", ln, pdf_page)
                elif cur is None:
                    self._start("p", ln, pdf_page)
                elif cur["type"] == "quote":
                    # flush-left text after a quotation continues the paragraph around it
                    self._start("p", ln, pdf_page, cont=True)
                else:
                    self._extend(ln, pdf_page)

    def _emit_pb(self, pdf_page):
        if self.pending_pb is not None:
            self.blocks.append({"type": "pb", **self.pending_pb})
            self.pending_pb = None

    def _start(self, kind, ln, pdf_page, cont=False):
        self._emit_pb(pdf_page)
        b = {"type": kind, "runs": [], "page": pdf_page, "n": 0, "size": ln["size"]}
        if kind == "quote":
            b["q_left"] = ln["x0"]
        if cont:
            b["cont"] = True
        self.blocks.append(b)
        self.open = b
        self._extend(ln, pdf_page)

    def _extend(self, ln, pdf_page):
        b = self.open
        if self.pending_pb is not None and b["runs"]:
            # the page turned inside this block
            mark = dict(self.pending_pb)
            if b.get("end") is not None:
                mark["mid"] = True        # the page turned inside a word
            b["runs"].append(mark)
            self.pending_pb = None
        elif self.pending_pb is not None:
            self._emit_pb(pdf_page)
        join_runs(b["runs"], strip_end(ln["runs"]), b.get("end"))
        b["end"] = line_end(ln["runs"])
        b["n"] += 1
        b["last_x1"] = ln["x1"]


def clean_runs(runs):
    out = []
    for r in runs:
        r = {k: v for k, v in r.items() if k in ("t", "i", "sc", "sup", "pb", "p", "fn", "mid") and v not in (False, None)}
        if "pb" in r:
            out.append(r); continue
        if not r.get("t"):
            continue
        r["t"] = re.sub(r"[ \t\r\n]+", " ", r["t"].replace(SOFT, ""))
        if out and "pb" not in out[-1] and all(out[-1].get(k) == r.get(k) for k in ("i", "sc", "sup")):
            out[-1]["t"] += r["t"]
        else:
            out.append(r)
    if out and "t" in out[0]:
        out[0]["t"] = out[0]["t"].lstrip()
    if out and "t" in out[-1]:
        out[-1]["t"] = out[-1]["t"].rstrip()
    return [r for r in out if "pb" in r or r.get("t")]


def plain(runs):
    return "".join(r.get("t", "") for r in runs if not r.get("sup"))


# ---- footnotes ----------------------------------------------------------------------------

def note_blocks(note_lines, left, open_note):
    """A page's footnote lines -> [(marker or None, runs)], continuing `open_note`."""
    out = []
    cur = None
    for ln in note_lines:
        if RULE.match(ln["text"]):
            continue
        runs = [dict(r) for r in ln["runs"]]
        ind = ln["x0"] - left
        marker = None
        if ind >= 4.0 and runs:
            first = runs[0]["t"].strip()
            m = re.match(r"^(\d{1,3}|[*†‡]{1,2})\s*", runs[0]["t"].lstrip())
            if (runs[0].get("sup") or runs[0].get("size", 9) <= ln["size"] - 1.5) and re.fullmatch(r"\d{1,3}|[*†‡]{1,2}", first):
                marker = first; runs = runs[1:]
            elif m and re.match(r"^[*†‡]", runs[0]["t"].lstrip()):
                marker = m.group(1); runs[0]["t"] = runs[0]["t"].lstrip()[m.end():]
        if marker is not None:
            cur = {"marker": marker, "paras": [{"runs": [], "end": None}]}
            out.append(cur)
        elif cur is None:
            cur = {"marker": None, "paras": [{"runs": [], "end": None}], "cont": True}
            out.append(cur)
        elif ind >= 4.0 and cur["paras"][-1]["runs"]:
            cur["paras"].append({"runs": [], "end": None})
        # Short notes can share a line ("33 Id., at 500. 34 Cong. Globe ... 35 Id.,
        # at 498. 36 Ibid."): a superscript number inside the line that follows
        # on from the note before it opens the next note.
        pieces = [[]]
        for r in runs:
            t = r["t"].strip()
            prev = cur["marker"] if len(pieces) == 1 else pieces[-1][0]["t"].strip()
            if r.get("sup") and t.isdigit() and pieces[-1] and (prev or "").isdigit() and int(t) == int(prev) + 1:
                pieces.append([r])
            else:
                pieces[-1].append(r)
        for k, piece in enumerate(pieces):
            if k:
                cur = {"marker": piece[0]["t"].strip(), "paras": [{"runs": [], "end": None}]}
                out.append(cur)
                piece = piece[1:]
            p = cur["paras"][-1]
            join_runs(p["runs"], strip_end(piece), p["end"])
            p["end"] = line_end(piece) if k == len(pieces) - 1 else None
    return out


# ---- the document -----------------------------------------------------------------------

def body_size_of(all_lines):
    c = collections.Counter()
    for lines in all_lines:
        for ln in lines:
            if len(ln["text"]) > 40:
                c[ln["size"]] += len(ln["text"])
    return c.most_common(1)[0][0] if c else 11.0


def section_title(kind, who, byline):
    if kind == "syllabus":
        return "Syllabus"
    if kind == "counsel":
        return "Counsel"
    if kind == "court":
        return "Opinion of the Court"
    if kind == "percuriam":
        return "Per Curiam"
    name = who.title() if who else ""
    name = re.sub(r"\b(Mc|Mac)([a-z])", lambda m: m.group(1) + m.group(2).upper(), name)
    chief = bool(byline and re.match(r"(?:The )?Chief Justice", byline, re.I))
    j = ("Chief Justice " if chief else "Justice ") + name
    return {"concurring": j + ", concurring", "dissenting": j + ", dissenting",
            "judgment": j + ", concurring in the judgment", "mixed": j + ", concurring in part and dissenting in part",
            "opinion": "Opinion of " + j, "plurality": "Opinion of " + j}.get(kind, j)


def classify_byline(text):
    """A paragraph's opening byline -> (kind, who, byline text, rest) or None."""
    m = PER_CURIAM.match(text)
    if m:
        return "percuriam", "", m.group(1), ""
    m = BYLINE_LEAD.match(text)
    if m:
        who = re.search(r"Justice ([A-Z][A-Za-z'’]+)", m.group(1), re.I).group(1).lower()
        kind = "court" if re.search(r"delivered the opinion of the Court\.?$", m.group(1).strip(), re.I) and \
            not re.search(r"announced the judgment", m.group(1), re.I) else "plurality"
        if re.search(r"delivered the opinion of the Court", m.group(1), re.I):
            kind = "court"
        return kind, who, m.group(1), text[m.end():]
    m = BYLINE_SEP.match(text)
    if m:
        b = m.group(1)
        who = re.search(r"Justice ([A-Z][A-Za-z'’]+)", b, re.I).group(1).lower()
        tail = b[b.lower().rfind(", concurring") if ", concurring" in b.lower() else b.lower().rfind(", dissenting"):].lower()
        if "dissenting" in tail and "concurring" in tail:
            kind = "mixed"
        elif "dissenting" in tail:
            kind = "dissenting"
        elif "in the judgment" in tail or "in judgment" in tail:
            kind = "judgment"
        else:
            kind = "concurring"
        return kind, who, b, text[m.end():]
    m = BYLINE_OTHER.match(text)
    if m:
        who = re.search(r"Justice ([A-Z][A-Za-z'’]+)", m.group(1), re.I).group(1).lower()
        return "opinion", who, m.group(1), text[m.end():]
    return None


def drop_prefix(runs, n):
    """Remove the first n characters of text from a list of runs."""
    out = []
    for r in runs:
        if "pb" in r:
            out.append(r); continue
        t = r["t"]
        if n >= len(t):
            n -= len(t); continue
        if n > 0:
            r = dict(r); r["t"] = t[n:]; n = 0
        out.append(r)
    return out


def extract(path, first=None, last=None, words_path=None):
    doc = pymupdf.open(path)
    lig = Ligatures(load_words(words_path or os.path.join(os.path.dirname(os.path.abspath(__file__)), "fi_fl_words.json")))
    lo = (first or 1) - 1
    hi = min(last or len(doc), len(doc))
    pages = [page_lines(doc[k], lig) for k in range(lo, hi)]
    global HYPH
    here = os.path.dirname(os.path.abspath(__file__))
    try:
        with io.open(os.path.join(here, "vocab.txt"), encoding="utf-8") as f:
            vocab = set(f.read().split())
    except OSError:
        vocab = set()
    HYPH = Hyphens(vocab)
    for lines in pages:
        for ln in lines:
            t = ln["text"]
            # a line's last word may be the half of one; it teaches nothing
            HYPH.learn(re.sub(r"\S+[\-­]$", "", t))
    page_h = doc[lo].rect.height
    size = body_size_of(pages)

    layout = "slip"
    cite = None
    short = None
    flow = Flow()
    notes_by_page = {}
    labels = {}
    split = []
    for k, lines in enumerate(pages):
        head, body, notes, flags, sec = split_page(lines, size, page_h)
        split.append({"page": lo + k + 1, "head": head, "body": body, "notes": notes, "flags": flags, "sec": sec})
    stop = next((k for k, pg in enumerate(split) if pg["flags"]["reporter"]), len(split))
    split = split[:stop]
    if any(pg["flags"]["cover"] for pg in split):
        layout = "print"
    split = [pg for pg in split if not pg["flags"]["cover"]]

    # The left edge of each column of type, from the whole document: a page
    # with two lines of opinion above a page of footnote (the first page of a
    # print's opinion) cannot say where its own margin is. Keyed by zone (the
    # syllabus is set in from the opinion), type size and page parity.
    edges = collections.defaultdict(collections.Counter)
    for pg in split:
        zone = "syl" if pg["sec"] == "syllabus" else "op"
        for ln in pg["body"]:
            if len(ln["text"]) > 30:
                x = round(ln["x0"] * 2) / 2
                edges[(zone, ln["size"], pg["page"] % 2)][x] += 1
                edges[(zone, ln["size"], None)][x] += 1
    # The right edge is where full lines end, not the furthest any line reaches:
    # one overlong line (a table, a URL) would move it.
    ends = sorted(ln["x1"] for pg in split for ln in pg["body"] if len(ln["text"]) > 40)
    right = ends[int(0.9 * (len(ends) - 1))] + 1.5 if ends else 457.0

    nx = collections.Counter(round(ln["x0"] * 2) / 2 for pg in split for ln in pg["notes"] if len(ln["text"]) > 30)
    note_left = nx.most_common(1)[0][0] if nx else None

    def left_for(zone, parity):
        def f(ln):
            for key in ((zone, ln["size"], parity), (zone, ln["size"], None), ("op", size, None)):
                c = edges.get(key)
                if c and sum(c.values()) >= 4:
                    return c.most_common(1)[0][0]
            return ln["x0"]
        return f

    for pg in split:
        pdf_page, head, body, notes, flags, sec = pg["page"], pg["head"], pg["body"], pg["notes"], pg["flags"], pg["sec"]
        head_text = " | ".join(ln["text"] for ln in head)
        # The page's own number: alone on its line, or merged with the caption
        # it shares a baseline with ("2 TRUMP v. BARBARA", "Cite as: ... (2026) 3").
        label = ""
        for ln in head:
            m = re.fullmatch(r"(\d{1,4})", ln["text"]) or re.match(r"^(\d{1,4})\s+\D", ln["text"]) or                 re.search(r"\(\d{4}\)\s+(\d{1,4})$", ln["text"])
            if m:
                label = m.group(1); break
        labels[pdf_page] = {"label": label, "sec": sec}
        for ln in head:
            hm = re.search(r"([A-Z][A-Z0-9 .,'’&\-]+ v\. [A-Z][A-Z0-9 .,'’&\-]+|(?:IN RE|EX PARTE) [A-Z0-9 .,'’&\-]+)$", ln["text"])
            if hm and short is None and not re.match(r"Cite as", ln["text"]):
                short = hm.group(1).strip()
        m = re.search(r"Cite as:\s*(\d+)\s*U\.\s*S\.\s*(\d+|_+)", head_text)
        if m and cite is None:
            cite = "%s U. S. %s" % (m.group(1), m.group(2) if m.group(2).isdigit() else "___")
        if re.search(r"Cite as: \d+ U\. ?S\. \d+", head_text):
            layout = "print"
        if flags["masthead"]:
            # A slip writing's first page: everything down to the date is the
            # NOTICE, the masthead, the docket number and the caption.
            cut = next((j for j, ln in enumerate(body) if DATE_LINE.match(ln["text"])), None)
            if cut is not None:
                body = body[cut + 1:]
            flow.close()
            flow.blocks.append({"type": "start", "page": pdf_page, "sec": sec})
        flow.page_break(label, pdf_page)
        col = edges.get(("op", size, pdf_page % 2)) or edges.get(("op", size, None))
        flow.add_page(body, left_for("syl" if sec == "syllabus" else "op", pdf_page % 2), right, pdf_page,
                      syllabus=sec == "syllabus", col_left=col.most_common(1)[0][0] if col else None)
        if notes:
            # The notes' left edge is the document's, for the same reason: a
            # page whose notes are all one line long has no flush-left line.
            n_left = note_left if note_left is not None else min(ln["x0"] for ln in notes)
            notes_by_page[pdf_page] = note_blocks(notes, n_left, None)

    # ---- sections: a new one at each byline ---------------------------------------
    sections = []
    cur = None

    def start(kind, who, byline, page):
        nonlocal cur
        cur = {"kind": kind, "who": who, "byline": byline, "blocks": [], "notes": [], "first_page": page}
        sections.append(cur)

    for b in flow.blocks:
        if b["type"] == "start":
            if b.get("sec") == "syllabus" and cur is None:
                start("syllabus", "", None, b["page"])
            continue
        if b["type"] == "pb":
            if cur is None:
                sec = labels.get(b["p"], {}).get("sec")
                if sec == "syllabus":
                    start("syllabus", "", None, b["p"])
            if cur is not None:
                cur["blocks"].append(b)
            continue
        runs = clean_runs(b["runs"])
        text = plain(runs)
        hit = classify_byline(text) if b["type"] == "p" else None
        if hit and len(hit[2]) < 700:
            kind, who, byline, rest = hit
            start(kind, who, byline, b["page"])
            if rest.strip():
                cur["blocks"].append({"type": "p", "runs": clean_runs(drop_prefix(runs, len(text) - len(rest)))})
            continue
        if cur is None:
            sec = labels.get(b["page"], {}).get("sec")
            if sec == "syllabus":
                start("syllabus", "", None, b["page"])
            else:
                continue                      # a cover, a title page
        if cur["kind"] == "syllabus" and b["type"] == "p" and b.get("size", 0) >= size - 0.6 and \
                re.search(r"argued the cause|on the briefs? (?:was|were)|filed (?:a )?briefs?", text):
            # A print sets the counsel between the syllabus and the opinion.
            start("counsel", "", None, b["page"])
        blk = {"type": b["type"], "runs": runs}
        if b["type"] == "quote" and re.fullmatch(r"It is so ordered\.|So ordered\.", text.strip()):
            blk["type"] = "p"                # set in like a quotation; it is the Court speaking
        if b.get("cont"):
            blk["cont"] = True
        if b["type"] in ("p", "quote") and not text.strip():
            continue
        cur["blocks"].append(blk)

    # ---- the syllabus: caption lines out, counsel apart -----------------------------
    for s in sections:
        if s["kind"] != "syllabus":
            continue
        cut = next((j for j, b in enumerate(s["blocks"]) if b["type"] != "pb" and DECIDED_LINE.search(plain(b["runs"]))), None)
        if cut is not None and cut < 14:
            # The line can run on into the syllabus's first paragraph (a print
            # sets them close); only the line itself goes.
            blk = s["blocks"][cut]
            text = plain(blk["runs"])
            m = re.search(DECIDED_LINE.pattern + r"\*?\.?\s*", text)
            s["argued"] = text[:m.end()].strip()
            rest = clean_runs(drop_prefix(blk["runs"], m.end()))
            s["blocks"] = ([dict(blk, runs=rest)] if plain(rest).strip() else []) + s["blocks"][cut + 1:]

    # ---- footnotes to the section that cites them ----------------------------------
    for s in sections:
        s["_refs"] = set()
    page_secs = collections.defaultdict(list)
    for s in sections:
        page = s["first_page"]
        for b in s["blocks"]:
            if b["type"] == "pb":
                page = b["p"]; page_secs[page].append(s); continue
            page_secs[page].append(s)
            for r in b.get("runs", []):
                if "pb" in r:
                    page = r["p"]; page_secs[page].append(s)
                elif r.get("sup") and re.fullmatch(r"\d{1,3}", r["t"].strip()):
                    r["fn"] = r["t"].strip()
                    s["_refs"].add((page, r["fn"]))
    last_note = None
    for page in sorted(notes_by_page):
        secs = page_secs.get(page) or ([sections[-1]] if sections else [])
        for nb in notes_by_page[page]:
            paras = [clean_runs(p["runs"]) for p in nb["paras"]]
            paras = [p for p in paras if p]
            if not paras:
                continue
            if nb["marker"] is None:
                if last_note is not None and last_note["page"] in (page, page - 1):
                    # carried over from the page before: the first paragraph continues
                    tail = last_note["note"]["paras"][-1]
                    if tail and tail[-1].get("t") is not None:
                        tail[-1]["t"] = tail[-1]["t"].rstrip() + " "
                    tail.extend(paras[0])
                    last_note["note"]["paras"][-1] = clean_runs(tail)
                    last_note["note"]["paras"].extend(paras[1:])
                    last_note["page"] = page
                continue
            target = next((s for s in secs if (page, nb["marker"]) in s["_refs"]), None)
            if target is None:
                target = next((s for s in reversed(secs) if s["kind"] != "syllabus"), secs[-1] if secs else None)
                if not nb["marker"].isdigit() and secs:
                    target = secs[0]
            if target is None:
                continue
            note = {"n": nb["marker"], "paras": paras}
            target["notes"].append(note)
            last_note = {"note": note, "page": page}

    out_sections = []
    for k, s in enumerate(sections):
        s.pop("_refs", None)
        first_page = s.pop("first_page")
        # leading and trailing page markers belong to no text
        while s["blocks"] and s["blocks"][-1]["type"] == "pb":
            s["blocks"].pop()
        if not s["blocks"] and not s["byline"]:
            continue
        s["id"] = "s%d" % (len(out_sections) + 1)
        s["title"] = section_title(s["kind"], s["who"], s["byline"])
        if s["byline"] and s["kind"] not in ("court", "percuriam", "plurality"):
            t = re.sub(r", with whom .*?(?=, (?:concurring|dissenting))", "", s["byline"]).rstrip(". ")
            if re.match(r"(?:Chief )?Justice ", t, re.I) and len(t) < 110:
                s["title"] = re.sub(r"^(chief )?justice (\w)", lambda m: ("Chief " if m.group(1) else "") + "Justice " + m.group(2).upper(), t, flags=re.I)
        s["pdf_page"] = first_page
        out_sections.append(s)
    loose = []
    for s in out_sections:
        refs = [r["fn"] for b in s["blocks"] for r in b.get("runs", []) if r.get("fn")]
        have = [n["n"] for n in s["notes"] if n["n"].isdigit()]
        if sorted(set(refs), key=int) != sorted(set(have), key=int):
            loose.append([s["id"], sorted(set(refs) - set(have), key=int), sorted(set(have) - set(refs), key=int)])
    words = sum(len(plain(b["runs"]).split()) for s in out_sections for b in s["blocks"] if b["type"] in ("p", "quote"))
    return {
        "version": EXTRACT_VERSION, "layout": layout, "cite": cite, "short": short, "pages": hi - lo, "body_size": size,
        "sections": out_sections,
        "qa": {"words": words, "notes": sum(len(s["notes"]) for s in out_sections),
               "ligatures_fixed": sum(lig.fixed.values()), "ligatures_guessed": dict(lig.guessed.most_common(40)),
               "notes_unmatched": loose, "hyphens_kept": dict(HYPH.kept.most_common(60)),
               "sections": [[s["kind"], s["who"], len(s["blocks"]), len(s["notes"])] for s in out_sections]},
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("pdf")
    ap.add_argument("-o", "--out")
    ap.add_argument("--pages", help="A-B, 1-based, for a case inside a volume")
    a = ap.parse_args()
    first = last = None
    if a.pages:
        first, last = (int(x) for x in a.pages.split("-"))
    doc = extract(a.pdf, first, last)
    data = json.dumps(doc, ensure_ascii=False, separators=(",", ":"))
    if a.out:
        with io.open(a.out, "w", encoding="utf-8", newline="\n") as f:
            f.write(data)
        print(json.dumps(doc["qa"], ensure_ascii=False))
    else:
        sys.stdout.buffer.write(data.encode("utf-8"))


if __name__ == "__main__":
    main()
