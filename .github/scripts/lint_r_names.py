#!/usr/bin/env python3
"""Fail on a top-level name defined in more than one file under R/.

Every entry point here source()s a handful of R/ files into one global
environment, in an order that differs per script, so two files defining the
same name is a silent last-one-wins. It has bitten twice: `.cal_df` in
argument_calendar.R replaced site_calendar.R's and killed the landing-page
calendar for six green runs (docs/workflows.md), and `caption_sides` in
docket_page.R replaced cert_model.R's for six days (2026-09-16 to 22), during
which every federal petitioner was scored as a private individual.

A duplicate is fine only when it is the same definition in both places (a
shared helper like `%||%` pasted where it is needed) -- those are compared by
their text and allowed. Anything else fails.

Run: python .github/scripts/lint_r_names.py
"""
import pathlib
import re
import sys
from collections import defaultdict

ROOT = pathlib.Path(__file__).resolve().parents[2]
R_DIR = ROOT / "R"
# The pre-JSON HTML scraper. Nothing sources it (grep for the name finds only
# this line), and its fetch helpers share names with scotus_dash_new.R's.
IGNORE = {"scotus_docket.R"}

# `name <- function(` or `name <- value` at column 0; backtick names allowed.
DEF = re.compile(r"^(`[^`]+`|[A-Za-z.][A-Za-z0-9._]*)\s*(<-|=)\s*(.*)$")


def definitions(path):
    """Yield (name, first line of the definition) for each top-level assignment."""
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        m = DEF.match(line)
        if m and not line.lstrip().startswith("#"):
            yield m.group(1).strip("`"), m.group(3).strip()


def main():
    seen = defaultdict(list)
    for path in sorted(R_DIR.glob("*.R")):
        if path.name in IGNORE:
            continue
        for name, head in definitions(path):
            seen[name].append((path.name, head))
    bad = []
    for name, defs in sorted(seen.items()):
        files = sorted({f for f, _ in defs})
        if len(files) < 2:
            continue
        # The same one-liner in two files is a pasted helper, not a collision.
        heads = {h for _, h in defs}
        if len(heads) == 1 and "function" in next(iter(heads)) and next(iter(heads)).rstrip().endswith(("}", ")")):
            continue
        bad.append((name, files))
    if bad:
        print("Top-level names defined in more than one R/ file (last source() wins, silently):")
        for name, files in bad:
            print(f"  {name:32s} {', '.join(files)}")
        print("\nRename the file-local one (a leading dot marks it private to its file).")
        return 1
    print(f"lint_r_names: {len(seen)} top-level names across R/, no collisions.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
