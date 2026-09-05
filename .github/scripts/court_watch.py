#!/usr/bin/env python3
"""Watch the Court's Hermes transfer feed and dispatch the daily when it changes.

supremecourt.gov/rss/hermes_transfer.xml is a 1 KB RSS feed naming the files
the Court's internal system just pushed to the web site, with timestamps: on
4 Sep 2026 it listed 090426ZOR.xml (that day's order list) and 26A274.xml (a
per curiam), the latter stamped 15:14 ET. The XML files it names are not
served, so the content is not usable -- but the feed is a "something changed"
signal, and it is the cheapest one the Court offers.

The daily runs on three fixed crons. An opinion posted at 15:14 ET waited
until the 16:33 ET slot; an order list posted at 9:30 ET on a Monday waited
until 12:33. This script runs every fifteen minutes, fingerprints the feed
(item names and modified times, the channel's own timestamp; Thumbs.db
excluded), and when the fingerprint differs from the last one it saw,
dispatches daily.yml -- unless a daily is already queued or running, or one
was dispatched from here in the last DISPATCH_GAP_MIN minutes.

State is one small JSON file carried between runs by the Actions cache (see
watch-court.yml). The first run with no state records the fingerprint and
dispatches nothing. Every decision is printed, so a week of logs says what
the feed actually does around an order list.

Env: STATE_FILE (default .hermes_state/state.json), GH_TOKEN / GH_REPO for
`gh`, DRY_RUN=1 to decide without dispatching.
"""
import hashlib
import json
import os
import re
import subprocess
import sys
import urllib.request
from datetime import datetime, timedelta, timezone

FEED = "https://www.supremecourt.gov/rss/hermes_transfer.xml"
UA = "ceRt SCOTUS docketing dashboard (court watch)"
STATE_FILE = os.environ.get("STATE_FILE", ".hermes_state/state.json")
DISPATCH_GAP_MIN = 20
IGNORE = {"thumbs.db"}


def fetch(url):
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as r:
        return r.read().decode("utf-8", "replace")


def items(xml):
    out = []
    for block in re.findall(r"<item>(.*?)</item>", xml, flags=re.S):
        title = re.sub(r"<!\[CDATA\[|\]\]>", "", re.search(r"<title>(.*?)</title>", block, re.S).group(1)).strip()
        pub = (re.search(r"<pubDate>(.*?)</pubDate>", block, re.S) or [None, ""])[1].strip()
        if title.lower() in IGNORE:
            continue
        out.append((title, pub))
    return sorted(out)


def channel_date(xml):
    m = re.search(r"<channel>.*?<pubDate>(.*?)</pubDate>", xml, re.S)
    return m.group(1).strip() if m else ""


def load_state():
    try:
        with open(STATE_FILE, encoding="utf-8") as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def save_state(state):
    os.makedirs(os.path.dirname(STATE_FILE) or ".", exist_ok=True)
    with open(STATE_FILE, "w", encoding="utf-8") as f:
        json.dump(state, f, indent=1, sort_keys=True)


def gh(*args):
    return subprocess.run(["gh", *args], check=True, capture_output=True, text=True).stdout


def daily_busy():
    runs = json.loads(gh("run", "list", "--workflow", "daily.yml", "--limit", "8",
                         "--json", "status,createdAt,event"))
    live = [r for r in runs if r["status"] in ("queued", "in_progress", "waiting", "pending", "requested")]
    return live


def main():
    now = datetime.now(timezone.utc)
    xml = fetch(FEED)
    its = items(xml)
    chan = channel_date(xml)
    fp = hashlib.sha1(json.dumps([its, chan]).encode()).hexdigest()
    print(f"feed: channel {chan!r}; {len(its)} item(s)")
    for t, p in its:
        print(f"  {t:28} {p}")
    print(f"fingerprint {fp[:12]}")

    state = load_state()
    prev = state.get("fingerprint")
    last_dispatch = state.get("last_dispatch")
    decision = "no change"
    if prev is None:
        decision = "first run: baseline recorded, no dispatch"
    elif prev != fp:
        decision = "changed"
        gap_ok = True
        if last_dispatch:
            since = now - datetime.fromisoformat(last_dispatch)
            gap_ok = since >= timedelta(minutes=DISPATCH_GAP_MIN)
            if not gap_ok:
                decision = f"changed, but dispatched {int(since.total_seconds() // 60)} min ago -- wait"
        if gap_ok:
            live = daily_busy()
            if live:
                # Leave the fingerprint unrecorded, so the next poll looks again
                # once that run is out of the way; a run that started before the
                # change may not have seen it.
                decision = f"changed, but a daily is {live[0]['status']} (started {live[0]['createdAt']}) -- retry next poll"
                print(f"decision: {decision}")
                save_state({"fingerprint": prev, "last_dispatch": last_dispatch,
                            "checked": now.isoformat(timespec="seconds")})
                return 0
            if os.environ.get("DRY_RUN"):
                decision = "changed: would dispatch daily.yml (DRY_RUN)"
            else:
                gh("workflow", "run", "daily.yml", "--ref", "main")
                decision = "changed: dispatched daily.yml"
                last_dispatch = now.isoformat(timespec="seconds")
    print(f"decision: {decision}")
    save_state({"fingerprint": fp, "last_dispatch": last_dispatch,
                "checked": now.isoformat(timespec="seconds"), "channel": chan,
                "items": [f"{t} {p}" for t, p in its]})
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as e:  # never let a watcher failure look like a Court event
        print(f"court watch failed: {e}", file=sys.stderr)
        sys.exit(1)
