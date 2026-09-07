#!/usr/bin/env python3
"""Watch the Court's Hermes transfer feed and dispatch the daily when it changes.

supremecourt.gov/rss/hermes_transfer.xml is a 1 KB RSS feed naming the files
the Court's internal system just pushed to the web site, with timestamps: on
4 Sep 2026 it listed 090426ZOR.xml (that day's order list) and 26A274.xml (a
per curiam), the latter stamped 15:14 ET. The XML files it names are not
served, so the content is not usable -- but the feed is a "something changed"
signal, and it is the cheapest one the Court offers.

The daily runs on fixed crons. An opinion posted at 15:14 ET waited until the
16:33 ET slot; an order list posted at 9:30 ET on a Monday waited until 12:33.
This script fingerprints the feed (item names and modified times, the
channel's own timestamp; Thumbs.db excluded), and when the fingerprint differs
from the last one it saw, dispatches daily.yml -- unless a daily is already
queued or running, or one was dispatched from here in the last
DISPATCH_GAP_MIN minutes.

WHY IT LOOPS. This began as a fifteen-minute cron. Measured over its first 44
hours (5-7 Sep 2026): GitHub honoured 16 of 177 slots, a median of 126 minutes
apart, never closer than 94. The scheduler sheds a frequent cron on this repo
to roughly two-hourly whatever the cron asks for. So a run now stays up for
WATCH_LOOP_MIN minutes (just under GitHub's six-hour job cap), polling every
POLL_SEC seconds from inside the job, and before it exits dispatches the next
run of its own workflow -- unless another watcher run is already queued or
active, in which case that one is the next link. The cron stays as the
restart floor: a chain that breaks is picked up at the next slot the
scheduler honours.

State (the last fingerprint) is written to STATE_FILE after every poll and
carried between runs by the Actions cache (see watch-court.yml). The first
run with no state records the fingerprint and dispatches nothing. Every
decision is printed.

Env: STATE_FILE (default .hermes_state/state.json), GH_TOKEN / GH_REPO for
`gh`, GITHUB_RUN_ID (this run, for the active-watcher check), WATCH_LOOP_MIN
(0 = poll once and exit), POLL_SEC (default 900), RECHAIN=1 to dispatch the
next run at exit, DRY_RUN=1 to decide without dispatching anything.
"""
import hashlib
import json
import os
import re
import subprocess
import sys
import time
import urllib.request
from datetime import datetime, timedelta, timezone

FEED = "https://www.supremecourt.gov/rss/hermes_transfer.xml"
UA = "ceRt SCOTUS docketing dashboard (court watch)"
STATE_FILE = os.environ.get("STATE_FILE", ".hermes_state/state.json")
DISPATCH_GAP_MIN = 20
IGNORE = {"thumbs.db"}
LOOP_MIN = float(os.environ.get("WATCH_LOOP_MIN", "0") or 0)
POLL_SEC = int(os.environ.get("POLL_SEC", "900") or 900)
DRY = bool(os.environ.get("DRY_RUN"))
RUN_ID = os.environ.get("GITHUB_RUN_ID", "")


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


LIVE = ("queued", "in_progress", "waiting", "pending", "requested")


def live_runs(workflow):
    runs = json.loads(gh("run", "list", "--workflow", workflow, "--limit", "10",
                         "--json", "status,createdAt,databaseId,event"))
    return [r for r in runs if r["status"] in LIVE]


def stamp():
    return datetime.now(timezone.utc).strftime("%H:%M:%SZ")


def poll(state):
    """One look at the feed; returns the updated state."""
    now = datetime.now(timezone.utc)
    xml = fetch(FEED)
    its = items(xml)
    chan = channel_date(xml)
    fp = hashlib.sha1(json.dumps([its, chan]).encode()).hexdigest()
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
            live = live_runs("daily.yml")
            if live:
                # Leave the fingerprint unrecorded, so the next poll looks again
                # once that run is out of the way; a run that started before the
                # change may not have seen it.
                decision = f"changed, but a daily is {live[0]['status']} (started {live[0]['createdAt']}) -- retry next poll"
                print(f"[{stamp()}] feed {fp[:12]} ({len(its)} item(s), channel {chan!r}): {decision}")
                state.update({"checked": now.isoformat(timespec="seconds")})
                save_state(state)
                return state
            if DRY:
                decision = "changed: would dispatch daily.yml (DRY_RUN)"
            else:
                gh("workflow", "run", "daily.yml", "--ref", "main")
                decision = "changed: dispatched daily.yml"
                last_dispatch = now.isoformat(timespec="seconds")
    print(f"[{stamp()}] feed {fp[:12]} ({len(its)} item(s), channel {chan!r}): {decision}")
    if prev is None or prev != fp:
        for t, p in its:
            print(f"    {t:28} {p}")
    state = {"fingerprint": fp, "last_dispatch": last_dispatch,
             "checked": now.isoformat(timespec="seconds"), "channel": chan,
             "items": [f"{t} {p}" for t, p in its]}
    save_state(state)
    return state


def rechain():
    """Dispatch the next run of this workflow, unless one is already queued or active."""
    others = [r for r in live_runs("watch-court.yml") if str(r.get("databaseId")) != str(RUN_ID)]
    if others:
        print(f"[{stamp()}] next link already {others[0]['status']} (run {others[0]['databaseId']}); not re-dispatching")
        return
    if DRY:
        print(f"[{stamp()}] would dispatch the next watch-court.yml run (DRY_RUN)")
        return
    gh("workflow", "run", "watch-court.yml", "--ref", "main")
    print(f"[{stamp()}] dispatched the next watch-court.yml run")


def main():
    # A cron-started run that finds a watcher already active is redundant: the
    # active one is the chain. (The concurrency group would queue this run
    # behind it anyway; exiting now frees the slot for the real next link.)
    if LOOP_MIN > 0 and RUN_ID:
        others = [r for r in live_runs("watch-court.yml")
                  if str(r.get("databaseId")) != str(RUN_ID) and r["status"] == "in_progress"]
        if others:
            print(f"[{stamp()}] another watcher is in progress (run {others[0]['databaseId']}, started {others[0]['createdAt']}); exiting")
            return 0
    state = load_state()
    started = datetime.now(timezone.utc)
    deadline = started + timedelta(minutes=LOOP_MIN)
    n = 0
    while True:
        n += 1
        try:
            state = poll(state)
        except Exception as e:  # a bad poll is a missed poll, not a dead watcher
            print(f"[{stamp()}] poll failed: {e}", file=sys.stderr)
        if LOOP_MIN <= 0:
            break
        nxt = datetime.now(timezone.utc) + timedelta(seconds=POLL_SEC)
        if nxt >= deadline:
            break
        time.sleep(max(1, (nxt - datetime.now(timezone.utc)).total_seconds()))
    print(f"[{stamp()}] {n} poll(s) over {int((datetime.now(timezone.utc) - started).total_seconds() // 60)} min")
    if os.environ.get("RECHAIN"):
        try:
            rechain()
        except Exception as e:
            print(f"[{stamp()}] re-dispatch failed: {e} (the cron is the floor)", file=sys.stderr)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as e:  # never let a watcher failure look like a Court event
        print(f"court watch failed: {e}", file=sys.stderr)
        sys.exit(1)
