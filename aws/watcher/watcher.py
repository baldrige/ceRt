"""The court watcher: notices the Court publishing an order, an opinion or an
argument transcript/recording within a minute or two, and makes sure a daily
run of the site has processed it.

Runs as the cert-scheduler Lambda (aws/scheduler.yaml), deployed from this file
by .github/workflows/deploy-watcher.yml. Two kinds of event:

  {"workflow": "daily.yml"}  the fixed schedules: dispatch that workflow unless
                             one is already queued or running.
  {"action": "watch"}        every minute: poll the sources below.

WHAT IT WATCHES, and the key that makes an item "new" (docs/data-sources.md):

  orders      /orders/ordersofthecourt/NN, this Term and the last  order PDF stem
  slip        /rss/slipopinion_rss.aspx?TYear=NN                   item link
  relating    /opinions/relatingtoorders/NN                        opinion PDF path
  transcripts /rss/argument_transcripts_rss.aspx?TYear=NN          docket
  audio       /rss/argument_audio_rss.aspx?TYear=NN                docket
  hermes      /rss/hermes_transfer.xml                             file + its date

Detection is by CONTENT, never by timestamp: the orders page's Last-Modified
moved at 1:33 p.m. on 6 Oct 2026 with no new order on it. Last-Modified/ETag
only let an unchanged page answer 304. Each source keeps the set of keys it
has seen, so a poll that is missed or fails is caught up on the next one; a
source that errors is skipped (and backed off), never read as "everything
vanished". The first look at a source records it and dispatches nothing.

WHAT "HANDLED" MEANS. Every new key becomes a pending event. It is cleared only
when a daily that STARTED after the event was seen concludes success -- not
when one is dispatched: on 5 Oct 2026 three runs were cancelled unstarted in a
GitHub Actions outage. While events are pending, the watcher dispatches a
daily unless one created after the oldest event is queued, running or green;
after a failure it waits 5, 10, 20 (max 30) minutes before the next try. An
event still pending after ALERT_AFTER_MIN sends one email (SNS).

Cadence: every minute on weekdays 9 a.m.-6 p.m. ET, when orders, opinions and
transcripts appear; every 5 minutes otherwise. State is one JSON document in
DynamoDB; a 90-second lease in the same table keeps polls from overlapping
(reserved concurrency would do it, but a new AWS account cannot reserve any).
"""
import datetime as dt
import json
import os
import re
import urllib.error
import urllib.request

BASE = "https://www.supremecourt.gov"
UA = "ceRt SCOTUS docketing dashboard (court watch v2; +https://supremecourt.report)"
LIVE = ("queued", "in_progress", "waiting", "pending", "requested")
ALERT_AFTER_MIN = 45
RETRY_MIN = (5, 10, 20, 30)          # wait after the 1st, 2nd, 3rd, later failure
DISPATCH_GRACE_S = 180               # a just-dispatched run may not be listed yet
KEEP_KEYS = 3000                     # per source; a Term's orders are ~150 files


# ---- time ------------------------------------------------------------------------

def et_now(now):
    """Eastern time: EDT from the second Sunday of March to the first Sunday of
    November, EST otherwise. Only the polling cadence depends on it."""
    y = now.year
    mar = dt.datetime(y, 3, 8, 7, tzinfo=dt.timezone.utc)      # 2 a.m. EST = 07:00 UTC
    mar += dt.timedelta(days=(6 - mar.weekday()) % 7)
    nov = dt.datetime(y, 11, 1, 6, tzinfo=dt.timezone.utc)     # 2 a.m. EDT = 06:00 UTC
    nov += dt.timedelta(days=(6 - nov.weekday()) % 7)
    off = -4 if mar <= now < nov else -5
    return now.astimezone(dt.timezone(dt.timedelta(hours=off)))


def busy_hours(now):
    e = et_now(now)
    return e.weekday() < 5 and 9 <= e.hour < 18


def should_poll(now):
    return busy_hours(now) or now.minute % 5 == 0


def terms(now):
    """Two-digit Terms to watch: the current one and the last. An October Term
    starts in October, but the Court's September orders, and the first days of
    October before the Term opens, sit on the previous Term's pages (the 29 Sep
    and 1 Oct 2026 orders are on /25), and the last opinions of a Term arrive
    in June and July; watching both covers every boundary."""
    cur = now.year % 100 if now.month >= 10 else (now.year - 1) % 100
    return [cur, (cur - 1) % 100]


def iso(t):
    return t.astimezone(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def parse_iso(s):
    return dt.datetime.strptime(s, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=dt.timezone.utc)


# ---- parsers: page text -> set of keys --------------------------------------------

def parse_orders(html):
    """Order documents on an /orders/ordersofthecourt page, by stem:
    courtorders/100526zor_2a34.pdf -> "100526zor"; ...100126zr1_j4el.pdf -> "100126zr1"."""
    return set(m.lower() for m in re.findall(r"courtorders/([0-9]{6}[a-z0-9]+?)_[a-z0-9]+\.pdf", html, re.I))


def parse_relating(html):
    """Opinion PDFs on an /opinions/relatingtoorders page, without #page= fragments."""
    return set(m.lower() for m in re.findall(r"/opinions/\d{2}pdf/[^\"'#\s>]+\.pdf", html, re.I))


def _items(xml):
    return re.findall(r"<item>(.*?)</item>", xml, re.S)


def _field(block, tag):
    m = re.search(r"<%s>(.*?)</%s>" % (tag, tag), block, re.S)
    return re.sub(r"<!\[CDATA\[|\]\]>", "", m.group(1)).strip() if m else ""


def parse_slip(xml):
    """Slip-opinion RSS: each item's PDF link (http and https are the same
    document). A Term's empty feed holds a placeholder item whose link is the
    bare /opinions/ page; only a link to a PDF is an opinion."""
    links = (_field(b, "link").replace("http://", "https://") for b in _items(xml))
    return set(u for u in links if u.lower().endswith(".pdf"))


_DOCKET = re.compile(r"\((\d{2}-\d{1,5}|\d{2}A\d{1,4}|\d{1,4}-Orig)\)")


def parse_argument_feed(xml):
    """Transcript or audio RSS: the docket each item names. The empty
    placeholder item a feed holds before its Term's first argument ("()") has
    none and is skipped."""
    out = set()
    for b in _items(xml):
        m = _DOCKET.search(_field(b, "title"))
        if m:
            out.add(m.group(1))
    return out


def parse_hermes(xml):
    """Hermes: each file with its modification time, plus the channel's own
    date (the last transfer). A changed set is the "something moved" hint."""
    keys = set()
    for b in _items(xml):
        t = _field(b, "title")
        if t and t.lower() != "thumbs.db":
            keys.add("%s|%s" % (t, _field(b, "pubDate")))
    m = re.search(r"<channel>.*?<pubDate>(.*?)</pubDate>", xml, re.S)
    if m:
        keys.add("channel|" + m.group(1).strip())
    return keys


def sources(now):
    """(name, url, parser) for every source this poll reads."""
    out = [("hermes", BASE + "/rss/hermes_transfer.xml", parse_hermes)]
    for t in terms(now):
        out += [
            ("orders/%02d" % t, BASE + "/orders/ordersofthecourt/%02d" % t, parse_orders),
            ("relating/%02d" % t, BASE + "/opinions/relatingtoorders/%02d" % t, parse_relating),
            ("slip/%02d" % t, BASE + "/rss/slipopinion_rss.aspx?TYear=%02d" % t, parse_slip),
            ("transcripts/%02d" % t, BASE + "/rss/argument_transcripts_rss.aspx?TYear=%02d" % t, parse_argument_feed),
            ("audio/%02d" % t, BASE + "/rss/argument_audio_rss.aspx?TYear=%02d" % t, parse_argument_feed),
        ]
    return out


# ---- the pure core: observations + runs -> new state and actions ------------------

def observe(state, name, keys, now):
    """Fold one source's poll into the state. `keys` is a set, or None when the
    fetch failed. Returns the list of new event ids."""
    src = state.setdefault("sources", {}).setdefault(name, {})
    if keys is None:
        n = src.get("fails", 0) + 1
        src["fails"] = n
        src["skip_until"] = iso(now + dt.timedelta(minutes=min(2 ** n, 30)))
        return []
    src["fails"] = 0
    src.pop("skip_until", None)
    if "keys" not in src:                      # first look: baseline, no events
        src["keys"] = sorted(keys)[-KEEP_KEYS:]
        return []
    old = set(src["keys"])
    new = sorted(keys - old)
    if new:
        src["keys"] = sorted(old | keys)[-KEEP_KEYS:]
    pend = state.setdefault("pending", {})
    ids = []
    for k in new:
        eid = "%s:%s" % (name.split("/")[0], k)
        if eid not in pend:
            pend[eid] = {"seen": iso(now)}
            ids.append(eid)
    return ids


def skipped(state, name, now):
    s = state.get("sources", {}).get(name, {}).get("skip_until")
    return bool(s) and now < parse_iso(s)


def decide(state, runs, now):
    """Given the daily's recent runs (GitHub API objects: created_at, status,
    conclusion), clear the events a green run has handled and decide whether to
    dispatch. Returns {"dispatch": bool, "cleared": [...], "alert": [...]}."""
    pend = state.get("pending", {})
    out = {"dispatch": False, "cleared": [], "alert": []}
    if not pend:
        return out
    rs = [dict(r, t=parse_iso(r["created_at"])) for r in runs]
    for eid, ev in list(pend.items()):
        seen = parse_iso(ev["seen"])
        # >=, not >: the watcher dispatches only after seeing an event, so a
        # run created in the same second as the sighting is one it started.
        if any(r["t"] >= seen and r.get("conclusion") == "success" for r in rs):
            out["cleared"].append(eid)
            del pend[eid]
    if not pend:
        state.pop("dispatch", None)
        return out
    oldest = min(parse_iso(ev["seen"]) for ev in pend.values())
    after = [r for r in rs if r["t"] >= oldest]
    working = [r for r in after if r.get("status") in LIVE]
    failed = [r for r in after if r.get("status") == "completed" and r.get("conclusion") != "success"]
    last = state.get("dispatch", {}).get("at")
    if working:
        pass                                       # a run after the event is on its way
    elif last and (now - parse_iso(last)).total_seconds() < DISPATCH_GRACE_S:
        pass                                       # ours may not be listed yet
    elif failed and last:
        wait = RETRY_MIN[min(len(failed), len(RETRY_MIN)) - 1]
        newest_fail = max(r["t"] for r in failed)
        if (now - max(newest_fail, parse_iso(last))).total_seconds() >= wait * 60:
            out["dispatch"] = True
    else:
        out["dispatch"] = True
    if out["dispatch"]:
        state["dispatch"] = {"at": iso(now)}
    for eid, ev in pend.items():
        if not ev.get("alerted") and (now - parse_iso(ev["seen"])).total_seconds() >= ALERT_AFTER_MIN * 60:
            ev["alerted"] = iso(now)
            out["alert"].append(eid)
    return out


# ---- side effects ------------------------------------------------------------------

def http_get(url, src_state):
    """GET with If-Modified-Since / If-None-Match. Returns text, or "" for 304
    (unchanged), or raises."""
    h = {"User-Agent": UA}
    if src_state.get("lm"):
        h["If-Modified-Since"] = src_state["lm"]
    if src_state.get("etag"):
        h["If-None-Match"] = src_state["etag"]
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers=h), timeout=15) as r:
            src_state["lm"] = r.headers.get("Last-Modified") or src_state.get("lm")
            src_state["etag"] = r.headers.get("ETag") or src_state.get("etag")
            return r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        if e.code == 304:
            return ""
        raise


class Env:
    """AWS and GitHub, behind four methods the tests replace."""

    def __init__(self):
        import boto3
        self.repo = os.environ["REPO"]
        self.ddb = boto3.client("dynamodb")
        self.table = os.environ["STATE_TABLE"]
        self.sns = boto3.client("sns")
        self.topic = os.environ.get("ALERT_TOPIC", "")
        self._tok = boto3.client("secretsmanager").get_secret_value(
            SecretId=os.environ["TOKEN_SECRET"])["SecretString"].strip()

    def gh(self, method, path, body=None):
        req = urllib.request.Request(
            "https://api.github.com/repos/" + self.repo + path, method=method,
            data=None if body is None else json.dumps(body).encode(),
            headers={"Authorization": "Bearer " + self._tok, "User-Agent": UA,
                     "Accept": "application/vnd.github+json"})
        with urllib.request.urlopen(req, timeout=20) as r:
            t = r.read()
            return json.loads(t) if t else {}

    def runs(self, wf):
        return self.gh("GET", "/actions/workflows/%s/runs?per_page=30" % wf).get("workflow_runs", [])

    def dispatch(self, wf):
        self.gh("POST", "/actions/workflows/%s/dispatches" % wf, {"ref": "main"})

    def load(self):
        r = self.ddb.get_item(TableName=self.table, Key={"pk": {"S": "state"}})
        return json.loads(r["Item"]["doc"]["S"]) if "Item" in r else {}

    def save(self, state):
        self.ddb.put_item(TableName=self.table, Item={"pk": {"S": "state"}, "doc": {"S": json.dumps(state)}})

    def lock(self, now, seconds=90):
        """Take the poll lease unless another poll holds an unexpired one."""
        t = int(now.timestamp())
        try:
            self.ddb.put_item(
                TableName=self.table, Item={"pk": {"S": "lease"}, "until": {"N": str(t + seconds)}},
                ConditionExpression="attribute_not_exists(pk) OR #u < :now",
                ExpressionAttributeNames={"#u": "until"}, ExpressionAttributeValues={":now": {"N": str(t)}})
            return True
        except self.ddb.exceptions.ConditionalCheckFailedException:
            return False

    def unlock(self):
        self.ddb.delete_item(TableName=self.table, Key={"pk": {"S": "lease"}})

    def alert(self, subject, text):
        if self.topic:
            self.sns.publish(TopicArn=self.topic, Subject=subject[:99], Message=text)

    def fetch(self, url, src_state):
        return http_get(url, src_state)


def poll(env, now=None, force=False):
    now = now or dt.datetime.now(dt.timezone.utc)
    if not force and not should_poll(now):
        return "off-minute"
    if not env.lock(now):
        print("another poll holds the lease; skipping this minute")
        return "busy"
    try:
        return _poll(env, now)
    finally:
        env.unlock()


def _poll(env, now):
    state = env.load()
    new, failed, notes = [], [], []
    for name, url, parse in sources(now):
        if skipped(state, name, now):
            continue
        src = state.setdefault("sources", {}).setdefault(name, {})
        try:
            text = env.fetch(url, src)
            keys = set(src.get("keys", [])) if text == "" else parse(text)
        except Exception as e:                      # noqa: BLE001 -- one source, never the poll
            keys = None
            failed.append(name)
            notes.append("%s: %s" % (name, e))
        new += observe(state, name, keys, now)
    act = {"dispatch": False, "cleared": [], "alert": []}
    if state.get("pending"):
        act = decide(state, env.runs("daily.yml"), now)
        if act["dispatch"]:
            env.dispatch("daily.yml")
        if act["alert"]:
            env.alert("Court watch: not yet on the site after %d min" % ALERT_AFTER_MIN,
                      "These were published by the Court but no daily run has succeeded since:\n\n"
                      + "\n".join("  %s (seen %s)" % (e, state["pending"][e]["seen"]) for e in act["alert"])
                      + "\n\nRuns: https://github.com/%s/actions/workflows/daily.yml" % env.repo)
    env.save(state)
    msg = "new %d %s | pending %d | dispatch %s | cleared %d | failed %s" % (
        len(new), new[:6], len(state.get("pending", {})), act["dispatch"], len(act["cleared"]), failed or "-")
    print(msg, *notes, sep="\n  ")
    return msg


def dispatch_scheduled(env, wf):
    if any(r.get("status") in LIVE for r in env.runs(wf)[:10]):
        print(wf, "already queued or running; not dispatching")
        return "skipped " + wf
    env.dispatch(wf)
    print("dispatched", wf)
    return "dispatched " + wf


def handler(event, context=None):
    env = Env()
    if event.get("action") == "watch":
        return poll(env, force=bool(event.get("force")))
    return dispatch_scheduled(env, event["workflow"])
