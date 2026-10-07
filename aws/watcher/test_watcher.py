"""Tests for the court watcher. Offline: the parsers run on pages saved from
supremecourt.gov on 7 Oct 2026 (fixtures/), and the poll runs against a fake
Court and a fake GitHub.   python -m unittest discover aws/watcher"""
import datetime as dt
import os
import unittest

import watcher as w

FX = os.path.join(os.path.dirname(__file__), "fixtures")
UTC = dt.timezone.utc


def fx(name):
    with open(os.path.join(FX, name), encoding="utf-8") as f:
        return f.read()


def T(s):
    return dt.datetime.strptime(s, "%Y-%m-%d %H:%M").replace(tzinfo=UTC)


class Parsers(unittest.TestCase):
    def test_orders(self):
        k26 = w.parse_orders(fx("orders_26.html"))
        self.assertEqual(k26, {"100526zor"})
        k25 = w.parse_orders(fx("orders_25.html"))
        # The late-September and pre-Term October orders sit on the OT25 page.
        self.assertTrue({"100126zr", "100126zr1", "092926zr4"} <= k25)
        self.assertTrue(all(len(k) >= 8 for k in k25))   # "092926zr" is a plain misc order

    def test_relating(self):
        k25 = w.parse_relating(fx("relating_25.html"))
        self.assertIn("/opinions/25pdf/26a428_4f15.pdf", k25)
        self.assertIn("/opinions/25pdf/26a305_4g15.pdf", k25)
        self.assertFalse(any("#" in k for k in k25))   # "#page=2" is the same document
        self.assertEqual(len(w.parse_relating(fx("relating_26.html"))), 1)

    def test_slip(self):
        k = w.parse_slip(fx("slip_25.xml"))
        self.assertEqual(len(k), 74)
        self.assertTrue(all(u.startswith("https://") for u in k))
        self.assertEqual(w.parse_slip(fx("slip_26.xml")), set())

    def test_argument_feeds(self):
        self.assertEqual(w.parse_argument_feed(fx("transcripts_26.xml")), {"25-170", "25-735", "25-498"})
        self.assertTrue({"25-170", "25-735"} <= w.parse_argument_feed(fx("audio_26.xml")))
        placeholder = "<item><title><![CDATA[ () ]]></title></item>"
        self.assertEqual(w.parse_argument_feed(placeholder), set())
        self.assertEqual(w.parse_argument_feed("<item><title>X v. Y (141-Orig)</title></item>"), {"141-Orig"})

    def test_hermes(self):
        k = w.parse_hermes(fx("hermes.xml"))
        self.assertTrue(any(x.startswith("100526ZOR.xml|") for x in k))
        self.assertTrue(any(x.startswith("channel|") for x in k))
        self.assertFalse(any(x.lower().startswith("thumbs.db") for x in k))


class Clock(unittest.TestCase):
    def test_terms(self):
        self.assertEqual(w.terms(T("2026-10-07 15:00")), [26, 25])
        self.assertEqual(w.terms(T("2026-09-29 15:00")), [25, 24])
        self.assertEqual(w.terms(T("2027-06-30 15:00")), [26, 25])

    def test_cadence(self):
        self.assertTrue(w.should_poll(T("2026-10-07 14:01")))    # Wed 10:01 EDT
        self.assertFalse(w.should_poll(T("2026-10-07 03:01")))   # 23:01 EDT Tue
        self.assertTrue(w.should_poll(T("2026-10-07 03:05")))
        self.assertFalse(w.should_poll(T("2026-10-10 15:01")))   # Saturday
        # EST after 1 Nov: 9 a.m. EST is 14:00 UTC, 8:59 is not busy.
        self.assertFalse(w.busy_hours(T("2026-11-04 13:59")))
        self.assertTrue(w.busy_hours(T("2026-11-04 14:00")))


class FakeEnv:
    """A Court whose pages we set, and a GitHub whose daily runs we set."""
    repo = "baldrige/ceRt"

    def __init__(self, pages):
        self.pages = dict(pages)      # url substring -> text, or Exception to raise
        self.state = {}
        self.daily = []               # run dicts
        self.dispatched = []
        self.alerts = []

    def fetch(self, url, src):
        for k, v in self.pages.items():
            if k in url:
                if isinstance(v, Exception):
                    raise v
                return v
        return "<rss><channel></channel></rss>"

    def runs(self, wf):
        return list(self.daily)

    def dispatch(self, wf):
        self.dispatched.append(wf)

    def load(self):
        return self.state

    def save(self, s):
        self.state = s

    def lock(self, now):
        return not getattr(self, "held", False)

    def unlock(self):
        pass

    def alert(self, subj, text):
        self.alerts.append(subj)


def run(created, status="completed", conclusion="success"):
    return {"created_at": w.iso(T(created)), "status": status, "conclusion": conclusion}


class Poll(unittest.TestCase):
    def setUp(self):
        self.env = FakeEnv({"argument_transcripts_rss.aspx?TYear=26":
                            "<item><title>Suncor (25-170)</title></item>"})
        w.poll(self.env, T("2026-10-06 16:00"))          # baseline

    def test_baseline_dispatches_nothing(self):
        self.assertEqual(self.env.dispatched, [])
        self.assertEqual(self.env.state.get("pending", {}), {})

    def test_new_transcript_dispatches_once_and_clears_on_success(self):
        self.env.pages["argument_transcripts_rss.aspx?TYear=26"] += "<item><title>Anderson v. Intel (25-498)</title></item>"
        w.poll(self.env, T("2026-10-06 17:10"))
        self.assertEqual(self.env.dispatched, ["daily.yml"])
        self.assertIn("transcripts:25-498", self.env.state["pending"])
        # Next minute: the run is queued -- no second dispatch.
        self.env.daily = [run("2026-10-06 17:10", "queued", None)]
        w.poll(self.env, T("2026-10-06 17:11"))
        self.assertEqual(len(self.env.dispatched), 1)
        # It succeeds: the event clears.
        self.env.daily = [run("2026-10-06 17:10")]
        w.poll(self.env, T("2026-10-06 17:30"))
        self.assertEqual(self.env.state["pending"], {})
        self.assertEqual(len(self.env.dispatched), 1)

    def test_outage_redispatches_with_backoff_then_alerts(self):
        self.env.pages["argument_transcripts_rss.aspx?TYear=26"] += "<item><title>Anderson v. Intel (25-498)</title></item>"
        w.poll(self.env, T("2026-10-06 17:10"))                       # dispatch 1
        self.env.daily = [run("2026-10-06 17:10", conclusion="cancelled")]
        w.poll(self.env, T("2026-10-06 17:12"))                       # inside grace
        w.poll(self.env, T("2026-10-06 17:14"))                       # failed, wait 5 min
        self.assertEqual(len(self.env.dispatched), 1)
        w.poll(self.env, T("2026-10-06 17:16"))                       # 5 min after the failure
        self.assertEqual(len(self.env.dispatched), 2)
        self.env.daily.append(run("2026-10-06 17:16", conclusion="cancelled"))
        w.poll(self.env, T("2026-10-06 17:25"))                       # 2nd failure: wait 10
        self.assertEqual(len(self.env.dispatched), 2)
        w.poll(self.env, T("2026-10-06 17:27"))
        self.assertEqual(len(self.env.dispatched), 3)
        self.assertEqual(self.env.alerts, [])
        w.poll(self.env, T("2026-10-06 17:56"))                       # 46 min: one alert
        w.poll(self.env, T("2026-10-06 17:58"))
        self.assertEqual(len(self.env.alerts), 1)

    def test_run_started_before_the_event_does_not_count(self):
        self.env.daily = [run("2026-10-06 17:00", "in_progress", None)]
        self.env.pages["argument_transcripts_rss.aspx?TYear=26"] += "<item><title>Anderson v. Intel (25-498)</title></item>"
        w.poll(self.env, T("2026-10-06 17:10"))
        self.assertEqual(self.env.dispatched, ["daily.yml"])          # queues behind it
        self.env.daily = [run("2026-10-06 17:00")]                     # the earlier one goes green
        w.poll(self.env, T("2026-10-06 17:20"))
        self.assertIn("transcripts:25-498", self.env.state["pending"])

    def test_failed_fetch_is_not_a_removal(self):
        self.env.pages["argument_transcripts_rss.aspx?TYear=26"] = RuntimeError("403")
        w.poll(self.env, T("2026-10-06 17:10"))
        src = self.env.state["sources"]["transcripts/26"]
        self.assertEqual(src["keys"], ["25-170"])
        self.assertEqual(src["fails"], 1)
        self.assertTrue(w.skipped(self.env.state, "transcripts/26", T("2026-10-06 17:11")))
        self.assertFalse(w.skipped(self.env.state, "transcripts/26", T("2026-10-06 17:12")))
        # Back, with a new item: caught up, one event.
        self.env.pages["argument_transcripts_rss.aspx?TYear=26"] = (
            "<item><title>Suncor (25-170)</title></item><item><title>Anderson (25-498)</title></item>")
        w.poll(self.env, T("2026-10-06 17:13"))
        self.assertEqual(list(self.env.state["pending"]), ["transcripts:25-498"])

    def test_unchanged_page_304_keeps_keys(self):
        self.env.pages["argument_transcripts_rss.aspx?TYear=26"] = ""   # what http_get returns on 304
        w.poll(self.env, T("2026-10-06 17:10"))
        self.assertEqual(self.env.state["sources"]["transcripts/26"]["keys"], ["25-170"])
        self.assertEqual(self.env.dispatched, [])

    def test_lease_held_skips(self):
        self.env.held = True
        self.assertEqual(w.poll(self.env, T("2026-10-06 17:10")), "busy")

    def test_off_minute_does_nothing(self):
        self.assertEqual(w.poll(self.env, T("2026-10-07 03:01")), "off-minute")


class Scheduled(unittest.TestCase):
    def test_skips_when_live(self):
        e = FakeEnv({})
        e.daily = [run("2026-10-06 16:30", "in_progress", None)]
        self.assertEqual(w.dispatch_scheduled(e, "daily.yml"), "skipped daily.yml")
        e.daily = [run("2026-10-06 16:30")]
        self.assertEqual(w.dispatch_scheduled(e, "daily.yml"), "dispatched daily.yml")


if __name__ == "__main__":
    unittest.main()
