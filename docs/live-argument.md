# Live argument audio

The landing page's **Live now** panel plays the Court's own live audio of an
oral argument, and appears only while the Court is streaming one. It sits at
the top of the panels, above the forecast.

## Pieces

| piece | where | what it does |
| --- | --- | --- |
| `live_argument_panel()` | `R/page_style.R` | writes the panel **hidden**, with one `<ol data-day="YYYY-MM-DD">` per argument day in the next 21 days (`arguments/calendar.json`), the cases in the Court's order, captions from `cases/search.json` |
| `day_calls_ahead()` | `R/argument_calendar.R`, called by the daily | the arguing counsel: `daycalls.json` plus every Day Call in the window the Court has posted, read again; **read-only** (the weekly run is the file's one writer) |
| `/live.js` | repo root, copied by `build_dashboards.R` beside `search.js` | decides, in the reader's browser, whether to show it |

Each row reads: the docket in the small label over "First" / "Second", the
caption (its case page), and the counsel in the order they rise --
"Kannon K. Shanmugam (pet.) · Sarah M. Harris (amicus) · Kevin K. Russell
(resp.)". The Court posts a day's Day Call the afternoon of the business day
before (Monday 5 October's: Friday 2 October, 1:01 p.m. ET), after that
week's weekly run, which is why the daily reads them itself; until one is
posted the counsel line is simply absent.
| CSS | `INDEX_CSS`, `.live` / `.lplay` | the pulsing dot is `--accent`; no new colour |

The page is rebuilt three times a day and cannot know at build time whether
the Court is sitting at the moment it is read, so the decision is the
script's. No argument day in the window: no panel and no script.

## What live.js does

1. Reads today's date **in Washington** (`Intl`, `America/New_York`). No list
   for today, or past 4 p.m. ET: it stops, having made no request.
2. From 9:30 a.m. to 4 p.m. ET it reads the stream's playlist once a minute
   (`cache: no-store`).
3. **Live** = the playlist answers 200 and is moving. On the first look, its
   newest `#EXT-X-PROGRAM-DATE-TIME` is under 120 s old; after that, its
   `#EXT-X-MEDIA-SEQUENCE` has advanced since the last look (which does not
   depend on the reader's clock). A 404, `#EXT-X-ENDLIST`, or a playlist that
   has stopped moving hides the panel -- unless the reader is listening, in
   which case the player is left to finish.
4. **Listen live** loads hls.js 1.6.16 (`hls.light.min.js`, cdnjs, with its SRI
   hash) on the click, never before. hls.js wherever the browser has Media
   Source (desktop browsers; iOS 17.1+ through ManagedMediaSource), as the
   Court's own player does (`overrideNative`); the browser's native HLS where
   there is none, and as the fallback if hls.js fails.

## The stream

From the Court's live page, `supremecourt.gov/oral_arguments/live.aspx`, which
plays it with Video.js and lists two sources:

- `https://scotus_stream.akamaized.net/hls/live/2032703/oa_staging/master.m3u8` (primary)
- `https://scotus_stream.akamaized.net/hls/live/2032703-b/oa_staging/master.m3u8` (backup; 404 while unused)

Measured 2026-10-05, during the first argument of OT2026: a media playlist
(not a master), 6 s MPEG-TS segments of about 2.4 MB (the stream carries a
picture as well as the audio), each dated with `PROGRAM-DATE-TIME`, served
with `Access-Control-Allow-Origin: *` and `Cache-Control: no-store` -- so the
landing page can read and play it with no proxy.

**It is undocumented.** "oa_staging" is not a promise. If the Court moves it,
the panel never appears and nothing else on the page changes; update `STREAMS`
in `live.js` from the live page's script.

## Testing

The automation browser runs its tab hidden, and Chrome will not open media in
a hidden tab (even a bare `MediaSource` never reaches `sourceopen`), so
playback has to be checked by hand in a visible window. Detection can be
checked anywhere: on an argument day, `document.getElementById('live-arg').hidden`
is `false` while the Court sits.
