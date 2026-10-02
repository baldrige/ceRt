# R/argument_reader.R -- one page per oral argument: listen, read along, and see
# how the bench divided its questioning.
#
#   arguments/{yyyy}/{dkt}.html   the page: the post-argument lean, each
#                                 Justice's questioning and vote, the Term chart,
#                                 and a player over the transcript
#   arguments/{yyyy}/{dkt}.json   the parsed transcript (R/argument_transcript.R),
#                                 which arguments/reader.js loads into the page
#   arguments/reader.js           the player and transcript, shared by every page
#   arguments/readers.json        {dkt: href} -- which dockets have a page, for
#                                 the Navigator and the case pages to link
#
# The lean, the bench table and the chart are written into the HTML, so the page
# reads complete without script; only the transcript and player need it. The
# audio is the Court's own MP3, played from supremecourt.gov. Each line's start
# time comes from align-arguments.yml, which matches the recording to the
# transcript (.github/scripts/align_arguments.py) and writes it into the JSON;
# until an argument is aligned, the times are an even-rate estimate, marked as
# such on the page (docs/argument-transcripts.md).

suppressPackageStartupMessages({ library(stringr); library(dplyr); library(tibble); library(purrr); library(htmltools) })
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

READERS_FILE <- "readers.json"
# Bump to rewrite every reader page after a markup change; stamped into each.
# r2: each argument plays its own recording (a reargument's "_2" MP3, an
#     original action's "-Orig" file), and the earlier argument of a reargued
#     docket plays rather than linking out.
READER_TEMPLATE_VERSION <- "r2"
READER_ORDER <- c("Roberts", "Kennedy", "Thomas", "Ginsburg", "Breyer", "Alito", "Sotomayor",
                  "Kagan", "Gorsuch", "Kavanaugh", "Barrett", "Jackson")

reader_href <- function(term, dkt) paste0("/arguments/", term, "/", dkt, ".html")
# The Court's MP3 for a docket's nth argument: "141-Orig" for the original
# action the API calls 22O141, and "_2" for a docket's second argument
# (24-109_2.mp3 is Louisiana v. Callais reargued in OT2025; 24-109.mp3 is OT2024).
# Kept in step with mp3_url() in .github/scripts/align_arguments.py.
argument_mp3 <- function(dkt, nth = 1L) {
  m <- regmatches(dkt, regexec("^[0-9]{2}O([0-9]+)$", dkt))[[1]]
  stem <- if (length(m) == 2) paste0(m[2], "-Orig") else dkt
  paste0("https://www.supremecourt.gov/media/audio/mp3files/", stem, if (nth > 1) paste0("_", nth) else "", ".mp3")
}

.rd_pct <- function(p) if (is.na(p)) "—" else paste0(round(100 * p), "%")
`%|na|%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
.rd_esc <- function(x) htmlEscape(x %||% "")
.rd_side_word <- function(s) ifelse(is.na(s), "neither side", ifelse(s == "pet", "the petitioners", "the respondents"))
# "for neither side" is an amicus's; a party whose side could not be placed
# (R/argument_sides.R) is named without one.
.rd_side_phrase <- function(s, amicus) ifelse(is.na(s) & !amicus, "", paste0(" for ", .rd_side_word(s)))

READER_CSS <- fill_palette("
  .wrap.rd{max-width:54rem}
  .rd .meta{font-size:.92rem;color:var(--ink-soft);display:flex;flex-wrap:wrap;gap:.2rem 1.2rem;margin:.2rem 0 0}
  .rd .meta b{color:var(--ink);font-weight:600}
  .rd section{margin-top:2.4rem}
  .rd .sh{display:flex;justify-content:space-between;align-items:baseline;gap:.4rem 1rem;flex-wrap:wrap;
    border-bottom:1px solid var(--rule);padding-bottom:.35rem;margin-bottom:1rem}
  .rd .sh h2{font-family:'Fraunces',Georgia,serif;font-weight:500;font-size:1.35rem;margin:0;color:var(--ink)}
  .rd .sh p{margin:0;font-size:.82rem;color:var(--faint);font-style:italic}
  .rd .note{font-size:.95rem;color:var(--ink-soft);max-width:42rem}
  .rd .fine{font-size:.82rem;color:var(--faint)}
  .rd .lean{display:grid;grid-template-columns:minmax(0,14rem) minmax(0,1fr);gap:1.2rem 2rem;align-items:start}
  @media (max-width:640px){.rd .lean{grid-template-columns:1fr}}
  .rd .big{font-family:'Fraunces',Georgia,serif;font-weight:500;font-size:3.6rem;line-height:1;
    font-variant-numeric:tabular-nums lining-nums;color:var(--ink)}
  .rd .big small{font-size:1.3rem;color:var(--faint);margin-left:.15rem}
  .rd .bigcap{font-size:.9rem;color:var(--ink-soft);margin-top:.45rem;line-height:1.4}
  .rd .chip{display:inline-block;font-size:.78rem;font-weight:600;padding:.05rem .45rem;border-radius:2px;border:1px solid currentColor;white-space:nowrap}
  .rd .chip.pet{color:@side-pet@} .rd .chip.resp{color:@side-resp@} .rd .chip.wait{color:var(--faint)}
  .rd .outcome{margin-top:.8rem;font-size:.88rem;color:var(--ink-soft);line-height:1.5}
  .rd .scale{position:relative;height:2.7rem;margin:.6rem 0 1.2rem}
  .rd .scale .track{position:absolute;left:0;right:0;top:1.05rem;height:.5rem;border-radius:999px;
    background:linear-gradient(90deg,@side-resp-tint@,var(--panel) 50%,@side-pet-tint@);border:1px solid var(--rule)}
  .rd .scale .mk{position:absolute;width:2px;transform:translateX(-1px)}
  .rd .scale .mk.base{top:.55rem;height:1.5rem;background:var(--faint)}
  .rd .scale .mk.fc{top:.35rem;height:1.9rem;width:3px;background:var(--accent)}
  .rd .scale .lb{position:absolute;transform:translateX(-50%);font-size:.74rem;color:var(--faint);white-space:nowrap}
  .rd .scale .lb.base{top:2.15rem} .rd .scale .lb.fc{top:-.75rem;color:var(--accent);font-weight:600}
  .rd .scale .end{position:absolute;top:2.15rem;font-size:.74rem;color:var(--faint)}
  .rd .split{display:flex;height:1.55rem;border-radius:2px;overflow:hidden;margin:.3rem 0 .35rem}
  .rd .split div{display:flex;align-items:center;padding:0 .5rem;font-size:.78rem;font-weight:600;color:var(--paper);white-space:nowrap;overflow:hidden}
  .rd .split .p{background:@side-pet@} .rd .split .r{background:@side-resp@;justify-content:flex-end}
  .rd .leg{display:flex;justify-content:space-between;gap:1rem;font-size:.8rem;color:var(--ink-soft)}
  .rd .leg .p{color:@side-pet@;font-weight:600} .rd .leg .r{color:@side-resp@;font-weight:600}
  .rd .tw{overflow-x:auto}
  .rd table.bench{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums lining-nums;min-width:32rem}
  .rd .bench th{font-size:.7rem;letter-spacing:.12em;text-transform:uppercase;color:var(--faint);font-weight:600;
    text-align:left;padding:.3rem .4rem;border-bottom:1px solid var(--ink)}
  .rd .bench th.n,.rd .bench td.n{text-align:right}
  .rd .bench td{padding:.42rem .4rem;border-bottom:1px solid var(--rule);vertical-align:middle;font-size:.95rem}
  .rd .bench td.nm{font-family:'Fraunces',Georgia,serif;font-weight:500;white-space:nowrap}
  .rd .bench tbody tr{cursor:pointer} .rd .bench tbody tr:hover td,.rd .bench tr.sel td{background:var(--stripe)}
  .rd .dv{display:grid;grid-template-columns:1fr 1px 1fr;align-items:center;min-width:11rem}
  .rd .dv .l{display:flex;justify-content:flex-end} .rd .dv .ax{height:1.25rem;background:var(--ink-soft)}
  .rd .dv .b{height:.68rem} .rd .dv .l .b{background:@side-pet@} .rd .dv .r .b{background:@side-resp@}
  .rd .dl{font-size:.75rem;color:var(--faint);margin-left:.25rem}
  .rd .vt{white-space:nowrap;font-size:.88rem} .rd .ok{color:@c-dgreen@;font-weight:700} .rd .no{color:var(--accent);font-weight:700}
  .rd .player{display:flex;align-items:center;gap:.9rem;flex-wrap:wrap;background:var(--panel);border:1px solid var(--rule);
    border-radius:4px;padding:.65rem .85rem;position:sticky;top:0;z-index:3}
  .rd .play{width:2.5rem;height:2.5rem;border-radius:50%;border:0;background:var(--accent);color:var(--paper);cursor:pointer;display:grid;place-items:center;flex:none}
  .rd .play svg{width:1rem;height:1rem;fill:currentColor} .rd .play:disabled{background:var(--rule);cursor:not-allowed}
  .rd .pt{flex:1 1 14rem;min-width:0}
  .rd .pt .now{font-size:.9rem;color:var(--ink);white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
  .rd .pt .sub{font-size:.76rem;color:var(--faint)}
  .rd .prog{height:4px;background:var(--rule);border-radius:2px;margin-top:.35rem;overflow:hidden;cursor:pointer}
  .rd .prog div{height:100%;width:0;background:var(--accent)}
  .rd .clock{font-size:.85rem;color:var(--ink-soft);font-variant-numeric:tabular-nums lining-nums}
  .rd .flt{display:flex;gap:.35rem;flex-wrap:wrap;align-items:center;margin:.9rem 0 .3rem}
  .rd .flt button{font:inherit;font-size:.8rem;background:none;border:1px solid var(--rule);border-radius:999px;padding:.15rem .6rem;cursor:pointer;color:var(--ink-soft)}
  .rd .flt button[aria-pressed=true]{background:var(--ink);border-color:var(--ink);color:var(--paper)}
  .rd .tx{margin-top:.6rem;max-height:36rem;overflow-y:auto;border-top:1px solid var(--rule);padding-right:.3rem}
  .rd .seg{font-size:.72rem;letter-spacing:.12em;text-transform:uppercase;padding:1rem 0 .4rem;border-bottom:1px solid var(--rule);
    margin-bottom:.35rem;display:flex;gap:.6rem;flex-wrap:wrap;align-items:baseline;position:sticky;top:0;background:var(--paper);z-index:1;color:var(--faint)}
  .rd .seg b{color:var(--ink)} .rd .seg .pet{color:@side-pet@} .rd .seg .resp{color:@side-resp@}
  .rd .turn{display:grid;grid-template-columns:8.5rem minmax(0,1fr);gap:.8rem;padding:.35rem .4rem;border-radius:3px;cursor:pointer}
  @media (max-width:560px){.rd .turn{grid-template-columns:1fr;gap:.05rem}}
  .rd .turn:hover{background:var(--stripe)}
  .rd .turn .who{font-size:.78rem;font-weight:600;color:var(--ink-soft);padding-top:.15rem}
  .rd .turn.j .who{color:var(--accent)}
  .rd .turn .who span{display:block;font-weight:400;color:var(--faint);font-size:.72rem}
  .rd .turn p{margin:0;max-width:40rem;font-size:1rem;line-height:1.55}
  .rd .turn.now{background:var(--stripe);box-shadow:inset 3px 0 0 var(--accent)}
  .rd .turn.dim{opacity:.32}
  .rd .chart svg{width:100%;height:auto;display:block}
  .rd .chart text{font-family:'Newsreader',Georgia,serif;font-size:12px;fill:var(--faint)}
  .rd .cal{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:1rem;margin-top:.8rem}
  @media (max-width:560px){.rd .cal{grid-template-columns:1fr}}
  .rd .cal div{border-top:1px solid var(--rule);padding-top:.45rem}
  .rd .cal .v{font-family:'Fraunces',Georgia,serif;font-size:1.45rem;font-variant-numeric:tabular-nums lining-nums}
  .rd .cal .k{font-size:.82rem;color:var(--faint);line-height:1.35}
  .rd .about{margin-top:2.6rem;border-top:1px solid var(--rule);padding-top:.8rem;font-size:.92rem;color:var(--ink-soft)}
  .rd .about li{margin:.3rem 0}
  @media (prefers-reduced-motion: reduce){.rd .tx{scroll-behavior:auto}}
")

# ---- page pieces -----------------------------------------------------------------

.rd_lean_html <- function(a, base) {
  if (is.na(a$p)) return(paste0(
    "<p class='note'>The transcript could not be divided between the two sides — an advocate whose ",
    "side neither the transcript nor the case captions settle, or a header the parser could not read — ",
    "so there is no lean for this argument.</p>"))
  s <- a$sides
  tot <- s$w_pet + s$w_resp; sp <- 100 * s$w_pet / tot
  ratio <- s$w_pet / max(s$w_resp, 1)
  heavier <- if (ratio >= 1) "petitioner" else "respondent"; r <- if (ratio >= 1) ratio else 1 / ratio
  dir <- if (a$p < base - 0.03) "below" else if (a$p > base + 0.03) "above" else "close to"
  outcome <- if (identical(a$disp, "orig"))
               "<span class='chip wait'>Original action</span><br>A case between States, decided on exceptions to a Special Master’s report rather than for a petitioner or a respondent, so the lean is not scored against a result."
             else if (identical(a$disp, "dismissed")) paste0("<span class='chip wait'>Dismissed</span> ",
                  .rd_esc(str_trunc(str_remove(a$judgment %||% "", "\\s*(Opinion|Justice|[A-Z][a-z]+, (C\\. )?J\\.).*$"), 90)),
                  "<br>Disposed of without deciding who wins, so there is nothing to score the lean against.")
             else if (is.na(a$pw)) "<span class='chip wait'>Awaiting decision</span>"
             else paste0("<span class='chip ", if (a$pw == 1) "pet'>Petitioner won" else "resp'>Respondent won",
                         "</span> ", .rd_esc(str_trunc(str_remove(a$judgment %||% "", "\\s*\\(.*$"), 90)))
  verdict <- if (is.na(a$pw)) "" else if ((a$p >= 0.5) == (a$pw == 1)) " The lean pointed the right way." else " Here the lean pointed the wrong way."
  paste0(
    "<div class='lean'><div>",
    "<div class='big'>", round(100 * a$p), "<small>%</small></div>",
    "<div class='bigcap'>chance the <b>petitioner</b> prevails, from how the bench divided its words</div>",
    "<div class='outcome'>", outcome, "</div></div><div>",
    "<div class='scale' aria-hidden='true'><div class='track'></div>",
    "<span class='end' style='left:0'>0%</span><span class='end' style='right:0'>100%</span>",
    sprintf("<div class='mk base' style='left:%.1f%%'></div><span class='lb base' style='left:%.1f%%'>usual %s</span>", 100 * base, 100 * base, .rd_pct(base)),
    sprintf("<div class='mk fc' style='left:%.1f%%'></div><span class='lb fc' style='left:%.1f%%'>%s</span>", 100 * a$p, 100 * a$p, .rd_pct(a$p)),
    "</div><div class='fine'>The Justices’ words, by the side they were spoken to</div>",
    sprintf("<div class='split'><div class='p' style='width:%.1f%%'>%s</div><div class='r' style='width:%.1f%%'>%s</div></div>",
            sp, .rd_pct(s$w_pet / tot), 100 - sp, .rd_pct(s$w_resp / tot)),
    sprintf("<div class='leg'><span><span class='p'>To the petitioner</span> · %s words, %d turns</span><span>%d turns, %s words · <span class='r'>to the respondent</span></span></div>",
            format(s$w_pet, big.mark = ","), as.integer(s$q_pet), as.integer(s$q_resp), format(s$w_resp, big.mark = ",")),
    sprintf("<p class='note' style='margin:.9rem 0 0'>The bench spoke %.1f× as many words to the %s as to the other side. Since OT2017 the side that draws more of the bench’s words has tended to lose, so this argument reads %s the usual %s petitioner win rate.%s</p>",
            r, heavier, dir, .rd_pct(base), verdict),
    "</div></div>")
}

.rd_bench_html <- function(jl, votes) {
  if (is.null(jl) || !nrow(jl)) return("")
  jl <- jl |> arrange(match(name, READER_ORDER))
  mx <- max(c(jl$words_pet, jl$words_resp), 1)
  has_votes <- !is.null(votes) && nrow(votes) > 0
  rows <- vapply(seq_len(nrow(jl)), function(i) {
    j <- jl[i, ]
    d <- round(100 * (j$p - j$p0))
    v <- if (has_votes) votes$voted_pet[match(j$name, votes$name)] else NA
    vote <- if (!has_votes) "" else if (is.na(v)) "<td class='vt'>—</td>" else {
      hit <- (j$p >= 0.5) == (v == 1)
      sprintf("<td class='vt'>%s <span class='%s' title='%s'>%s</span></td>", if (v == 1) "Petitioner" else "Respondent",
              if (hit) "ok" else "no", if (hit) "the lean matched" else "the lean missed", if (hit) "✓" else "✗")
    }
    sprintf(paste0("<tr data-j='%s'><td class='nm'>%s</td><td><div class='dv' title='%s words to the petitioner, %s to the respondent'>",
                   "<div class='l'><div class='b' style='width:%.1f%%'></div></div><div class='ax'></div>",
                   "<div class='r'><div class='b' style='width:%.1f%%'></div></div></div></td>",
                   "<td class='n'>%s<span class='dl'>%s</span></td>%s</tr>"),
            .rd_esc(j$name), .rd_esc(if (j$name == "Roberts") "Roberts, C.J." else j$name),
            format(j$words_pet, big.mark = ","), format(j$words_resp, big.mark = ","),
            100 * j$words_pet / mx, 100 * j$words_resp / mx,
            .rd_pct(j$p), if (j$silent) "silent" else sprintf("%+d", d), vote)
  }, character(1))
  paste0("<div class='tw'><table class='bench'><thead><tr><th>Justice</th><th>Words to the petitioner ◂ │ ▸ to the respondent</th>",
         "<th class='n'>Lean to petitioner</th>", if (has_votes) "<th>Voted for</th>" else "", "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table></div>",
         "<p class='fine' style='margin-top:.55rem'>A Justice’s lean is the chance they vote for the petitioner, from their own word split and the whole bench’s; ",
         "the small figure is the change from their usual petitioner rate. Click a row to pick out that Justice in the transcript.</p>")
}

# The Term, forecast against result: one dot per argument with a lean, in three
# rows (petitioner won / respondent won / awaiting decision), each a link to
# its own page. The current case is drawn larger, in the accent.
.rd_chart_html <- function(pts, current, base) {
  pts <- pts[!is.na(pts$p), , drop = FALSE]
  if (nrow(pts) < 3) return("")
  W <- 760; L <- 30; R <- 24; top <- 34; rowH <- 64
  # Rows by disposition. "Dismissed" is its own row: a DIG is disposed of, with
  # no winner, and filing it under "awaiting decision" said the Court had not
  # acted on it.
  rows <- list(list(v = "pet", lab = "Petitioner won"), list(v = "resp", lab = "Respondent won"),
               list(v = "dismissed", lab = "Dismissed, no winner"), list(v = NA, lab = "Awaiting decision"))
  in_row <- function(r) if (is.na(r$v)) is.na(pts$disp) else pts$disp %in% r$v
  rows <- Filter(function(r) any(in_row(r)), rows)
  H <- top + length(rows) * rowH + 22
  x <- function(p) L + p * (W - L - R)
  s <- sprintf("<svg viewBox='0 0 %d %d' role='img' aria-label='This Term’s arguments by lean and result'>", W, H)
  for (t in c(0, .25, .5, .75, 1)) s <- paste0(s, sprintf(
    "<line x1='%.1f' x2='%.1f' y1='%d' y2='%d' stroke='var(--rule)'%s/><text x='%.1f' y='%d' text-anchor='middle'>%d%%</text>",
    x(t), x(t), top, top + length(rows) * rowH, if (t == .5) "" else " stroke-dasharray='2 3'", x(t), H - 4, as.integer(100 * t)))
  s <- paste0(s, sprintf("<line x1='%.1f' x2='%.1f' y1='%d' y2='%d' stroke='var(--faint)' stroke-width='1.5'/><text x='%.1f' y='%d' text-anchor='middle'>usual %s</text>",
                         x(base), x(base), top - 6, top + length(rows) * rowH, x(base), top - 11, .rd_pct(base)))
  for (k in seq_along(rows)) {
    r <- rows[[k]]; y0 <- top + (k - 1) * rowH + 34
    s <- paste0(s, sprintf("<text x='%d' y='%d' style='fill:var(--ink-soft);font-weight:600'>%s</text>", L, y0 - 18, r$lab))
    d <- pts[in_row(r), , drop = FALSE]
    d <- d[order(d$p), , drop = FALSE]
    placed_x <- numeric(); placed_k <- integer()
    col <- if (is.na(r$v) || r$v == "dismissed") "var(--faint)" else if (r$v == "pet") pal("side-pet") else pal("side-resp")
    for (i in seq_len(nrow(d))) {
      cx <- x(d$p[i]); kk <- 0L
      while (any(abs(placed_x - cx) < 10 & placed_k == kk)) kk <- kk + 1L
      placed_x <- c(placed_x, cx); placed_k <- c(placed_k, kk)
      cy <- y0 + ceiling(kk / 2) * 10 * (if (kk %% 2) -1 else 1)
      sel <- identical(d$dkt[i], current)
      s <- paste0(s, sprintf("<a href='%s'><circle cx='%.1f' cy='%.1f' r='%s' fill='%s' fill-opacity='%s'%s><title>%s: lean %s%s</title></circle></a>",
        d$href[i], cx, cy, if (sel) "7" else "4.5", if (sel) "var(--accent)" else col, if (sel) "1" else ".78",
        if (sel) " stroke='var(--paper)' stroke-width='2'" else "",
        .rd_esc(d$label[i]), .rd_pct(d$p[i]),
        switch(d$disp[i] %|na|% "none", pet = ", petitioner won", resp = ", respondent won",
               dismissed = ", dismissed", ", awaiting decision")))
    }
  }
  s <- paste0(s, "</svg>")
  dd <- pts[!is.na(pts$pw), , drop = FALSE]
  cal <- if (nrow(dd) >= 5) {
    hi <- dd[dd$p >= .75, ]; lo <- dd[dd$p < .5, ]
    right <- sum((dd$p >= .5) == (dd$pw == 1))
    paste0("<div class='cal'>",
      sprintf("<div><div class='v'>%d of %d</div><div class='k'>leaned 75%% or more to the petitioner, and the petitioner won</div></div>", sum(hi$pw == 1), nrow(hi)),
      sprintf("<div><div class='v'>%d of %d</div><div class='k'>leaned toward the respondent, and the respondent won</div></div>", sum(lo$pw == 0), nrow(lo)),
      sprintf("<div><div class='v'>%d of %d</div><div class='k'>decided cases where the lean pointed the right way (always picking the petitioner: %d)</div></div>", right, nrow(dd), sum(dd$pw == 1)),
      "</div>")
  } else ""
  paste0("<div class='chart'>", s, "</div>", cal)
}

.rd_about_html <- function(bt, timed, aligned = FALSE) {
  pc <- function(x) paste0(round(100 * x, 1), "%")
  paste0("<div class='about'><b>About this page.</b> Built from the Court’s own transcript and recording.<ul>",
    sprintf("<li><b>The lean.</b> Backtested leave-one-Term-out on %d decided arguments (%s): %s of outcomes called right against %s for “the petitioner always wins”, with forecast error (Brier score) %.3f against %.3f. It refines a strong base rate more than it flips calls. Each Justice’s lean: %s of %s votes against %s for their usual rate.</li>",
            bt$case_n, bt$terms, pc(bt$case_model_acc), pc(bt$case_base_acc), bt$case_model_brier, bt$case_base_brier,
            pc(bt$justice_model_acc), format(bt$justice_n, big.mark = ","), pc(bt$justice_base_acc)),
    "<li><b>Words, not questions.</b> Every Justice turn in an advocate’s time counts toward that advocate’s side; amicus time counts for neither. Counting turns adds nothing once words are counted, and the raw rule “more questions loses” does worse than the base rate, because petitioners argue first and have rebuttal.</li>",
    if (timed && aligned) "<li><b>Timing.</b> Each line is timed to the recording: speech recognition matched the Court’s audio to its transcript, and its own text is used for nothing but the clock. Click a line to hear it.</li>"
    else if (timed) "<li><b>Timing.</b> Line times spread the recording evenly over the words until this argument is matched to its audio: close near the start, further out later in a long argument. Click a line to jump near it.</li>" else "",
    "<li>Method and findings: <a href='https://github.com/baldrige/ceRt/blob/main/docs/argument-transcripts.md'>docs/argument-transcripts.md</a>.</li></ul></div>")
}

# ---- the page --------------------------------------------------------------------

render_argument_reader <- function(site_dir, a, pts, model) {
  base <- model$base_rate %||% 0.71
  term <- a$term; dkt <- a$dkt
  # The feed's posting date is the argument's (transcripts go up the same day);
  # one OT2017 item carries none, and its page simply names the Term instead.
  pd <- suppressWarnings(as.Date(if (nzchar(a$posted %||% "")) a$posted else NA_character_))
  when <- if (is.na(pd)) paste0("October Term ", term) else str_squish(format(pd, "%B %e, %Y"))
  title <- paste0(a$short, " — Oral argument, ", when)
  segs <- a$tx$segments
  adv <- segs[!segs$rebuttal, , drop = FALSE]
  meta <- paste0("<div class='meta'>", paste(sprintf("<span><b>%s</b>%s%s</span>", .rd_esc(adv$advocate),
                 .rd_side_phrase(adv$side, adv$amicus), ifelse(adv$amicus, " (amicus)", "")), collapse = ""), "</div>")
  has_audio <- isTRUE(a$audio)
  # Aligned by align-arguments.yml (each turn carries its start time); the page
  # script reads the same flag from the JSON, so this only sets the first paint.
  aligned <- isTRUE(a$tx$meta$align$ok)
  player <- paste0(
    "<div class='player'><button class='play' id='rd-play' aria-label='Play'", if (has_audio) "" else " disabled",
    "><svg viewBox='0 0 16 16' aria-hidden='true'><path id='rd-icon' d='M3 1.5v13l11-6.5z'/></svg></button>",
    "<div class='pt'><div class='now' id='rd-now'>", if (has_audio) "Press play, or click any line of the transcript to start there" else "Transcript only — this argument’s recording is on the Court’s site", "</div>",
    "<div class='sub' id='rd-sub'>", if (has_audio && aligned) "The Court’s recording · each line timed to the audio"
      else if (has_audio) "The Court’s recording · line times estimated" else
      sprintf("<a href='https://www.supremecourt.gov/oral_arguments/audio/%d/%s' rel='noopener'>Listen on supremecourt.gov</a>", term, dkt), "</div>",
    "<div class='prog' id='rd-prog'><div></div></div></div><span class='clock' id='rd-clock'>0:00</span>",
    if (has_audio) sprintf("<audio id='rd-audio' preload='metadata' src='%s'></audio>", a$mp3 %||% argument_mp3(dkt)) else "",
    "</div>")
  crumb <- list(href = "/arguments/", label = "Arguments")
  dek <- paste0("The argument in No. ", dkt, ", ", when,
                ": the Court’s recording and transcript, and how the bench divided its questioning.")
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head(paste0(title, " — Supreme Court Report"), site_breadcrumb_jsonld(a$short, crumb),
              extra_css = READER_CSS, description = dek, path = reader_href(term, dkt), og_type = "article",
              extra_head = paste0("<meta name='rtv' content='", READER_TEMPLATE_VERSION, "'>")),
    "<body>", site_masthead(active = "/arguments/"),
    "<main class='wrap rd' id='main' data-dkt='", dkt, "' data-json='", dkt, ".json'>",
    site_breadcrumb(a$short, crumb),
    "<p class='kicker'>No. ", dkt, " · argued ", when, "</p>",
    "<h1 style='font-size:clamp(1.7rem,4.6vw,2.4rem);line-height:1.12'>", .rd_esc(a$caption), "</h1>", meta,
    "<section><div class='sh'><h2>Post-argument lean</h2><p>",
    if (is.na(a$disp)) "A forecast from the argument alone" else "From the argument alone, before the decision", "</p></div>",
    .rd_lean_html(a, base), "</section>",
    if (!is.null(a$jl) && nrow(a$jl)) paste0("<section><div class='sh'><h2>Each Justice’s questioning</h2><p>",
      if (is.null(a$votes)) "Votes appear once the case is decided" else "Lean beside the vote each Justice cast", "</p></div>",
      .rd_bench_html(a$jl, a$votes), "</section>") else "",
    "<section id='listen'><div class='sh'><h2>Listen and read</h2><p id='rd-count'>", nrow(a$tx$turns), " turns</p></div>",
    player, "<div class='flt' id='rd-flt'></div>",
    "<div class='tx' id='rd-tx' tabindex='0' aria-label='Transcript'><p class='fine'>Loading the transcript…</p></div>",
    sprintf("<p class='fine' style='margin-top:.5rem'>From the Court’s <a href='%s' rel='noopener'>official transcript</a>.</p>", a$url),
    "</section>",
    "<section><div class='sh'><h2>October Term ", term, ", lean against result</h2><p>Each dot an argument; click one to open it</p></div>",
    .rd_chart_html(pts, dkt, base), "</section>",
    .rd_about_html(model$backtest, has_audio, aligned),
    "<p class='back'><a href='/arguments/arg_", term, ".html'>&larr; October Term ", term, " arguments</a> · ",
    "<a href='/cases/", dkt, ".html'>The docket &rarr;</a></p>",
    "</main><script src='/arguments/reader.js' defer></script></body>\n</html>\n")
  out <- file.path(site_dir, "arguments", term, paste0(dkt, ".html"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(smarten_html(html)), out, useBytes = TRUE)
  invisible(out)
}

#' Every reader page the transcript index supports. `cases` is the combined
#' docket table (captions and judgments); lineups come from the Justices
#' section's cache. Returns one row per argument: dkt, term, href, p, pw.
render_argument_readers <- function(site_dir, cases, model = load_argument_lean(), fetch_max = 0L,
                                    sides_fetch_max = 60L) {
  idx <- read_transcript_index(site_dir)
  if (!length(idx) || is.null(model)) { message("render_argument_readers(): nothing to render"); return(invisible(tibble())) }
  writeLines(READER_JS, file.path(site_dir, "arguments", "reader.js"), useBytes = TRUE)
  lu_path <- file.path(site_dir, "justices", "lineups.json")
  lu <- if (file.exists(lu_path)) tryCatch(jsonlite::fromJSON(lu_path, simplifyVector = FALSE), error = function(e) list()) else list()
  alias <- unlist(unname(imap(lu, function(e, k) setNames(rep(k, length(unlist(e$also)) + 1L), c(k, unlist(e$also))))))
  alias <- alias[!duplicated(names(alias))]
  by_dkt <- split(seq_len(nrow(cases)), cases$dkt)
  # Which argument of its docket each one is, for the MP3 name (argument_mp3()).
  keys <- names(idx); dk <- vapply(idx, function(e) e$dkt, ""); tm <- vapply(idx, function(e) as.integer(e$term), 1L)
  nth_of <- function(d, t) match(t, sort(tm[dk == d]))

  # The judgment: cached in the index once known (a decision does not change),
  # else from this run's dockets, else fetched by name -- newest first, at most
  # fetch_max a run. The arguments run holds the current Terms' dockets but not
  # an OT2017 case docketed in 2015, and a decision can postdate its fetch.
  # (A posting date the feed lacked is stored as an empty value, not a string.)
  posted_of <- function(e) { x <- unlist(e$posted); if (length(x) && !is.na(x[1])) as.character(x[1]) else "" }
  keys <- keys[order(vapply(idx[keys], posted_of, ""), decreasing = TRUE)]
  fetched <- 0L; dirty <- FALSE
  for (k in keys) {
    e <- idx[[k]]
    # A judgment cached under older reading rules is read again (JUDGMENT_RULES).
    if (!is.null(e$judgment) && identical(e$judgment_v, JUDGMENT_RULES)) next
    i <- by_dkt[[e$dkt]][1]
    j <- if (!is.na(i %||% NA)) judgment_of(cases$events[[i]], application = is_application_docket(e$dkt)) else NA_character_
    if (is.na(j) && fetched < fetch_max) { j <- fetch_judgment(e$dkt); fetched <- fetched + 1L }
    if (!is.na(j)) { idx[[k]]$judgment <- j; idx[[k]]$judgment_v <- JUDGMENT_RULES; dirty <- TRUE }
    else if (!is.null(e$judgment)) { idx[[k]]$judgment <- NULL; idx[[k]]$judgment_v <- NULL; dirty <- TRUE }
  }
  # Which side each advocate argued for, where a header names no role --
  # "ON BEHALF OF THE FEDERAL PARTIES" (R/argument_sides.R). Resolved once from
  # the Court's party records, cached in the index under SIDES_RULES, and written
  # back into the transcript JSON when it changes anything, so the page's
  # segment labels ("for the petitioners") agree with the lean. An argument whose
  # sides cannot all be placed gets no lean.
  n_sides <- 0L; n_changed <- 0L
  for (k in keys) {
    e <- idx[[k]]
    if (identical(e$sides_v, SIDES_RULES)) next
    jp <- file.path(site_dir, "arguments", paste0(k, ".json"))
    raw <- tryCatch(jsonlite::fromJSON(jp, simplifyVector = FALSE), error = function(err) NULL)
    if (is.null(raw) || !length(raw$segments)) next
    sg <- tibble(header = map_chr(raw$segments, ~ .x$header %||% ""),
                 advocate = map_chr(raw$segments, ~ .x$advocate %||% ""),
                 rebuttal = map_lgl(raw$segments, ~ isTRUE(.x$rebuttal)),
                 amicus = map_lgl(raw$segments, ~ isTRUE(.x$amicus)))
    explicit <- vapply(sg$header, tx_header_side, "")
    recs <- list()
    if (any(is.na(explicit) & !sg$amicus)) {
      if (n_sides >= sides_fetch_max) next
      recs <- fetch_party_records(e$dkt); n_sides <- n_sides + 1L
      if (is.null(recs)) next
    }
    new <- resolve_argument_sides(sg, recs)
    old <- map_chr(raw$segments, ~ { s <- .x$side; if (is.null(s) || is.na(s)) NA_character_ else s })
    if (!identical(unname(old), unname(as.vector(new)))) {
      for (s in seq_along(raw$segments)) raw$segments[[s]]$side <- if (is.na(new[s])) NA else new[s]
      jsonlite::write_json(raw, jp, auto_unbox = TRUE, na = "null", digits = NA, null = "null")
      n_changed <- n_changed + 1L
    }
    idx[[k]]$sides_v <- SIDES_RULES; idx[[k]]$sides_complete <- isTRUE(attr(new, "complete"))
    dirty <- TRUE
  }
  if (n_changed) message("render_argument_readers(): sides corrected in ", n_changed, " transcript(s) (",
                         n_sides, " resolved from party records)")

  if (dirty) jsonlite::write_json(idx[order(names(idx))], file.path(site_dir, "arguments", TX_INDEX), auto_unbox = TRUE)
  if (fetched) message("render_argument_readers(): ", fetched, " judgment(s) fetched by name")

  args <- map(keys, function(k) {
    e <- idx[[k]]
    p <- read_transcript(site_dir, k); if (is.null(p)) return(NULL)
    i <- by_dkt[[e$dkt]][1]
    cap <- if (!is.na(i %||% NA)) cases$caption[i] else e$dkt
    jd <- if (identical(e$judgment_v, JUDGMENT_RULES)) e$judgment %||% NA_character_ else NA_character_
    pw <- petitioner_won(jd)
    # An original action (22O###) ends in exceptions sustained or overruled and a
    # decree -- no petitioner and respondent to score the lean against, so it is
    # neither "awaiting decision" nor a point on the Term chart.
    disp <- if (grepl("^\\d{2}O\\d+$", e$dkt)) "orig" else argument_disposition(jd)
    # No lean where the advocates' sides could not all be placed.
    sides <- if (isFALSE(e$sides_complete)) NULL else argument_sides(p)
    jl <- justice_leans(model, p, sides)
    votes <- NULL
    lk <- alias[intersect(unlist(e$dkts), names(alias))]
    if (!is.na(pw) && length(lk) && isTRUE(lu[[lk[[1]]]]$parsed) && exists("decision_votes")) {
      en <- lu[[lk[[1]]]]
      v <- tryCatch(decision_votes(en, term_court(as.integer(e$term) %% 100L), decided = en$decided), error = function(err) NULL)
      if (!is.null(v)) votes <- v |> filter(side %in% c("majority", "dissent")) |>
        transmute(name, voted_pet = if_else(side == "majority", pw, 1 - pw))
    }
    list(key = k, dkt = e$dkt, term = as.integer(e$term), posted = posted_of(e), url = e$url, tx = p,
         caption = cap, short = strip_caption_roles(cap), judgment = jd, pw = pw, disp = disp, sides = sides,
         p = case_lean(model, sides), jl = jl, votes = votes,
         audio = TRUE, mp3 = argument_mp3(e$dkt, nth_of(e$dkt, as.integer(e$term))))
  }) |> compact()

  pts_all <- tibble(dkt = map_chr(args, "dkt"), term = map_int(args, "term"),
                    p = map_dbl(args, "p"), pw = map_dbl(args, "pw"),
                    disp = map_chr(args, ~ .x$disp %||% NA_character_),
                    label = map_chr(args, ~ paste0(.x$short, " (No. ", .x$dkt, ")")),
                    href = map2_chr(map_int(args, "term"), map_chr(args, "dkt"), reader_href))
  for (a in args) tryCatch(render_argument_reader(site_dir, a, pts_all[pts_all$term == a$term, ], model),
                           error = function(err) message("reader page ", a$key, " failed: ", conditionMessage(err)))
  # dkt -> its latest argument's page, for the Navigator and the case pages.
  latest_href <- pts_all |> group_by(dkt) |> slice_max(term, n = 1, with_ties = FALSE) |> ungroup()
  jsonlite::write_json(as.list(setNames(latest_href$href, latest_href$dkt)),
                       file.path(site_dir, "arguments", READERS_FILE), auto_unbox = TRUE)
  message(sprintf("render_argument_readers(): %d page(s), %d with a lean", length(args), sum(!is.na(pts_all$p))))
  invisible(pts_all)
}

read_readers <- function(site_dir) {
  p <- file.path(site_dir, "arguments", READERS_FILE)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) list())
}

# ---- the shared script ---------------------------------------------------------

READER_JS <- r"---(// arguments/reader.js -- written by R/argument_reader.R. The transcript and
// player on an argument page: loads {dkt}.json beside the page, renders the
// turns by segment, plays the Court's recording, and follows along.
(function () {
  var main = document.getElementById('main'); if (!main) return;
  var tx = document.getElementById('rd-tx'), audio = document.getElementById('rd-audio');
  var now = document.getElementById('rd-now'), clock = document.getElementById('rd-clock');
  var prog = document.querySelector('#rd-prog div'), play = document.getElementById('rd-play');
  var icon = document.getElementById('rd-icon'), flt = document.getElementById('rd-flt');
  var turns = [], els = [], times = [], cur = -1, filterJ = null;
  function esc(s) { return String(s == null ? '' : s).replace(/[&<>"]/g, function (c) { return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]; }); }
  function mmss(s) { s = Math.max(0, Math.floor(s)); var h = Math.floor(s / 3600), m = Math.floor(s % 3600 / 60), ss = ('0' + s % 60).slice(-2); return h ? h + ':' + ('0' + m).slice(-2) + ':' + ss : m + ':' + ss; }
  function who(t) {
    if (t.r === 'j') return t.sp === 'Roberts' ? 'Chief Justice Roberts' : 'Justice ' + t.sp;
    return t.sp.replace(/^GENERAL /, 'General ').replace(/^(MR|MS|MRS|MISS)\. /, function (m, a) { return a.charAt(0) + a.slice(1).toLowerCase() + '. '; })
      .replace(/([A-Z])([A-Z'-]+)$/, function (m, a, b) { return a + b.toLowerCase(); });
  }
  function side(s, amicus) { return s === 'pet' ? "<span class='pet'>for the petitioners</span>" : s === 'resp' ? "<span class='resp'>for the respondents</span>" : amicus ? '<span>for neither side</span>' : ''; }
  var aligned = false;
  function setTimes() {
    if (aligned) return;
    if (!audio || !isFinite(audio.duration) || !audio.duration) return;
    var total = turns.reduce(function (a, t) { return a + t.w; }, 0) || 1, spw = audio.duration / total, acc = 0;
    times = turns.map(function (t) { var s = acc * spw; acc += t.w; return s; });
    els.forEach(function (el, i) { var sp = el.querySelector('.who span'); if (sp) sp.textContent = '≈ ' + mmss(times[i]); });
  }
  function setFilter(j) {
    filterJ = j;
    flt.querySelectorAll('button').forEach(function (b) { b.setAttribute('aria-pressed', String((b.dataset.j || null) === j)); });
    document.querySelectorAll('.bench tbody tr').forEach(function (tr) { tr.classList.toggle('sel', tr.dataset.j === j); });
    els.forEach(function (el) { el.classList.toggle('dim', !!j && el.dataset.sp !== j); });
    if (j) { var f = els.find(function (el) { return el.dataset.sp === j; }); if (f) tx.scrollTop = f.offsetTop - tx.offsetTop - 40; }
  }
  fetch(main.dataset.json).then(function (r) { if (!r.ok) throw new Error(r.status); return r.json(); }).then(function (d) {
    turns = d.turns; var segs = {}; (d.segments || []).forEach(function (s) { segs[s.segment] = s; });
    var html = '', last = null;
    turns.forEach(function (t, i) {
      if (t.s !== last) {
        last = t.s; var s = segs[t.s];
        html += s ? "<div class='seg'><b>" + (s.rebuttal ? 'Rebuttal' : 'Argument') + ' · ' + esc(s.advocate) + '</b>' + side(s.side, s.amicus) + '</div>'
                  : "<div class='seg'><b>Opening</b></div>";
      }
      html += "<div class='turn" + (t.r === 'j' ? ' j' : '') + "' data-i='" + i + "' data-sp='" + esc(t.sp) + "'><div class='who'>" + esc(who(t)) +
              (audio ? '<span></span>' : '') + '</div><p>' + esc(t.x) + '</p></div>';
    });
    tx.innerHTML = html; els = Array.prototype.slice.call(tx.querySelectorAll('.turn'));
    var nj = turns.filter(function (t) { return t.r === 'j'; }).length;
    document.getElementById('rd-count').textContent = turns.length + ' turns · ' + nj + ' from the bench';
    var js = []; turns.forEach(function (t) { if (t.r === 'j' && js.indexOf(t.sp) < 0) js.push(t.sp); });
    flt.innerHTML = "<span class='fine' style='margin-right:.2rem'>Pick out</span><button data-j='' aria-pressed='true'>Everyone</button>" +
      js.map(function (n) { return "<button data-j='" + esc(n) + "' aria-pressed='false'>" + esc(n) + '</button>'; }).join('');
    flt.querySelectorAll('button').forEach(function (b) { b.addEventListener('click', function () { setFilter(b.dataset.j || null); }); });
    els.forEach(function (el) { el.addEventListener('click', function () {
      if (!audio) return; if (!times.length) setTimes(); if (!times.length) return;
      audio.currentTime = times[+el.dataset.i]; audio.play().catch(function () {}); }); });
    // Aligned to the recording (.github/scripts/align_arguments.py): each line
    // carries its own start time. Otherwise the even-rate estimate, marked ≈.
    aligned = !!(d.align && d.align.ok) && turns.every(function (t) { return typeof t.t === 'number'; });
    if (aligned) {
      times = turns.map(function (t) { return t.t; });
      els.forEach(function (el, i) { var sp = el.querySelector('.who span'); if (sp) sp.textContent = mmss(times[i]); });
      var sub = document.getElementById('rd-sub'); if (sub && audio) sub.textContent = 'The Court’s recording · each line timed to the audio';
    }
    if (audio) { if (audio.readyState >= 1) setTimes(); audio.addEventListener('loadedmetadata', setTimes); }
  }).catch(function () { tx.innerHTML = "<p class='fine'>The transcript did not load. It is on the Court’s site, linked below.</p>"; });
  document.querySelectorAll('.bench tbody tr').forEach(function (tr) { tr.addEventListener('click', function () { setFilter(filterJ === tr.dataset.j ? null : tr.dataset.j); }); });
  if (!audio) return;
  play.addEventListener('click', function () { if (audio.paused) audio.play().catch(function () {}); else audio.pause(); });
  audio.addEventListener('play', function () { icon.setAttribute('d', 'M3 1.5h3.5v13H3zM9.5 1.5H13v13H9.5z'); play.setAttribute('aria-label', 'Pause'); });
  audio.addEventListener('pause', function () { icon.setAttribute('d', 'M3 1.5v13l11-6.5z'); play.setAttribute('aria-label', 'Play'); });
  audio.addEventListener('error', function () { now.innerHTML = "The recording did not load — <a href='https://www.supremecourt.gov/oral_arguments/audio/'>listen on the Court’s site</a>"; play.disabled = true; });
  document.getElementById('rd-prog').addEventListener('click', function (e) {
    if (!audio.duration) return; var r = this.getBoundingClientRect(); audio.currentTime = audio.duration * (e.clientX - r.left) / r.width; });
  audio.addEventListener('timeupdate', function () {
    var t = audio.currentTime; clock.textContent = mmss(t);
    if (audio.duration) prog.style.width = (100 * t / audio.duration) + '%';
    if (!times.length) return;
    var i = 0; while (i + 1 < times.length && times[i + 1] <= t) i++;
    if (i === cur) return;
    if (els[cur]) els[cur].classList.remove('now');
    cur = i; var el = els[i]; if (!el) return; el.classList.add('now');
    now.textContent = who(turns[i]) + ' — ' + turns[i].x.slice(0, 90) + (turns[i].x.length > 90 ? '…' : '');
    var top = el.offsetTop - tx.offsetTop;
    if (!filterJ && (top < tx.scrollTop + 30 || top > tx.scrollTop + tx.clientHeight - 80)) tx.scrollTop = top - 60;
  });
})();
)---"
