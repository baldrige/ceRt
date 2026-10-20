# opinion_watch.R ----------------------------------------------------------------
# Opinion Watch (/justices/opinion-watch.html): the argued cases still awaiting decision, how long
# each has waited against the Court's own history, and -- from the convention
# that the Chief spreads each sitting's majority opinions evenly -- which Justices
# are likely writing them.
#
# One source: the Granted & Noted list (arguments/granted_noted.json,
# R/granted_noted.R), which carries argued, decided and author for every argued
# case from OT2016 on.
#
# MEASURED (OT2016-OT2025, 567 signed majority opinions): when an opinion issued,
# its author was among the Justices with the FEWEST majority opinions from that
# sitting so far 93% of the time (526/567), against 63% for a random pick from
# the same candidate set; with one case left in the sitting, 83% against 30%,
# from 2.7 candidates. Misses are the Court's own exceptions (a recusal, a
# reassignment after a lost majority, a sitting of ten cases where someone writes
# two), so the page names candidates, never an author.

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(htmltools) })
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

OW_SITTING_GAP <- 10L   # days: a longer gap between argument days starts a new sitting
OW_FILE <- "opinion-watch.html"   # under JUSTICES_DIR

# One row per DECISION from the list: consolidated cases share a group and are
# decided by one opinion. Reargued cases count from their last argument.
ow_decisions <- function(gn) {
  gn |>
    mutate(across(any_of(c("argued", "decided")), as.Date)) |>
    filter(!is.na(argued)) |>
    mutate(key = if_else(is.na(group), paste(term, dkt), paste(term, group))) |>
    group_by(key) |>
    summarise(term = first(term), dkts = list(dkt), dkt = first(dkt), caption = first(caption),
              argued = max(argued),
              decided = suppressWarnings(min(decided, na.rm = TRUE)),
              author = first(na.omit(author)), .groups = "drop") |>
    mutate(decided = if_else(is.infinite(decided), as.Date(NA), decided),
           # Decided, date unknown: the list occasionally names an author with no
           # decision date (OT2025's 24-813). The case is not waiting, so it must
           # not sit in the clock forever; it counts as decided.
           undated = is.na(decided) & !is.na(author))
}

# Sittings: runs of argument days with no gap over OW_SITTING_GAP, named by the
# month of their median day. Calendar months misname them -- "Oct 30 - Nov 8" is
# the November sitting, and the February sitting runs into March.
ow_sittings <- function(d) {
  s <- d |> distinct(term, argued) |> arrange(term, argued) |> group_by(term) |>
    mutate(sitting_no = cumsum(is.na(lag(argued)) | as.numeric(argued - lag(argued)) > OW_SITTING_GAP)) |>
    group_by(term, sitting_no) |>
    mutate(sitting = format(median(argued), "%B"), s_from = min(argued), s_to = max(argued)) |>
    ungroup()
  d |> left_join(s, by = c("term", "argued"))
}

.ow_court_on <- function(day) {
  r <- JUSTICE_ROSTER
  r$name[r$from <= day & (is.na(r$to) | r$to >= day)]
}
.ow_signed <- function(a) !is.na(a) & !str_detect(a, regex("curiam", ignore_case = TRUE))

# The state of a Term as of a date: everything decided after `as_of` is treated as
# still pending, so a past Term can be replayed (and the page checked against what
# then happened). History for the clock is every Term BEFORE `term`.
#
# `dismissed`: dockets to leave out because the case was dismissed before decision.
# The list keeps a case's argument date after a Rule 46 dismissal and never marks
# it (Genalo v. Black, 25-886, dismissed before its October 13, 2026 argument),
# so the caller supplies these from the dockets (classify_argument() status).
ow_state <- function(gn, term, as_of = Sys.Date(), dismissed = character()) {
  as_of <- as.Date(as_of)
  d <- ow_sittings(ow_decisions(gn |> filter(!dkt %in% dismissed)))
  hist <- d |> filter(term < !!term, !is.na(decided)) |>
    mutate(lag = as.numeric(decided - argued))
  cur <- d |> filter(term == !!term, argued <= as_of) |>
    mutate(decided = if_else(!is.na(decided) & decided > as_of, as.Date(NA), decided),
           done    = !is.na(decided) | undated,
           author  = if_else(done, author, NA_character_),
           aj = if_else(.ow_signed(author), justice_key(author), NA_character_),
           waited = if_else(done, NA_real_, as.numeric(as_of - argued)))
  # The clock: the share of past cases from the same-named sitting decided
  # within this many days, and that sitting's median wait.
  # Vectorised, not rowwise(): a finished Term has no rows here, and rowwise()
  # evaluates `if (n_hist)` on a zero-length column and errors.
  out <- cur |> filter(!done)
  h <- lapply(out$sitting, function(s) hist$lag[hist$sitting == s])
  out$n_hist  <- lengths(h)
  out$pct_out <- vapply(seq_along(h), function(i)
    if (length(h[[i]])) mean(h[[i]] <= out$waited[i]) else NA_real_, numeric(1))
  out$typical <- out$argued + vapply(h, function(x) if (length(x)) round(median(x)) else NA_real_, numeric(1))
  out <- arrange(out, desc(waited))
  # Per sitting: majority opinions so far by Justice, and the candidates.
  bench <- .ow_court_on(max(cur$argued, as_of - 1))
  sits <- cur |> group_by(sitting_no, sitting, s_from, s_to) |>
    summarise(n = n(), n_out = sum(!done),
              written = list(table(factor(na.omit(aj), levels = bench))), .groups = "drop") |>
    rowwise() |> mutate(cands = list(if (n_out > 0) names(written)[written == min(written)] else character())) |>
    ungroup() |> arrange(s_from)
  out <- out |> left_join(select(sits, sitting_no, cands), by = "sitting_no")
  list(term = term, as_of = as_of, cur = cur, out = out, sits = sits, bench = bench,
       hist_terms = range(hist$term), n_hist = nrow(hist))
}

# ---- page -----------------------------------------------------------------------

OW_CSS <- "
main.wrap{max-width:54rem}
.ow-tiles{display:grid;grid-template-columns:repeat(4,1fr);gap:.8rem;margin:1.2rem 0 2rem}
@media (max-width:620px){.ow-tiles{grid-template-columns:1fr 1fr}}
.ow-tile{border-top:2px solid var(--ink);padding-top:.4rem}
.ow-tile b{display:block;font-family:'Fraunces',Georgia,serif;font-size:1.9rem;font-weight:600;line-height:1.1;font-variant-numeric:tabular-nums}
.ow-tile span{font-size:.8rem;color:var(--ink-soft);letter-spacing:.04em}
.ow h2{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.45rem;margin:2.2rem 0 .3rem}
.ow .note{font-style:italic;color:var(--ink-soft);font-size:.95rem;margin:.2rem 0 1rem;max-width:46rem}
.ow-tw{overflow-x:auto}
.ow table{border-collapse:collapse;width:100%;font-size:.92rem;font-variant-numeric:tabular-nums}
.ow th{font-weight:600;font-size:.72rem;letter-spacing:.06em;text-transform:uppercase;color:var(--accent);text-align:left;border-bottom:2px solid var(--ink);padding:.4rem .5rem;white-space:nowrap}
.ow td{padding:.45rem .5rem;border-bottom:1px solid var(--rule);vertical-align:top}
.ow tbody tr:nth-child(even){background:var(--stripe)}
.ow td.n,.ow th.n{text-align:right;white-space:nowrap}
.ow .sub{display:block;font-size:.8rem;color:var(--faint);white-space:nowrap}
.ow td.ow-cands .sub{white-space:normal}
.ow td:nth-child(2){white-space:nowrap}
.ow td:nth-child(3){min-width:8rem}
.ow-clock{position:relative;height:.7rem;min-width:7rem;background:var(--stripe);border:1px solid var(--rule);border-radius:2px;margin-top:.25rem}
.ow-clock i{position:absolute;top:0;bottom:0;left:0;background:var(--accent);border-radius:1px}
.ow-clock u{position:absolute;top:-.2rem;bottom:-.2rem;width:2px;background:var(--ink);text-decoration:none}
.ow-cands{font-size:.88rem}
.ow-sits{display:grid;gap:1.1rem;margin:1rem 0}
.ow-sit{border-top:1px solid var(--rule);padding-top:.6rem}
.ow-sit h3{font-family:'Newsreader',serif;font-weight:600;font-size:.85rem;letter-spacing:.1em;text-transform:uppercase;color:var(--ink-soft);margin:0 0 .5rem}
.ow-sit h3 span{text-transform:none;letter-spacing:0;font-weight:400;color:var(--faint)}
.ow-js{display:flex;flex-wrap:wrap;gap:.35rem;margin:0;padding:0;list-style:none}
.ow-js li{border:1px solid var(--rule);border-radius:2px;padding:.2rem .5rem;font-size:.85rem;background:var(--panel);color:var(--faint)}
.ow-js li b{font-variant-numeric:tabular-nums;color:var(--ink);margin-left:.3rem}
.ow-js li.c{border-color:var(--accent);color:var(--ink);box-shadow:inset 0 -2px 0 var(--accent)}
.ow-done{font-size:.88rem;color:var(--ink-soft)}
"

.ow_esc <- function(x) htmlEscape(x %||% "")
.ow_date <- function(d) if (is.na(d)) "" else paste(format(d, "%b"), as.integer(format(d, "%d")))
.ow_names <- function(x) if (!length(x)) "" else if (length(x) == 1) x else
  paste0(paste(head(x, -1), collapse = ", "), if (length(x) > 2) "," else "", " and ", tail(x, 1))
# The likely-author cell: a short list names them; a long one names who is out.
.ow_cands <- function(cands, bench) {
  if (!length(cands)) return("")
  if (setequal(cands, bench)) return("<span class='sub'>Any Justice &mdash; none has written from this sitting yet</span>")
  out <- setdiff(bench, cands)
  if (length(cands) > 5) return(paste0("Any but ", .ow_names(out)))
  .ow_names(cands)
}
# The list's captions are upper case; .jx_caption() title-cases them, which
# capitalises the small words too ("Dept. Of Corrections And Public Safety").
.ow_caption <- function(x) {
  x <- .jx_caption(x)
  str_replace_all(x, "(?<=\\s)(Of|And|The|For|In|On|To|At|By)(?=\\s)", tolower)
}

render_opinion_watch <- function(st, out_path, term_page = FALSE) {
  yyyy <- 2000L + as.integer(st$term)
  cur <- st$cur; out <- st$out; sits <- st$sits
  n_arg <- nrow(cur); n_dec <- sum(cur$done); n_out <- nrow(out)
  title <- paste0("Opinion Watch — October Term ", yyyy)
  dek <- if (n_out) sprintf("%d of %d argued case%s await%s decision as of %s. How long each has waited, against every Term since OT%d, and which Justices are likely writing.",
                            n_out, n_arg, if (n_arg == 1) "" else "s", if (n_out == 1) "s" else "",
                            format(st$as_of, "%B %e, %Y"), 2000L + st$hist_terms[1])
         else sprintf("Every argued case of the Term has been decided (as of %s).", format(st$as_of, "%B %e, %Y"))
  tiles <- sprintf("<div class='ow-tiles'>%s</div>", paste0(
    sprintf("<div class='ow-tile'><b>%d</b><span>argued</span></div>", n_arg),
    sprintf("<div class='ow-tile'><b>%d</b><span>decided</span></div>", n_dec),
    sprintf("<div class='ow-tile'><b>%d</b><span>awaiting decision</span></div>", n_out),
    sprintf("<div class='ow-tile'><b>%s</b><span>longest wait, days</span></div>",
            if (n_out) format(max(out$waited)) else "&mdash;")))

  # The clock.
  clock <- if (n_out) {
    scale <- max(c(out$waited, as.numeric(out$typical - out$argued)), na.rm = TRUE)
    rows <- vapply(seq_len(n_out), function(i) {
      r <- out[i, ]
      cap <- .ow_caption(r$caption)
      w <- 100 * r$waited / scale
      typ <- as.numeric(r$typical - r$argued)
      bar <- sprintf("<div class='ow-clock' title='%d days; median for a %s case %s days'><i style='width:%.1f%%'></i>%s</div>",
                     r$waited, r$sitting, if (is.na(typ)) "?" else typ, w,
                     if (is.na(typ)) "" else sprintf("<u style='left:%.1f%%'></u>", 100 * typ / scale))
      pct <- if (is.na(r$pct_out)) "&mdash;" else sprintf("%d%%", round(100 * r$pct_out))
      sprintf("<tr><td><a href='/cases/%s.html'>%s</a><span class='sub'>%s</span></td><td>%s<span class='sub'>argued %s</span></td><td class='n'>%d%s</td><td class='n'>%s<span class='sub'>%s</span></td><td class='ow-cands'>%s</td></tr>",
              .ow_esc(r$dkt), .ow_esc(cap), .ow_esc(paste(r$dkts[[1]], collapse = ", ")),
              .ow_esc(r$sitting), .ow_date(r$argued), r$waited, bar, pct,
              if (is.na(r$typical)) "" else paste("typical by", .ow_date(r$typical)),
              .ow_cands(r$cands[[1]], st$bench))
    }, character(1))
    paste0("<section class='ow'><h2>The clock</h2>",
           "<p class='note'>Days since argument. The bar runs to the longest wait on the page; the tick is the median wait for a case argued in the same sitting in earlier Terms. &ldquo;Out by now&rdquo; is the share of those cases already decided after as many days.</p>",
           "<div class='ow-tw'><table><thead><tr><th>Case</th><th>Sitting</th><th class='n'>Days waiting</th><th class='n'>Out by now</th><th>Likely author</th></tr></thead><tbody>",
           paste(rows, collapse = ""), "</tbody></table></div></section>")
  } else ""

  # Who's writing what.
  open <- sits[sits$n_out > 0, ]
  done <- sits[sits$n_out == 0, ]
  sit_html <- if (nrow(open)) paste(vapply(seq_len(nrow(open)), function(i) {
    s <- open[i, ]; w <- s$written[[1]]; cand <- s$cands[[1]]
    chips <- paste(vapply(names(w), function(nm) sprintf("<li%s>%s<b>%d</b></li>",
                     if (nm %in% cand) " class='c'" else "", nm, w[[nm]]), character(1)), collapse = "")
    sprintf("<div class='ow-sit'><h3>%s sitting <span>&middot; %s&ndash;%s &middot; %d case%s, %d awaiting decision</span></h3><ul class='ow-js'>%s</ul><p class='note'>%s</p></div>",
            .ow_esc(s$sitting), .ow_date(s$s_from), .ow_date(s$s_to), s$n, if (s$n == 1) "" else "s", s$n_out, chips,
            if (sum(w) == 0) "No majority opinion from this sitting yet, so every Justice is a candidate."
            else sprintf("Highlighted: the Justices with the fewest majority opinions from this sitting (%d each) &mdash; %s. The %s outstanding opinion%s most likely come%s from them.",
                         min(w), .ow_names(cand), s$n_out, if (s$n_out == 1) "" else "s", if (s$n_out == 1) "s" else ""))
  }, character(1)), collapse = "") else ""
  done_html <- if (nrow(done)) sprintf("<p class='ow-done'>Complete: %s.</p>",
    paste(sprintf("%s sitting (%d)", done$sitting, done$n), collapse = ", ")) else ""
  who <- if (nrow(sits)) paste0("<section class='ow'><h2>Who is writing what</h2>",
    "<p class='note'>The Chief Justice (or the senior Justice in the majority) assigns each sitting's majority opinions so that every Justice writes about the same number. Chips count each Justice's signed majority opinions from that sitting so far; per curiam decisions are not counted.</p>",
    "<div class='ow-sits'>", sit_html, "</div>", done_html, "</section>") else ""

  method <- paste0("<section class='ow'><h2>How reliable is this?</h2>",
    "<p class='note'>Checked against every signed majority opinion from OT2016 to OT2025 (567). When each opinion issued, its author was among the Justices with the fewest majority opinions from that sitting 93% of the time; picking at random from the same Justices would have been right 63% of the time. With one case left in a sitting, the rule names 2.7 Justices on average and was right 83% of the time, against 30% by chance. It misses when a Justice is recused, when an assignment moves after a majority is lost, and in sittings of ten or more cases, where someone writes two.</p></section>")

  crumb <- list(href = paste0("/", JUSTICES_DIR, "/"), label = "Justices")
  html <- paste0("<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head(paste0(title, " — Supreme Court Report"),
              site_breadcrumb_jsonld("Opinion Watch", crumb),
              extra_css = OW_CSS, description = dek,
              path = paste0("/", JUSTICES_DIR, "/", OW_FILE)),
    "<body>", site_masthead(active = paste0("/", JUSTICES_DIR, "/")),
    "<main class='wrap' id='main'>",
    site_breadcrumb("Opinion Watch", crumb),
    "<p class='kicker'>The Justices · October Term ", yyyy, "</p>",
    "<h1>Opinion Watch</h1><p class='dek'>", dek, "</p><hr class='brule'>",
    tiles, clock, who, method,
    "<p class='back'><a href='index.html'>&larr; The Justices</a>",
    # The Term's own page exists only once it has a decision (render_justices()).
    if (isTRUE(term_page)) sprintf(" · <a href='ot%d.html'>OT%d opinions so far &rarr;</a>", yyyy, yyyy) else "",
    "</p>",
    "</main></body>\n</html>\n")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(smarten_html(html)), out_path, useBytes = TRUE)
  invisible(out_path)
}

# ---- in the Justices render -------------------------------------------------------

# Dockets whose own case page says the case was dismissed. The list never marks a
# Rule 46 dismissal (see ow_state()), and render_justices.R has no dockets of its
# own -- but the conferences run renders every live case page from fresh JSON, and
# docket_disposition() writes "Dismissed" as the page's disposition word for a
# granted case withdrawn before decision (classify_argument() status). Only the
# undecided argued cases are read, a handful of small files.
ow_dismissed <- function(site_dir, dkts) {
  dkts[vapply(dkts, function(k) {
    p <- file.path(site_dir, "cases", paste0(k, ".html"))
    if (!file.exists(p)) return(FALSE)
    h <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "")
    grepl("disp-word'>(<a[^>]*>)?Dismissed", h)
  }, logical(1))]
}

# The Term on the page: the newest with an argument on or before `as_of`. Between
# the last June opinion and the first October argument that is the finished Term,
# which then reads "every argued case has been decided".
ow_current_term <- function(gn, as_of = Sys.Date()) {
  a <- suppressWarnings(as.Date(gn$argued))
  t <- gn$term[!is.na(a) & a <= as.Date(as_of)]
  if (length(t)) max(t) else NA_integer_
}

# Render the page into the Justices section; returns the state (for the index
# row) or NULL when there is no Term to show. Never fatal to the caller.
write_opinion_watch <- function(site_dir, gn, as_of = Sys.Date()) {
  term <- ow_current_term(gn, as_of)
  if (is.na(term)) return(NULL)
  pend <- gn$dkt[gn$term == term & !is.na(gn$argued) & is.na(gn$decided)]
  st <- ow_state(gn, term, as_of, dismissed = ow_dismissed(site_dir, pend))
  yyyy <- 2000L + as.integer(term)
  out_dir <- file.path(site_dir, JUSTICES_DIR)
  render_opinion_watch(st, file.path(out_dir, OW_FILE),
                       term_page = file.exists(file.path(out_dir, sprintf("ot%d.html", yyyy))))
  st
}
