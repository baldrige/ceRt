# docket_page.R ----------------------------------------------------------------
# Generate a lightweight single-case "docket page" from a case record (as built
# by scotus_dash_new.R's build_case) plus the enrichments we already compute:
# cert forecast, Rule 10 signals, Questions Presented, conference history, and
# the argument/decision lifecycle -- in the site's editorial theme.
#
# Design goals (keep loads manageable at ~tens of thousands of pages):
#   * PURE / no per-case network -- forecast comes from in-memory models, QP and
#     Rule 10 signals from caches passed in; a page just omits a section it lacks.
#   * ONE shared stylesheet (cases/style.css), linked (not inlined) by every page.
#   * Minimal markup; a denial is a few KB, a fully-litigated case ~30KB.
#   * Incremental: render_docket_pages() skips a docket whose content is unchanged
#     (a manifest of per-page hashes), so re-runs rewrite only active cases.
#
# Depends (sourced alongside): cert_funnel.R (classify_petitions/_argument helpers
# live in argument_nav.R + cert_funnel.R), cert_model.R (score_case/score_disposition),
# argument_nav.R (classify_argument, extract_advocates, argument_term),
# interactive_theme.R (petitioner_counsel_html), qp_extract.R (reflow/strip QP).

suppressPackageStartupMessages({ library(tidyverse); library(htmltools) })

# ---- shared stylesheet (written once per output dir) --------------------------
DOCKET_CSS <- ":root{--paper:#f3ecdd;--panel:#f7f1e4;--ink:#23262d;--soft:#5f5847;--faint:#8a8271;--ox:#8a2b2b;--rule:#d8cdb4;--sienna:#b5651d}
*{box-sizing:border-box}html{-webkit-text-size-adjust:100%}
body{font-family:'Newsreader',Georgia,serif;font-size:19px;line-height:1.6;color:var(--ink);background:var(--paper);margin:0;font-feature-settings:'onum' 1}
body::before{content:'';position:fixed;inset:0;z-index:-1;pointer-events:none;opacity:.5;mix-blend-mode:multiply;background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='140' height='140'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='2' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.045'/%3E%3C/svg%3E\")}
.case{max-width:54rem;margin:0 auto;padding:2.8rem 1.5rem 4rem}
a{color:var(--ox)}
.kicker{font:600 .74rem/1 'Newsreader';letter-spacing:.22em;text-transform:uppercase;color:var(--ox);margin:0 0 .8rem}
h1{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:clamp(1.8rem,4.5vw,2.8rem);line-height:1.05;letter-spacing:-.015em;margin:0 0 .5rem}
.posture{font-size:1.02rem;color:var(--soft);margin:.2rem 0 0}
.brule{border:0;border-top:2px solid var(--ink);margin:1.2rem 0 1.4rem;position:relative}
.brule::after{content:'';position:absolute;left:0;top:4px;width:100%;border-top:1px solid var(--rule)}
h2{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.02rem;text-transform:uppercase;letter-spacing:.12em;color:var(--ox);margin:2rem 0 .7rem;padding-bottom:.35rem;border-bottom:1px solid var(--rule)}
h3{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:.82rem;text-transform:uppercase;letter-spacing:.14em;color:var(--faint);margin:0 0 .6rem}
p{margin:.5rem 0}
.disp{display:flex;align-items:center;gap:1.2rem;background:var(--panel);border:1px solid var(--rule);border-left:4px solid var(--ox);padding:1rem 1.3rem;margin:.4rem 0 0}
.disp-num{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:2.9rem;line-height:.9;color:var(--ox);white-space:nowrap}
.disp-lab{font-size:.98rem;color:var(--ink)}
.disp-lab span{color:var(--faint);font-size:.86rem}
.disp-sig{font-size:.9rem;color:var(--soft);font-style:italic;margin-top:.15rem}
.disp-sub{font-size:.86rem;color:var(--faint);margin-top:.15rem}
.disp-word{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.5rem;color:var(--ox);line-height:1.1}
.forecast-why{margin:.55rem 0 0;font-size:.95rem;line-height:1.5;color:var(--soft);max-width:46rem}
.disp-word a{color:inherit;text-decoration:underline;text-decoration-color:rgba(138,43,43,.4);text-underline-offset:4px}
.qp{font-size:1.05rem;line-height:1.55}.qp ol,.qp ul{padding-left:1.2rem;margin:.3rem 0}.qp li{margin:.35rem 0}.qp p{margin:.4rem 0}
.grid{display:grid;grid-template-columns:1fr 1fr;gap:1.5rem;margin-top:1.6rem}
.panel{background:var(--panel);border:1px solid var(--rule);padding:1rem 1.2rem}
.panel.wide{grid-column:1/-1}
.side{font:600 .72rem/1.3 'Newsreader';letter-spacing:.1em;text-transform:uppercase;color:var(--faint)}
.cslot{margin:.2rem 0 .9rem}.cname{font-weight:600}.firm{color:var(--soft);font-size:.94rem}
.timeline{list-style:none;margin:.4rem 0 0;padding:0;position:relative}
.timeline::before{content:'';position:absolute;left:7.4rem;top:.3rem;bottom:.3rem;border-left:1px solid var(--rule)}
.timeline li{display:grid;grid-template-columns:7rem 1fr;gap:1.1rem;padding:.4rem 0;position:relative}
.tl-date{font-variant-numeric:tabular-nums;font-size:.86rem;color:var(--faint);text-align:right;padding-top:.1rem;padding-right:.28rem}
.tl-body{font-size:.98rem;position:relative}
.tl-body::before{content:'';position:absolute;left:-.9rem;top:.5rem;width:7px;height:7px;border-radius:50%;background:var(--sienna);box-shadow:0 0 0 3px var(--paper)}
.tl-docs{margin-top:.2rem;display:flex;flex-wrap:wrap;gap:.2rem .8rem}
.tl-docs a{font-size:.85rem;color:var(--sienna);border-bottom:1px solid rgba(181,101,29,.35);text-decoration:none}
.kicker a{color:inherit;border-bottom:1px solid rgba(138,43,43,.4)}
.back{margin-top:2rem;font-size:.95rem}.back a{color:var(--sienna);text-decoration:none;border-bottom:1px solid rgba(181,101,29,.35)}
.stamp{margin-top:.7rem;font-size:.8rem;color:var(--faint);font-style:italic}
@media(max-width:640px){.grid{grid-template-columns:1fr}.timeline li{grid-template-columns:5rem 1fr}.timeline::before{left:5.4rem}}"

DOCKET_FONTS <- "https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght@0,9..144,500;0,9..144,600;1,9..144,500&family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&display=swap"

write_docket_css <- function(out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(DOCKET_CSS), file.path(out_dir, "style.css"), useBytes = TRUE)
}

# Bumped whenever the markup/CSS changes, to force a one-time full re-render.
# v5: plain-English forecast description on pending paid-petition pages, plus the
# NA-safe elite_counsel fix (more cases now score, so more get an estimate).
PAGE_TEMPLATE_VERSION <- "v5"

# ---- small helpers ------------------------------------------------------------
.esc <- function(x) { x <- x %||% ""; x[is.na(x)] <- ""; htmltools::htmlEscape(x) }
# Date -> "June 5, 2025" (no %e double-space); "" for missing.
.fmtdate <- function(d) {
  if (is.null(d) || length(d) == 0 || all(is.na(d))) return("")
  str_squish(format(as.Date(d[1]), "%B %e, %Y"))
}
.mdq <- function(x) {                    # QP markdown -> HTML (reflowed)
  if (is.null(x) || length(x) == 0) return("")
  x <- x[1]
  if (is.na(x) || x %in% c("", "-")) return("")
  commonmark::markdown_html(reflow_qp(strip_qp_heading(x)))
}

# Counsel of record + firm for the side matching `rx`, "Name<br><firm>".
docket_counsel <- function(parties, rx) {
  if (!is.data.frame(parties) || nrow(parties) == 0 || !all(c("attys", "type") %in% names(parties)))
    return("&mdash;")
  s <- parties[str_detect(parties$type %||% "", rx), , drop = FALSE]
  if (!nrow(s)) return("&mdash;")
  cor <- s[which(s$counsel_of_record %in% TRUE), , drop = FALSE]
  row <- if (nrow(cor)) cor[1, ] else s[1, ]
  nm <- row$attys %||% NA; fm <- row$firm %||% NA
  if (is.na(nm) || !nzchar(nm)) return("&mdash;")
  paste0("<span class='cname'>", .esc(nm), "</span>",
         if (!is.na(fm) && nzchar(fm)) paste0("<br><span class='firm'>", .esc(fm), "</span>") else "")
}

# Proceedings timeline, newest first (by parsed date, so a bad source date can't
# scramble the order). Proceeding text is stripped of any inline HTML and escaped;
# document links come from the docs_/links_ (JSON) or Document_/links_ (historical
# scrape) columns. The links div is emitted only when there is at least one link.
docket_timeline <- function(ev) {
  if (!is.data.frame(ev) || nrow(ev) == 0) return("")
  dcols <- str_subset(names(ev), "^(docs_|Document_)"); lcols <- str_subset(names(ev), "^links_")
  edate <- suppressWarnings(lubridate::mdy(ev$Date))
  ord <- order(edate, decreasing = TRUE, na.last = TRUE)
  items <- map_chr(ord, function(i) {
    dt <- ev$Date[i] %||% ""
    tx <- .esc(str_replace_all(ev[["Proceedings and Orders"]][i] %||% "", "<[^>]*>", ""))
    docs <- ""
    if (length(lcols)) {
      ls <- unlist(ev[i, lcols], use.names = FALSE)
      ds <- if (length(dcols)) unlist(ev[i, dcols], use.names = FALSE) else rep(NA_character_, length(ls))
      k <- which(!is.na(ls))
      if (length(k)) {
        anchors <- sprintf("<a href='%s' target='_blank' rel='noopener'>%s</a>", ls[k],
          ifelse(is.na(ds[k]) | ds[k] == "", "Document", .esc(ds[k])))
        docs <- paste0("<div class='tl-docs'>", paste(anchors, collapse = ""), "</div>")
      }
    }
    sprintf("<li><div class='tl-date'>%s</div><div class='tl-body'>%s%s</div></li>", .esc(dt), tx, docs)
  })
  paste(items, collapse = "")
}

# Status-adaptive disposition box. Pending paid petitions get the forecast
# (a prediction); resolved cases lead with the outcome and keep the pre-decision
# estimate as a retrospective note.
docket_disposition <- function(outcome, outcome_date, arg, p_base, p_gvr, sig, is_app = FALSE, why = "") {
  pct <- function(p) sprintf("%d%%", round(100 * p))
  sig_txt <- if (!is.null(sig)) {
    bits <- c(if (isTRUE(sig$dissent_below)) "dissent below",
              if (isTRUE(sig$split_argued)) "circuit split argued")
    if (length(bits)) paste0("Rule 10: ", paste(bits, collapse = ", ")) else NULL
  } else NULL
  est_note <- if (!is.na(p_base)) sprintf("<div class='disp-sub'>Pre-decision estimate: %s cert probability</div>", pct(p_base)) else ""

  if (is.na(outcome) || outcome %in% c("pending", "relisted")) {
    if (!is.na(p_base)) {
      gvr <- if (!is.null(p_gvr) && !is.na(p_gvr)) sprintf("<div class='disp-sub'>GVR risk %s</div>", pct(p_gvr)) else ""
      sg  <- if (!is.null(sig_txt)) sprintf("<div class='disp-sig'>%s</div>", .esc(sig_txt)) else ""
      box <- sprintf("<div class='disp'><div class='disp-num'>%s</div><div class='disp-lab'><div>estimated cert probability<br><span>(petition-stage, structural)</span></div>%s%s</div></div>", pct(p_base), sg, gvr)
      why_html <- if (nzchar(why %||% "")) sprintf("<p class='forecast-why'>%s</p>", why) else ""
      return(paste0(box, why_html))
    }
    return(sprintf("<div class='disp'><div class='disp-word'>%s</div></div>", if (is_app) "Application pending" else "Pending"))
  }
  # Resolved: lead with a word + date, keep the estimate as a footnote.
  word <- if (is_app) switch(outcome, granted = "Application granted", denied = "Application denied",
                              dismissed = "Application dismissed", "Application acted on")
    else switch(outcome,
      granted = if (!is.na(arg$decided_date)) "Decided" else if (!is.na(arg$argued_date)) "Argued"
                else if (!is.na(arg$scheduled_date)) "Set for argument" else "Certiorari granted",
      denied = "Certiorari denied", dismissed = "Dismissed", gvr = "GVR'd", outcome)
  # Link the "Decided" word to the slip opinion (the primary way to reach it).
  if (identical(word, "Decided") && !is.na(arg$opinion_url))
    word <- sprintf("<a href='%s' target='_blank' rel='noopener'>Decided</a>", arg$opinion_url)
  dt <- if (!is_app && identical(outcome, "granted"))
    coalesce(arg$decided_date, arg$argued_date, arg$scheduled_date, as.Date(outcome_date)) else as.Date(outcome_date)
  when <- if (length(dt) && !is.na(dt)) paste0(" &middot; ", .fmtdate(dt)) else ""
  sprintf("<div class='disp'><div class='disp-word'>%s%s</div>%s</div>", word, when, est_note)
}

# ---- the page -----------------------------------------------------------------
# `cx` is a one-row case record. `cls_row` is its classify_petitions() row
# (outcome/outcome_date); if NULL it is computed. `models`/`signals`/`qp` are
# optional enrichments (no network is ever performed here).
docket_page <- function(cx, out_dir, models = NULL, cls_row = NULL,
                        signals = NULL, qp = NA_character_, rendered = Sys.Date()) {
  dkt <- cx$dkt; ev <- cx$events[[1]]; par <- cx$parties[[1]]; rel <- cx$related %||% ""
  if (length(qp) > 1) qp <- paste(qp, collapse = "\n")   # a qp_map value may be a vector
  is_app <- identical(cx$type %||% "", "app")
  if (is.null(cls_row) && !is_app && exists("classify_petitions"))
    cls_row <- tryCatch(classify_petitions(cx)[1, ], error = function(e) NULL)
  outcome <- cls_row$outcome %||% NA_character_
  outcome_date <- cls_row$outcome_date %||% as.Date(NA)
  arg <- if (exists("classify_argument")) classify_argument(ev) else
    list(argued_date = as.Date(NA), decided_date = as.Date(NA), scheduled_date = as.Date(NA),
         argued_text = NA, opinion_author = NA, opinion_url = NA)

  # Applications are excluded from classify_petitions; derive their disposition
  # from the docket text ("Application (...) granted/denied ...").
  if (is_app && is.na(outcome) && is.data.frame(ev)) {
    et <- ev[["Proceedings and Orders"]] %||% ""; ed <- suppressWarnings(lubridate::mdy(ev$Date))
    gi <- which(str_detect(et, regex("^Application.*grant", ignore_case = TRUE)))
    di <- which(str_detect(et, regex("^Application.*(deni|dismiss)", ignore_case = TRUE)))
    if (length(gi)) { outcome <- "granted"; outcome_date <- ed[gi[length(gi)]] }
    else if (length(di)) { outcome <- "denied"; outcome_date <- ed[di[length(di)]] }
  }

  # Forecast (paid only; pure, from in-memory models).
  p_base <- NA_real_; p_gvr <- NA_real_; fc_why <- ""
  if (!is.null(models) && !is.null(models$baseline) && identical(cx$type %||% "", "paid") &&
      exists("score_case")) {
    sc_base <- tryCatch(score_case(models$baseline, cx$caption, cx$lower, par, cx$date,
                cx$lower_date, rel, signals = signals), error = function(e) NULL)
    p_base <- if (!is.null(sc_base)) sc_base$prob else NA_real_
    fc_why <- if (!is.null(sc_base) && exists("describe_forecast"))
      tryCatch(describe_forecast(sc_base), error = function(e) "") else ""
    if (!is.null(models$enhanced) && !is.null(models$gvr) && exists("score_disposition")) {
      s <- tryCatch(score_disposition(models$enhanced, models$gvr, cx$caption, cx$lower, par,
             cx$date, cx$lower_date, rel, events = ev,
             as_of = suppressWarnings(max(case_conference_dates(ev)))), error = function(e) NULL)
      if (!is.null(s)) p_gvr <- s$p_gvr
    }
  }

  disp <- docket_disposition(outcome, outcome_date, arg, p_base, p_gvr, signals,
                             is_app = is_app, why = fc_why)
  # Conference history = TOTAL distributions (a case seen at one conference counts).
  n_dist <- if (is.data.frame(ev))
    sum(str_detect(ev[["Proceedings and Orders"]] %||% "", "DISTRIBUTED for Conference"), na.rm = TRUE) else 0L
  qp_html <- .mdq(qp)
  adv <- if (exists("extract_advocates")) extract_advocates(arg$argued_text) else NA

  # Argument & decision -- only for a genuine merits track (suppressed for GVR /
  # dismissed, whose "Judgment VACATED" order otherwise reads as a decision).
  ad <- c()
  if (!is.na(arg$argued_date))
    ad <- c(ad, sprintf("<p><b>Argued</b> %s%s. <a href='https://www.supremecourt.gov/oral_arguments/audio/%s/%s' target='_blank' rel='noopener'>Audio and transcript</a></p>",
      .fmtdate(arg$argued_date), if (!is.na(adv)) paste0(" &mdash; ", .esc(adv)) else "",
      argument_term(arg$argued_date), dkt))
  if (!is.na(arg$decided_date)) {
    dword <- if (!is.na(arg$opinion_url))
      sprintf("<a href='%s' target='_blank' rel='noopener'>Decided</a>", arg$opinion_url) else "Decided"
    ad <- c(ad, sprintf("<p><b>%s</b> %s.%s</p>", dword, .fmtdate(arg$decided_date),
      if (!is.na(arg$opinion_author)) paste0(" Opinion by <b>", .esc(arg$opinion_author), "</b>.") else ""))
  }
  argsec <- if (length(ad) && !(outcome %in% c("gvr", "dismissed")))
    paste0("<section><h2>Argument &amp; decision</h2>", paste(ad, collapse = ""), "</section>") else ""

  ty <- cx$type %||% "paid"; if (is.na(ty)) ty <- "paid"
  ptype <- unname(c("paid" = "Paid petition", "ifp" = "IFP petition", "app" = "Application")[ty])
  if (is.na(ptype)) ptype <- "Petition"
  posture <- str_squish(paste0(ptype,
    if (!is.na(cx$lower) && nzchar(cx$lower)) paste0(" &middot; ", .esc(cx$lower)),
    if (!is.null(cx$lower_dkt) && !is.na(cx$lower_dkt) && nzchar(cx$lower_dkt)) paste0(", No. ", .esc(cx$lower_dkt)),
    if (!is.na(cx$lower_date)) paste0(" &middot; judgment ", .fmtdate(cx$lower_date))))
  conf_line <- if (n_dist > 0) sprintf("Distributed for %d conference%s", n_dist, if (n_dist == 1) "" else "s") else "&mdash;"

  # Counsel of record -- omit the panel entirely when we hold no counsel data
  # (rather than showing misleading em-dashes for both sides).
  pc <- docket_counsel(par, "Petitioner|Applicant|Appellant")
  rc <- docket_counsel(par, "Respondent|Appellee")
  counsel_panel <- if (pc != "&mdash;" || rc != "&mdash;")
    paste0("<div class='panel'><h3>Counsel of record</h3>",
      "<p class='cslot'><span class='side'>For petitioner</span><br>", pc, "</p>",
      "<p class='cslot'><span class='side'>For respondent</span><br>", rc, "</p></div>") else ""
  case_panel <- paste0("<div class='panel", if (!nzchar(counsel_panel)) " wide" else "", "'><h3>Case</h3>",
    "<p><span class='side'>Conference history</span><br>", conf_line, "</p>",
    if (nzchar(rel)) paste0("<p><span class='side'>Related</span><br>", .esc(rel), "</p>") else "",
    "</div>")
  cap <- .esc(str_squish(str_remove_all(cx$caption %||% dkt, ", Petitioners?|, Respondents?")))
  dkurl <- paste0("https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/", dkt, ".html")

  page <- paste0(
    "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", cap, " &mdash; No. ", dkt, "</title>",
    "<link rel='preconnect' href='https://fonts.googleapis.com'>",
    "<link rel='stylesheet' href='", DOCKET_FONTS, "'>",
    "<link rel='stylesheet' href='style.css'>",
    "</head><body><main class='case'>",
    "<p class='kicker'>Supreme Court of the United States &middot; <a href='", dkurl, "' target='_blank' rel='noopener'>No. ", dkt, "</a></p>",
    "<h1>", cap, "</h1>",
    "<p class='posture'>", posture, "</p><hr class='brule'>",
    disp,
    if (nzchar(qp_html)) paste0("<section><h2>Question", if (str_count(qp, "(?m)^\\s*\\d+[.)]") >= 2) "s" else "", " presented</h2><div class='qp'>", qp_html, "</div></section>") else "",
    "<div class='grid'>", counsel_panel, case_panel, "</div>",
    argsec,
    "<section><h2>Proceedings</h2><ol class='timeline'>", docket_timeline(ev), "</ol></section>",
    "<p class='back'><a href='", dkurl, "' target='_blank' rel='noopener'>Full docket on supremecourt.gov &rarr;</a></p>",
    "<p class='stamp'>Last refreshed ", .fmtdate(rendered), ".</p>",
    "</main></body></html>")
  writeLines(enc2utf8(page), file.path(out_dir, paste0(dkt, ".html")), useBytes = TRUE)
  invisible(nchar(page))
}

# ---- batch render (incremental) -----------------------------------------------
# Renders a page per row of `cases`. `qp_map`/`signals_map` are named by docket;
# absent entries just omit that section. A manifest of per-page content hashes
# (cases/.manifest.json) makes re-runs rewrite only dockets whose page changed.
render_docket_pages <- function(cases, out_dir, models = NULL, qp_map = NULL,
                                signals_map = NULL, incremental = TRUE, rendered = Sys.Date()) {
  write_docket_css(out_dir)
  mpath <- file.path(out_dir, ".manifest.json")
  # Always load the existing manifest and MERGE this batch into it, so rendering
  # one term (or subset) doesn't wipe another term's hashes from a shared /cases/.
  manifest <- if (file.exists(mpath))
    tryCatch(as.list(jsonlite::fromJSON(mpath)), error = function(e) list()) else list()
  # Classify the whole batch once (cheaper than per-page).
  cls <- tryCatch(classify_petitions(cases), error = function(e) NULL)
  cls_by <- if (!is.null(cls)) split(cls, cls$dkt) else list()
  # Stable model id so retraining invalidates pending-case (forecast) pages.
  model_id <- if (!is.null(models)) digest::digest(models) else ""

  n_written <- 0L; new_manifest <- manifest   # preserve entries for cases not in this batch
  for (i in seq_len(nrow(cases))) {
    cx <- cases[i, ]; dkt <- cx$dkt
    sig <- if (!is.null(signals_map)) signals_map[[dkt]] else NULL
    qp  <- if (!is.null(qp_map)) qp_map[[dkt]] %||% NA_character_ else NA_character_
    clr <- if (length(cls_by)) cls_by[[dkt]][1, ] else NULL
    # Hash every page-determining input (+ template + model); skip if unchanged.
    key <- digest::digest(list(PAGE_TEMPLATE_VERSION, model_id, cx$caption, cx$events,
             cx$parties, cx$lower, cx$lower_dkt, cx$lower_date, cx$date, cx$type,
             qp, sig, cx$related))
    if (incremental && identical(manifest[[dkt]] %||% "", key) &&
        file.exists(file.path(out_dir, paste0(dkt, ".html")))) {
      new_manifest[[dkt]] <- key; next
    }
    tryCatch({ docket_page(cx, out_dir, models = models, cls_row = clr, signals = sig,
                           qp = qp, rendered = rendered)
               n_written <- n_written + 1L }, error = function(e)
      message("docket_page failed for ", dkt, ": ", conditionMessage(e)))
    new_manifest[[dkt]] <- key
  }
  jsonlite::write_json(new_manifest, mpath, auto_unbox = TRUE)
  message("docket pages: ", n_written, " written / ", nrow(cases), " total (",
          nrow(cases) - n_written, " unchanged)")
  invisible(n_written)
}

# CI convenience: render docket pages for `cases` into `site_dir/cases`, loading
# the models and the on-site QP/Rule-10 caches. Called at the end of the daily
# and conference build scripts so any freshly-fetched case gets a current page
# (incremental -- only changed dockets rewrite). Defensive: never fatal.
render_dockets_for <- function(cases, site_dir, model_dir = "data") {
  if (is.null(cases) || nrow(cases) == 0) return(invisible(0L))
  tryCatch({
    models <- if (exists("load_cert_models")) load_cert_models(model_dir) else NULL
    read_qpc <- function(p) if (file.exists(p))
      tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) list()) else list()
    qp_map <- list()
    for (p in c(file.path(site_dir, "conferences", "qp_cache.json"),
                file.path(site_dir, "arguments", "qp_cache.json"))) {
      qc <- read_qpc(p); for (d in names(qc)) if (!is.null(qc[[d]]$qp)) qp_map[[d]] <- qc[[d]]$qp
    }
    # The cache is a JSON object keyed by docket -> {dissent_below, split_argued,
    # ...}, so load it as a named list (NOT a data frame); score_case reads
    # signals[[feature]].
    signals_map <- tryCatch(
      jsonlite::fromJSON("data-raw/petition_signals.json", simplifyVector = FALSE),
      error = function(e) NULL)
    if (is.null(signals_map)) signals_map <- list()
    # The bulk data-raw file only covers historical terms (the enrich-petitions
    # workflow runs per closed term). The daily resolves FRESH Rule 10 signals
    # for each day's current-term paid petitions into this on-site cache and
    # threads them into the dashboard forecast. Merge the cache OVER data-raw so
    # a docket page uses the SAME signals as the dashboard -- otherwise a current
    # petition with a dissent/split reads structural-only here (e.g. 2%) but
    # signal-boosted on the dashboard (e.g. 5%).
    cache_p <- file.path(site_dir, "dashboards", "petition_signals_cache.json")
    if (file.exists(cache_p)) {
      fresh <- tryCatch(jsonlite::fromJSON(cache_p, simplifyVector = FALSE),
                        error = function(e) NULL)
      if (!is.null(fresh) && length(fresh)) signals_map[names(fresh)] <- fresh
    }
    render_docket_pages(cases, file.path(site_dir, "cases"),
                        models = models, qp_map = qp_map, signals_map = signals_map)
    write_search_index(cases, file.path(site_dir, "cases"))
  }, error = function(e) message("render_dockets_for failed: ", conditionMessage(e)))
}

# Maintain cases/search.json (docket -> caption) for the home-page search box,
# merged across batches so it accumulates the whole corpus and stays current as
# workflows render newly-fetched cases.
write_search_index <- function(cases, cases_dir) {
  if (is.null(cases) || nrow(cases) == 0) return(invisible(0L))
  ipath <- file.path(cases_dir, "search.json")
  idx <- if (file.exists(ipath))
    tryCatch(as.list(jsonlite::fromJSON(ipath)), error = function(e) list()) else list()
  cap <- str_squish(str_remove_all(cases$caption %||% NA_character_, ", Petitioners?|, Respondents?"))
  cap <- ifelse(is.na(cap) | cap == "", cases$dkt, cap)
  for (i in seq_len(nrow(cases))) idx[[cases$dkt[i]]] <- cap[i]
  dir.create(cases_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(idx, ipath, auto_unbox = TRUE)
  invisible(length(idx))
}
