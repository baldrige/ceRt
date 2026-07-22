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
DOCKET_CSS <- ":root{--paper:#f3ecdd;--panel:#f7f1e4;--ink:#23262d;--soft:#5f5847;--faint:#8a8271;--ox:#8a2b2b;--rule:#d8cdb4;--sienna:#b5651d;--c-white:#fff;--c-orange:#e07b1f;--c-cream:#efe1a8;--c-tan:#c8a56b;--c-blue:#7fa8cf;--c-red:#cf5f5f;--c-lgreen:#7fb069;--c-dgreen:#2f6b3d;--c-yellow:#e9cb3f;--c-neutral:#bcae90}
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
.amic-side{font-size:.86rem;color:var(--faint)}
.timeline{list-style:none;margin:.4rem 0 0;padding:0;position:relative}
.timeline::before{content:'';position:absolute;left:7.4rem;top:.3rem;bottom:.3rem;border-left:1px solid var(--rule)}
.timeline li{display:grid;grid-template-columns:7rem 1fr;gap:1.1rem;padding:.4rem 0;position:relative}
.tl-date{font-variant-numeric:tabular-nums;font-size:.86rem;color:var(--faint);text-align:right;padding-top:.1rem;padding-right:.28rem}
.tl-body{font-size:.98rem;position:relative}
.tl-body::before{content:'';position:absolute;left:-.9rem;top:.5rem;width:8px;height:8px;border-radius:50%;background:var(--dot,var(--sienna));border:1px solid rgba(35,38,45,.45);box-shadow:0 0 0 3px var(--paper)}
.timeline li.proc .tl-body::before{background:var(--paper);border:1.5px solid var(--c-neutral)}
.tl-legend{display:flex;flex-wrap:wrap;gap:.3rem 1.1rem;margin:.1rem 0 1rem;font-size:.78rem;color:var(--faint)}
.tl-legend span{display:inline-flex;align-items:center;gap:.35rem;white-space:nowrap}
.tl-legend i{width:9px;height:9px;border-radius:50%;border:1px solid rgba(35,38,45,.45);flex:none}
.tl-legend i.hollow{background:var(--paper);border:1.5px solid var(--c-neutral)}
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
# v6: retrospective forecast description on DECIDED paid-petition pages too.
# v7: strip_qp_heading anchored to the start -- a QP whose body repeats the
# phrase "question presented" no longer loses everything before the repetition.
# v8: brief-cover dots -- each proceedings-timeline dot is tinted to the Court's
# Rule 33.1(g) booklet-cover color for that filing (petition white, opposition
# orange, merits briefs blue/red, amicus cream/green, reply yellow), procedural
# entries hollow, with a compact legend under the Proceedings heading.
# v9: split merits amicus by side using Rule 37 timing (dark green once the
# respondent's merits brief is filed, light green before), since the docket text
# rarely states the side; explicit "in support of ..." still overrides.
# v10: use the LATEST respondent merits brief as that split point, so a respondent
# aligned with the petitioner (e.g. private plaintiffs when the US is petitioner,
# 23-477) filing on the petitioner's earlier schedule no longer mis-dates it.
# v11: Case panel gains an "Amicus briefs" tally (cert-stage vs merits, merits
# split by Rule 37 side), counted from the same brief_cover classification as the
# timeline dots so the numbers always agree with the colors.
PAGE_TEMPLATE_VERSION <- "v11"

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

# ---- brief-cover classification (Rule 33.1(g)) --------------------------------
# The Court prints each filing on a fixed booklet-cover color. We have no
# structured "brief type" field -- only the docket's regular `Proceedings and
# Orders` phrasing -- so a small ordered set of patterns resolves each entry to
# its cover, and the grant date splits the cert-stage covers (cream amicus /
# orange opposition) from the merits covers (green amicus / blue-red briefs).
# Returns list(color, label) for a filed brief, or NULL for a procedural entry
# (order, application, distribution, waiver, argument) -- which renders hollow.
brief_cover <- function(text, granted_on = as.Date(NA), entry_date = as.Date(NA),
                        resp_brief_on = as.Date(NA)) {
  t <- text %||% ""
  if (length(t) == 0 || is.na(t)) return(NULL)
  t <- str_squish(str_replace_all(t, "<[^>]*>", ""))
  if (!nzchar(t)) return(NULL)
  low <- tolower(t)
  has <- function(rx) str_detect(low, rx)
  cov <- function(tok, lab) list(color = sprintf("var(--c-%s)", tok), label = lab)
  merits <- !is.na(granted_on) && !is.na(entry_date) && entry_date >= granted_on

  # Motions and applications are procedural even when they name a brief.
  if (has("^motion\\b") || has("^application\\b")) return(NULL)
  # Amicus: cream at the petition stage; green on the merits. Dark green =
  # supporting respondent, light green = supporting petitioner or neither party.
  # The docket text usually omits the side, so Rule 37's schedule is the tell:
  # amici for petitioner/neither are due after the PETITIONER's merits brief,
  # amici for respondent after the RESPONDENT's -- so a merits amicus filed on or
  # after the respondent's brief supports the respondent. An explicit "in support
  # of ..." in the text (rare) overrides the timing.
  if (has("brief\\s+amic(us|i)\\s+curiae")) {
    if (merits) {
      resp_lab <- "Amicus brief (supporting respondent)"
      pet_lab  <- "Amicus brief (supporting petitioner or neither party)"
      if (has("in support of respond")) return(cov("dgreen", resp_lab))
      if (has("in support of (petition|neither)")) return(cov("lgreen", pet_lab))
      if (!is.na(resp_brief_on) && !is.na(entry_date) && entry_date >= resp_brief_on)
        return(cov("dgreen", resp_lab))
      return(cov("lgreen", pet_lab))
    }
    return(cov("cream", "Amicus brief (petition stage)"))
  }
  # Respondent's cert-stage answer.
  if (has("in opposition") || has("motion to dismiss or affirm"))
    return(cov("orange", "Brief in opposition"))
  # Merits reply (yellow) vs. cert-stage reply to the opposition (tan). A merits
  # reply reads "Reply brief of petitioner ..."; the cert reply, "Reply of ...".
  if (has("reply brief") || (has("^reply\\b") && merits))
    return(cov("yellow", "Reply brief on the merits"))
  if (has("^reply\\b"))
    return(cov("tan", "Reply to brief in opposition"))
  # The petition itself (and its jurisdictional-statement / extraordinary-writ
  # cousins). Guarded on "filed" so a granted/denied ORDER line stays procedural.
  if (has("filed") && !has("rehearing") &&
      (has("^petition for (a )?writ of certiorari") ||
       has("^jurisdictional statement") ||
       has("^petition for an? extraordinary writ")))
    return(cov("white", "Petition for certiorari"))
  # Fixed tan (checked before the merits briefs so a "supplemental brief of
  # petitioner" isn't mistaken for the merits opener).
  if (has("joint appendix") || has("supplemental brief") || has("petition for rehearing"))
    return(cov("tan", "Supplemental / rehearing filing"))
  # Merits briefs -- only after a grant; before it these strings don't occur.
  if (merits && has("brief (of|for) (the )?(petitioner|appellant)"))
    return(cov("blue", "Petitioner's brief on the merits"))
  if (merits && has("brief (of|for) (the )?(respondent|appellee)"))
    return(cov("red", "Respondent's brief on the merits"))
  NULL
}

# Amicus tallies for the Case panel, derived from the SAME brief_cover
# classification as the timeline dots (so the counts always match the colors):
# cert-stage amici (cream) and merits amici, the latter split by Rule 37 side
# (light green = petitioner/neither, dark green = respondent). Returns a list of
# integer counts; brief_cover is a cheap regex pass so re-classifying here (rather
# than threading counts back out of docket_timeline) keeps the two concerns clean.
amicus_counts <- function(ev, granted_on = as.Date(NA), resp_brief_on = as.Date(NA)) {
  out <- list(cert = 0L, merits = 0L, mpet = 0L, mresp = 0L)
  if (!is.data.frame(ev) || nrow(ev) == 0) return(out)
  po <- ev[["Proceedings and Orders"]] %||% ""
  ed <- suppressWarnings(lubridate::mdy(ev$Date))
  for (i in seq_along(po)) {
    cv <- brief_cover(po[i], granted_on, ed[i], resp_brief_on)
    if (is.null(cv)) next
    if (grepl("c-cream", cv$color, fixed = TRUE)) out$cert  <- out$cert  + 1L
    else if (grepl("c-lgreen", cv$color, fixed = TRUE)) out$mpet  <- out$mpet  + 1L
    else if (grepl("c-dgreen", cv$color, fixed = TRUE)) out$mresp <- out$mresp + 1L
  }
  out$merits <- out$mpet + out$mresp
  out
}

# Compact key shown once under the Proceedings heading (only on pages that carry
# at least one colored dot). aria-hidden: it re-states the per-entry tooltips.
DOCKET_LEGEND <- paste0(
  "<div class='tl-legend' aria-hidden='true'>",
  "<span><i style='background:var(--c-white)'></i>Petition</span>",
  "<span><i style='background:var(--c-orange)'></i>Opposition</span>",
  "<span><i style='background:var(--c-cream)'></i>Amicus (cert)</span>",
  "<span><i style='background:var(--c-blue)'></i>Petitioner brief</span>",
  "<span><i style='background:var(--c-red)'></i>Respondent brief</span>",
  "<span><i style='background:var(--c-lgreen)'></i>Amicus (for pet./neither)</span>",
  "<span><i style='background:var(--c-dgreen)'></i>Amicus (for resp.)</span>",
  "<span><i style='background:var(--c-yellow)'></i>Reply (merits)</span>",
  "<span><i style='background:var(--c-tan)'></i>Reply / other</span>",
  "<span><i class='hollow'></i>Procedural</span></div>")

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
docket_timeline <- function(ev, granted_on = as.Date(NA), resp_brief_on = as.Date(NA)) {
  if (!is.data.frame(ev) || nrow(ev) == 0) return("")
  dcols <- str_subset(names(ev), "^(docs_|Document_)"); lcols <- str_subset(names(ev), "^links_")
  edate <- suppressWarnings(lubridate::mdy(ev$Date))
  ord <- order(edate, decreasing = TRUE, na.last = TRUE)
  any_cover <- FALSE
  items <- map_chr(ord, function(i) {
    dt <- ev$Date[i] %||% ""
    raw <- ev[["Proceedings and Orders"]][i] %||% ""
    tx <- .esc(str_replace_all(raw, "<[^>]*>", ""))
    # Booklet-cover dot: colored + tooltipped for a filed brief, hollow (proc)
    # for orders/applications/etc.
    cov <- brief_cover(raw, granted_on, edate[i], resp_brief_on)
    if (is.null(cov)) {
      li_open <- "<li class='proc'>"
    } else {
      any_cover <<- TRUE
      # single-quoted attribute, so escape any apostrophe in the label too.
      li_open <- sprintf("<li style='--dot:%s' title='%s'>", cov$color,
                         gsub("'", "&#39;", .esc(cov$label), fixed = TRUE))
    }
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
    sprintf("%s<div class='tl-date'>%s</div><div class='tl-body'>%s%s</div></li>", li_open, .esc(dt), tx, docs)
  })
  out <- paste(items, collapse = "")
  attr(out, "any_cover") <- any_cover
  out
}

# Status-adaptive disposition box. Pending paid petitions get the forecast
# (a prediction); resolved cases lead with the outcome and keep the pre-decision
# estimate as a retrospective note.
docket_disposition <- function(outcome, outcome_date, arg, p_base, p_gvr, sig, is_app = FALSE, why = "", why_retro = "") {
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
  box <- sprintf("<div class='disp'><div class='disp-word'>%s%s</div>%s</div>", word, when, est_note)
  # Retrospective forecast note for decided paid petitions: what the model
  # predicted before the ruling, and why. Same layout as the pending note.
  why_html <- if (nzchar(why_retro %||% "")) sprintf("<p class='forecast-why'>%s</p>", why_retro) else ""
  paste0(box, why_html)
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

  # Cert-grant date -- the stage line for brief-cover coloring. Detected straight
  # from the docket ("Petition GRANTED" / "certiorari ... granted"); NA if never
  # granted, so an ungranted case keeps the safe cert-stage covers throughout.
  granted_on <- as.Date(NA)
  if (is.data.frame(ev)) {
    po <- ev[["Proceedings and Orders"]] %||% ""
    # A real cert grant ("Petition GRANTED"); exclude stay/conditional orders whose
    # boilerplate ("in the event the petition ... is granted") otherwise reads as a
    # grant and back-dates the merits stage.
    gi <- which(str_detect(po, regex("(petition|certiorari)\\b.*grant", ignore_case = TRUE)) &
                !str_detect(po, regex("for a stay|in the event|should the petition|pending (the )?disposition|if such writ",
                                      ignore_case = TRUE)))
    if (length(gi)) granted_on <- suppressWarnings(min(lubridate::mdy(ev$Date[gi]), na.rm = TRUE))
    if (is.infinite(granted_on)) granted_on <- as.Date(NA)
  }

  # Respondent's merits-brief date -- the split point for coloring merits amicus
  # (Rule 37: amici for respondent are due after it, amici for petitioner/neither
  # before it), taking the LATEST "Brief of/for respondent" on or after the grant
  # (minus the cert-stage opposition and any supplemental brief). Latest, not
  # earliest: a respondent aligned WITH the petitioner (e.g. private plaintiffs
  # when the United States is petitioner, as in 23-477) files on the petitioner's
  # earlier schedule, so only the last respondent brief marks the party actually
  # opposing the petitioner. NA if the respondent filed no merits brief, in which
  # case merits amici default to the petitioner/neither (light-green) reading.
  resp_brief_on <- as.Date(NA)
  if (is.data.frame(ev) && !is.na(granted_on)) {
    et <- ev[["Proceedings and Orders"]] %||% ""; ed <- suppressWarnings(lubridate::mdy(ev$Date))
    ri <- which(str_detect(et, regex("^brief (of|for) (the )?(respondent|appellee)", ignore_case = TRUE)) &
                !str_detect(et, regex("in opposition|supplement", ignore_case = TRUE)) &
                !is.na(ed) & ed >= granted_on)
    if (length(ri)) resp_brief_on <- suppressWarnings(max(ed[ri], na.rm = TRUE))
    if (is.infinite(resp_brief_on)) resp_brief_on <- as.Date(NA)
  }

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
  p_base <- NA_real_; p_gvr <- NA_real_; fc_why <- ""; fc_why_retro <- ""
  if (!is.null(models) && !is.null(models$baseline) && identical(cx$type %||% "", "paid") &&
      exists("score_case")) {
    sc_base <- tryCatch(score_case(models$baseline, cx$caption, cx$lower, par, cx$date,
                cx$lower_date, rel, signals = signals), error = function(e) NULL)
    p_base <- if (!is.null(sc_base)) sc_base$prob else NA_real_
    if (!is.null(sc_base) && exists("describe_forecast")) {
      fc_why       <- tryCatch(describe_forecast(sc_base), error = function(e) "")
      fc_why_retro <- tryCatch(describe_forecast(sc_base, retrospective = TRUE),
                               error = function(e) "")
    }
    if (!is.null(models$enhanced) && !is.null(models$gvr) && exists("score_disposition")) {
      s <- tryCatch(score_disposition(models$enhanced, models$gvr, cx$caption, cx$lower, par,
             cx$date, cx$lower_date, rel, events = ev,
             as_of = suppressWarnings(max(case_conference_dates(ev)))), error = function(e) NULL)
      if (!is.null(s)) p_gvr <- s$p_gvr
    }
  }

  disp <- docket_disposition(outcome, outcome_date, arg, p_base, p_gvr, signals,
                             is_app = is_app, why = fc_why, why_retro = fc_why_retro)
  # Conference history = TOTAL distributions (a case seen at one conference counts).
  n_dist <- if (is.data.frame(ev))
    sum(str_detect(ev[["Proceedings and Orders"]] %||% "", "DISTRIBUTED for Conference"), na.rm = TRUE) else 0L
  qp_html <- .mdq(qp)
  tl <- docket_timeline(ev, granted_on, resp_brief_on)
  tl_legend <- if (isTRUE(attr(tl, "any_cover"))) DOCKET_LEGEND else ""
  amic <- amicus_counts(ev, granted_on, resp_brief_on)
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
  # Amicus tally -- shown only when the case drew at least one amicus. The stage
  # segments (cert / merits) and, under the merits, the Rule 37 side split mirror
  # the timeline's cream / light-green / dark-green dots; each segment appears only
  # when nonzero, so an ungranted petition reads "N cert-stage" alone.
  amicus_line <- ""
  if (amic$cert + amic$merits > 0) {
    seg <- c(if (amic$cert > 0)   sprintf("%d cert-stage", amic$cert),
             if (amic$merits > 0) sprintf("%d merits", amic$merits))
    side <- if (amic$merits > 0) {
      sp <- c(if (amic$mpet > 0)  sprintf("%d supporting pet./neither", amic$mpet),
              if (amic$mresp > 0) sprintf("%d respondent", amic$mresp))
      if (length(sp)) paste0("<br><span class='amic-side'>(", paste(sp, collapse = ", "), ")</span>") else ""
    } else ""
    amicus_line <- paste0("<p><span class='side'>Amicus briefs</span><br>",
                          paste(seg, collapse = " &middot; "), side, "</p>")
  }
  case_panel <- paste0("<div class='panel", if (!nzchar(counsel_panel)) " wide" else "", "'><h3>Case</h3>",
    "<p><span class='side'>Conference history</span><br>", conf_line, "</p>",
    amicus_line,
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
    "<section><h2>Proceedings</h2>", tl_legend, "<ol class='timeline'>", tl, "</ol></section>",
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
    # Merge the QP caches. Conferences/arguments cover distributed/granted cases;
    # the daily's own cache (dashboards/qp_cache.json) covers RECENT paid petitions
    # that haven't reached a conference yet -- without it those docket pages show
    # no QP even though the dashboard extracted it. Listed last so a recent case
    # that later reaches conference keeps the same QP.
    qp_map <- list()
    for (p in c(file.path(site_dir, "conferences", "qp_cache.json"),
                file.path(site_dir, "arguments", "qp_cache.json"),
                file.path(site_dir, "dashboards", "qp_cache.json"))) {
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
