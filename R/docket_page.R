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

# Sitewide nav. Loaded here as well as from page_style.R because the backfill and
# re-render entry points source THIS file without that one, and a case page that
# silently lost its masthead would be indistinguishable from one that never had
# it -- which is the bug this whole change exists to fix.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f) else f
  }
  sys.source(find("palette.R"),  envir = globalenv())
  sys.source(find("site_nav.R"), envir = globalenv())
  sys.source(find("site_meta.R"), envir = globalenv())   # social_meta()
})

# A procedural entry is marked with a TICK across the timeline rule, not a dot.
# Bone & Cochineal moved --paper to near-white and the petition cover is
# --c-white, so the two markers came to differ by a fraction of a percent of
# fill and a hairline of ring colour -- which is to say not at all. The
# parchment palette's cream --paper had been carrying that distinction, so the
# recolour broke this without touching it.
#
# (Those two values were written out here as hex when this comment was first
# drafted, and audit_site.R failed on it. Correctly: a colour spelled anywhere
# but palette.R is a copy, and a copy in a COMMENT is the worst kind -- nothing
# renders from it, so it just quietly becomes untrue at the next recolour.)
#
# Shrinking the circle was tried first and is not enough -- both markers still
# read as empty rings, and a white cover cannot show as a white fill on
# near-white paper at any size. A tick is categorically not a dot: filings are
# circles, procedural steps are marks on the line, and the eye separates them
# without comparing sizes. The legend swatch matches.
#
# ---- shared stylesheet (written once per output dir) --------------------------
# Colours come from palette.R, which is also where the note about the two WCAG
# corrections now lives. Token NAMES were unified at the same time this file and
# interactive_theme.R stopped spelling the accent --ox and the muted ink --soft:
# a nav component shared across three stylesheets cannot carry two spellings of
# its own accent colour. The accent is now --accent; --ox is long gone.
#
# 54rem is the case-page measure, and the ten --c-* event categories ride along
# because they appear on this page type only.
DOCKET_CSS <- paste0(palette_root(extra = PALETTE_EVENTS), "
*{box-sizing:border-box}html{-webkit-text-size-adjust:100%}
body{font-family:'Newsreader',Georgia,serif;font-size:19px;line-height:1.6;color:var(--ink);background:var(--paper);margin:0;font-feature-settings:'onum' 1}
body::before{content:'';position:fixed;inset:0;z-index:-1;pointer-events:none;opacity:.5;mix-blend-mode:multiply;background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='140' height='140'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='2' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.045'/%3E%3C/svg%3E\")}
.case{max-width:54rem;margin:0 auto;padding:2.8rem 1.5rem 4rem}
a{color:var(--accent)}
.kicker{font:600 .74rem/1 'Newsreader';letter-spacing:.22em;text-transform:uppercase;color:var(--accent);margin:0 0 .8rem}
h1{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:clamp(1.8rem,4.5vw,2.8rem);line-height:1.05;letter-spacing:-.015em;margin:0 0 .5rem}
.posture{font-size:1.02rem;color:var(--ink-soft);margin:.2rem 0 0}
.brule{border:0;border-top:2px solid var(--ink);margin:1.2rem 0 1.4rem;position:relative}
.brule::after{content:'';position:absolute;left:0;top:4px;width:100%;border-top:1px solid var(--rule)}
h2{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.02rem;text-transform:uppercase;letter-spacing:.12em;color:var(--accent);margin:2rem 0 .7rem;padding-bottom:.35rem;border-bottom:1px solid var(--rule)}
h3{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:.82rem;text-transform:uppercase;letter-spacing:.14em;color:var(--faint);margin:0 0 .6rem}
p{margin:.5rem 0}
.disp{display:flex;align-items:center;gap:1.2rem;background:var(--panel);border:1px solid var(--rule);border-left:4px solid var(--accent);padding:1rem 1.3rem;margin:.4rem 0 0}
.disp-num{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:2.9rem;line-height:.9;color:var(--accent);white-space:nowrap}
.disp-lab{font-size:.98rem;color:var(--ink)}
.disp-lab span{color:var(--faint);font-size:.86rem}
.disp-sig{font-size:.9rem;color:var(--ink-soft);font-style:italic;margin-top:.15rem}
.disp-sub{font-size:.86rem;color:var(--faint);margin-top:.15rem}
.disp-word{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.5rem;color:var(--accent);line-height:1.1}
.forecast-why{margin:.55rem 0 0;font-size:.95rem;line-height:1.5;color:var(--ink-soft);max-width:46rem}
.disp-word a{color:inherit;text-decoration:underline;text-decoration-color:rgba(@accent:rgb@,.4);text-underline-offset:4px}
.qp{font-size:1.05rem;line-height:1.55}.qp ol,.qp ul{padding-left:1.2rem;margin:.3rem 0}.qp li{margin:.35rem 0}.qp p{margin:.4rem 0}
.grid{display:grid;grid-template-columns:1fr 1fr;gap:1.5rem;margin-top:1.6rem}
.panel{background:var(--panel);border:1px solid var(--rule);padding:1rem 1.2rem}
.panel.wide{grid-column:1/-1}
.side{font:600 .72rem/1.3 'Newsreader';letter-spacing:.1em;text-transform:uppercase;color:var(--faint)}
.cslot{margin:.2rem 0 .9rem}.cname{font-weight:600}.firm{color:var(--ink-soft);font-size:.94rem}
.amic-side{font-size:.86rem;color:var(--faint)}
.timeline{list-style:none;margin:.4rem 0 0;padding:0;position:relative}
.timeline::before{content:'';position:absolute;left:7.4rem;top:.3rem;bottom:.3rem;border-left:1px solid var(--rule)}
.timeline li{display:grid;grid-template-columns:7rem 1fr;gap:1.1rem;padding:.4rem 0;position:relative}
.tl-date{font-variant-numeric:tabular-nums;font-size:.86rem;color:var(--faint);text-align:right;padding-top:.1rem;padding-right:.28rem}
.tl-body{font-size:.98rem;position:relative}
.tl-body::before{content:'';position:absolute;left:-.9rem;top:.5rem;width:8px;height:8px;border-radius:50%;background:var(--dot,var(--link));border:1px solid rgba(@ink:rgb@,.45);box-shadow:0 0 0 3px var(--paper)}
.timeline li.proc .tl-body::before{width:11px;height:0;left:-1.05rem;top:.82rem;border-radius:0;background:none;border:0;border-top:1.5px solid var(--faint);opacity:.7;box-shadow:none}
.tl-legend{display:flex;flex-wrap:wrap;gap:.3rem 1.1rem;margin:.1rem 0 1rem;font-size:.78rem;color:var(--faint)}
.tl-legend span{display:inline-flex;align-items:center;gap:.35rem;white-space:nowrap}
.tl-legend i{width:9px;height:9px;border-radius:50%;border:1px solid rgba(@ink:rgb@,.45);flex:none}
.tl-legend i.hollow{width:11px;height:0;border-radius:0;background:none;border:0;border-top:1.5px solid var(--faint);opacity:.7;align-self:center}
.tl-none{font-size:.94rem;color:var(--faint);font-style:italic;margin:.2rem 0 0}
.tl-docs{margin-top:.2rem;display:flex;flex-wrap:wrap;gap:.2rem .8rem}
.tl-docs a{font-size:.85rem;color:var(--link);border-bottom:1px solid rgba(@link:rgb@,.4);text-decoration:none}
.kicker a{color:inherit;border-bottom:1px solid rgba(@accent:rgb@,.4)}
@media(max-width:640px){.grid{grid-template-columns:1fr}.timeline li{grid-template-columns:5rem 1fr}.timeline::before{left:5.4rem}}") |> fill_palette()

DOCKET_FONTS <- "https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght@0,9..144,500;0,9..144,600;1,9..144,500&family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&display=swap"

write_docket_css <- function(out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(paste0(DOCKET_CSS, NAV_CSS)),
             file.path(out_dir, "style.css"), useBytes = TRUE)
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
# v12: stamp the template version into each page's <head> as
# <meta name='tv' content='vNN'>, so the fill-throttled mop-up can detect a page
# left behind by a version bump (a throttle casualty keeps its OLD stamp) instead
# of relying only on the pre-v8 bare-<li> heuristic. See docs/docket-pages.md.
# v13: two-tier conference forecast (P(granted here) / P(granted ever)), a 95%
# interval on the case-page estimate, and a GVR line scored at the conference the
# petition actually faces rather than max() of every conference on the docket.
# v14: load /analytics.js, which every OTHER page on the site has carried since
# 2026-07-17 but these did not -- the GA4 wiring covered the index/dashboard/
# funnel generators and missed this one. The omission was invisible because
# nothing reads the analytics data back, until the landing page's Most-Read
# Cases panel asked GA which case pages were popular and correctly got nothing:
# /cases/ had never reported a single view. See R/site_analytics.R.
# v15: sitewide navigation. Until now a case page linked NOWHERE internally --
# its only anchors went off-site to supremecourt.gov -- so all 55,167 of them were
# terminal nodes, and they are the site's entire search surface. Adds the shared
# masthead, a Home > Cases > No. NN-NNNN breadcrumb (plus BreadcrumbList JSON-LD,
# which turns the bare URL in a Google result into a labelled path), and a footer
# that keeps the supremecourt.gov link primary while offering the five sections
# beneath it. Also normalises --ox/--soft onto --oxblood/--ink-soft and corrects
# --faint and --sienna to clear WCAG AA (both since renamed --accent/--link).
# See docs/navigation.md and R/site_nav.R.
# v16: an About page joins SITE_SECTIONS, so every case page's masthead and
# footer now carry a route to the contact address. Deliberately not left on the
# landing page alone: 99.5% of this site is case pages, a reader who spots a
# wrong caption or a misread docket entry got there from a search result, and
# giving them nowhere to report it wastes the only error-detection channel a
# 55,000-page generated site has.
# v17: merits amicus side. resp_brief_on required the docket to use the WORD
# "respondent", and the Court often names the party instead -- 25-170's respondent
# brief reads "Brief of Cty. Comm'rs of Boulder Cty., et al. submitted", so the
# date came out NA and all 21 of its respondent-side amici rendered light green,
# indistinguishable from the 41 petitioner-side ones. Falls back to the date the
# Court states when it extends the merits schedule. Re-render needed because the
# manifest keys on this constant and the docket events themselves did not change.
# v18: the docket has two phrasings for an amicus brief and we matched one.
# "Amicus brief of X submitted" -- the newer e-filing wording -- fell through to
# a hollow procedural dot, so 25-170 showed seven of its respondent-side amici
# uncoloured. Rejected tenders ("not accepted for filing") are excluded with it:
# the new wording is used for those MORE often than for accepted briefs, and the
# corrected brief re-appears under its own entry, so counting both double-counts.
# v19: feed autodiscovery in the <head>. A markup change, so it bumps -- roll it
# with rerender-dockets.yml (reuse_from_runs, ~20 min, no re-fetch).
# v20: direct appeals. Their jurisdictional statement now fills the petition slot
# (document link + QP + Rule 10 signals) and its docket entry takes the white
# petition cover instead of rendering hollow. Only ~34 pages in the whole
# back-catalogue change, but the bump is still the right call: `qp` and `sig` are
# in the manifest key, so without it those pages would re-render one at a time as
# their QP cache happened to refill -- some appeals fixed, some not, for as long
# as the caps took. Roll it with rerender-dockets.yml (reuse_from_runs).
# v21: social metadata in the <head> -- description, canonical, og:*, twitter:*.
# A markup change on all 55k case pages, and the whole point of it is that case
# pages are the unit people share, so it has to reach the back-catalogue rather
# than only pages a daily happens to touch.
#
# v22: no markup change at all -- the CLASSIFIER changed underneath the pages.
# GRANT_FORMS gained three wording families it could not read (#93), which moved
# four petitions: 18-6943 pending -> granted, 21-1087 gvr -> granted (and its
# date back sixteen months), 24-1177 and 24-1246 pending -> gvr.
#
# This bump exists because the manifest key CANNOT see that change. It digests
# the page's INPUTS -- caption, events, parties, lower court, qp, sig, model_id,
# and this constant -- not the classifier's OUTPUT. A fix that reinterprets the
# same docket entries leaves every hashed input byte-identical, so the key
# matches and the page is skipped forever. 18-6943 sat at "Pending" on a page
# that also said "Decided June 1, 2020" three lines down, because the argument
# block reads the events through classify_argument() and never went through the
# broken pattern. Two readings of one docket, one of them wrong, and nothing in
# the incremental path could tell.
#
# So: bump on a logic change that alters what a page SAYS, not only on markup.
# The alternative -- adding the classified outcome to the key -- would re-render
# a page every time a forecast moved, which is what the key is designed to avoid.
#
# v20 and v21 have not been rolled out either (55,345 of 55,498 pages are still
# v19 as of the 2026-08-19 audit), so one re-render now carries all three --
# rerender-dockets.yml with reuse_from_runs, ~20 min, no re-fetch.
#
# v23: application dispositions (classify_application_events). A pure classifier
# change over unchanged docket text -- exactly the case this constant exists for,
# and one nothing in the incremental path could have caught on its own. Corrects
# 9 pages that said the Court GRANTED an application it had denied (two of them
# capital stays) and resolves 85 of the 119 pages stuck at "Application pending",
# NFIB v. OSHA among them. The whole back-catalogue is at v22 as of the
# 2026-08-30 audit, so the stamp check in fetch_missing_dockets.R flags every
# affected page without an explicit docket list.
#
# v24: a docket with no proceedings says so, instead of rendering a bare
# "Proceedings" heading over an empty <ol>. Markup + one CSS rule; it reaches a
# single page (22A226, whose JSON returns "ProceedingsandOrder":[] and whose
# official HTML docket is equally empty -- an upstream absence, not a fetch
# failure). Bumped anyway for the usual reason: the manifest digests inputs, and
# that page's inputs have not moved since v23, so without a bump it keeps the
# empty list forever.
#
# v25: surface the docket's linked application/petition ("Linked docket"), from
# the top-level `Links` field build_case() now carries. See #99 -- it is a
# DIFFERENT relationship from `related` and is kept in its own field and its own
# row, because hold_signal()'s companion tier reads `related` and would score a
# case's own stay application as a companion grant.
#
# ⚠️ Unlike v23 and v24, this one CANNOT be rolled out with reuse_from_runs. The
# existing cases-*.rds snapshots were built before build_case() read the field,
# so they carry no `linked` column and a render-only pass would publish nothing
# new. Needs a full-fetch rerender.
#
# v26: the Related and Linked docket rows link to the referenced case pages
# (#101). Markup only over data already in the snapshots, so this one CAN roll
# out with reuse_from_runs -- unlike v25. The set of linkable dockets is resolved
# once per batch (resolvable_dockets) and each page's slice of it is part of its
# manifest key, so a reference that could not resolve on one run becomes a link
# on the run after its target appears, instead of being skipped forever.
PAGE_TEMPLATE_VERSION <- "v26"

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
  # Two phrasings, and the docket uses both. The long-standing one is "Brief
  # amicus curiae of X filed."; the newer electronic-filing one reverses it to
  # "Amicus brief of X submitted." Only the first was matched, so 25-170's seven
  # Aug 3 amici -- filed the day the respondent's amici were due -- rendered as
  # hollow procedural dots instead of dark green. The new wording appears in 9 of
  # 14 argued cases sampled, back to Sep 2025.
  #
  # "not accepted for filing" is excluded, and that matters more than it looks:
  # in the sample the new wording is used for REJECTED tenders more often than
  # for accepted ones (10 vs 7), because it is the e-filing intake record rather
  # than the docket entry. A rejected brief is corrected and re-filed under its
  # own entry, so counting the rejection double-counts the brief -- 25-170's one
  # rejected new-wording entry is Professor Jason Johnston, already on the docket
  # from Oct 9. The old wording never carries a rejection (0 across the sample),
  # so this guard only ever bites the new one; it is applied to both for safety.
  #
  # Anchored at the start so "Motion for leave to file an amicus brief" does not
  # match (motions already return procedural above, but only just).
  if ((has("brief\\s+amic(us|i)\\s+curiae") || has("^amicus\\s+brief\\b")) &&
      !has("not accepted for filing")) {
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
  # cousins), all three white under Rule 33.1(g). Guarded on "filed" so a
  # granted/denied ORDER line stays procedural.
  #
  # "^statement as to jurisdiction" is how the Court actually words a direct
  # appeal's opening filing; "^jurisdictional statement" is how this code assumed
  # it did, and matched 0 of the 34 appeals in the 2017-2024 archive, so their
  # opening filing has always rendered as a hollow procedural dot. The wrong
  # pattern is kept because it costs nothing and the docket's wording is not
  # guaranteed uniform.
  if (has("filed") && !has("rehearing") &&
      (has("^petition for (a )?writ of certiorari") ||
       has("^statement as to jurisdiction") ||
       has("^jurisdictional statement") ||
       has("^petition for an? extraordinary writ"))) {
    # Same cover, but name the document the reader is hovering over. The legend
    # is keyed on the colour ("Petition"), not on this label, so it stays true.
    js <- has("^statement as to jurisdiction") || has("^jurisdictional statement")
    return(cov("white", if (js) "Jurisdictional statement" else "Petition for certiorari"))
  }
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

# ---- application dispositions -------------------------------------------------
# Applications are excluded from classify_petitions(), so this is the ONLY
# classifier an NNA### docket gets and nothing downstream cross-checks it. Two
# properties of how the Court writes these orders defeat the obvious rule, and
# both were live on the published site until v23.
#
#  1. A COLLATERAL MOTION IS GRANTED IN THE ORDER THAT DENIES THE APPLICATION.
#     18A1238 reads "Application (18A1238) denied by the Court. ... The
#     applications for leave to file the application for stay and the response
#     under seal ... are granted." The old rule was "^Application.*grant" -- an
#     unanchored ".*" that walks straight across the sentence boundary into that
#     second clause -- and a grant hit outranked a deny hit unconditionally. Dunn
#     v. Price, a stay of execution DENIED over a four-Justice dissent, published
#     as "Application granted". Nine pages asserted a grant the Court had
#     refused; two of them were capital stays.
#
#     So the span below is [^.], which cannot leave the sentence, and it is
#     tempered against the collateral vocabulary besides. Tempering works because
#     the qualifier always sits BETWEEN the noun and its verb -- it postmodifies
#     the subject ("the applications FOR LEAVE TO FILE ... are granted").
#
#  2. "^Application" ONLY MATCHES THE CLERK. When a Justice refers an application
#     to the full Court, the disposition is a per curiam order in the Court's own
#     voice beginning "The application(s) ...", which that anchor never saw. 45
#     pages sat at "Application pending" with the order sitting in their own
#     timeline -- 21A244 (NFIB v. OSHA) among them, argued and decided January
#     13, 2022. Withdrawals (29 pages), completions and closures had no rule at
#     all.
#
# LAST terminal entry wins, not the first: an application's final docketed
# disposition is the operative one (an administrative stay granted early and the
# application denied weeks later resolves to the denial), and it reproduces the
# date the previous rule already published on the ~8.5k pages it got right.
# Sorted by date first -- ev arrives in raw docket order, which is why
# docket_timeline() sorts it too rather than trusting it.

# Postmodifiers marking the subject as a collateral motion rather than the
# application itself. Routine relief, granted in the same breath as a refusal.
#
# These words are only a signal in the Court's UNNUMBERED prose. They cannot be
# used on their own, because plenty of applications ARE a request for exactly
# this relief and get their own NNA### docket for it: "Application (18A653) to
# file a consolidated brief on the merits in excess of the word limit granted by
# The Chief Justice." Blanket tempering read 11 such pages as undisposed. Hence
# the two-tier rule below.
APP_COLLATERAL <- paste0(
  "leave to file|under seal|supplemental appendix|",
  "excess of the word limit|redacted cop")

# One sentence's worth of characters. A period ends the sentence UNLESS it is the
# abbreviation in "Application No. 22A489 DENIED AS MOOT" -- the clerk's bare
# docket reference -- so a period followed by a number stays inside the span. A
# real boundary is followed by a capital, never a digit.
APP_SPAN <- "(?:[^.]|\\.(?=\\s*\\d))"

# Tier 1, authoritative: the entry ties THIS docket's own number to the verb, so
# whatever the clause says about it is the disposition of this application and no
# tempering applies -- the number has already identified the subject.
app_self_rx <- function(dkt, verb) regex(
  paste0("\\bapplications?\\b", APP_SPAN, "{0,120}?\\b", dkt, "\\b",
         APP_SPAN, "{0,200}?\\b", verb),
  ignore_case = TRUE)

# Tier 2, for the Court's per curiam prose, which names no docket number ("The
# application for a stay is, in all respects, denied."). Here a collateral clause
# in the same order really is another motion, so the span is tempered against it.
#
# Read from the start of a sentence, because without the docket number the only
# thing identifying the application as the subject is its position -- and the run
# up to it must not pass a "motion". 17A745 (Rucho v. Common Cause) is the case
# that proves it: the Court GRANTED that stay, and the docket also carries "The
# motion of appellees to construe the application for a stay as a jurisdictional
# statement ... is denied." Read without that guard, the object becomes the
# subject and the page publishes a denial of an application that was granted --
# the same false-outcome failure as the bug this all started with, in reverse.
#
# Requiring the noun to sit FLUSH at the sentence start is too strict, though:
# 25A172 is "Order entered by Justice Thomas: Upon consideration of the
# application for stay ... it is ordered that the application for stay is DENIED",
# and 25A608 opens "[See Detached Opinion for full order language.]  The
# application for stay ... is granted." Hence the bounded, motion-free run-up, and
# a sentence break that tolerates a bracket closing behind the period.
app_generic_rx <- function(verb) regex(
  paste0("(?:^|\\.[\\])\"']*\\s+)",
         "(?:(?!motion)", APP_SPAN, "){0,160}?\\bapplications?\\b",
         "(?:(?!", APP_COLLATERAL, ")", APP_SPAN, "){0,240}?\\b", verb),
  ignore_case = TRUE)

APP_GRANT_V   <- "grant(ed|s)?\\b"
APP_DENY_V    <- "(deni(ed|es)|refused)\\b"
APP_DISMISS_V <- "dismiss(ed|es)\\b"

# A withdrawal is the applicant's own act and is written verb-first ("Letter ...
# withdrawing the application"), so the tempered subject->verb form above does
# not apply.
#
# No collateral guard here, deliberately. The tempting reading of 18A1271's
# "Letter withdrawing application to to file a reply in excess of the word limit
# received" is that it withdraws some collateral motion -- but that docket IS the
# word-limit application ("Application (18A1271) to file reply in excess of word
# limits, submitted to Justice Alito"), so the letter ends it. On an application
# docket the withdrawal on file is the withdrawal of that application.
APP_WITHDRAW_RX <- regex(
  paste0("\\bapplications?\\b[^.]{0,60}withdrawn",
         "|withdraw(ing|al of)\\b[^.]{0,40}\\bapplications?\\b"),
  ignore_case = TRUE)
# The clerk's housekeeping close. Terminal, but it is not a ruling: say so
# rather than inventing a grant or a denial the docket never recorded.
APP_CLOSED_RX <- regex("^applications?\\b[^.]{0,60}(completed|closed)\\b",
                       ignore_case = TRUE)

# Disposition of one application docket from its proceedings text.
# Returns list(outcome, date); outcome NA means genuinely undisposed.
classify_application_events <- function(et, ed, dkt = NA_character_) {
  none <- list(outcome = NA_character_, date = as.Date(NA))
  if (!length(et)) return(none)
  et[is.na(et)] <- ""
  ord <- order(ed); et <- et[ord]; ed <- ed[ord]

  # Tier 1 where the entry names this docket, tier 2 otherwise -- decided per
  # entry, so an order that numbers its own disposition is never second-guessed
  # by the tempered rule, and prose orders still resolve.
  self <- function(v) if (is.na(dkt) || !nzchar(dkt)) rep(FALSE, length(et))
                      else str_detect(et, app_self_rx(dkt, v))
  sg <- self(APP_GRANT_V); sd <- self(APP_DENY_V); sdis <- self(APP_DISMISS_V)
  numbered <- sg | sd | sdis

  g   <- ifelse(numbered, sg,   str_detect(et, app_generic_rx(APP_GRANT_V)))
  d   <- ifelse(numbered, sd,   str_detect(et, app_generic_rx(APP_DENY_V)))
  dis <- ifelse(numbered, sdis, str_detect(et, app_generic_rx(APP_DISMISS_V)))

  w    <- str_detect(et, APP_WITHDRAW_RX)
  cl   <- str_detect(et, APP_CLOSED_RX)

  kind <- case_when(
    w   ~ "withdrawn",
    dis & !g ~ "dismissed",
    # Deny outranks grant within an entry: never publish a grant on an order
    # that also refuses the application.
    d   ~ "denied",
    g   ~ "granted",
    cl  ~ "closed",
    TRUE ~ NA_character_
  )
  hit <- which(!is.na(kind))
  if (!length(hit)) return(none)
  last <- hit[length(hit)]
  out <- kind[last]

  # A grant of the application outranks a LATER denial, because the two-step
  # extension docket is the common shape of that pairing and the second refusal
  # does not take back the time already given: "Application (22A209) granted by
  # Justice Thomas extending the time to file until October 12, 2022", then a
  # request "to extend further" and "Application (22A209) denied." 69 dockets run
  # exactly that way, and calling them denied would be as one-sided as calling
  # them granted -- but "granted" is the relief that actually issued, and it is
  # what the site has always published for them.
  #
  # This precedence is only safe now that detection is sentence-scoped and
  # subject-anchored. Applied to contaminated matches it is precisely the rule
  # that published Dunn v. Price -- a stay of execution denied -- as a grant.
  # A withdrawal, dismissal or closure still wins on recency; only a denial defers.
  #
  # And only to an EXTENSION grant. Widened to any grant it swallows the case
  # where the Court acts and the application is then mooted: 22A489 reads
  # "Application No. 22A489 DENIED AS MOOT" after the stay was treated as a cert
  # petition and granted, and 25A952/25A999 are "Application for stay denied as
  # moot". The denial is the last word on those, and it is the honest one.
  if (identical(out, "denied")) {
    gi <- hit[kind[hit] %in% c("granted", "partial") &
              str_detect(et[hit], regex("extend", ignore_case = TRUE))]
    if (length(gi)) { last <- gi[length(gi)]; out <- kind[last] }
  }
  # "granted in part and denied in part" -- and the bare "granted in part" -- is
  # neither a grant nor a refusal, and calling it either overstates the order.
  if (out %in% c("granted", "denied") &&
      str_detect(et[last], regex("granted in part", ignore_case = TRUE)))
    out <- "partial"
  list(outcome = out, date = ed[last])
}

# Status-adaptive disposition box. Pending paid petitions get the forecast
# (a prediction); resolved cases lead with the outcome and keep the pre-decision
# estimate as a retrospective note.
docket_disposition <- function(outcome, outcome_date, arg, p_base, p_gvr, sig, is_app = FALSE, why = "", why_retro = "",
                               p_lo = NA_real_, p_hi = NA_real_, p_ever = NA_real_) {
  pct <- function(p) sprintf("%d%%", round(100 * p))
  # A 95% interval, shown only where it is wide enough to change how the number
  # reads. Measured widths: ~0.5pp below 1%, 3.5pp around 3%, 14pp around 16%,
  # 21pp above 25% -- so a bare "39%" asserts precision the model does not have,
  # while a bare "0.2%" is honest on its own. Case pages only: the dashboards and
  # conference tables stay scannable.
  ci_note <- if (!is.na(p_base) && !is.na(p_lo) && !is.na(p_hi) && p_base >= 0.05)
      sprintf("<div class='disp-sub'>95%% interval %s&ndash;%s</div>", pct(p_lo), pct(p_hi)) else ""
  sig_txt <- if (!is.null(sig)) {
    bits <- c(if (isTRUE(sig$dissent_below)) "dissent below",
              if (isTRUE(sig$split_argued)) "circuit split argued")
    if (length(bits)) paste0("Rule 10: ", paste(bits, collapse = ", ")) else NULL
  } else NULL
  est_note <- if (!is.na(p_base))
    sprintf("<div class='disp-sub'>Pre-decision estimate: %s cert probability%s</div>",
            pct(p_base),
            if (!is.na(p_lo) && !is.na(p_hi) && p_base >= 0.05)
              sprintf(" (95%% interval %s&ndash;%s)", pct(p_lo), pct(p_hi)) else "") else ""

  if (is.na(outcome) || outcome %in% c("pending", "relisted")) {
    if (!is.na(p_base)) {
      gvr <- if (!is.null(p_gvr) && !is.na(p_gvr)) sprintf("<div class='disp-sub'>GVR risk %s</div>", pct(p_gvr)) else ""
      sg  <- if (!is.null(sig_txt)) sprintf("<div class='disp-sig'>%s</div>", .esc(sig_txt)) else ""
      # The conference report scores the same petition with far more information
      # (relists, a reply brief, an opposition) and publishes a different number.
      # Showing only the petition-stage figure here left the two pages
      # contradicting each other -- median 2.7% on the case page against 16.1% on
      # the conference report for the same relisted petition. Surface both, each
      # labelled with the stage it belongs to.
      ev <- if (!is.na(p_ever) && !is.na(p_base) && abs(p_ever - p_base) >= 0.02)
        sprintf("<div class='disp-sub'>Conference-stage estimate: %s</div>", pct(p_ever)) else ""
      box <- sprintf("<div class='disp'><div class='disp-num'>%s</div><div class='disp-lab'><div>estimated cert probability<br><span>(petition-stage, structural)</span></div>%s%s%s%s</div></div>", pct(p_base), ci_note, ev, sg, gvr)
      why_html <- if (nzchar(why %||% "")) sprintf("<p class='forecast-why'>%s</p>", why) else ""
      return(paste0(box, why_html))
    }
    return(sprintf("<div class='disp'><div class='disp-word'>%s</div></div>", if (is_app) "Application pending" else "Pending"))
  }
  # Resolved: lead with a word + date, keep the estimate as a footnote.
  word <- if (is_app) switch(outcome, granted = "Application granted", denied = "Application denied",
                              dismissed = "Application dismissed",
                              # "granted in part and denied in part" is neither, and
                              # flattening it to "granted" overstates what the Court did.
                              partial = "Application granted in part",
                              withdrawn = "Application withdrawn",
                              closed = "Application closed",
                              "Application acted on")
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
                        signals = NULL, qp = NA_character_, rendered = Sys.Date(),
                        available = character()) {
  dkt <- cx$dkt; ev <- cx$events[[1]]; par <- cx$parties[[1]]; rel <- cx$related %||% ""
  # The linked application/petition docket. NA-safe on purpose: %||% catches a
  # snapshot rendered before build_case() carried the column, but a present-and-NA
  # value survives it, and nzchar(NA) is TRUE -- the same trap that silently made
  # related_present a constant (cert_model.R:427). Strip the "Linked with "
  # prefix; the row is already labelled.
  lnk <- cx$linked %||% ""
  lnk <- if (length(lnk) == 0 || is.na(lnk[1])) "" else
    str_squish(str_remove(lnk[1], regex("^linked with\\s*", ignore_case = TRUE)))
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
    # (a) The role is stated: "Brief of respondents ... filed." LATEST, not
    # earliest: a respondent aligned WITH the petitioner (private plaintiffs
    # where the United States is petitioner, as in 23-477) files on the
    # petitioner's earlier schedule, so only the last respondent brief marks the
    # party actually opposing.
    ri <- which(str_detect(et, regex("^brief (of|for) (the )?(respondent|appellee)", ignore_case = TRUE)) &
                !str_detect(et, regex("in opposition|supplement", ignore_case = TRUE)) &
                !is.na(ed) & ed >= granted_on)
    if (length(ri)) resp_brief_on <- suppressWarnings(max(ed[ri], na.rm = TRUE))
    if (is.infinite(resp_brief_on)) resp_brief_on <- as.Date(NA)

    # (b) The Court often names the PARTY instead of its role, and then (a)
    # matches nothing at all. 25-170's respondent brief reads "Brief of Cty.
    # Comm'rs of Boulder Cty., et al. submitted." -- no "respondent", and not
    # even "filed" -- so resp_brief_on came out NA and all 21 of its
    # respondent-side amici rendered light green.
    #
    # The fix is not to loosen (a) to any "Brief of ...": on a VIDED cross-
    # petition that matches the wrong side. 24-1287 carries "Brief of State
    # Respondents in No. 25-250" and "Brief of private respondents V.O.S.
    # Selections (as to 25-250)" -- parties aligned WITH this docket's
    # petitioners -- and a loosened rule flipped 33 of its amici to the
    # respondent's side.
    #
    # Use instead the date the Court states outright when it extends the merits
    # schedule. Where both signals exist they agree exactly (7 of 7 argued cases
    # sampled), so this only ever fills a gap. Take the EARLIER of the two: Rule
    # 37 keys amici off the date the brief was DUE, so 25-170's brief -- rejected
    # on its due date and corrected a week later -- still anchors its Jul 31
    # amicus to Jul 27.
    due <- suppressWarnings(lubridate::mdy(str_match(et, regex(
      paste0("respondents?.{0,3} brief on the merits is extended to and ",
             "including ([A-Z][a-z]+ \\d{1,2}, \\d{4})"),
      ignore_case = TRUE))[, 2]))
    due <- due[!is.na(due) & due >= granted_on]
    if (length(due)) {
      d_due <- max(due)   # the last extension granted is the operative deadline
      resp_brief_on <- if (is.na(resp_brief_on)) d_due else min(resp_brief_on, d_due)
    }
  }

  # Applications are excluded from classify_petitions; derive their disposition
  # from the docket text. See classify_application_events() for why this is not
  # simply "does the entry contain the word granted".
  if (is_app && is.na(outcome) && is.data.frame(ev)) {
    ac <- classify_application_events(ev[["Proceedings and Orders"]] %||% "",
                                      suppressWarnings(lubridate::mdy(ev$Date)), dkt)
    outcome <- ac$outcome; outcome_date <- ac$date
  }

  # Forecast (paid only; pure, from in-memory models).
  p_base <- NA_real_; p_gvr <- NA_real_; fc_why <- ""; fc_why_retro <- ""
  p_lo <- NA_real_; p_hi <- NA_real_; p_ever <- NA_real_
  if (!is.null(models) && !is.null(models$baseline) && identical(cx$type %||% "", "paid") &&
      exists("score_case")) {
    sc_base <- tryCatch(score_case(models$baseline, cx$caption, cx$lower, par, cx$date,
                cx$lower_date, rel, signals = signals,
                counsel_index = models$counsel_index), error = function(e) NULL)
    p_base <- if (!is.null(sc_base)) sc_base$prob else NA_real_
    p_lo   <- if (!is.null(sc_base)) sc_base$ci_low  %||% NA_real_ else NA_real_
    p_hi   <- if (!is.null(sc_base)) sc_base$ci_high %||% NA_real_ else NA_real_
    if (!is.null(sc_base) && exists("describe_forecast")) {
      fc_why       <- tryCatch(describe_forecast(sc_base), error = function(e) "")
      fc_why_retro <- tryCatch(describe_forecast(sc_base, retrospective = TRUE),
                               error = function(e) "")
    }
    # GVR risk, scored at the conference this petition is actually facing.
    # Previously as_of = max(case_conference_dates(ev)), which had three faults:
    # max() of an empty Date is -Inf (36% of pending paid petitions have never
    # been distributed), the LAST conference is after the disposition for 6.5% of
    # decided petitions, and case_conference_dates() lives in conference_dash.R,
    # which three of the four render entry points never source -- so every docket
    # page the daily job wrote silently lost this line.
    cds <- if (exists("conference_dates_from_events")) conference_dates_from_events(ev)
           else as.Date(character())
    as_of_conf <- if (length(cds) == 0) as.Date(NA) else {
      ahead <- cds[cds >= rendered]
      if (length(ahead)) min(ahead) else max(cds)
    }
    if (!is.na(as_of_conf) && exists("score_conference")) {
      s <- tryCatch(score_conference(models, cx$caption, cx$lower, par, cx$date,
             cx$lower_date, rel, events = ev, as_of = as_of_conf),
             error = function(e) NULL)
      if (!is.null(s)) { p_gvr <- s$p_gvr_now; p_ever <- s$p_grant_ever }
    }
  }

  disp <- docket_disposition(outcome, outcome_date, arg, p_base, p_gvr, signals,
                             is_app = is_app, why = fc_why, why_retro = fc_why_retro,
                             p_lo = p_lo, p_hi = p_hi, p_ever = p_ever)
  # Conference history = TOTAL distributions (a case seen at one conference counts).
  n_dist <- if (is.data.frame(ev))
    sum(str_detect(ev[["Proceedings and Orders"]] %||% "", "DISTRIBUTED for Conference"), na.rm = TRUE) else 0L
  qp_html <- .mdq(qp)
  tl <- docket_timeline(ev, granted_on, resp_brief_on)
  tl_legend <- if (isTRUE(attr(tl, "any_cover"))) DOCKET_LEGEND else ""
  # A docket with no proceedings at all renders a bare "Proceedings" heading over
  # an empty <ol>, which reads as a broken page rather than as an absence. Say
  # what is true instead: the Court's own record is empty here, not ours.
  #
  # 22A226 is the only one of 55,686 pages in this state -- its JSON returns
  # "ProceedingsandOrder":[] and the official HTML docket goes straight from the
  # heading to the attorney table -- so this is a rare-but-real upstream
  # condition, not a fetch failure. A throttled fetch never lands here: it fails
  # the case outright and leaves the previous page standing.
  tl_body <- if (!nzchar(str_trim(tl %||% "")))
    paste0("<p class='tl-none'>No proceedings are recorded on this docket. ",
           "The Court's docket for this case lists no entries.</p>")
  else paste0("<ol class='timeline'>", tl, "</ol>")
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
    if (nzchar(rel)) paste0("<p><span class='side'>Related</span><br>",
                            docket_refs_html(rel, available), "</p>") else "",
    # A separate row from Related, not merged into it: "Vide, 25-566" is a
    # companion petition, "Linked with 22A539" is this case's own stay or
    # extension application. Labelled for what it is so the two never read as
    # one. %||% guards a snapshot rendered before build_case() carried the field.
    if (nzchar(lnk)) paste0("<p><span class='side'>Linked docket</span><br>",
                            docket_refs_html(lnk, available), "</p>") else "",
    "</div>")
  cap <- .esc(str_squish(str_remove_all(cx$caption %||% dkt, ", Petitioners?|, Respondents?")))
  dkurl <- paste0("https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/", dkt, ".html")

  page <- paste0(
    "<!DOCTYPE html><html lang='en'><head>",
    # Root-absolute so it resolves from /cases/ as well as the site root; async so
    # it never blocks rendering. Same one line every other page carries.
    "<script async src='/analytics.js'></script>",
    "<meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    # Machine-readable template-version stamp: the fill-throttled scanner reads it
    # to spot a page a version bump left behind (see fetch_missing_dockets.R).
    "<meta name='tv' content='", PAGE_TEMPLATE_VERSION, "'>",
    # The shared-link card for a case page, which is the unit people actually
    # post. Built from what the docket already knows -- number, court below,
    # docketing date -- rather than a generic line, so a shared link says which
    # case it is. Same argument as the feed autodiscovery below: these pages are
    # 99.5% of the site and the ones a search result lands on.
    if (exists("social_meta")) social_meta(
      paste0(str_squish(str_remove_all(cx$caption %||% dkt,
                                       ", Petitioners?|, Respondents?")),
             " — No. ", dkt),
      paste0("Supreme Court docket No. ", dkt,
             if (!is.na(cx$lower %||% NA) && nzchar(cx$lower %||% ""))
               paste0(", on review from ", str_squish(cx$lower)) else "",
             if (!is.na(cx$date %||% NA))
               paste0(". Docketed ", format(cx$date, "%B %e, %Y")) else "",
             ". Filings, proceedings timeline and questions presented."),
      paste0("/cases/", dkt, ".html"), "article") else "",
    # The FOURTH hand-built <head> on this site, and the last to get feed
    # autodiscovery -- which means 55,357 pages, 99.5% of the site, carried none
    # while the eight index pages read green. These matter most, not least: a
    # search result lands a reader on a case page, never on /cases/.
    #
    # Guarded because docket_page.R is sourced by entry points that may not have
    # loaded page_style.R; those emit no links rather than failing.
    if (exists("feed_autodiscovery_links")) feed_autodiscovery_links() else "",
    "<title>", cap, " &mdash; No. ", dkt, "</title>",
    "<link rel='preconnect' href='https://fonts.googleapis.com'>",
    "<link rel='stylesheet' href='", DOCKET_FONTS, "'>",
    "<link rel='stylesheet' href='style.css'>",
    site_breadcrumb_jsonld(paste0("No. ", dkt), CASES_CRUMB),
    "</head><body>",
    site_masthead(),
    "<main class='case' id='main'>",
    site_breadcrumb(paste0("No. ", dkt), CASES_CRUMB),
    # The docket number moved to the breadcrumb; printing it again 8px below
    # would be the same string twice. The kicker keeps the LINK, which does a
    # different job -- it leaves the site for the official docket.
    "<p class='kicker'>Supreme Court of the United States &middot; <a href='", dkurl, "' target='_blank' rel='noopener'>Official docket &rarr;</a></p>",
    "<h1>", cap, "</h1>",
    "<p class='posture'>", posture, "</p><hr class='brule'>",
    disp,
    if (nzchar(qp_html)) paste0("<section><h2>Question", if (str_count(qp, "(?m)^\\s*\\d+[.)]") >= 2) "s" else "", " presented</h2><div class='qp'>", qp_html, "</div></section>") else "",
    "<div class='grid'>", counsel_panel, case_panel, "</div>",
    argsec,
    "<section><h2>Proceedings</h2>", tl_legend, tl_body, "</section>",
    case_footer(dkurl, .fmtdate(rendered)),
    "</main></body></html>")
  writeLines(enc2utf8(page), file.path(out_dir, paste0(dkt, ".html")), useBytes = TRUE)
  invisible(nchar(page))
}

# ---- cross-links between case pages -------------------------------------------
# The Related ("Vide, 25-566") and Linked docket ("21A758, 22A226") rows name
# other dockets in this corpus, so each one that has a page becomes a link.
#
# The set of linkable dockets is resolved ONCE per batch, before any page
# renders, and that is the whole point of doing it here rather than inside
# docket_page(). A file.exists() check per page would depend on render ORDER: a
# page could fall back to plain text purely because its target had not been
# written yet this run. And the manifest key digests a page's INPUTS, so that
# page would keep the hash it already has, be skipped on every later run, and
# stay unlinked forever -- the 18-6943 failure mode again, in new clothes. A set
# fixed before the loop is order-independent, and each page's slice of it goes
# into the key, so a page really does re-render when its target appears.
resolvable_dockets <- function(cases, out_dir) {
  ipath <- file.path(out_dir, "search.json")
  known <- if (file.exists(ipath))
    tryCatch(names(as.list(jsonlite::fromJSON(ipath))), error = function(e) character())
  else character()
  # search.json accumulates across runs and can outlive a renumbered docket, so
  # confirm the page is actually on disk -- write_cases_index() does the same,
  # for the same reason: linking one that isn't there publishes a 404.
  if (length(known))
    known <- known[file.exists(file.path(out_dir, paste0(known, ".html")))]
  # This batch's own dockets will exist by the time the run finishes, so they are
  # linkable even though they may not be written yet.
  unique(c(known, cases$dkt %||% character()))
}

# The dockets a given page will actually link to. Part of its manifest key.
link_targets <- function(cx, available) {
  refs <- c(cx$related %||% "", cx$linked %||% "")
  refs <- refs[!is.na(refs) & nzchar(refs)]
  if (!length(refs) || !length(available)) return(character())
  toks <- unlist(str_extract_all(paste(refs, collapse = ", "), "[0-9]{2}[-A][0-9]+"))
  sort(unique(toks[toks %in% available]))
}

# Render a comma-separated docket reference with the resolvable dockets linked.
# Prose in the field is preserved: "Vide, 25-566" keeps its "Vide," and links
# only the number. An unresolvable docket (pre-OT2017, renumbered, never
# fetched) stays plain text rather than becoming a 404.
docket_refs_html <- function(x, available = character()) {
  if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(x[1])) return("")
  parts <- str_split(x[1], ",")[[1]]
  out <- vapply(parts, function(p) {
    d <- str_squish(p)
    if (!nzchar(d)) return("")
    ok <- str_detect(d, "^[0-9]{2}[-A][0-9]+$") && d %in% available
    if (ok) sprintf("<a href='%s.html'>%s</a>", d, .esc(d)) else .esc(d)
  }, character(1), USE.NAMES = FALSE)
  paste(out[nzchar(out)], collapse = ", ")
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
  # Linkable dockets, fixed before the loop so no page's markup depends on the
  # order pages happen to be written in. See resolvable_dockets().
  available <- resolvable_dockets(cases, out_dir)

  n_written <- 0L; new_manifest <- manifest   # preserve entries for cases not in this batch
  for (i in seq_len(nrow(cases))) {
    cx <- cases[i, ]; dkt <- cx$dkt
    sig <- if (!is.null(signals_map)) signals_map[[dkt]] else NULL
    qp  <- if (!is.null(qp_map)) qp_map[[dkt]] %||% NA_character_ else NA_character_
    clr <- if (length(cls_by)) cls_by[[dkt]][1, ] else NULL
    # Hash every page-determining input (+ template + model); skip if unchanged.
    # link_targets() is in here because WHICH of this page's references resolve
    # is page-determining too: without it, a page that rendered a reference as
    # plain text would never re-render once that target's page appeared.
    key <- digest::digest(list(PAGE_TEMPLATE_VERSION, model_id, cx$caption, cx$events,
             cx$parties, cx$lower, cx$lower_dkt, cx$lower_date, cx$date, cx$type,
             qp, sig, cx$related, cx$linked, link_targets(cx, available)))
    if (incremental && identical(manifest[[dkt]] %||% "", key) &&
        file.exists(file.path(out_dir, paste0(dkt, ".html")))) {
      new_manifest[[dkt]] <- key; next
    }
    tryCatch({ docket_page(cx, out_dir, models = models, cls_row = clr, signals = sig,
                           qp = qp, rendered = rendered, available = available)
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

# ---- /cases/ browse index -----------------------------------------------------
# /cases/ held 55,167 files and no index.html, so it returned a 404 while every
# other section had one. That was survivable while nothing linked there; the case
# breadcrumb's middle crumb now does.
#
# Writes cases/index.html (the current term, by bucket, plus a term list) and one
# cases/ot<NN>.html per term (that term in full). Sourced entirely from
# search.json, which write_search_index() already maintains across the whole
# corpus -- no second index to keep in sync, and no filesystem walk of 55k files.
#
# Bucket rules mirror the fetcher's docket ranges (see scotus_dash_new.R):
# paid NN-1..NN-4999, IFP NN-5001.., applications NNA### (an "A", not a dash).
.docket_bucket <- function(dkt) {
  ifelse(grepl("^\\d{2}A\\d+$", dkt), "applications",
    ifelse(grepl("^\\d{2}-\\d+$", dkt),
      ifelse(suppressWarnings(as.integer(sub("^\\d{2}-", "", dkt))) >= 5000L,
             "ifp", "paid"),
      NA_character_))
}
.docket_term <- function(dkt) suppressWarnings(as.integer(substr(dkt, 1, 2)))
.docket_seq  <- function(dkt)
  suppressWarnings(as.integer(sub("^\\d{2}[-A]", "", dkt)))

BUCKET_LABELS <- list(paid = "Paid petitions", ifp = "In forma pauperis",
                      applications = "Applications")

write_cases_index <- function(cases_dir, n_recent = 60L) {
  ipath <- file.path(cases_dir, "search.json")
  if (!file.exists(ipath)) {
    message("write_cases_index(): no ", ipath, " -- skipping the /cases/ index.")
    return(invisible(NULL))
  }
  idx <- tryCatch(as.list(jsonlite::fromJSON(ipath)), error = function(e) NULL)
  if (is.null(idx) || !length(idx)) {
    message("write_cases_index(): search.json is empty or unreadable -- skipping.")
    return(invisible(NULL))
  }
  dkt <- names(idx)
  df <- data.frame(dkt = dkt, cap = unlist(idx, use.names = FALSE),
                   bucket = .docket_bucket(dkt), term = .docket_term(dkt),
                   seq = .docket_seq(dkt), stringsAsFactors = FALSE)
  df <- df[!is.na(df$bucket) & !is.na(df$term) & !is.na(df$seq), , drop = FALSE]
  if (!nrow(df)) return(invisible(NULL))
  # A page is only listed if it actually exists: search.json accumulates across
  # runs and can outlive a renumbered docket, and linking one would publish a 404.
  df <- df[file.exists(file.path(cases_dir, paste0(df$dkt, ".html"))), , drop = FALSE]
  if (!nrow(df)) return(invisible(NULL))

  row_html <- function(d) paste0(
    "<li><a class='row' href='", d$dkt, ".html'>",
    "<span class='d'>", htmlEscape(d$cap), "</span>",
    "<span class='count'>No. ", d$dkt, "</span></a></li>")
  section <- function(d, heading, note = NULL) {
    if (!nrow(d)) return("")
    paste0("<h2 class='csec'>", heading, "</h2>",
           if (!is.null(note)) paste0("<p class='cnote'>", note, "</p>") else "",
           "<ul class='idx'>",
           paste(vapply(seq_len(nrow(d)), function(i) row_html(d[i, ]), character(1)),
                 collapse = ""), "</ul>")
  }
  terms <- sort(unique(df$term), decreasing = TRUE)
  ot <- function(t) paste0("OT", if (t >= 90L) 1900L + t else 2000L + t)

  # ---- per-term pages ----
  for (t in terms) {
    dt <- df[df$term == t, , drop = FALSE]
    body <- paste0(vapply(names(BUCKET_LABELS), function(b) {
      d <- dt[dt$bucket == b, , drop = FALSE]
      d <- d[order(-d$seq), , drop = FALSE]
      section(d, paste0(BUCKET_LABELS[[b]], " <span class='cn'>",
                        format(nrow(d), big.mark = ","), "</span>"))
    }, character(1)), collapse = "")
    .write_cases_page(
      file.path(cases_dir, paste0("ot", t, ".html")),
      title = paste0(ot(t), " cases — Supreme Court Report"),
      heading = paste0(ot(t)), kicker = "Cases",
      dek = paste0(format(nrow(dt), big.mark = ","),
                   " dockets from October Term ", substr(ot(t), 3, 6), "."),
      crumb_label = ot(t), body = body,
      search = FALSE)
  }

  # ---- the hub ----
  cur <- terms[1]
  dc <- df[df$term == cur, , drop = FALSE]
  hub <- paste0(vapply(names(BUCKET_LABELS), function(b) {
    d <- dc[dc$bucket == b, , drop = FALSE]
    d <- d[order(-d$seq), , drop = FALSE]
    total <- nrow(d)
    shown <- utils::head(d, n_recent)
    note <- if (total > nrow(shown)) paste0(
      "Most recent ", nrow(shown), " of ", format(total, big.mark = ","),
      " &mdash; <a href='ot", cur, ".html'>see all of ", ot(cur), "</a>.") else NULL
    section(shown, paste0(BUCKET_LABELS[[b]], " <span class='cn'>",
                          format(total, big.mark = ","), "</span>"), note)
  }, character(1)), collapse = "")
  termlist <- paste0(
    "<h2 class='csec'>By Term</h2><ul class='terms'>",
    paste(vapply(terms, function(t) paste0(
      "<li><a href='ot", t, ".html'>", ot(t), "</a> <span class='cn'>",
      format(sum(df$term == t), big.mark = ","), "</span></li>"), character(1)),
      collapse = ""), "</ul>")
  .write_cases_page(
    file.path(cases_dir, "index.html"),
    title = "Cases — Supreme Court Report",
    heading = "Cases", kicker = "Supreme Court of the United States",
    dek = paste0("Every docket the Court has opened since ", ot(min(terms)),
                 " &mdash; ", format(nrow(df), big.mark = ","),
                 " in all. Search by name or number, or browse by Term."),
    crumb_label = "Cases", body = paste0(hub, termlist), search = TRUE)
  message("cases index: ", nrow(df), " dockets across ", length(terms), " term(s)")
  invisible(nrow(df))
}

# Small bespoke page writer: styled_index_page() renders one flat list, and these
# pages are grouped by bucket with per-section counts and notes. Reuses
# page_head() so the palette, fonts and NAV_CSS stay in one place.
.write_cases_page <- function(out_path, title, heading, kicker, dek,
                              crumb_label, body, search = FALSE) {
  crumb <- if (identical(crumb_label, "Cases")) NULL else CASES_CRUMB
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head(title, site_breadcrumb_jsonld(crumb_label, crumb)),
    "<body>", site_masthead(),
    "<main class='wrap' id='main'>",
    site_breadcrumb(crumb_label, crumb),
    "<p class='kicker'>", kicker, "</p><h1>", heading, "</h1>",
    "<hr class='brule'><p class='dek'>", dek, "</p>",
    if (isTRUE(search)) SEARCH_HTML else "",
    body,
    if (isTRUE(search)) search_script("search.json", "") else "",
    "</main></body>\n</html>\n")
  writeLines(enc2utf8(html), out_path, useBytes = TRUE)
  invisible(out_path)
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
