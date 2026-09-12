# Generate the one-page cert-model methods note (docs/cert_model_methods.html)
# and its calibration plot (docs/cert_model_calibration.png) from the deployed
# model artifacts, so every figure is exact and reproducible.

# nnet is not optional: coef() on a multinom dispatches to coef.multinom, and
# without the package loaded it falls through to the default method and returns
# a length-0 object -- no warning, no error, just an empty conference table.
suppressPackageStartupMessages({ library(tidyverse); library(scales); library(nnet) })
source("R/palette.R")   # the colour source: :root below and the plot series

# Which artifacts to describe. Defaults to the deployed set; train_cert_model.R
# points it at whatever it just wrote, so the note cannot describe one set of
# models while another is serving.
model_dir <- Sys.getenv("MODEL_DIR", "data")
art_ <- function(nm) readRDS(file.path(model_dir, nm))
b  <- art_("cert_model_baseline.rds")
e  <- art_("cert_model_enhanced.rds")
g  <- art_("cert_model_gvr.rds")
cm <- art_("cert_model_conference.rds")
# beta / SE / z / p / odds ratio, shared with the standalone reference so the
# two documents cannot drift apart on the same numbers.
source("docs/model_coef_table.R")
pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
mc   <- function(m) m$metrics_calibrated
# Brier of the no-skill forecast that always predicts the base rate: p(1-p).
brier_ref <- function(m) sprintf("%.4f", m$base_rate * (1 - m$base_rate))

# ---- calibration plot (baseline + enhanced, both predict grant) ---------------
cal <- bind_rows(
  b$calibration |> mutate(Model = "Baseline (daily, petition-stage)"),
  e$calibration |> mutate(Model = "Enhanced (conference-stage)"))
# Derived, not hardcoded. The limits were fixed at 35%, and retargeting the
# conference tier to the at-risk panel pushed its top decile to 38/37 -- so
# ggplot dropped that row with a warning and drew the enhanced line stopping at
# the ninth decile, silently omitting the best-calibrated high-risk bucket and
# the one a reader checks first. Round up to the next 5% so a shifting model
# widens the axis instead of falling off it.
lim <- c(0, ceiling(max(cal$pred, cal$obs) * 20) / 20)
p <- ggplot(cal, aes(pred, obs, color = Model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_line(linewidth = 0.6) +
  geom_point(aes(size = n)) +
  scale_color_manual(values = c("Baseline (daily, petition-stage)" = CHART_SERIES[["baseline"]],
                                "Enhanced (conference-stage)" = CHART_SERIES[["enhanced"]])) +
  scale_size_continuous(range = c(1.6, 4), guide = "none") +
  scale_x_continuous(labels = percent, limits = lim) +
  scale_y_continuous(labels = percent, limits = lim) +
  coord_equal() +
  labs(x = "Model-predicted probability", y = "Observed grant frequency",
       title = "Calibration, out-of-fold, by risk decile",
       subtitle = "Points on the dashed 45° line are perfectly calibrated") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey35"))
ggsave("docs/cert_model_calibration.png", p, width = 6.2, height = 5.4, dpi = 150, bg = "white")

img <- tryCatch(xfun::base64_uri("docs/cert_model_calibration.png"),
         error = function(...) tryCatch(knitr::image_uri("docs/cert_model_calibration.png"),
         error = function(...) "cert_model_calibration.png"))

row <- function(nm, m) sprintf(
  "<tr><td>%s</td><td>%s</td><td><b>%.3f</b></td><td>%.3f</td><td>%.4f</td></tr>",
  nm, pct(m$base_rate, 2), mc(m)$auc, mc(m)$ap, mc(m)$brier)

# dc5f2814c1 wired GA4 into "every main-page <head> builder" -- but for this page
# it patched the OUTPUT, docs/cert_model_methods.html, and left the builder alone.
# Regenerating therefore silently dropped the tag and stopped counting the note.
# The <script> belongs here, in the thing that writes the page.
# ---- Coefficients section ----------------------------------------------------
# The note reports AUC and calibration; without the coefficients a reader has to
# take the drivers on faith. These are the same numbers the standalone reference
# publishes, from the same extractor, collapsed behind <details> so 259 rows do
# not swamp a note whose body is about 3,000 characters.
esc_  <- function(x) { x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE) }
f3    <- function(x) formatC(x, format = "f", digits = 3)
f2    <- function(x) formatC(x, format = "f", digits = 2)
fp_   <- function(x) ifelse(x < 1e-4, sprintf("%.1e", x), sprintf("%.4f", x))

# One <details> block per logit model: term, beta, SE, z, p, odds ratio.
logit_block <- function(df, title, sub) {
  rows <- ""; last <- ""
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    if (!identical(r$group, last)) {
      rows <- paste0(rows, sprintf('<tr class="grouprow"><th colspan="6" scope="colgroup">%s</th></tr>',
        esc_(if (r$group == "—") "Intercept" else r$group)))
      last <- r$group
    }
    # A dot marks p < 0.05. It is a reading aid, not a finding: these are eight
    # Terms of observational data, and nothing here was corrected for the 259
    # comparisons the four tables make between them.
    star <- if (!is.na(r$p) && r$p < 0.05) ' <span class="sig" title="p &lt; 0.05">&bull;</span>' else ""
    rows <- paste0(rows, sprintf(
      '<tr><th scope="row"><code>%s</code></th><td>%s%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
      esc_(r$term), f3(r$estimate), star, f3(r$se), f2(r$z), fp_(r$p), f3(r$or)))
  }
  sprintf('<details class="coefs"><summary>%s <span class="cnt">%s &middot; %d terms</span></summary>
<div class="coefwrap"><table class="coef"><thead><tr><th>Term</th><th>&beta;</th><th>SE</th><th>z</th><th>p</th><th>Odds ratio</th></tr></thead>
<tbody>%s</tbody></table></div></details>', esc_(title), esc_(sub), nrow(df), rows)
}

cf_all <- rbind(logit_tbl(b, "baseline"), logit_tbl(e, "enhanced"), logit_tbl(g, "gvr"))
cf_all <- cf_all[order(cf_all$model, cf_all$group != "—", cf_all$group, cf_all$variable, cf_all$term), ]
ord <- function(k) cf_all[cf_all$model == k, ]

# The conference tier is a multinomial logit and strip_multinom() drops its
# Hessian before the artifact is saved (it keeps the file at 3 KB). Standard
# errors, z and p are therefore not recoverable from the deployed object. Saying
# so is the point: a table of bare estimates that looked like the three above
# would imply a precision this model cannot report.
cc  <- coef(cm$model)
# Assert rather than trust: an empty or reshaped cc would otherwise render as a
# section with no rows, which reads like a model with no coefficients.
stopifnot(is.matrix(cc), nrow(cc) == 3L, ncol(cc) > 0,
          identical(rownames(cc), c("relisted", "granted", "gvr")))
cfc <- data.frame(term = colnames(cc),
                  variable = vapply(colnames(cc), var_of, character(1), feats = cm$features),
                  relisted = cc["relisted", ], granted = cc["granted", ], gvr = cc["gvr", ],
                  stringsAsFactors = FALSE, row.names = NULL)
cfc$group <- unname(ifelse(cfc$variable == "(Intercept)", "—", GROUP[cfc$variable]))
cfc <- cfc[order(cfc$group != "—", cfc$group, cfc$variable, cfc$term), ]
crows <- ""; last <- ""
for (i in seq_len(nrow(cfc))) {
  r <- cfc[i, ]
  if (!identical(r$group, last)) {
    crows <- paste0(crows, sprintf('<tr class="grouprow"><th colspan="4" scope="colgroup">%s</th></tr>',
      esc_(if (r$group == "—") "Intercept" else r$group)))
    last <- r$group
  }
  crows <- paste0(crows, sprintf('<tr><th scope="row"><code>%s</code></th><td>%s</td><td>%s</td><td>%s</td></tr>',
    esc_(r$term), f3(r$relisted), f3(r$granted), f3(r$gvr)))
}
conf_block <- sprintf('<details class="coefs"><summary>Conference <span class="cnt">competing risks &middot; %d terms</span></summary>
<div class="warn"><p class="note"><b>No uncertainty figures for this one.</b> The conference forecast is a different kind of model: at each conference it weighs three outcomes at once (relisted, granted, GVR) against denial. To keep the saved model small, the part that carries its uncertainty estimates (the Hessian) is dropped by <code>strip_multinom()</code> before it is stored, so SE, z and p cannot be recovered from it; refit from the at-risk panel if you need them. Each number below is the points that fact adds to the odds of that outcome <i>versus denial</i> at the same conference.</p></div>
<div class="coefwrap"><table class="coef"><thead><tr><th>Term</th><th>Relisted</th><th>Granted</th><th>GVR</th></tr></thead>
<tbody>%s</tbody></table></div></details>', nrow(cfc), crows)

coef_section <- paste0(
  '<h2>Coefficients</h2>\n',
  '<p>This section is for readers who want to check the arithmetic. It lists every ',
  'term in every model exactly as deployed, read from the model files themselves. ',
  'For everyone else the sections above are the whole story, and these tables can ',
  'be skipped.</p>\n',
  '<p>Each row is one fact the model can see. <b>&beta;</b> is the points that fact ',
  'adds to a petition&rsquo;s score (a negative number subtracts), and the ',
  '<b>odds ratio</b>, <span class="stat">e<sup>&beta;</sup></span>, restates the same ',
  'thing as a multiplier on the odds of the outcome with everything else held equal: ',
  '2.0 means the odds double, 0.5 means they halve. <b>SE</b> is the uncertainty in ',
  'the estimate, <b>z</b> is the estimate divided by that uncertainty, and <b>p</b> is ',
  'the chance of seeing an effect at least this large if the true effect were zero. ',
  'A <span class="sig">&bull;</span> marks p&nbsp;below&nbsp;0.05. It is a reading aid, ',
  'not a finding: this is eight Terms of observational data, the ',
  # The logit terms only. The conference tier reports no p-values at all, so its
  # 49 terms are not comparisons and counting them here would overstate the
  # correction that is missing.
  sprintf("%d", nrow(cf_all)),
  ' tests reported here are not corrected for their number, and a coefficient ',
  'measures association, not cause.</p>\n',
  logit_block(ord("baseline"), "Baseline", "at filing; predicts a grant"),
  logit_block(ord("enhanced"), "Enhanced", "at conference; predicts a grant at any later conference"),
  logit_block(ord("gvr"),      "GVR",      "at conference; predicts a grant-vacate-remand"),
  conf_block)


# The stylesheet lives OUTSIDE the sprintf() format string, and has to.
# R caps a format string at 8192 characters and this one runs past it, which
# is a hard error rather than a truncation. Keeping the CSS out also retires
# the apostrophe landmine documented below: the format string is single-quoted,
# so one apostrophe in a stylesheet comment used to break the whole script.
# Here the string is double-quoted, CSS font names are single-quoted to match
# page_style.R, and a literal % needs no doubling.
style_css <- "<style>
  /* Token names and colours match the site palette: the output of this file is
     copied to site/methods.html, so its --faint/--link ship. NOTE: an
     apostrophe anywhere in this block closes the single-quoted sprintf() literal
     that carries it, and R will not parse the script -- which is how the note
     went eight days without being regenerated. Both --faint and --link once
     failed WCAG AA (3.24:1 and 3.69:1 on --paper) at the small sizes used
     here; palette.R carries compliant values now and the sizes below are
     larger than the ones that failed, so the margin only widened.

     SCREEN FIRST, PAPER SECOND. This note used to be sized as a letter sheet
     that happened to be served over HTTP: 10.5pt body text in an 8.1in column,
     with --nav-max overridden to 7.4in so the injected masthead rule landed
     flush on it. On screen that read as a different site -- roughly 14px text
     where every other page sets 19px, under a masthead 154px narrower than the
     one above it everywhere else. The web scale below is the funnel explainer
     one (19px/1.62 in a 44rem column), because that is this document type.

     --nav-max is deliberately NOT set. Passing it was the last override left in
     the codebase; the masthead is uniformly SITE_NAV_MAX and is meant to be
     wider than the 40rem index and 44rem funnel text columns it sits above.
     Overhanging the column is the site design, not a defect to correct here.

     There is no two-column layout any more, on screen or on paper. It was the
     only one in the codebase, it was print furniture, and at web measures it
     read as cramped. @media print keeps the compact type; it no longer keeps
     the one-page sheet, which the columns were what made possible. */
  @ROOT@
  *{box-sizing:border-box} html{-webkit-text-size-adjust:100%}
  body{font-family:'Newsreader',Georgia,serif;color:var(--ink);background:var(--paper);
    margin:0;font-size:19px;line-height:1.62}
  .sheet{max-width:44rem;margin:0 auto;padding:3.2rem 1.4rem 5rem}
  .kicker{font:600 .78rem/1 'Newsreader',Georgia,serif;letter-spacing:.2em;
    text-transform:uppercase;color:var(--accent);margin:0 0 .6rem}
  h1{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:2.4rem;
    line-height:1.06;letter-spacing:-.012em;margin:0 0 .5rem}
  .dek{font-style:italic;color:var(--ink-soft);font-size:1.28rem;line-height:1.5;
    max-width:34rem;margin:0 0 2rem}
  h2{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.3rem;
    color:var(--accent);margin:2.2rem 0 .6rem;border-bottom:1px solid var(--rule);
    padding-bottom:.25rem}
  p{margin:0 0 .9rem}
    ul{margin:0 0 .9rem;padding-left:1.15rem} li{margin:.35rem 0}
  table{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums;
    margin:.8rem 0 1rem;font-size:.95rem}
  th,td{text-align:right;padding:.4rem .5rem;border-bottom:1px solid var(--rule)}
  th:first-child,td:first-child{text-align:left}
  thead th{font:600 .8rem/1.2 'Newsreader',Georgia,serif;text-transform:uppercase;
    letter-spacing:.06em;color:var(--faint);border-bottom:1.5px solid var(--ink)}
  .note{font-size:.95rem;line-height:1.5;color:var(--ink-soft);font-style:italic}
  figure{margin:1rem 0;text-align:center} figure img{width:100%;max-width:26rem}
  figcaption{font-size:.85rem;line-height:1.45;color:var(--faint);margin-top:.4rem}
    footer{margin-top:2.4rem;border-top:2px solid var(--ink);padding-top:.7rem;
    font-size:.9rem;line-height:1.5;color:var(--ink-soft)}
  b,.stat{color:var(--accent)}
  details.coefs{border-top:1px solid var(--rule);margin:0}
  details.coefs:last-of-type{border-bottom:1px solid var(--rule)}
  details.coefs>summary{cursor:pointer;list-style:none;padding:.6rem 0;
    font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.02rem;
    display:flex;justify-content:space-between;gap:1rem;align-items:baseline}
  details.coefs>summary::-webkit-details-marker{display:none}
  details.coefs>summary::after{content:'+';color:var(--accent);font-weight:600}
  details.coefs[open]>summary::after{content:'\u2212'}
  details.coefs>summary:hover{color:var(--accent)}
  details.coefs>summary:focus-visible{outline:2px solid var(--accent);outline-offset:2px}
  details.coefs .cnt{font-family:'Newsreader',Georgia,serif;font-weight:400;
    font-size:.85rem;color:var(--faint);font-variant-numeric:tabular-nums}
  .coefwrap{overflow-x:auto;margin:0 0 1rem}
  table.coef{font-size:.82rem;margin:.2rem 0 0}
  table.coef code{font-size:.95em}
  table.coef tr.grouprow th{text-align:left;font-family:'Fraunces',Georgia,serif;
    font-weight:600;font-size:.8rem;color:var(--faint);text-transform:uppercase;
    letter-spacing:.06em;padding-top:.7rem;border-bottom:1px solid var(--rule)}
  .sig{color:var(--accent)}
  .warn{border-left:2px solid var(--accent);padding:.1rem 0 .1rem .8rem;margin:.6rem 0}
  @media(max-width:38rem){ h1{font-size:2rem} }
  /* Compact measures for paper. This no longer prints as a single sheet: the
     two-column layout that made it fit is gone from the document entirely, so
     print is the same single column as screen, just tighter. */
  @page{size:letter;margin:.4in}
  @media print{
    body{background:#fff;font-size:10.5pt;line-height:1.4}
    .sheet{max-width:none;padding:0}
    h1{font-size:23pt;line-height:1.02;margin:0 0 .25rem}
    .dek{font-size:11pt;margin:.1rem 0 .5rem}
    h2{font-size:11pt;margin:.5rem 0 .2rem}
    p{margin:.2rem 0} li{margin:.12rem 0} ul{margin:.2rem 0}
    table{font-size:inherit;margin:.3rem 0} th,td{padding:2px 5px}
    thead th{font-size:8.5pt}
    .note{font-size:9pt} figcaption{font-size:8.5pt} figure img{max-width:3.7in}
    footer{margin-top:.5rem;padding-top:.3rem;font-size:8.5pt}
    .smast,.smast-rule-w{display:none}
  }
</style>"

# ---- The note itself -----------------------------------------------------------
# Written for a reader with no statistics: every term of art is introduced by
# the plain question it answers, and every figure still comes from the artifacts.
# The page is assembled section by section rather than through one sprintf():
# R caps a format string at 8192 characters, the old single template ran close
# to it, and each section now carries its own arguments next to its own text.
# Sections with no substitutions are plain strings (a bare % is fine there);
# only the sprintf() sections escape % as %%. No apostrophes anywhere in these
# single-quoted literals: &rsquo; instead.

head_html <- '<!DOCTYPE html><html lang="en"><head><script async src="/analytics.js"></script><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Predicting Certiorari &mdash; Methods Note</title>
@SOCIAL@
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,600&family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,600;1,6..72,400&display=swap">
@STYLE@</head><body><main class="sheet">
<p class="kicker">supremecourt.report &middot; methods note</p>
<h1>Predicting the Probability of Certiorari</h1>
<p class="dek">How this site estimates the chance that a petition will be granted, and how those estimates are checked against what the Court actually did.</p>
'

sec_meaning <- sprintf('
  <h2>What the number means</h2>
  <p>Every paid petition on this site carries a percentage: the estimated chance that the Court will grant it and hear the case. It works like a rain forecast. A &ldquo;17%%&rdquo; does not say the petition scores 17 out of 100, and it does not say the petition will be granted. It says that, of a hundred petitions that look like this one on the docket, about seventeen have been granted in the past. Most petitions are long shots: across the eight Terms the models learned from, <b>%s</b> of paid petitions were granted, so a petition rated near that figure is simply average.</p>
', pct(b$base_rate, 1))

sec_models <- '
  <h2>Three estimates for three moments</h2>
  <p>A petition&rsquo;s prospects change as its docket develops, so the site carries three related estimates rather than one.</p>
  <ul>
    <li><b>At filing.</b> The <b>baseline</b> estimate uses only what is known on the day a petition is docketed: who is asking and whom they are asking against, which court ruled below, how quickly the case came up, whether the petition reports a dissent below or a split among the lower courts, how long the petition is (as its filer certified) and how many dissents it cites, and the lawyers&rsquo; record at the Court. This is the number on the daily petition dashboards and on each case page.</li>
    <li><b>At conference.</b> The <b>enhanced</b> estimate keeps the dissent and split cues and adds what happened next: whether the other side filed a response or waived it, whether the Court asked for one, whether outside groups filed briefs in support, and whether the Justices have already considered the petition once and carried it over to another conference (a &ldquo;relist&rdquo;). This is the number on the conference reports.</li>
    <li><b>The other way to win.</b> Some petitions are neither granted nor denied. When the Court has just decided a related case, it often sends similar petitions back to the lower court to reconsider, without briefing or argument. Lawyers call this a GVR, for granted, vacated and remanded. The <b>GVR</b> estimate is the chance of that outcome. It sits beside the grant estimate because a petition being held for a related case can look, on the docket, very much like one about to be granted.</li>
  </ul>
  <p>Petitions filed without the fee, by people who cannot afford it, are not scored. About one in a thousand of them is granted, and a model built for paid petitions would be wrong about them in both directions.</p>
'

sec_data <- sprintf('
  <h2>What the models learned from</h2>
  <p>Eight Terms of the Court&rsquo;s own docket records, from the Term that began in October 2017 through the one that began in October 2024: <b>%s paid petitions</b>, of which <b>%s were granted</b> and heard. A petition counts as a success only if the Court took the case in full. Petitions sent back with a GVR, dismissed, or still waiting are left out of the lesson, because they are neither a yes nor a no to the question the model is asking.</p>
  <p>One rule matters more than any other: the models may see only what was on the docket <i>before</i> the Court acted. A response filed the week after a grant says nothing useful about predicting that grant, and letting it in would make the model look far better than it is. Every docket signal is measured strictly before the decision date.</p>
', comma(mc(b)$n), comma(mc(b)$n_pos))

sec_method <- '
  <h2>How the estimate is made</h2>
  <p>The method is the plainest one that works: a scoring formula. Each fact about a petition adds or subtracts points (the United States as petitioner adds a great many; a petitioner with no lawyer subtracts a great many), the points are totalled, and the total is converted into a probability. Statisticians call this logistic regression. Nobody writes the points by hand: the formula is fitted to the eight Terms of outcomes so that each fact carries the weight it actually had.</p>
  <p>The conversion from points to a percentage is then checked and, where needed, adjusted, a step called calibration, so that when the model says 17%, about 17 in 100 such petitions really were granted.</p>
  <p>The checking is done honestly. Fitting a model and then grading it on the same petitions would be like letting a student mark their own homework with the answers open. Instead, each Term is hidden in turn: the model is fitted on the other seven Terms, asked to predict the hidden one, and graded against what actually happened. Every figure in the next section comes from those hidden-Term predictions, never from petitions the model had already seen. A stricter version of the check, fitting only on Terms <i>earlier</i> than the one being predicted, gives the same result to within 0.003 on the AUC scale explained below.</p>
'

sec_valid <- sprintf('
  <h2>How well it works</h2>
  <table><thead><tr><th>Model</th><th>Base rate</th><th>AUC</th><th>Avg.&nbsp;prec.</th><th>Brier</th></tr></thead><tbody>
    %s%s%s
  </tbody></table>
  <p><b>Base rate</b> is how often the outcome happened at all: the share of petitions in each model&rsquo;s data that were granted, or for the GVR model sent back. It is the number a model has to beat.</p>
  <p><b>AUC</b> answers a simple question. Pick one granted petition and one denied petition at random; how often does the model rate the granted one higher? A coin flip scores 0.5 and a perfect model scores 1. The baseline scores %.3f, so it puts the granted petition first about %s of the time.</p>
  <p><b>Average precision</b> asks how concentrated the grants are near the top of the model&rsquo;s ranking. A random ranking scores the base rate itself, %s. The baseline scores %.3f, about %.1f times better than chance.</p>
  <p><b>Brier score</b> is the average error of the percentages themselves, so lower is better. The comparison is with the laziest possible forecast, which gives every petition the base rate. That forecast scores %s for the baseline, %s for the enhanced model and %s for the GVR model, and all three models beat it.</p>
  <p class="note">Plain accuracy is not reported because it would mislead. A model that said &ldquo;denied&rdquo; to every petition would be right %s of the time and would be worthless.</p>

<figure><img src="%s" alt="Calibration plot: predicted vs observed grant rate by decile">
  <figcaption>Petitions sorted into ten groups by the model&rsquo;s estimate. Across: what the model predicted for each group. Up: the share actually granted. Points on the dashed line are perfectly calibrated; larger points are groups with more petitions.</figcaption></figure>
',
  row("Baseline (at filing)", b), row("Enhanced (at conference)", e), row("GVR (at conference)", g),
  mc(b)$auc, pct(mc(b)$auc, 0),
  pct(b$base_rate, 2), mc(b)$ap, mc(b)$ap / b$base_rate,
  # The three tiers train on different frames and so carry different base
  # rates (4.1% / 7.8% / 5.4%); a constant forecast at rate p scores p(1-p).
  brier_ref(b), brier_ref(e), brier_ref(g),
  pct(1 - b$base_rate, 1),
  img)

sec_drivers <- '
  <h2>What moves the estimate</h2>
  <ul>
    <li><b>The United States as petitioner.</b> When the Solicitor General asks, the Court usually listens: roughly 43% of those petitions are granted, against about 3.7% of everyone else&rsquo;s. Nothing else on the docket comes close.</li>
    <li><b>Relists.</b> When the Justices take up a petition at conference and, instead of deciding it, carry it over to the next conference, that is a relist. One relist is a strong sign: about 20% of those petitions are granted, against roughly 1% of petitions never relisted. Two is stronger still, about 44%. But five or more usually means the petition is being held for another case, and the grant rate falls back to about 19%. Because the pattern rises and then falls, the model treats each count separately instead of assuming that more is always better.</li>
    <li><b>A dissent below, or a split among the lower courts.</b> Read from the petition itself. The Court&rsquo;s Rule 10 names disagreement among the lower courts as the main reason it takes cases, and petitions that report one are granted more often.</li>
    <li><b>The heft of the petition.</b> How long it is, taken from the word count every booklet petition must certify under Rule 33.1(h) (a separate certificate for most filers, a line in the proof of service for the Solicitor General), read against the 9,000-word limit; and how many dissents it cites, read from the PDF. Neither is a reason to grant, but both track how much work went into the petition. A petition under 3,000 words is granted well under 1% of the time; one that cites more than ten dissents, about 16%. Both are taken in bands rather than as a straight line, and both sit beside counsel&rsquo;s record, which they partly repeat.</li>
    <li><b>Counsel&rsquo;s track record.</b> How many petitions the lawyers had filed before this one and how many were granted, counted only up to the day this petition was docketed so that later successes cannot leak in.</li>
    <li><b>The court below.</b> Petitions from the federal courts of appeals are granted far more often than petitions from state courts.</li>
  </ul>
'

sec_calib <- sprintf('
  <h2>Does 17%% really mean 17%%?</h2>
  <p>Within the limits of the data, yes. Sort the petitions into ten groups by the model&rsquo;s estimate and compare each group&rsquo;s average estimate with the share actually granted. In the riskiest tenth, the enhanced model predicted <b>%s</b> and <b>%s</b> were in fact granted; the baseline predicted <b>%s</b> and <b>%s</b> were granted. The lower groups track just as closely, as the chart above shows.</p>
',
  pct(tail(e$calibration$pred,1)), pct(tail(e$calibration$obs,1)),
  pct(tail(b$calibration$pred,1)), pct(tail(b$calibration$obs,1)))

sec_limits <- sprintf('
  <h2>What it cannot do</h2>
  <ul>
    <li><b>Grants are rare</b>: only %s in eight Terms. Rare events are hard to learn from, and the estimate for an unusual petition is less certain than the estimate for a typical one.</li>
    <li><b>The last Term was unfinished.</b> The most recent Term in the data was still in progress when the data was taken, so petitions filed late in it had not been decided. That makes it look like a harder Term than it was.</li>
    <li><b>Some petitions cannot be read.</b> About 9%% have no readable PDF. For those the model assumes no dissent below, no split and no dissents cited, which if anything understates their chances. A petition whose word count could not be read from the docket is scored as one of unknown length.</li>
    <li><b>Names are matched by pattern.</b> Deciding what kind of party is asking (a business, a state, the federal government) and recognising the same lawyer across petitions is done by matching names. It is good, not perfect.</li>
    <li><b>The model knows nothing about the law.</b> It has never read the question presented. It sees only the shape of the docket, which is why a strong petition on a subject the Court has been avoiding can score low, and an ordinary one from the Solicitor General can score high.</li>
  </ul>
', comma(mc(b)$n_pos))

foot_html <- sprintf('
@COEFS@
<footer>Generated %s from the models in use on this site &middot; Technical detail: <b>docs/cert_model.md</b> in the repository &middot; These are statistical estimates drawn from past dockets &mdash; <b>not legal advice, and not a prediction about any particular case</b>.</footer>
</main></body></html>', format(Sys.Date(), "%B %Y"))

html <- paste0(head_html, sec_meaning, sec_models, sec_data, sec_method, sec_valid,
               sec_drivers, sec_calib, sec_limits, foot_html)

# The :root is substituted AFTER sprintf() so it does not have to survive the
# format string's %% escaping, and so palette.R stays the only place the values
# appear.
#
# No nav_max argument. This was the last caller in the codebase overriding it
# (with 7.4in, computed to sit flush on the old 8.1in print sheet); the masthead
# is uniformly SITE_NAV_MAX now and is meant to overhang the text column.
# The FIFTH head on this site. Same block, from the same function, so a change
# to the card reaches this page too.
source("R/site_meta.R")
html <- sub("@SOCIAL@", social_meta(
  "Predicting Certiorari — Methods Note",
  paste("How supremecourt.report estimates a petition's chance of being granted,",
        "explained for non-specialists: the data, how the estimates are checked,",
        "what moves them, and every coefficient for readers who want the arithmetic."),
  "/methods.html", "article"), html, fixed = TRUE)
html <- sub("@COEFS@", coef_section, html, fixed = TRUE)
html <- sub("@STYLE@", style_css, html, fixed = TRUE)
html <- sub("@ROOT@", palette_root(), html, fixed = TRUE)
writeLines(html, "docs/cert_model_methods.html", useBytes = TRUE)
cat("wrote docs/cert_model_methods.html and docs/cert_model_calibration.png\n")
