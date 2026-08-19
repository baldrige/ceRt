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
<div class="warn"><p class="note"><b>No standard errors.</b> This tier is a multinomial logit and <code>strip_multinom()</code> drops the Hessian before the artifact is saved, which keeps it at 3&nbsp;KB. SE, z and p cannot be recovered from the stored object &mdash; refit from the at-risk panel if you need them. Below are point estimates only, each the log-odds of that outcome <i>versus denial</i> at the same conference.</p></div>
<div class="coefwrap"><table class="coef"><thead><tr><th>Term</th><th>Relisted</th><th>Granted</th><th>GVR</th></tr></thead>
<tbody>%s</tbody></table></div></details>', nrow(cfc), crows)

coef_section <- paste0(
  '<h2>Coefficients</h2>\n',
  '<p>Every term in every deployed model, from the artifacts themselves. ',
  '&beta; is on the log-odds scale; the odds ratio is <span class="stat">e<sup>&beta;</sup></span>, ',
  'the multiplier on the odds of the outcome for a one-unit change, holding the rest fixed. ',
  'A <span class="sig">&bull;</span> marks p&nbsp;&lt;&nbsp;0.05 &mdash; a reading aid, not a finding: ',
  'this is eight Terms of observational data, nothing is corrected for the ',
  # The logit terms only. The conference tier reports no p-values at all, so its
  # 49 terms are not comparisons and counting them here would overstate the
  # correction that is missing.
  sprintf("%d", nrow(cf_all)),
  ' tests reported here, and a coefficient is not a cause.</p>\n',
  logit_block(ord("baseline"), "Baseline", "petition-stage, predicts grant"),
  logit_block(ord("enhanced"), "At-risk",  "granted ever"),
  logit_block(ord("gvr"),      "GVR",      "granted, vacated, remanded"),
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

html <- sprintf('<!DOCTYPE html><html lang="en"><head><script async src="/analytics.js"></script><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Predicting Certiorari &mdash; Methods Note</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,600&family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,600;1,6..72,400&display=swap">
@STYLE@</head><body><main class="sheet">
<p class="kicker">supremecourt.report &middot; methods note</p>
<h1>Predicting the Probability of Certiorari</h1>
<p class="dek">Three calibrated models estimating whether a <i>paid</i> petition will be granted plenary review.</p>

  <h2>The models</h2>
  <p><b>Baseline</b> &mdash; grant probability from case structure known at filing (shown on the daily petition dashboards). <b>Enhanced</b> &mdash; adds docket-development signals (conference reports). <b>GVR</b> &mdash; the companion &ldquo;hold&rdquo; risk of a grant-vacate-remand. IFP petitions (grant rate ~0.1%%) are a separate regime and excluded.</p>
  <h2>Data &amp; target</h2>
  <p>Eight Terms, OT2017&ndash;OT2024: <b>%s paid petitions</b>, of which <b>%s were granted</b>. Target is plenary <i>granted</i> vs. <i>denied</i>; GVRs, dismissals, and pending petitions are excluded from training. Every docket-development feature is snapshotted <i>strictly before</i> the decision date &mdash; leakage-safe.</p>
  <h2>Method</h2>
  <p>Logistic regression, Platt-calibrated. Validated <b>leave-one-term-out</b>: each Term is scored by a model trained on the other seven, and the calibration map is fitted out-of-fold. That is out-of-<i>fold</i>, not out-of-time — a rolling-origin check (train only on earlier Terms) reproduces it to within 0.003 AUC.</p>
  <h2>Validation (leave-one-term-out)</h2>
  <table><thead><tr><th>Model</th><th>Base rate</th><th>AUC</th><th>Avg.&nbsp;prec.</th><th>Brier</th></tr></thead><tbody>
    %s%s%s
  </tbody></table>
  <p class="note"><b>AUC</b> &mdash; chance the model ranks a random grant above a random denial. <b>Avg. precision</b> is the imbalance-aware metric (baseline %.3f vs. a %s base rate &asymp; %.1f&times; chance). <b>Brier</b> &mdash; the reference is a constant forecast at each model&rsquo;s own base rate, %s, %s and %s; all three beat it. Accuracy is meaningless at a %s base rate.</p>

<figure><img src="%s" alt="Calibration plot: predicted vs observed grant rate by decile">
  <figcaption>Predicted vs. observed grant rate by risk decile. Point size &prop; petitions in the bin.</figcaption></figure>
  <h2>What drives the estimate</h2>
  <ul>
    <li><b>U.S. as petitioner</b> (the Solicitor General): ~43%% granted vs. 3.7%% &mdash; the largest structural cue.</li>
    <li><b>Relists</b> (enhanced): non-monotonic &mdash; ~1%% at zero, ~20%% at one, ~44%% at two, falling to ~19%% at 5+ (the &ldquo;hold&rdquo; zone). Modeled as a bucket, not a line.</li>
    <li><b>Rule&nbsp;10 dissent / circuit split</b>, parsed from the petition PDF.</li>
    <li><b>Counsel track record</b> — prior petitions and prior wins, counted strictly before this petition was docketed.</li>
    <li><b>Court below</b>: federal circuits far above state courts.</li>
  </ul>

  <h2>Calibration</h2>
  <p>Predicted probabilities track observed frequencies across deciles: the enhanced model&rsquo;s top decile predicts %s and observes %s; the baseline&rsquo;s predicts %s and observes %s. A &ldquo;17%%&rdquo; means about 17%%.</p>
  <h2>Limitations</h2>
  <ul>
    <li>Rare outcome &mdash; only %s grants across eight Terms.</li>
    <li>OT2024 is right-censored (late petitions undecided): a pessimistic test Term.</li>
    <li>The dissent signal defaults to &ldquo;absent&rdquo; for the ~9%% of petitions with no parseable PDF, conservatively understating it.</li>
    <li>Entity typing and counsel matching are heuristic (regex).</li>
  </ul>

@COEFS@
<footer>Generated %s from the deployed model artifacts &middot; Full methods: <b>docs/cert_model.md</b> &middot; These are descriptive statistical estimates &mdash; <b>not legal advice, and not a prediction about any particular case</b>.</footer>
</main></body></html>',
  comma(mc(b)$n), comma(mc(b)$n_pos),
  row("Baseline (daily)", b), row("Enhanced (conference)", e), row("GVR companion", g),
  mc(b)$ap, pct(b$base_rate, 2), mc(b)$ap / b$base_rate,
  # One number no longer serves: the three tiers train on different frames and
  # so carry different base rates (4.1% / 7.8% / 5.4%), and a constant forecast
  # at rate p scores p(1-p). The note published a flat 0.042 for all three,
  # which was only ever the petition-stage figure.
  brier_ref(b), brier_ref(e), brier_ref(g), pct(b$base_rate, 1),
  img,
  pct(tail(e$calibration$pred,1)), pct(tail(e$calibration$obs,1)),
  pct(tail(b$calibration$pred,1)), pct(tail(b$calibration$obs,1)),
  comma(mc(b)$n_pos),
  format(Sys.Date(), "%B %Y"))

# The :root is substituted AFTER sprintf() so it does not have to survive the
# format string's %% escaping, and so palette.R stays the only place the values
# appear.
#
# No nav_max argument. This was the last caller in the codebase overriding it
# (with 7.4in, computed to sit flush on the old 8.1in print sheet); the masthead
# is uniformly SITE_NAV_MAX now and is meant to overhang the text column.
html <- sub("@COEFS@", coef_section, html, fixed = TRUE)
html <- sub("@STYLE@", style_css, html, fixed = TRUE)
html <- sub("@ROOT@", palette_root(), html, fixed = TRUE)
writeLines(html, "docs/cert_model_methods.html", useBytes = TRUE)
cat("wrote docs/cert_model_methods.html and docs/cert_model_calibration.png\n")
