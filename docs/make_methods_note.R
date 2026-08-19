# Generate the one-page cert-model methods note (docs/cert_model_methods.html)
# and its calibration plot (docs/cert_model_calibration.png) from the deployed
# model artifacts, so every figure is exact and reproducible.

suppressPackageStartupMessages({ library(tidyverse); library(scales) })
source("R/palette.R")   # the colour source: :root below and the plot series

b <- readRDS("data/cert_model_baseline.rds")
e <- readRDS("data/cert_model_enhanced.rds")
g <- readRDS("data/cert_model_gvr.rds")
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

     The letter sheet is not lost, only moved: @media print below restores the
     compact measures, so the note still prints as the one-pager it was built
     to be. */
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
  .grid{display:block}
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
  .fig-wrap{display:block}
  footer{margin-top:2.4rem;border-top:2px solid var(--ink);padding-top:.7rem;
    font-size:.9rem;line-height:1.5;color:var(--ink-soft)}
  b,.stat{color:var(--accent)}
  /* The two-column .grid and the figure pair are PRINT layout, restored in the
     print block below. Holding them on screen would set body prose about 40
     characters to the line inside a 44rem column; letting them run full width
     gives roughly 65, which is the measure the rest of the site reads at. */
  @media(max-width:38rem){ h1{font-size:2rem} }
  /* The original letter sheet, restored for paper only. */
  @page{size:letter;margin:.4in}
  @media print{
    body{background:#fff;font-size:10.5pt;line-height:1.4}
    .sheet{max-width:none;padding:0}
    h1{font-size:23pt;line-height:1.02;margin:0 0 .25rem}
    .dek{font-size:11pt;margin:.1rem 0 .5rem}
    h2{font-size:11pt;margin:.5rem 0 .2rem}
    p{margin:.2rem 0} li{margin:.12rem 0} ul{margin:.2rem 0}
    .grid{display:grid;grid-template-columns:1fr 1fr;gap:0 1.3rem}
    .fig-wrap{display:grid;grid-template-columns:1.1fr 1fr;gap:1.3rem;align-items:center}
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

<div class="grid">
  <div>
    <h2>The models</h2>
    <p><b>Baseline</b> &mdash; grant probability from case structure known at filing (shown on the daily petition dashboards). <b>Enhanced</b> &mdash; adds docket-development signals (conference reports). <b>GVR</b> &mdash; the companion &ldquo;hold&rdquo; risk of a grant-vacate-remand. IFP petitions (grant rate ~0.1%%) are a separate regime and excluded.</p>
    <h2>Data &amp; target</h2>
    <p>Eight Terms, OT2017&ndash;OT2024: <b>%s paid petitions</b>, of which <b>%s were granted</b>. Target is plenary <i>granted</i> vs. <i>denied</i>; GVRs, dismissals, and pending petitions are excluded from training. Every docket-development feature is snapshotted <i>strictly before</i> the decision date &mdash; leakage-safe.</p>
    <h2>Method</h2>
    <p>Logistic regression, Platt-calibrated. Validated <b>leave-one-term-out</b>: each Term is scored by a model trained on the other seven, and the calibration map is fitted out-of-fold. That is out-of-<i>fold</i>, not out-of-time — a rolling-origin check (train only on earlier Terms) reproduces it to within 0.003 AUC.</p>
  </div>
  <div>
    <h2>Validation (leave-one-term-out)</h2>
    <table><thead><tr><th>Model</th><th>Base rate</th><th>AUC</th><th>Avg.&nbsp;prec.</th><th>Brier</th></tr></thead><tbody>
      %s%s%s
    </tbody></table>
    <p class="note"><b>AUC</b> &mdash; chance the model ranks a random grant above a random denial. <b>Avg. precision</b> is the imbalance-aware metric (baseline %.3f vs. a %s base rate &asymp; %.1f&times; chance). <b>Brier</b> &mdash; the reference is a constant forecast at each model&rsquo;s own base rate, %s, %s and %s; all three beat it. Accuracy is meaningless at a %s base rate.</p>
  </div>
</div>

<div class="fig-wrap">
  <figure><img src="%s" alt="Calibration plot: predicted vs observed grant rate by decile">
    <figcaption>Predicted vs. observed grant rate by risk decile. Point size &prop; petitions in the bin.</figcaption></figure>
  <div>
    <h2>What drives the estimate</h2>
    <ul>
      <li><b>U.S. as petitioner</b> (the Solicitor General): ~43%% granted vs. 3.7%% &mdash; the largest structural cue.</li>
      <li><b>Relists</b> (enhanced): non-monotonic &mdash; ~1%% at zero, ~20%% at one, ~44%% at two, falling to ~19%% at 5+ (the &ldquo;hold&rdquo; zone). Modeled as a bucket, not a line.</li>
      <li><b>Rule&nbsp;10 dissent / circuit split</b>, parsed from the petition PDF.</li>
      <li><b>Counsel track record</b> — prior petitions and prior wins, counted strictly before this petition was docketed.</li>
      <li><b>Court below</b>: federal circuits far above state courts.</li>
    </ul>
  </div>
</div>

<div class="grid">
  <div>
    <h2>Calibration</h2>
    <p>Predicted probabilities track observed frequencies across deciles: the enhanced model&rsquo;s top decile predicts %s and observes %s; the baseline&rsquo;s predicts %s and observes %s. A &ldquo;17%%&rdquo; means about 17%%.</p>
  </div>
  <div>
    <h2>Limitations</h2>
    <ul>
      <li>Rare outcome &mdash; only %s grants across eight Terms.</li>
      <li>OT2024 is right-censored (late petitions undecided): a pessimistic test Term.</li>
      <li>The dissent signal defaults to &ldquo;absent&rdquo; for the ~9%% of petitions with no parseable PDF, conservatively understating it.</li>
      <li>Entity typing and counsel matching are heuristic (regex).</li>
    </ul>
  </div>
</div>

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
html <- sub("@STYLE@", style_css, html, fixed = TRUE)
html <- sub("@ROOT@", palette_root(), html, fixed = TRUE)
writeLines(html, "docs/cert_model_methods.html", useBytes = TRUE)
cat("wrote docs/cert_model_methods.html and docs/cert_model_calibration.png\n")
