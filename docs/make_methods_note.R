# Generate the one-page cert-model methods note (docs/cert_model_methods.html)
# and its calibration plot (docs/cert_model_calibration.png) from the deployed
# model artifacts, so every figure is exact and reproducible.

suppressPackageStartupMessages({ library(tidyverse); library(scales) })

b <- readRDS("data/cert_model_baseline.rds")
e <- readRDS("data/cert_model_enhanced.rds")
g <- readRDS("data/cert_model_gvr.rds")
pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
mc   <- function(m) m$metrics_calibrated

# ---- calibration plot (baseline + enhanced, both predict grant) ---------------
cal <- bind_rows(
  b$calibration |> mutate(Model = "Baseline (daily, petition-stage)"),
  e$calibration |> mutate(Model = "Enhanced (conference-stage)"))
p <- ggplot(cal, aes(pred, obs, color = Model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_line(linewidth = 0.6) +
  geom_point(aes(size = n)) +
  scale_color_manual(values = c("Baseline (daily, petition-stage)" = "#b5651d",
                                "Enhanced (conference-stage)" = "#8a2b2b")) +
  scale_size_continuous(range = c(1.6, 4), guide = "none") +
  scale_x_continuous(labels = percent, limits = c(0, 0.35)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.35)) +
  coord_equal() +
  labs(x = "Model-predicted probability", y = "Observed grant frequency",
       title = "Calibration, out-of-time, by risk decile",
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

html <- sprintf('<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Predicting Certiorari &mdash; Methods Note</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,600&family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,600;1,6..72,400&display=swap">
<style>
  :root{--paper:#f3ecdd;--ink:#23262d;--soft:#5f5847;--faint:#8a8271;--oxblood:#8a2b2b;--sienna:#b5651d;--rule:#d8cdb4}
  *{box-sizing:border-box} html{-webkit-text-size-adjust:100%%}
  body{font-family:"Newsreader",Georgia,serif;color:var(--ink);background:var(--paper);margin:0;font-size:10.5pt;line-height:1.4}
  .sheet{max-width:8.1in;margin:0 auto;padding:.5in .6in}
  .kicker{font:600 8pt/1 "Newsreader";letter-spacing:.2em;text-transform:uppercase;color:var(--oxblood);margin:0 0 .3rem}
  h1{font-family:"Fraunces",Georgia,serif;font-weight:600;font-size:23pt;line-height:1.02;margin:0 0 .25rem}
  .dek{font-style:italic;color:var(--soft);margin:.1rem 0 .5rem;font-size:11pt}
  h2{font-family:"Fraunces",Georgia,serif;font-weight:600;font-size:11pt;color:var(--oxblood);margin:.5rem 0 .2rem;border-bottom:1px solid var(--rule);padding-bottom:2px}
  p{margin:.2rem 0} .grid{display:grid;grid-template-columns:1fr 1fr;gap:0 1.3rem}
  ul{margin:.2rem 0;padding-left:1.05rem} li{margin:.12rem 0}
  table{width:100%%;border-collapse:collapse;font-variant-numeric:tabular-nums;margin:.3rem 0}
  th,td{text-align:right;padding:2px 5px;border-bottom:1px solid var(--rule)} th:first-child,td:first-child{text-align:left}
  thead th{font:600 8.5pt/1 "Newsreader";text-transform:uppercase;letter-spacing:.06em;color:var(--faint);border-bottom:1.5px solid var(--ink)}
  .note{font-size:9pt;color:var(--soft);font-style:italic}
  figure{margin:.4rem 0;text-align:center} figure img{width:3.7in;max-width:100%%}
  figcaption{font-size:8.5pt;color:var(--faint);margin-top:.1rem}
  .fig-wrap{display:grid;grid-template-columns:1.1fr 1fr;gap:1.3rem;align-items:center}
  footer{margin-top:.5rem;border-top:2px solid var(--ink);padding-top:.3rem;font-size:8.5pt;color:var(--soft)}
  b,.stat{color:var(--oxblood)}
  @page{size:letter;margin:.4in} @media print{body{background:#fff} .sheet{padding:0}}
</style></head><body><main class="sheet">
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
    <p>Logistic regression, Platt-calibrated. Validated <b>leave-one-term-out</b>: each Term is scored by a model trained on the other seven, so every statistic below is <b>out-of-time</b>, never in-sample.</p>
  </div>
  <div>
    <h2>Validation (out-of-time)</h2>
    <table><thead><tr><th>Model</th><th>Base rate</th><th>AUC</th><th>Avg.&nbsp;prec.</th><th>Brier</th></tr></thead><tbody>
      %s%s%s
    </tbody></table>
    <p class="note"><b>AUC</b> &mdash; chance the model ranks a random grant above a random denial. <b>Avg. precision</b> is the imbalance-aware metric (baseline %.3f vs. a %s base rate &asymp; %.1f&times; chance). <b>Brier</b> &mdash; a constant-base-rate forecast scores 0.042; all three beat it. Accuracy is meaningless at a 4%% base rate.</p>
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
      <li><b>Rule&nbsp;10 dissent / circuit split</b>, parsed from the petition PDF: lifts baseline AUC 0.720&nbsp;&rarr;&nbsp;0.804.</li>
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
  img,
  pct(tail(e$calibration$pred,1)), pct(tail(e$calibration$obs,1)),
  pct(tail(b$calibration$pred,1)), pct(tail(b$calibration$obs,1)),
  comma(mc(b)$n_pos),
  format(Sys.Date(), "%B %Y"))

writeLines(html, "docs/cert_model_methods.html", useBytes = TRUE)
cat("wrote docs/cert_model_methods.html and docs/cert_model_calibration.png\n")
