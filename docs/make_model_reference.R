# Generate the cert-model coefficient reference (docs/cert_model_reference.html)
# from the deployed model artifacts, so every figure is exact and reproducible.
# Companion to make_methods_note.R: that one explains the models in prose for a
# general reader, this one lays out every coefficient for someone checking them.
#
#   Rscript docs/make_model_reference.R
#
# Set ARTIFACT=1 to emit the body-only variant used for a published Artifact
# (no <html>/<head> wrapper -- the platform supplies one -- and system serif
# stacks, because the Artifact CSP blocks the Google Fonts the site itself uses
# and a linked webfont would silently fall back to Times).

suppressPackageStartupMessages({ library(nnet); library(jsonlite) })

standalone <- !nzchar(Sys.getenv("ARTIFACT", ""))
out_path   <- if (standalone) "docs/cert_model_reference.html" else
              "docs/cert_model_reference.artifact.html"

esc  <- function(s) { s <- gsub("&","&amp;",s,fixed=TRUE); s <- gsub("<","&lt;",s,fixed=TRUE)
                      gsub(">","&gt;",s,fixed=TRUE) }
fnum <- function(x, d = 3) formatC(x, format = "f", digits = d, big.mark = ",")
fp   <- function(p) ifelse(p < 1e-4, sprintf("%.1e", p), sprintf("%.4f", p))
for_ <- function(x) ifelse(x >= 1000 | x < 0.001, sprintf("%.2e", x), sprintf("%.3f", x))
comma <- function(n) format(n, big.mark = ",")

# GROUP, var_of() and logit_tbl() are shared with the methods note, which
# publishes the same numbers in its Coefficients section.
source("docs/model_coef_table.R")

MODELS <- list(
  baseline = list(title="Baseline", sub="Petition-stage",
    frame="Disposition corpus &mdash; one row per petition, features as at its outcome",
    use="Daily dashboards and case pages"),
  enhanced = list(title="At-risk", sub="Granted ever",
    frame="At-risk panel &mdash; one row per petition &times; conference",
    use="Conference reports, &ldquo;Granted ever&rdquo; column"),
  gvr = list(title="GVR", sub="Granted, vacated, remanded",
    frame="At-risk panel &mdash; one row per petition &times; conference",
    use="Conference reports, GVR line"))

model_dir <- Sys.getenv("MODEL_DIR", "data")   # see make_methods_note.R
art <- lapply(names(MODELS), function(k)
  readRDS(file.path(model_dir, sprintf("cert_model_%s.rds", k))))
names(art) <- names(MODELS)
coefs <- do.call(rbind, Map(logit_tbl, art, names(art)))
cm <- readRDS(file.path(model_dir, "cert_model_conference.rds"))
built <- as.character(file.info(file.path(model_dir, "cert_model_baseline.rds"))$mtime)

# ---- pieces ------------------------------------------------------------------
bar <- function(est, scale) {
  if (is.na(scale) || scale <= 0) return("")
  w <- min(abs(est) / scale, 1) * 50
  sprintf('<span class="bar" aria-hidden="true"><i class="%s" style="%s"></i></span>',
          if (est >= 0) "pos" else "neg",
          if (est >= 0) sprintf('left:50%%;width:%.1f%%', w)
          else sprintf('right:50%%;width:%.1f%%', w))
}

coef_table <- function(k) {
  d <- coefs[coefs$model == k, ]
  d <- d[order(d$group != "—", d$group, d$variable, d$term), ]
  slopes <- d$estimate[d$term != "(Intercept)"]
  scale <- if (length(slopes)) max(abs(slopes)) else NA_real_
  rows <- ""; last <- ""
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    if (!identical(r$group, last)) {
      rows <- paste0(rows, sprintf('<tr class="grouprow"><th colspan="7" scope="colgroup">%s</th></tr>',
        esc(if (r$group == "—") "Intercept" else r$group))); last <- r$group
    }
    sig <- r$p < 0.05
    rows <- paste0(rows, sprintf(
      '<tr class="%s"><th scope="row"><code>%s</code></th><td class="num">%s%s</td>
       <td class="num">%s</td><td class="num">%s</td><td class="num">%s</td>
       <td class="num">%s</td><td class="barcell">%s</td></tr>',
      if (sig) "sig" else "nsig", esc(r$term), fnum(r$estimate),
      if (sig) '<span class="star" title="p &lt; 0.05">&nbsp;&bull;</span>' else "",
      fnum(r$se), fnum(r$z, 2), fp(r$p), for_(r$or),
      if (r$term == "(Intercept)") "" else bar(r$estimate, scale)))
  }
  sprintf('<div class="tablewrap"><table class="coef"><thead><tr>
    <th scope="col">Term</th><th scope="col" class="num">&beta;</th>
    <th scope="col" class="num">SE</th><th scope="col" class="num">z</th>
    <th scope="col" class="num">p</th><th scope="col" class="num">Odds ratio</th>
    <th scope="col">&minus;&nbsp;&nbsp;0&nbsp;&nbsp;+</th></tr></thead>
    <tbody>%s</tbody></table></div>', rows)
}

ref_block <- function(xl) {
  if (is.null(xl) || !length(xl)) return("")
  items <- paste0(vapply(names(xl), function(v)
    sprintf('<li><code>%s</code> <span class="ref">%s</span> <span class="lv">%d levels</span></li>',
            esc(v), esc(xl[[v]][1]), length(xl[[v]])), character(1)), collapse = "")
  sprintf('<div class="refs"><p class="refhead">Reference levels &mdash; every coefficient below is relative to these</p><ul>%s</ul></div>', items)
}

calib_svg <- function(m) {
  c0 <- m$calibration; if (is.null(c0) || !nrow(c0)) return("")
  mx <- max(c(c0$pred, c0$obs), na.rm = TRUE); if (!is.finite(mx) || mx <= 0) return("")
  sx <- function(v) 8 + (v / mx) * 104; sy <- function(v) 112 - (v / mx) * 104
  sprintf('<figure class="calib"><svg viewBox="0 0 120 120" role="img"
    aria-label="Calibration: predicted versus observed rate across ten equal-count bins">
    <line class="ident" x1="8" y1="112" x2="112" y2="8"/>
    <polyline class="curve" points="%s"/><g class="dots">%s</g></svg>
    <figcaption>Predicted vs observed, 10 equal-count bins. Diagonal is perfect calibration.</figcaption></figure>',
    paste(sprintf("%.1f,%.1f", sx(c0$pred), sy(c0$obs)), collapse = " "),
    paste0(sprintf('<circle cx="%.1f" cy="%.1f" r="2.4"/>', sx(c0$pred), sy(c0$obs)), collapse = ""))
}

cards <- ""
for (k in names(MODELS)) {
  m <- art[[k]]; d <- MODELS[[k]]; mc <- m$metrics_calibrated
  cards <- paste0(cards, sprintf('
  <article class="card"><header><h3>%s</h3><p class="eyebrow">%s</p></header>
    <p class="target"><code>%s</code></p>
    <dl class="stats">
      <div><dt>AUC</dt><dd class="num">%.3f</dd></div>
      <div><dt>Avg. precision</dt><dd class="num">%.3f</dd></div>
      <div><dt>Brier</dt><dd class="num">%.4f</dd></div>
      <div><dt>Base rate</dt><dd class="num">%.2f%%</dd></div>
      <div><dt>Rows</dt><dd class="num">%s</dd></div>
      <div><dt>Positives</dt><dd class="num">%s</dd></div>
    </dl><p class="cardnote">%s</p></article>',
    esc(d$title), esc(d$sub), esc(m$target), mc$auc, mc$ap, mc$brier,
    100 * m$base_rate, comma(mc$n), comma(mc$n_pos), d$use))
}

sections <- ""
for (k in names(MODELS)) {
  m <- art[[k]]; d <- MODELS[[k]]
  sections <- paste0(sections, sprintf('
<section class="model" id="%s"><div class="modelhead"><div>
  <p class="eyebrow">%s</p><h2>%s</h2><p class="frame">%s</p>
  <p class="frame"><strong>Target:</strong> <code>%s</code> &nbsp;&middot;&nbsp; %d coefficients &nbsp;&middot;&nbsp; Platt: a&nbsp;=&nbsp;%.4f, b&nbsp;=&nbsp;%.4f</p>
</div>%s</div>%s%s</section>',
  k, esc(d$sub), esc(d$title), d$frame, esc(m$target), length(coef(m$glm)),
  coef(m$calibrator)[1], coef(m$calibrator)[2], calib_svg(m),
  ref_block(m$xlevels), coef_table(k)))
}

# ---- conference (multinomial; no SEs survive strip_multinom) ------------------
cf0 <- coef(cm$model)
cf <- data.frame(term = colnames(cf0),
  variable = vapply(colnames(cf0), var_of, character(1), feats = cm$features),
  relisted = cf0["relisted",], granted = cf0["granted",], gvr = cf0["gvr",],
  stringsAsFactors = FALSE, row.names = NULL)
cf$group <- unname(ifelse(cf$variable == "(Intercept)", "—", GROUP[cf$variable]))
cf <- cf[order(cf$group != "—", cf$group, cf$variable, cf$term), ]
cscale <- max(abs(c(cf$relisted, cf$granted, cf$gvr)[cf$term != "(Intercept)"]))
crows <- ""; last <- ""
for (i in seq_len(nrow(cf))) {
  r <- cf[i, ]
  if (!identical(r$group, last)) {
    crows <- paste0(crows, sprintf('<tr class="grouprow"><th colspan="4" scope="colgroup">%s</th></tr>',
      esc(if (r$group == "—") "Intercept" else r$group))); last <- r$group
  }
  cell <- function(v) sprintf('<td class="num">%s%s</td>', fnum(v),
    if (r$term == "(Intercept)") "" else bar(v, cscale))
  crows <- paste0(crows, sprintf('<tr><th scope="row"><code>%s</code></th>%s%s%s</tr>',
    esc(r$term), cell(r$relisted), cell(r$granted), cell(r$gvr)))
}
conf_section <- sprintf('
<section class="model" id="conference"><div class="modelhead"><div>
  <p class="eyebrow">Competing risks &mdash; multinomial logit</p><h2>Conference</h2>
  <p class="frame">At-risk panel &mdash; one row per petition &times; conference. Models what happens <em>at</em> a given conference, four outcomes at once.</p>
  <p class="frame"><strong>Baseline outcome:</strong> <code>denied</code>. Each column is the log-odds of that outcome <em>versus denial</em> at the same conference. &nbsp;&middot;&nbsp; n&nbsp;=&nbsp;%s rows &nbsp;&middot;&nbsp; %d effective parameters &nbsp;&middot;&nbsp; residual deviance %s &nbsp;&middot;&nbsp; AIC %s</p>
</div></div>
<div class="ratebar"><p class="refhead">Observed outcome mix per conference</p><ul>%s</ul></div>
<div class="warn"><p><strong>No standard errors.</strong> <code>strip_multinom()</code> drops the Hessian before the model is saved, keeping the deployed artifact at 3&nbsp;KB. Standard errors, z and p cannot be recovered from the stored object &mdash; refit from the at-risk panel if you need them. The coefficients below are point estimates only.</p></div>
%s<div class="tablewrap"><table class="coef conf"><thead><tr>
  <th scope="col">Term</th><th scope="col" class="num">relisted</th>
  <th scope="col" class="num">granted</th><th scope="col" class="num">gvr</th></tr></thead>
  <tbody>%s</tbody></table></div></section>',
  comma(cm$n), cm$model$edf, fnum(cm$model$deviance, 1), fnum(cm$model$AIC, 1),
  paste(sprintf('<li><span>%s</span><b class="num">%.2f%%</b></li>',
                names(cm$rates), 100 * unlist(cm$rates)), collapse = ""),
  ref_block(cm$xlevels), crows)

# ---- styles ------------------------------------------------------------------
# The site's own law-review palette (see R/page_style.R). Direction is carried by
# the accent itself -- oxblood right, muted ink left -- rather than a second hue
# that would fight it.
fonts <- if (standalone) {
  list(link = paste0('<link rel="preconnect" href="https://fonts.googleapis.com">\n',
         '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?',
         'family=Fraunces:opsz,wght@9..144,500;9..144,600&',
         'family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,600;1,6..72,400&display=swap">'),
       serif = '"Newsreader",Georgia,serif', display = '"Fraunces",Georgia,serif')
} else {
  list(link = "",
       serif = '"Iowan Old Style","Palatino Linotype",Palatino,"Book Antiqua",Georgia,serif',
       display = '"Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif')
}

# Dark tokens only for the Artifact, which renders in the viewer's theme. The
# published site is committed to parchment, so the standalone doc stays with it.
dark <- if (standalone) "" else '
@media (prefers-color-scheme:dark){:root{
  --paper:#1b1916;--panel:#232019;--sunk:#191713;--ink:#e9e1d0;--ink-soft:#b0a48c;
  --faint:#857c6b;--oxblood:#cf7b74;--sienna:#d3924f;--rule:#38322a;
  --rule-soft:#2b2620;--pos:#cf7b74;--neg:#7d7466;}}
:root[data-theme="dark"]{
  --paper:#1b1916;--panel:#232019;--sunk:#191713;--ink:#e9e1d0;--ink-soft:#b0a48c;
  --faint:#857c6b;--oxblood:#cf7b74;--sienna:#d3924f;--rule:#38322a;
  --rule-soft:#2b2620;--pos:#cf7b74;--neg:#7d7466;}
:root[data-theme="light"]{
  --paper:#f3ecdd;--panel:#f7f1e4;--sunk:#efe7d5;--ink:#23262d;--ink-soft:#5f5847;
  --faint:#716b5d;--oxblood:#8a2b2b;--sienna:#a0591a;--rule:#d8cdb4;
  --rule-soft:#e4dcc7;--pos:#8a2b2b;--neg:#8b8172;}'

css <- sprintf('
:root{--paper:#f3ecdd;--panel:#f7f1e4;--sunk:#efe7d5;--ink:#23262d;--ink-soft:#5f5847;
  --faint:#716b5d;--oxblood:#8a2b2b;--sienna:#a0591a;--rule:#d8cdb4;--rule-soft:#e4dcc7;
  --pos:#8a2b2b;--neg:#8b8172;--serif:%s;--display:%s;
  --mono:ui-monospace,SFMono-Regular,"SF Mono",Menlo,Consolas,monospace;
  --step:clamp(.86rem,.83rem + .12vw,.92rem)}
%s
*{box-sizing:border-box} html{-webkit-text-size-adjust:100%%}
body{margin:0;background:var(--paper);color:var(--ink);font-family:var(--serif);
  font-size:17px;line-height:1.55;padding:clamp(1.6rem,4vw,3.2rem) clamp(1rem,4vw,2.4rem) 5rem;
  display:flex;flex-direction:column;gap:2.6rem;align-items:center}
body > *{width:100%%;max-width:72rem}
.num{font-variant-numeric:tabular-nums;text-align:right;white-space:nowrap}
code{font-family:var(--mono);font-size:.88em}
a:focus-visible{outline:2px solid var(--sienna);outline-offset:2px}
.masthead{display:flex;flex-direction:column;gap:.5rem;border-bottom:2px solid var(--ink);
  padding-bottom:1.1rem}
.eyebrow{margin:0;font-size:.7rem;letter-spacing:.2em;text-transform:uppercase;
  color:var(--oxblood);font-weight:600}
.masthead h1{margin:0;font-family:var(--display);font-size:clamp(1.9rem,1.3rem + 2.2vw,3rem);
  line-height:1.04;letter-spacing:-.015em;font-weight:600;text-wrap:balance}
.dek{margin:0;max-width:60ch;color:var(--ink-soft);font-style:italic;font-size:1.05rem}
.stamp{margin:.2rem 0 0;color:var(--faint);font-size:.82rem;font-variant-numeric:tabular-nums}
.intro{display:grid;gap:1.8rem}
.cards{display:grid;gap:1rem;grid-template-columns:repeat(auto-fit,minmax(14.5rem,1fr))}
.card{background:var(--panel);border:1px solid var(--rule);border-radius:2px;
  padding:1rem 1.05rem;display:flex;flex-direction:column;gap:.6rem}
.card header{display:flex;flex-direction:column;gap:.1rem}
.card h3{margin:0;font-family:var(--display);font-size:1.25rem;font-weight:600;color:var(--oxblood)}
.card .target{margin:0;font-size:.8rem;color:var(--ink-soft);min-height:2.6em}
.stats{margin:0;display:grid;grid-template-columns:1fr auto;gap:.18rem .6rem;
  border-top:1px solid var(--rule-soft);padding-top:.55rem}
.stats > div{display:contents}
.stats dt{color:var(--faint);font-size:.8rem}
.stats dd{margin:0;font-size:.86rem;font-variant-numeric:tabular-nums;text-align:right}
.cardnote{margin:0;font-size:.76rem;color:var(--faint);font-style:italic;
  border-top:1px solid var(--rule-soft);padding-top:.5rem}
.conf-card{border-color:var(--oxblood)}
.readnote{background:var(--sunk);border-left:3px solid var(--oxblood);padding:1rem 1.2rem;
  border-radius:2px}
.readnote h2{margin:0 0 .5rem;font-size:1rem;letter-spacing:.06em;text-transform:uppercase;
  color:var(--oxblood);font-weight:600}
.readnote ul{margin:0;padding-left:1.1rem;display:flex;flex-direction:column;gap:.42rem}
.readnote li{font-size:.92rem;color:var(--ink-soft);max-width:78ch}
.readnote b{color:var(--ink)}
.model{display:flex;flex-direction:column;gap:1rem;border-top:1px solid var(--rule);
  padding-top:1.7rem}
.modelhead{display:flex;gap:1.5rem;align-items:flex-start;flex-wrap:wrap;
  justify-content:space-between}
.modelhead > div{flex:1 1 22rem;display:flex;flex-direction:column;gap:.3rem}
.model h2{margin:0;font-family:var(--display);font-size:1.75rem;font-weight:600;
  letter-spacing:-.01em}
.frame{margin:0;font-size:.88rem;color:var(--ink-soft);max-width:70ch}
.calib{margin:0;flex:0 0 11rem;display:flex;flex-direction:column;gap:.3rem}
.calib svg{width:100%%;height:auto;background:var(--panel);border:1px solid var(--rule);
  border-radius:2px}
.calib .ident{stroke:var(--faint);stroke-width:.8;stroke-dasharray:3 3}
.calib .curve{fill:none;stroke:var(--oxblood);stroke-width:1.6;stroke-linejoin:round;
  stroke-linecap:round}
.calib .dots circle{fill:var(--oxblood)}
.calib figcaption{font-size:.68rem;color:var(--faint);line-height:1.35}
.refs,.ratebar{background:var(--sunk);border:1px solid var(--rule-soft);border-radius:2px;
  padding:.7rem .9rem}
.refhead{margin:0 0 .4rem;font-size:.68rem;letter-spacing:.16em;text-transform:uppercase;
  color:var(--faint);font-weight:600}
.refs ul,.ratebar ul{margin:0;padding:0;list-style:none;display:flex;flex-wrap:wrap;
  gap:.35rem .9rem}
.refs li{font-size:.82rem;color:var(--ink-soft)}
.refs .ref{color:var(--oxblood);font-family:var(--mono);font-size:.8rem}
.refs .lv{color:var(--faint);font-size:.72rem}
.ratebar li{display:flex;gap:.4rem;align-items:baseline;font-size:.84rem;color:var(--ink-soft)}
.ratebar b{color:var(--ink)}
.warn{background:var(--panel);border:1px solid var(--oxblood);border-left-width:3px;
  border-radius:2px;padding:.7rem .9rem}
.warn p{margin:0;font-size:.86rem;color:var(--ink-soft);max-width:80ch}
.warn strong{color:var(--oxblood)}
.tablewrap{overflow-x:auto;border:1px solid var(--rule);border-radius:2px;background:var(--panel)}
table.coef{border-collapse:collapse;width:100%%;font-size:var(--step)}
table.coef th,table.coef td{padding:.34rem .6rem;text-align:left}
table.coef thead th{position:sticky;top:0;z-index:1;background:var(--sunk);
  border-bottom:1.5px solid var(--ink);font-size:.72rem;letter-spacing:.1em;
  text-transform:uppercase;color:var(--ink-soft);font-weight:600;white-space:nowrap}
table.coef thead th.num{text-align:right}
table.coef tbody tr{border-bottom:1px solid var(--rule-soft)}
table.coef tbody tr:last-child{border-bottom:0}
table.coef tbody tr:hover{background:var(--sunk)}
table.coef tbody th{font-weight:400}
table.coef tbody th code{color:var(--ink)}
tr.grouprow th{background:var(--sunk);color:var(--oxblood);font-size:.68rem;
  letter-spacing:.16em;text-transform:uppercase;font-weight:600;padding-top:.5rem;
  padding-bottom:.3rem;border-bottom:1px solid var(--rule)}
tr.nsig td,tr.nsig th code{color:var(--faint)}
.star{color:var(--oxblood)}
.barcell{width:7.5rem;min-width:7.5rem;padding-right:.8rem !important}
.bar{position:relative;display:block;height:.62rem;width:100%%;
  border-left:1px solid var(--rule);border-right:1px solid var(--rule)}
.bar::before{content:"";position:absolute;left:50%%;top:-1px;bottom:-1px;
  border-left:1px solid var(--rule)}
.bar i{position:absolute;top:1px;bottom:1px;display:block;border-radius:1px}
.bar i.pos{background:var(--pos)}
.bar i.neg{background:var(--neg)}
table.conf .barcell{display:none}
table.conf td .bar{margin-top:.18rem;height:.4rem;width:5.5rem;margin-left:auto;
  border-color:var(--rule-soft)}
table.conf td{vertical-align:top}
.colophon{border-top:1px solid var(--rule);padding-top:1.1rem;display:flex;
  flex-direction:column;gap:.5rem}
.colophon p{margin:0;font-size:.82rem;color:var(--faint);max-width:82ch}
@media (max-width:640px){body{font-size:16px}
  .barcell,table.coef th:nth-child(4),table.coef td:nth-child(4){display:none}
  .calib{flex-basis:100%%}}
@media (prefers-reduced-motion:reduce){*{animation-duration:.01ms !important;
  transition-duration:.01ms !important}}', fonts$serif, fonts$display, dark)

# ---- assemble ----------------------------------------------------------------
body <- sprintf('
<header class="masthead">
  <p class="eyebrow">Supreme Court Report &mdash; model reference</p>
  <h1>Certiorari models: variables and regression output</h1>
  <p class="dek">Every coefficient in the four deployed models, with the standard errors, significance and odds ratios behind them. Generated directly from the fitted artifacts in <code>data/</code>.</p>
  <p class="stamp">Artifacts built %s &nbsp;&middot;&nbsp; %d coefficients across four models</p>
</header>
<section class="intro"><div class="cards">%s
  <article class="card conf-card"><header><h3>Conference</h3>
    <p class="eyebrow">Competing risks</p></header>
    <p class="target"><code>P(outcome at this conference)</code></p>
    <dl class="stats">
      <div><dt>Outcomes</dt><dd class="num">4</dd></div>
      <div><dt>Terms</dt><dd class="num">%d</dd></div>
      <div><dt>Rows</dt><dd class="num">%s</dd></div>
      <div><dt>Parameters</dt><dd class="num">%d</dd></div>
    </dl><p class="cardnote">Conference reports, &ldquo;Granted here&rdquo; and GVR</p></article>
</div>
<div class="readnote"><h2>Reading these tables</h2><ul>
  <li><b>&beta;</b> is on the log-odds scale. The <b>odds ratio</b> is e<sup>&beta;</sup>: how the odds multiply when the term is true, holding everything else fixed.</li>
  <li>Fitted with a <b>Firth (Jeffreys-prior) penalty</b>, so standard errors come from the penalised covariance rather than the ordinary one. The penalty is what keeps near-separated cells &mdash; rare court-below buckets, pro&nbsp;se petitions &mdash; from running to infinity.</li>
  <li>Every model is fitted on <b>all terms</b>; AUC, average precision and Brier are <b>leave-one-term-out</b>, with the Platt calibration itself refitted out-of-fold. They are held-out numbers, not in-sample ones.</li>
  <li>A dot (&bull;) marks p&nbsp;&lt;&nbsp;0.05; non-significant rows are set in a lighter tone. %d of %d coefficients clear the threshold.</li>
  <li>Factor coefficients are differences from the reference level named above each table, never absolute effects.</li>
</ul></div></section>
%s%s
<footer class="colophon">
  <p>Generated %s by <code>docs/make_model_reference.R</code> from <code>data/cert_model_{baseline,enhanced,gvr,conference}.rds</code>, fitted by <code>.github/scripts/train_cert_model.R</code>. Feature sets and fitting code live in <code>R/cert_model.R</code>; the modelling rationale is in <code>docs/cert_model.md</code>.</p>
  <p>Two known gaps ride along with these numbers: <code>gap_na</code> carries roughly +0.98 log-odds with no legal story behind it (likely a data-quality proxy), and the 5+ relist tail over-predicts in every specification fitted. Both are tracked as open issues.</p>
  <p>These are descriptive statistical estimates &mdash; <b>not legal advice, and not a prediction about any particular case</b>.</p>
</footer>', built, nrow(coefs) + 3 * nrow(cf), cards, nrow(cf), comma(cm$n),
   cm$model$edf, sum(coefs$p < 0.05), nrow(coefs), sections, conf_section,
   format(Sys.Date(), "%B %Y"))

title <- "Certiorari models &mdash; variables and regression output"
html <- if (standalone) {
  paste0('<!DOCTYPE html><html lang="en"><head><script async src="/analytics.js"></script>',
         '<meta charset="utf-8">\n<meta name="viewport" content="width=device-width, initial-scale=1">\n',
         '<title>', title, '</title>\n', fonts$link, '\n<style>', css, '</style>\n</head><body>\n',
         body, '\n</body></html>\n')
} else {
  paste0('<title>', title, '</title>\n<style>', css, '</style>\n', body, '\n')
}

writeLines(html, out_path, useBytes = TRUE)
cat("wrote ", out_path, " (", nrow(coefs), " logit coefficients + ", nrow(cf),
    " conference terms)\n", sep = "")
