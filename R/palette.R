# palette.R --------------------------------------------------------------------
# THE colour source for supremecourt.report.
#
# Before this file the palette lived in five separate `:root` blocks, 33 hex
# literals hardcoded into gt calls, and 33 more hidden inside `var(--token,
# #fallback)` in NAV_CSS. Changing a colour meant editing seven files in
# lockstep and hoping; the fallbacks in particular would have gone on rendering
# the OLD palette wherever a token was missing, silently and only on some pages.
#
# So: every colour the site publishes is a lookup in this file. Nothing below
# is a literal anywhere else in the tree, and .github/scripts/audit_site.R
# fails the build if one reappears.
#
# Two kinds of value live here and they are not interchangeable:
#
#   * CSS TOKENS become custom properties in a `:root{}` block. Stylesheets
#     reference them through var(), so they can be swapped at the top of a
#     cascade.
#   * DATA COLOURS are handed to gt as R values -- a shading ramp, a set of
#     chips. gt resolves them at render time into inline styles, so var() is
#     not available and the R value has to be the real colour.

# ---- CSS tokens --------------------------------------------------------------

# BONE & COCHINEAL. The ground is plain and the character is in the ink and one
# accent, which is the opposite of the parchment palette this replaces: that one
# spent its identity on a tinted paper and then needed accent, link, tan and
# gold on top of it, so nothing read as deliberate.
#
# --accent and --link hold the SAME value here, and that is the scheme rather
# than an oversight: one cold red, spent on editorial accent and on outbound
# document links, which never share a slot on the page. The names are now wrong
# -- neither value is accent or link -- and renaming them to --accent/--link
# touches every var() in five stylesheets, so it is deliberately not in this
# diff. See the note in CLAUDE.md.
#
# Contrast, measured, against the tightest ground each is used on:
#   ink 16.92:1   ink-soft 8.83:1   faint 5.81:1   accent 8.07:1
# The parchment palette passed at exactly 4.50:1 at its tightest. Nothing here
# is closer to the line than 5.37:1, including on --stripe and --field.
PALETTE <- list(
  "paper"    = "#fdfcfa",
  "panel"    = "#ffffff",
  "ink"      = "#1a1a1e",
  "ink-soft" = "#484850",
  "faint"    = "#63636b",
  "accent"  = "#9c0e3a",
  "link"   = "#9c0e3a",
  # 1.39:1 on --paper. Hairlines carry table and timeline structure, so this is
  # calibrated to beat the parchment rule's 1.34:1 rather than to disappear --
  # a tint that reads on cream goes invisible on bone.
  "rule"     = "#dbd8d2")

# The Cert Funnel's rail. --gold and --paper-2 were here too and are gone: both
# were declared in the funnel's :root and referenced by nothing, in either the
# funnel or anywhere else. Consolidating made that visible; this removes them.
PALETTE_FUNNEL <- list(
  "rail" = "#e4e0d9")

# Interactive-table furniture (reactable, injected by interactive_theme.R). These
# were literals in two places at once: --stripe was written into the CSS AND
# passed to gt's row.striping.background_color, and nothing tied the two
# together.
PALETTE_UI <- list(
  "stripe"     = "#f7f5f1",   # striped row
  "field"      = "#f5f3ef",   # search / filter / page-size input
  "link-hover" = "#78092c")   # a link under the cursor: the accent, darkened

# Docket-page event categories. These are a legend, not a scale: each one names
# a kind of docket entry, so they have to stay mutually distinguishable rather
# than ordered. Recolouring the site does not automatically mean recolouring
# these -- see docs/docket-pages.md.
PALETTE_EVENTS <- list(
  "c-white"   = "#fff",     "c-orange" = "#e07b1f",
  "c-cream"   = "#efe1a8",  "c-tan"    = "#c8a56b",
  "c-blue"    = "#7fa8cf",  "c-red"    = "#cf5f5f",
  "c-lgreen"  = "#7fb069",  "c-dgreen" = "#2f6b3d",
  "c-yellow"  = "#e9cb3f",  "c-neutral"= "#bcae90")

#' Look a token up, loudly.
#'
#' `PALETTE[["oxblod"]]` returns NULL and paints nothing; a NULL colour reaching
#' gt is either an error three frames away or, worse, a cell that silently loses
#' its shading. This is the codebase that shipped an aliased model coefficient
#' contributing exactly zero for months -- fail on the typo instead.
pal <- function(name) {
  v <- c(PALETTE, PALETTE_FUNNEL, PALETTE_UI, PALETTE_EVENTS)[[name]]
  if (is.null(v)) stop("pal(): no such colour token: ", name, call. = FALSE)
  v
}

#' Emit the `:root{}` block for a page.
#'
#' `nav_max` is the measure the masthead centres on and it is NOT a free
#' parameter -- NAV_CSS pads its rule by 1.5rem, so the rule lands flush with
#' the text column only when nav_max equals the container plus 2*1.5rem. Left
#' unset it falls back to 54rem and overhangs (see docs/navigation.md).
#'
#' Every block gets the full core, including pages that do not currently use
#' every token: a page that starts using --panel tomorrow should not have to
#' remember to add it, and an unused custom property costs twenty bytes.
#' The masthead's width. ONE value for the whole site.
#'
#' It used to track each page's own text column -- 40rem on the index pages,
#' 44rem on the funnel, 54rem on case pages -- so that the masthead rule landed
#' exactly on the column beneath it. That worked with five section links and
#' stopped working at seven: measured in Chrome at 1440/1280/1024, the nav needs
#' 654px, and a 40rem masthead offers 592px. Every index page wrapped its nav
#' onto a second line; the 44rem funnel fitted in exactly 654px, i.e. with no
#' slack at all, one label away from the same fate.
#'
#' 54rem was already the widest value in use (case pages) and leaves ~210px of
#' room, so the next section link does not reopen this.
#'
#' THE TRADE, stated because it is visible: on the 40rem index pages and the
#' 44rem funnel the masthead and its rule are now WIDER than the text column
#' beneath them, rather than flush with it. That is a deliberate broadsheet
#' masthead rather than a mistake -- but it is a real change, and if it reads
#' badly the honest alternative is not to re-couple the width, it is to spend
#' fewer characters on the nav.
SITE_NAV_MAX <- "54rem"

#' `nav_max` defaults to SITE_NAV_MAX and no caller should now pass anything
#' else. The parameter is kept so a one-off page can still opt out, and so this
#' reads as a decision rather than a hardcode.
palette_root <- function(nav_max = SITE_NAV_MAX, extra = NULL) {
  toks <- c(PALETTE, extra)
  paste0(":root{",
         paste0("--", names(toks), ":", unlist(toks), collapse = ";"),
         ";--nav-max:", nav_max, "}")
}

#' Substitute @token@ placeholders in a stylesheet written as a template.
#'
#' NAV_CSS carries `var(--rule,@rule@)` rather than a bare `var(--rule)`,
#' because a masthead that loses its tokens should still render styled rather
#' than merely present -- this codebase has been bitten three times by silent
#' degradation. But a hand-written fallback is a second copy of the palette,
#' and second copies go stale. These are filled from PALETTE at load time, so
#' the fallback cannot disagree with the token it backs up.
#' `@token@` gives the hex. `@token:rgb@` gives a bare "r,g,b" triplet, for the
#' handful of places that need the colour at partial alpha -- an underline at 40%
#' under a link, say. Those were written out as rgba(160,89,26,.4), which is
#' --link to three decimal places and no way to tell from reading it. The
#' relative-colour form rgb(from var(--link) ...) would avoid the duplication
#' in CSS itself, but it fails closed to a transparent border on anything older
#' than 2023, and an invisible underline is exactly the silent kind of loss this
#' file exists to prevent. Substituting at build time has neither problem.
#' A token as a bare "r,g,b" triplet, for markup built at RUNTIME.
#'
#' `@token:rgb@` only works in a string that is later handed to fill_palette().
#' Markup generated per-row inside a renderer never is, and a placeholder that
#' reaches the browser is not a wrong colour but NO colour -- the declaration is
#' invalid and simply drops.
#'
#' That failure took a visible form on the funnel's relist table for weeks:
#' `background:rgba(@accent:rgb@,0.762)` never painted, while the
#' `color:var(--paper)` set alongside it for contrast-on-dark still applied. Three
#' of the six Granted figures rendered as near-white text on the near-white page
#' -- present in the DOM, invisible on screen. Use this in any generated string.
pal_rgb <- function(name) paste(as.vector(grDevices::col2rgb(pal(name))), collapse = ",")

fill_palette <- function(css) {
  for (nm in names(c(PALETTE, PALETTE_FUNNEL, PALETTE_UI, PALETTE_EVENTS))) {
    hex <- pal(nm)
    css <- gsub(paste0("@", nm, ":rgb@"),
                paste(as.vector(grDevices::col2rgb(hex)), collapse = ","),
                css, fixed = TRUE)
    css <- gsub(paste0("@", nm, "@"), hex, css, fixed = TRUE)
  }
  left <- regmatches(css, gregexpr("@[a-z0-9:-]+@", css))[[1]]
  if (length(left))
    stop("fill_palette(): unresolved placeholder(s): ",
         paste(unique(left), collapse = ", "), call. = FALSE)
  css
}

# ---- data colours ------------------------------------------------------------

# The grant-probability ramp, light to dark. Read by data_color() on the daily
# dashboards and by fc_shade() on the conference pages, which is why it lives
# here rather than in either of them: the same probability has to shade the same
# on both, and it did not always.
#
# The domain is CLAMPED at 0.6, not at 1.0. Forecasts above 60% are real -- one
# case sat at 62% on 2026-07-31 -- and before the clamp they fell outside the
# scale and rendered as na_color, so the likeliest case on the page looked like
# a row with no forecast at all.
#
# The ends are the palette, not new colours: the scale runs from the page's own
# ground to its accent, so a recolour carries the ramp with it and the two middle
# stops are the only values that have to be chosen. na_color is --panel, so a
# cell with no forecast reads as empty table furniture rather than as a zero.
GRANT_RAMP   <- c(pal("paper"), "#f1c8d2", "#c85e79", pal("accent"))
GRANT_DOMAIN <- c(0, 0.6)
GRANT_NA     <- pal("panel")

# Docket-type chips. A nominal scale: three kinds of filing, no order.
TYPE_CHIPS <- c("Paid" = "#e8e8e1", "IFP" = "#f2e8dd", "Application" = "#e2e5ea")

# Argument Navigator status fills. Nominal again -- a case's position in the
# argument lifecycle, not a quantity -- but ORDERED in the legend, so keep the
# sequence readable left to right. `na_color` for both this and TYPE_CHIPS is
# GRANT_NA: an unfilled chip should read as table furniture, not as a category.
#
# Four progressive states, then the two ways a case ends without a merits
# decision. Those two are deliberately the only coloured-vs-neutral pair in the
# set: DIG'd takes pink, since the Court engaged and then let go, and Dismissed
# takes the one hue-free fill, since the case was withdrawn before the Court did
# anything with it. Everything else carries a faint cast (green, blue, warm,
# violet) and these are chips with text labels, so the colour is a second signal
# rather than the only one -- the closest existing pair sits at dE 2.1.
STATUS_FILL <- c("Granted"   = "#e6e9e0", "Scheduled" = "#e2e5ea",
                 "Argued"    = "#eceadf", "Decided"   = "#e8e6e9",
                 "DIG'd"     = "#f4d9e0", "Dismissed" = "#dcd9d4")

# Calibration-plot series (docs/make_methods_note.R). This is the #b5651d that
# #36 flagged: the value --link held before the WCAG correction, left behind
# when everything else moved, so the chart and the page around it drew "the
# same" orange differently. Reconciled here, as that note said to.
#
# Two lines that overlap closely need to separate by lightness, not only hue --
# hue alone fails for a deuteranope. #6b8caf reads 3.50:1 on the plot's white
# ground and 2.36:1 against the accent it shares the chart with.
CHART_SERIES <- c("baseline" = "#6b8caf", "enhanced" = pal("accent"))

# Interactive (reactable) table furniture, shared with the injected CSS.
ROW_STRIPE <- pal("stripe")
LINK_HOVER <- pal("link-hover")
