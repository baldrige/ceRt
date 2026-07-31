# CI build: fetch the current term and render the daily petitions dashboards into
# $SITE_DIR/dashboards, then refresh the section index and the site landing page.
# Conferences are NOT rebuilt here (they need a multi-term case database); the
# existing conferences/ directory is preserved by the workflow's gh-pages
# checkout and simply linked from the landing page.
#
# Env: SITE_DIR (default "site"), TERM_YEAR (default "26").

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse)
  library(httr2); library(jsonlite); library(pdftools); library(htmltools)
})

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
term <- Sys.getenv("TERM_YEAR", unset = "26")
dash_dir <- file.path(site_dir, "dashboards")
dir.create(dash_dir, recursive = TRUE, showWarnings = FALSE)

# Keep the site's Google Analytics loader (referenced by every page's <head> as
# /analytics.js) in sync with the repo source. It persists on gh-pages between
# runs; re-asserting it here (like the workflow's CNAME) means a full rebuild
# can never silently drop tracking. Runs before the fetch so a throttle-degraded
# day (which exits early) still refreshes it.
if (file.exists("analytics.js"))
  file.copy("analytics.js", file.path(site_dir, "analytics.js"), overwrite = TRUE)

# Load the dashboard functions without triggering the script's bottom call.
src <- readLines("R/scotus_dash_new.R")
src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))

# Baseline (structural + Rule 10) cert-grant model for the per-petition forecast.
# Absent artifact -> NULL -> scotus_dash() simply omits the column.
source("R/cert_model.R")
source("R/petition_signals.R")   # resolve_petition_signals (Rule 10 from the petition PDF)
source("R/argument_nav.R")       # classify_argument (for docket-page lifecycle)
source("R/docket_page.R")        # render_dockets_for
cert_models  <- load_cert_models("data")
grant_model  <- cert_models$baseline
counsel_ix   <- cert_models$counsel_index
cat("Baseline cert model:", if (is.null(grant_model)) "not found (no forecast column)" else "loaded", "\n")

cat("Fetching OT", term, "docket...\n")
ot <- get_scotus_update(term)
cat("Cases fetched:", nrow(ot),
    "| unresolved:", attr(ot, "n_failed") %||% 0,
    "/", attr(ot, "n_attempted") %||% nrow(ot), "\n")

# Never publish a throttle-degraded fetch: exit cleanly and leave the site as-is.
if (fetch_is_degraded(ot)) {
  cat("Fetch degraded (throttling); not rendering. Existing site left unchanged.\n")
  quit(status = 0)
}

# Serve the cached fetch so per-date renders don't re-hit the API.
get_scotus_update <- function(year) ot
dates <- ot |> filter(!is.na(date)) |> distinct(date) |> arrange(date) |> pull(date)

# Resolve the Rule 10 signals (dissent below / circuit split) for the paid
# petitions in view, parsed from each petition PDF. The cache persists on the
# site so runs don't re-fetch. Defensive: any failure -> empty map -> score_case
# defaults the signals to FALSE, so a render never blocks on this.
signals_map <- list()
if (!is.null(grant_model)) {
  paid <- ot |> filter(type == "paid", !is.na(petition_url), nzchar(petition_url)) |>
    distinct(dkt, .keep_all = TRUE)
  sig <- tryCatch(resolve_petition_signals(
    paid$dkt, paid$petition_url,
    cache_path = file.path(dash_dir, "petition_signals_cache.json"),
    max_new = as.integer(Sys.getenv("PET_SIG_MAX_NEW", unset = "400"))),
    error = function(e) NULL)
  if (!is.null(sig)) signals_map <- setNames(
    lapply(seq_len(nrow(sig)), function(i) as.list(sig[i, ])), sig$dkt)
  cat("Petition Rule 10 signals resolved for", length(signals_map), "paid docket(s)\n")
}

cat("Rendering", length(dates), "date(s) to", dash_dir, "\n")
for (i in seq_along(dates)) {
  d <- as.Date(dates[i], origin = "1970-01-01")
  scotus_dash(range = d, year = term, out_dir = dash_dir, model = grant_model,
              signals_map = signals_map, counsel_index = counsel_ix)
}
dashboard_index(dash_dir)

# Docket pages for the current-term cases just fetched (incremental: only dockets
# whose page changed are rewritten). Keeps /cases/ current for the daily links.
render_dockets_for(ot, site_dir)

# The /cases/ browse index. Must exist before any page links to it: the case
# breadcrumb's middle crumb points at /cases/, which returned a 404 until now.
write_cases_index(file.path(site_dir, "cases"))

# Refresh the site landing page (links the sections that exist). Each category
# also shows its three most recent pages in a compact strip beneath the link.
# Date-keyed sections (daily, conferences) sort by their YYYY-MM-DD; the
# argument navigator is one page per Term, keyed by year.
date_key   <- function(f) as.Date(str_extract(f, "\\d{4}-\\d{2}-\\d{2}"))
year_key   <- function(f) as.integer(str_extract(f, "\\d{4}"))
short_date <- function(f) { d <- date_key(f)
  paste(format(d, "%b"), as.integer(format(d, "%d"))) }  # "Jul 14", "Jul 3"
arg_label  <- function(f) paste0("OT", str_extract(f, "\\d{4}"))

items <- list(list(href = "dashboards/", label = "Daily Petitions & Applications",
                   meta = "new, daily",
                   recent = recent_children(
                     dash_dir, "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$",
                     date_key, short_date, "dashboards/")))
if (dir.exists(file.path(site_dir, "conferences"))) {
  items <- c(items, list(list(href = "conferences/", label = "Conference Reports",
                              meta = "weekly, by relists",
                              recent = recent_children(
                                file.path(site_dir, "conferences"),
                                "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$",
                                date_key, short_date, "conferences/"))))
}
if (dir.exists(file.path(site_dir, "arguments"))) {
  items <- c(items, list(list(href = "arguments/", label = "Oral Argument Navigator",
                              meta = "granted cases, by sitting",
                              recent = recent_children(
                                file.path(site_dir, "arguments"),
                                "^arg_\\d{4}\\.html$",
                                year_key, arg_label, "arguments/"))))
}
if (file.exists(file.path(site_dir, "cases", "index.html"))) {
  items <- c(items, list(list(href = "cases/", label = "All Cases",
                              meta = "browse by Term")))
}
if (dir.exists(file.path(site_dir, "funnel"))) {
  items <- c(items, list(list(href = "funnel/", label = "The Cert Funnel",
                              meta = "the explainer")))
}
# Publish the self-contained model methods note and link it last.
if (file.exists("docs/cert_model_methods.html")) {
  file.copy("docs/cert_model_methods.html", file.path(site_dir, "methods.html"),
            overwrite = TRUE)
  # Patch the nav in at copy time, not in the checked-in source: that file is a
  # hand-maintained document, and this way a regenerated one picks the masthead
  # up automatically rather than silently losing it. methods.html was one of the
  # two page types with zero internal links.
  inject_masthead(file.path(site_dir, "methods.html"), active = "/methods.html")
  items <- c(items, list(list(href = "methods.html", label = "The Forecast Model",
                              meta = "methods & validation")))
}
# The About page: authorship, the repository, and the address to send corrections
# to. Written unconditionally (it has no external dependency) and listed last, so
# the sections a reader came for stay above it.
write_about_page(file.path(site_dir, "about.html"))
items <- c(items, list(list(href = "about.html", label = "About",
                            meta = "who makes this")))
# Most-read cases over the trailing 30 days, from GA4. Needs the GA4_PROPERTY_ID
# and GA4_SA_KEY secrets; without them this is zero rows and the block is simply
# omitted, which is why it is not guarded here. See R/site_analytics.R.
source("R/site_analytics.R")
MOST_READ_DAYS <- 30L
most_read <- top_viewed_cases(site_dir, n = 5L, days = MOST_READ_DAYS)

# Likeliest grants: the week's newly-docketed paid-docket cases the baseline
# structural model rates furthest above its base rate. Same model and the same
# score_case() call the daily dashboard's "Grant forecast" column uses, so the
# home page and the dashboard cannot show different numbers for one case.
source("R/site_forecast.R")
sharpest <- top_forecast_cases(ot, grant_model, site_dir,
                               signals_map = signals_map,
                               counsel_index = counsel_ix, n = 5L)
# Note built only when there are rows: grant_model may be NULL, and the base
# rate has to come off the model rather than a literal so it stays true across
# refits.
sharpest_panel <- if (nrow(sharpest)) forecast_panel(
  sharpest,
  heading = "Likeliest Grants",
  # "paid-docket cases", not "paid petitions": the paid docket carries 28 U.S.C.
  # 1253 direct appeals alongside petitions, and the first two entries this panel
  # ever published were the Allen redistricting APPEALS. Calling them petitions
  # on the front page was wrong in the same way the model's cue text was.
  note = sprintf(paste0("Structural estimate for paid-docket cases filed in the ",
                        "last %d days, against a %.1f%% base rate. An estimate, ",
                        "not a prediction about any case."),
                 FORECAST_WINDOW_DAYS, 100 * grant_model$base_rate)) else NULL

styled_index_page(
  file.path(site_dir, "index.html"),
  title = "Supreme Court Report",
  kicker = "A window on the Court's docket",
  heading = "Supreme Court Report",
  dek = "Quantifying the U.S. Supreme Court's behavior and making it legible for the public.",
  items = items,
  new_tab = FALSE,
  search = TRUE,
  # Wordmark only: this page IS the section list, and repeating it 200px above
  # itself is noise.
  wordmark_only = TRUE,
  # Two panels; either may be NULL and tagList() drops it. Forecast first: it is
  # about petitions that arrived this week, which is the site's subject. What
  # readers clicked is a footnote to that, not a peer of it.
  panel = tagList(
    sharpest_panel,
    # Rank only, no counts: the ordering is the story, and printing the raw
    # numbers would publish the site's traffic volume as a side effect.
    most_read_panel(
      most_read,
      heading = "Most-Read Cases",
      show_counts = FALSE,
      note = sprintf("Ranked by page views over the %d days ending %s %d, %d.",
                     MOST_READ_DAYS, format(Sys.Date() - 1, "%B"),
                     as.integer(format(Sys.Date() - 1, "%d")),
                     as.integer(format(Sys.Date() - 1, "%Y")))))
)
cat("Done.\n")
