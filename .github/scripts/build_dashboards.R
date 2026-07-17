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
grant_model <- load_cert_models("data")$baseline
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
              signals_map = signals_map)
}
dashboard_index(dash_dir)

# Docket pages for the current-term cases just fetched (incremental: only dockets
# whose page changed are rewritten). Keeps /cases/ current for the daily links.
render_dockets_for(ot, site_dir)

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
if (dir.exists(file.path(site_dir, "funnel"))) {
  items <- c(items, list(list(href = "funnel/", label = "The Cert Funnel",
                              meta = "the explainer")))
}
# Publish the self-contained model methods note and link it last.
if (file.exists("docs/cert_model_methods.html")) {
  file.copy("docs/cert_model_methods.html", file.path(site_dir, "methods.html"),
            overwrite = TRUE)
  items <- c(items, list(list(href = "methods.html", label = "The Forecast Model",
                              meta = "methods & validation")))
}
styled_index_page(
  file.path(site_dir, "index.html"),
  title = "Supreme Court Report",
  kicker = "A window on the Court's docket",
  heading = "Supreme Court Report",
  dek = "Quantifying the U.S. Supreme Court's behavior and making it legible for the public.",
  items = items,
  new_tab = FALSE
)
cat("Done.\n")
