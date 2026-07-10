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
cat("Rendering", length(dates), "date(s) to", dash_dir, "\n")
for (i in seq_along(dates)) {
  d <- as.Date(dates[i], origin = "1970-01-01")
  scotus_dash(range = d, year = term, out_dir = dash_dir)
}
dashboard_index(dash_dir)

# Refresh the site landing page (links the sections that exist).
links <- list(tags$li(tags$a(href = "dashboards/", "Daily petitions & applications")))
if (dir.exists(file.path(site_dir, "conferences"))) {
  links <- c(links, list(tags$li(tags$a(href = "conferences/", "Conference reports"))))
}
if (dir.exists(file.path(site_dir, "funnel"))) {
  links <- c(links, list(tags$li(tags$a(
    href = "funnel/", "The Cert Funnel — how petitions live and die"))))
}
save_html(
  tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title("ceRt — Supreme Court Docket Dashboards"),
      tags$style(HTML(
        "body{font-family:'Source Sans Pro',system-ui,sans-serif;max-width:760px;
           margin:3rem auto;padding:0 1rem;color:#1a1a1a}
         h1{font-weight:600;border-bottom:2px solid #1a1a1a;padding-bottom:.4rem}
         ul{list-style:none;padding:0;font-size:1.1rem}
         li{padding:.6rem 0}
         a{text-decoration:none;color:#0b3d91;font-weight:600}
         a:hover{text-decoration:underline}"
      ))
    ),
    tags$body(
      tags$h1("Supreme Court Docket Dashboards"),
      tags$ul(links)
    )
  ),
  file.path(site_dir, "index.html")
)
cat("Done.\n")
