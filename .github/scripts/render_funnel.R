# CI render: build the Cert Funnel page from the per-term case artifacts in
# $CASES_DIR (fresh weekly fetches) plus the committed historical baselines,
# writing $SITE_DIR/funnel/index.html.
#
# Env: SITE_DIR (default "site"), CASES_DIR (default "cases"),
#      BASELINES (default "data/funnel_baselines.json").

source("R/cert_funnel.R")
source("R/page_style.R")   # smarten_html() for typographic quotes

site_dir <- Sys.getenv("SITE_DIR", unset = "site")
cases_dir <- Sys.getenv("CASES_DIR", unset = "cases")
baselines_path <- Sys.getenv("BASELINES", unset = "data/funnel_baselines.json")

baselines <- jsonlite::fromJSON(baselines_path)
# jsonlite round-trips data frames; rebuild the exact shapes the renderer expects.
baselines$pooled$total <- as_tibble(baselines$pooled$total)
baselines$pooled$by_type <- as_tibble(baselines$pooled$by_type)
baselines$pooled$relist_table <- as_tibble(baselines$pooled$relist_table)

files <- list.files(cases_dir, pattern = "^cases-\\d{2}\\.rds$",
                    full.names = TRUE, recursive = TRUE)
if (length(files) == 0) stop("no case artifacts found in ", cases_dir)

# The workflow fetches the current term plus the prior term (for conference
# cross-term coverage), but only the CURRENT (highest-numbered) term is a "term
# in progress" for the funnel's live section; the prior term is essentially
# complete and already represented by the baselines.
terms <- sort(str_extract(basename(files), "\\d{2}"), decreasing = TRUE)
live_term <- terms[1]
f <- files[str_detect(basename(files), paste0("cases-", live_term, "\\.rds"))][1]
cases <- readRDS(f)
cls <- classify_petitions(cases)
live <- list(); live_cls <- list(); data_dates <- list()
if (nrow(cls) > 0) {
  live[[live_term]] <- funnel_stats(cls)
  live_cls[[live_term]] <- cls
  data_dates[[live_term]] <- format(Sys.Date(), "%B %d, %Y")
  cat("Live term OT", live_term, ":", nrow(cls), "petitions classified;",
      sum(cls$outcome == "pending"), "pending\n")
}

p <- render_funnel_page(live, baselines, file.path(site_dir, "funnel"),
                        live_cls = live_cls, data_dates = data_dates)
# Typographic (smart) quotes across the funnel prose; skips <style>/<script>/tags.
writeLines(enc2utf8(smarten_html(paste(readLines(p, warn = FALSE, encoding = "UTF-8"),
                                        collapse = "\n"))), p, useBytes = TRUE)
cat("Rendered", p, "\n")
