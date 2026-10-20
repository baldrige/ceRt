# build_sg_recommendations.R ------------------------------------------------------
# Propose a label for every SG brief filed after a CVSG, OT2017 onward, and write
# data-raw/sg_recommendations.json. A label already marked `source = "hand"` is
# never overwritten. See docs/sg-recommendation-spec.md.
#
#   Rscript .github/scripts/build_sg_recommendations.R   # propose labels for new briefs
#   SG_TEXT_DIR=... Rscript .github/scripts/build_sg_recommendations.R   # cache the brief text

suppressPackageStartupMessages({ library(tidyverse); library(httr2); library(pdftools); library(jsonlite) })
source("R/cert_funnel.R")
source("R/petition_signals.R")
source("R/sg_views.R")

OUT <- "data-raw/sg_recommendations.json"
text_dir <- Sys.getenv("SG_TEXT_DIR", "")

files <- c(sprintf("data-raw/ot_%d.rds", 2017:2024), "data-raw/snapshot_25.rds", "data-raw/snapshot_26.rds")
files <- files[file.exists(files)]
# Later files win: a docket in both the archive and a snapshot is read from the
# fresher one.
cases <- map_dfr(rev(files), function(f) { d <- readRDS(f); d[, c("dkt", "events")] }) |>
  distinct(dkt, .keep_all = TRUE) |>
  filter(funnel_case_type(dkt) == "paid")

old <- if (file.exists(OUT)) fromJSON(OUT, simplifyVector = FALSE) else list()
out <- old
n_new <- 0L
for (i in seq_len(nrow(cases))) {
  dkt <- cases$dkt[i]
  b <- find_sg_brief(cases$events[[i]])
  if (is.null(b) || is.na(b$filed)) next
  if (identical(old[[dkt]]$source, "hand")) next
  text <- if (nzchar(text_dir)) {
    p <- file.path(text_dir, paste0(dkt, ".txt"))
    if (file.exists(p)) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    else { t <- fetch_petition_text(b$url); if (!is.na(t)) writeLines(t, p, useBytes = TRUE); t }
  } else fetch_petition_text(b$url)
  r <- sg_recommendation(sg_conclusion(text), dkt)
  out[[dkt]] <- list(cvsg = format(lubridate::mdy(b$cvsg_date)),
                     filed = format(lubridate::mdy(b$filed)), url = b$url,
                     rec = r$rec, quote = r$quote, source = "regex", sg_v = 1L)
  n_new <- n_new + 1L
}
out <- out[order(names(out))]
write_json(out, OUT, auto_unbox = TRUE, pretty = TRUE, na = "null")
recs <- vapply(out, function(e) e$rec %||% NA_character_, character(1))
message(sprintf("SG briefs: %d labelled (%d this run). %s", length(out), n_new,
                paste(names(table(recs, useNA = "ifany")), table(recs, useNA = "ifany"), sep = "=", collapse = ", ")))
