# Train the day-indexed cert model that runs in SHADOW (R/cert_daily.R) and
# write data/cert_model_daily.rds. Nothing a reader sees is built from it: the
# conferences run logs it beside the published headline in
# cases/headline_shadow.json, and docs/cert_model.md says what it has to show
# there before it may replace the headline.
#
# Reads the corpus and panel caches train_cert_model.R builds (run that first
# if they are missing) and the term archives; caches its own grid at
# DAILY_CACHE. No network. Leaves the shipped artifacts alone.
#
# Env: TERMS_GLOB (default "data-raw/ot_20*.rds"), CORPUS_CACHE, PANEL_CACHE,
# DAILY_CACHE (default "data-raw/cert_daily.rds"), OUT (default
# data/cert_model_daily.rds).

suppressPackageStartupMessages({library(tidyverse)})
source("R/cert_funnel.R")
source("R/cert_model.R")
source("R/cert_daily.R")

archives    <- Sys.glob(Sys.getenv("TERMS_GLOB", "data-raw/ot_20*.rds"))
corpus      <- readRDS(Sys.getenv("CORPUS_CACHE", "data-raw/cert_corpus.rds"))
panel       <- readRDS(Sys.getenv("PANEL_CACHE", "data-raw/cert_panel.rds"))
daily_cache <- Sys.getenv("DAILY_CACHE", "data-raw/cert_daily.rds")
out         <- Sys.getenv("OUT", DAILY_MODEL_PATH)

# Counsel tiers exactly as train_cert_model.R attaches them.
cix <- build_counsel_index(corpus)
corpus$counsel_tier <- counsel_tier(corpus$counsel_key, corpus$date, cix)
panel$counsel_tier  <- counsel_tier(panel$counsel_key,  panel$date,  cix)
complete <- complete_terms(corpus)

frame <- if (file.exists(daily_cache)) { message("Loaded cache ", daily_cache); readRDS(daily_cache) } else {
  f <- assemble_daily_frame(corpus, archives); saveRDS(f, daily_cache); message("Cached ", daily_cache); f }

cat("\n=== petition-days by stage (petition-weighted grant rate) ===\n")
print(as.data.frame(frame |> filter(!is.na(label)) |> group_by(stage) |>
  summarise(rows = n(), petition_days = round(sum(row_w)),
            grant_rate = sprintf("%.2f%%", 100 * weighted.mean(label, row_w)),
            response_requested = sprintf("%.0f%%", 100 * mean(response_requested)),
            relisted = sprintf("%.0f%%", 100 * mean(n_relists > 0)), .groups = "drop")),
  row.names = FALSE)

dm <- fit_daily_model(frame, corpus, panel, complete)

# ---- the like-for-like backtest ----------------------------------------------
# Every column out of fold: Term t scored by models fitted without it and
# mapped by per-stage maps fitted without it. `hyb` is the published hybrid's
# components given the same per-stage recalibration as the day model.
o <- dm$oof
cat("\n=== leave-one-Term-out, petition-weighted ===\n")
tab <- function(x) tibble(petitions = n_distinct(x$dkt),
                          grant_rate = sprintf("%.2f%%", 100 * sum(x$w * x$y) / sum(x$w)),
                          ll_hyb = daily_ll(x$y, x$hyb, x$w), ll_day = daily_ll(x$y, x$day, x$w),
                          br_hyb = daily_brier(x$y, x$hyb, x$w), br_day = daily_brier(x$y, x$day, x$w),
                          mean_hyb = sum(x$w * x$hyb) / sum(x$w), mean_day = sum(x$w * x$day) / sum(x$w))
print(as.data.frame(bind_rows(tab(o) |> mutate(stage = "all", .before = 1),
                              o |> group_by(stage) |> group_modify(~ tab(.x)) |> ungroup()) |>
                      mutate(across(where(is.double), ~ round(.x, 4)))), row.names = FALSE)
cat("\nPaired petition-clustered bootstrap, day - hyb (B = 200):\n")
print(as.data.frame(daily_paired_boot(o, "day", "hyb") |> mutate(across(where(is.double), ~ round(.x, 4)))),
      row.names = FALSE)
cat("\nBy Term, log loss day - hyb:\n")
print(as.data.frame(o |> group_by(term_year) |>
  summarise(d_ll = round(daily_ll(y, day, w) - daily_ll(y, hyb, w), 4), .groups = "drop")), row.names = FALSE)
cat("\nThe relist tail (post_conf rows forecast >= 20%):\n")
for (m in c("day", "hyb")) {
  x <- o[o$stage == "post_conf" & o[[m]] >= 0.2, ]
  cat(sprintf("  %-4s %4d petitions  forecast %.1f%%  observed %.1f%%\n", m, n_distinct(x$dkt),
              100 * sum(x$w * x[[m]]) / sum(x$w), 100 * sum(x$w * x$y) / sum(x$w)))
}

cat("\n=== day model ===\n")
co <- coef(dm$glm)
print(round(co[grep("stage|age_b|response|reply|waiver|cvsg|relist", names(co))], 3))
cat("Per-stage maps (intercept, slope on the raw logit):\n")
for (k in names(dm$stage_maps)) cat(sprintf("  %-10s %6.3f %6.3f\n", k, dm$stage_maps[[k]][1], dm$stage_maps[[k]][2]))
cat("Hybrid maps:\n")
for (k in names(dm$hybrid_maps)) cat(sprintf("  %-15s %6.3f %6.3f\n", k, dm$hybrid_maps[[k]][1], dm$hybrid_maps[[k]][2]))

dm$oof <- NULL
saveRDS(dm, out)
cat(sprintf("\nWrote %s (%.1f KB)\n", out, file.size(out) / 1024))
stopifnot(!is.null(load_daily_model(out)))
