# Measure the v1 and v2 Rule 10 cues (R/petition_signals.R) against outcomes on
# a stratified sample of paid, decided petitions, fetching each petition PDF
# once into the text cache so the patterns can be re-run for free afterwards.
#
# Grants are ~4% of paid petitions, so a uniform sample of a few hundred holds
# too few to say anything; the sample takes N_GRANT granted and N_DENY denied
# petitions spread evenly over TERMS. It prints, per cue, the share of grants
# and of denials it flags and the lift (grant rate flagged / unflagged), then
# writes the per-docket table so a hit can be traced back to its PDF.
#
# Env: TERMS (default "22,23,24"), N_GRANT (100), N_DENY (200), SEED (11),
# PETITION_TEXT_DIR (default data-raw/petition_text, gitignored),
# OUT (default data-raw/petition_signals_measure.csv), and CACHE (a signals
# JSON to accumulate into; default a file next to OUT). Run from the repo root:
#   Rscript .github/scripts/measure_petition_signals.R
suppressPackageStartupMessages({
  library(tidyverse); library(jsonlite)
})
source("R/conference_dash.R")    # find_petition_url, classify_petitions
source("R/cert_funnel.R")        # funnel_case_type
source("R/petition_signals.R")

terms   <- str_split(Sys.getenv("TERMS", "22,23,24"), ",")[[1]] |> str_trim()
n_grant <- as.integer(Sys.getenv("N_GRANT", "100"))
n_deny  <- as.integer(Sys.getenv("N_DENY", "200"))
seed    <- as.integer(Sys.getenv("SEED", "11"))
text_dir <- Sys.getenv("PETITION_TEXT_DIR", "data-raw/petition_text")
out     <- Sys.getenv("OUT", "data-raw/petition_signals_measure.csv")
cache   <- Sys.getenv("CACHE", sub("\\.csv$", ".json", out))

pool <- map_dfr(terms, function(tm) {
  f <- sprintf("data-raw/ot_20%s.rds", tm)
  if (!file.exists(f)) { message("no archive: ", f); return(NULL) }
  cases <- readRDS(f)
  cls <- classify_petitions(cases) |> select(dkt, outcome)
  cases |>
    left_join(cls, by = "dkt") |>
    mutate(term = tm, ctype = funnel_case_type(dkt),
           url = map_chr(events, find_petition_url),
           appx = map(events, find_appendix_urls)) |>
    filter(ctype == "paid", outcome %in% c("granted", "denied"),
           !is.na(url), nzchar(url)) |>
    distinct(dkt, .keep_all = TRUE) |>
    select(dkt, term, outcome, url, appx)
})
cat("pool:", nrow(pool), "paid decided petitions with a PDF;",
    sum(pool$outcome == "granted"), "granted;",
    sum(lengths(pool$appx) > 0), "with a separately filed appendix\n")
# A measurement recomputes every signal from the (cheap) text cache; only the
# PDFs are worth keeping between runs. KEEP_CACHE=1 skips the reset.
if (!nzchar(Sys.getenv("KEEP_CACHE"))) unlink(cache)

set.seed(seed)
per_term <- function(df, n) {
  k <- length(unique(df$term))
  df |> group_by(term) |> slice_sample(n = ceiling(n / k)) |> ungroup() |> slice_head(n = n)
}
sel <- bind_rows(per_term(filter(pool, outcome == "granted"), n_grant),
                 per_term(filter(pool, outcome == "denied"), n_deny))
cat("sample:", nrow(sel), "(", sum(sel$outcome == "granted"), "granted )\n")

sig <- resolve_petition_signals(sel$dkt, sel$url, cache_path = cache,
                                max_new = nrow(sel), text_dir = text_dir,
                                refresh_v1_entries = TRUE, appx_urls = sel$appx)
r <- sel |> select(-url, -appx) |> left_join(sig, by = "dkt") |>
  mutate(granted = outcome == "granted")
cat("text-extractable:", sum(r$pet_chars > 500, na.rm = TRUE), "of", nrow(r),
    "| appendix seen (in-line or separate):", sum(r$has_appendix, na.rm = TRUE),
    "| separate appendix text:", sum(r$appx_chars > 500, na.rm = TRUE), "\n\n")

cues <- c(v1_dissent_below = "dissent_below", v1_dissent_header = "dissent_header",
          v1_dissent_argued = "dissent_argued", v1_enbanc = "enbanc_dissent",
          v1_split = "split_argued",
          v2_dissent_below = "dissent_below2", v2_dissent_byline = "dissent_byline",
          v2_dissent_toc = "dissent_toc", v2_dissent_argued = "dissent_argued2",
          v2_enbanc = "enbanc_dissent2", v2_split = "split_argued2", v2_split_toc = "split_toc")
tab <- imap_dfr(cues, function(col, nm) {
  f <- coalesce(r[[col]], FALSE); g <- r$granted
  tibble(cue = nm,
         grants_flagged = sprintf("%d/%d", sum(f & g), sum(g)),
         denials_flagged = sprintf("%d/%d", sum(f & !g), sum(!g)),
         recall = round(mean(f[g]), 2), fpr = round(mean(f[!g]), 2),
         lift = round(mean(g[f]) / mean(g[!f]), 1))
})
print(as.data.frame(tab), row.names = FALSE)

cat("\nv1 dissent-header hits explained by citation parentheticals:",
    sum(r$n_dissent_cite, na.rm = TRUE), "citations across",
    sum(r$dissent_header, na.rm = TRUE), "flagged petitions\n")
cat("grant rate by net dissent count (mentions minus citations):\n")
r |> mutate(b = cut(n_dissent_net, c(-1, 0, 2, 5, 10, Inf))) |>
  group_by(b) |> summarise(n = n(), grant = round(mean(granted), 3)) |>
  as.data.frame() |> print(row.names = FALSE)

write_csv(r, out)
cat("\nper-docket table:", out, "| text cache:", text_dir, "\n")
