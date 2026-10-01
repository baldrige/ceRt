# Download and parse every oral-argument transcript for OT2017 onward, and
# report how cleanly the parser reads them. Research, not the publish path:
# writes data-raw/transcripts/*.pdf (gitignored) and data-raw/transcript_turns.rds.
#
#   Rscript .github/scripts/transcript_parse.R [first_term] [last_term]
#
# See docs/argument-transcripts.md.
suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(pdftools); library(jsonlite)
})
source("R/argument_nav.R")
source("R/argument_transcript.R")

args <- commandArgs(TRUE)
first <- as.integer(args[1] %||% 2017L); last <- as.integer(args[2] %||% 2025L)
dir <- "data-raw/transcripts"

idx <- transcript_index(first:last)
cat("Transcripts listed:", nrow(idx), "across OT", first, "-", last, "\n")
idx$file <- download_transcripts(idx, dir)
cat("Downloaded:", sum(!is.na(idx$file)), "of", nrow(idx), "\n")

parsed <- map(idx$file, function(f) {
  if (is.na(f)) return(NULL)
  tryCatch(parse_transcript(pdf_text(f)), error = function(e) { message(basename(f), ": ", conditionMessage(e)); NULL })
})
idx$parsed <- parsed
saveRDS(idx, "data-raw/transcript_turns.rds")

# ---- quality report --------------------------------------------------------------
q <- idx |>
  mutate(
    ok_parse   = map_lgl(parsed, ~ !is.null(.x) && nrow(.x$turns) > 0),
    submitted  = map_lgl(parsed, ~ isTRUE(.x$checks$submitted)),
    turns      = map_int(parsed, ~ if (is.null(.x)) 0L else nrow(.x$turns)),
    j_turns    = map_int(parsed, ~ if (is.null(.x)) 0L else sum(.x$turns$role == "justice")),
    segments   = map_int(parsed, ~ if (is.null(.x)) 0L else nrow(.x$segments)),
    sided      = map_int(parsed, ~ if (is.null(.x)) 0L else sum(!is.na(.x$segments$side))),
    has_both   = map_lgl(parsed, ~ !is.null(.x) && all(c("pet", "resp") %in% .x$segments$side)),
    pre_seg    = map_dbl(parsed, ~ if (is.null(.x) || !nrow(.x$turns)) NA else mean(.x$turns$segment == 0)),
    j_unsided  = map_dbl(parsed, function(p) {
      if (is.null(p) || !nrow(p$turns)) return(NA)
      jt <- p$turns[p$turns$role == "justice", ]
      s <- p$segments$side[match(jt$segment, p$segments$segment)]
      mean(is.na(s))
    }),
    unknown_j  = map_int(parsed, function(p) {
      if (is.null(p)) return(0L)
      sum(p$turns$role == "justice" & !p$turns$speaker %in% c(
        "Roberts", "Kennedy", "Thomas", "Ginsburg", "Breyer", "Alito", "Sotomayor",
        "Kagan", "Gorsuch", "Kavanaugh", "Barrett", "Jackson"))
    }))

cat("\n== Parse quality by Term ==\n")
q |> group_by(term) |>
  summarise(transcripts = n(), parsed = sum(ok_parse), ended_cleanly = sum(submitted),
            both_sides = sum(has_both), median_turns = median(turns),
            median_justice_turns = median(j_turns),
            pct_justice_turns_unsided = round(100 * mean(j_unsided, na.rm = TRUE), 1),
            unknown_justice_labels = sum(unknown_j), .groups = "drop") |>
  print(n = 50, width = 200)

cat("\n== Transcripts missing a side or with unsided Justice turns > 10% ==\n")
q |> filter(!has_both | j_unsided > 0.10) |>
  select(term, dkt, segments, sided, j_turns, j_unsided) |> print(n = 100)

cat("\n== Unsided segment headers (sample) ==\n")
hdr <- bind_rows(map2(idx$dkt, idx$parsed, ~ if (is.null(.y)) NULL else mutate(.y$segments, dkt = .x)))
hdr |> filter(is.na(side)) |> select(dkt, header) |> mutate(header = str_trunc(header, 140)) |> print(n = 60, width = 200)

cat("\n== Speaker labels seen (non-Justice, top) ==\n")
lab <- bind_rows(map(idx$parsed, ~ if (is.null(.x)) NULL else .x$turns))
print(head(sort(table(lab$speaker[lab$role == "advocate"]), decreasing = TRUE), 15))
cat("Justice labels:\n"); print(table(lab$speaker[lab$role == "justice"]))
