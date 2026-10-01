# Does the bench's questioning at oral argument predict who wins?
#
# Reads data-raw/transcript_turns.rds (transcript_parse.R), fetches each argued
# docket's JSON for its judgment, reads the Justices' lineups from the published
# site (justices/lineups.json), and backtests the classic signal -- the side
# that draws more of the bench's questions and words tends to lose -- at the
# case level and Justice by Justice, against a "petitioner always wins"
# baseline. Leave-one-Term-out, so no Term's outcomes inform its own forecast.
#
#   Rscript .github/scripts/transcript_backtest.R [site_dir]
#
# site_dir defaults to a lineups.json pulled from origin/gh-pages.
# See docs/argument-transcripts.md.
suppressPackageStartupMessages({
  library(tidyverse); library(httr2); library(jsonlite); library(pdftools)
  library(gt); library(gtExtras); library(htmltools)
})
src <- readLines("R/scotus_dash_new.R")
src <- src[-grep("^scotus_dash\\(", src)]
eval(parse(text = paste(src, collapse = "\n")))
source("R/argument_transcript.R")
source("R/justices.R")

tx <- readRDS("data-raw/transcript_turns.rds")
tx <- tx[!map_lgl(tx$parsed, is.null), ]

# ---- outcomes ------------------------------------------------------------------------
# The judgment line on the lead docket: REVERSED or VACATED (in whole or in
# part) is a petitioner win, AFFIRMED alone a loss, a dismissal no outcome.
judgment_of <- function(events) {
  if (!is.data.frame(events) || !"Proceedings and Orders" %in% names(events)) return(NA_character_)
  txt <- str_squish(str_remove_all(coalesce(events[["Proceedings and Orders"]], ""), "<[^>]+>"))
  hit <- txt[str_detect(txt, "REVERSED|AFFIRMED|VACATED|DISMISSED|improvidently granted") &
               !str_detect(txt, regex("^Judgment issued", ignore_case = TRUE))]
  if (length(hit)) hit[1] else NA_character_
}
petitioner_won <- function(j) {
  case_when(is.na(j) ~ NA_real_,
            str_detect(j, regex("DISMISSED|improvidently", ignore_case = TRUE)) ~ NA_real_,
            str_detect(j, "REVERSED|VACATED") ~ 1,
            str_detect(j, "AFFIRMED") ~ 0,
            TRUE ~ NA_real_)
}
oc_path <- "data-raw/transcript_outcomes.rds"
oc <- if (file.exists(oc_path)) readRDS(oc_path) else tibble(dkt = character(), judgment = character())
need <- setdiff(tx$dkt[!str_detect(tx$dkt, "^22O")], oc$dkt)
if (length(need)) {
  cat("Fetching", length(need), "docket(s) for their judgments\n")
  got <- fetch_cases(need)
  oc <- bind_rows(oc, tibble(dkt = got$dkt, judgment = map_chr(got$events, judgment_of)))
  saveRDS(oc, oc_path)
}
cases <- tx |>
  select(term, dkt, dkts, parsed) |>
  left_join(oc, by = "dkt") |>
  mutate(pw = petitioner_won(judgment))
cat("\nOutcomes: ", sum(!is.na(cases$pw)), " of ", nrow(cases), " argued cases have a merits outcome (",
    sum(cases$pw, na.rm = TRUE), " petitioner wins)\n", sep = "")

# ---- bench measures --------------------------------------------------------------------
side_totals <- function(p, parties_only) {
  b <- tx_bench(p, parties_only)
  if (is.null(b) || !nrow(b)) return(tibble(q_pet = NA_real_, q_resp = NA_real_, w_pet = NA_real_, w_resp = NA_real_))
  s <- b |> group_by(side) |> summarise(q = sum(turns), w = sum(words), .groups = "drop")
  g <- function(col, sd) { v <- s[[col]][s$side == sd]; if (length(v)) v else 0 }
  tibble(q_pet = g("q", "pet"), q_resp = g("q", "resp"), w_pet = g("w", "pet"), w_resp = g("w", "resp"))
}
feat <- function(d) d |>
  mutate(lq = log((q_resp + 1) / (q_pet + 1)), lw = log((w_resp + 1) / (w_pet + 1)))

for (variant in c("parties_only", "all_segments")) {
  po <- variant == "parties_only"
  d <- cases |> filter(!is.na(pw)) |>
    mutate(st = map(parsed, side_totals, parties_only = po)) |> unnest(st) |>
    filter(!is.na(q_pet), q_pet > 0, q_resp > 0) |> feat()
  cat("\n================ Case level:", variant, "(n =", nrow(d), ") ================\n")
  # The rule: the side that draws more Justice turns loses (a tie goes to the petitioner).
  d <- d |> mutate(rule_q = as.numeric(q_pet <= q_resp), rule_w = as.numeric(w_pet <= w_resp))
  # Leave-one-Term-out logistic fit and a base-rate baseline fit the same way.
  loto <- map_dfr(sort(unique(d$term)), function(t) {
    tr <- d[d$term != t, ]; te <- d[d$term == t, ]
    m <- glm(pw ~ lq + lw, data = tr, family = binomial)
    te |> mutate(p_model = predict(m, te, type = "response"), p_base = mean(tr$pw))
  })
  acc <- function(pred, y) mean(pred == y)
  brier <- function(p, y) mean((p - y)^2)
  by_term <- loto |> group_by(term) |>
    summarise(n = n(), pet_win_rate = mean(pw),
              acc_baseline = mean(pw == 1),
              acc_rule_questions = acc(rule_q, pw), acc_rule_words = acc(rule_w, pw),
              acc_model = acc(as.numeric(p_model >= 0.5), pw), .groups = "drop")
  print(by_term |> mutate(across(where(is.double), ~ round(.x, 3))), n = 20, width = 200)
  cat(sprintf("\nPooled (n=%d): baseline %.3f | rule (questions) %.3f | rule (words) %.3f | LOTO model %.3f\n",
              nrow(loto), mean(loto$pw == 1), acc(loto$rule_q, loto$pw), acc(loto$rule_w, loto$pw),
              acc(as.numeric(loto$p_model >= 0.5), loto$pw)))
  cat(sprintf("Brier: base rate %.4f | LOTO model %.4f\n", brier(loto$p_base, loto$pw), brier(loto$p_model, loto$pw)))
  full <- glm(pw ~ lq + lw, data = d, family = binomial)
  print(summary(full)$coefficients |> round(3))
  # How strong is the signal where it is strong? Accuracy by imbalance tercile.
  cat("\nRule (words) accuracy by size of the imbalance:\n")
  print(d |> mutate(imb = ntile(abs(lw), 3)) |> group_by(imb) |>
          summarise(n = n(), mean_abs_log_ratio = round(mean(abs(lw)), 2),
                    acc_rule_words = round(acc(rule_w, pw), 3), .groups = "drop"))
  # Calibration: the petitioner's actual win rate by quintile of the LOTO forecast.
  cat("\nCalibration (LOTO model, quintiles of forecast):\n")
  print(loto |> mutate(q = ntile(p_model, 5)) |> group_by(q) |>
          summarise(n = n(), mean_forecast = round(mean(p_model), 3), pet_won = round(mean(pw), 3),
                    median_word_ratio_resp_to_pet = round(exp(median(lw)), 2), .groups = "drop"))
  if (po) case_d <- d
}

# ---- Justice level ---------------------------------------------------------------------
lineups_file <- commandArgs(TRUE)[1]
if (is.na(lineups_file)) {
  lineups_file <- tempfile(fileext = ".json")
  system2("git", c("show", "origin/gh-pages:justices/lineups.json"), stdout = lineups_file)
} else lineups_file <- file.path(lineups_file, "justices", "lineups.json")
lu <- fromJSON(lineups_file, simplifyVector = FALSE)
alias <- unlist(unname(imap(lu,function(e, k) setNames(rep(k, length(unlist(e$also)) + 1L), c(k, unlist(e$also))))))
alias <- alias[!duplicated(names(alias))]

jv <- case_d |>
  mutate(key = map_chr(dkts, function(ds) { k <- alias[intersect(ds, names(alias))]; if (length(k)) k[[1]] else NA_character_ })) |>
  filter(!is.na(key)) |>
  mutate(votes = map2(key, term, function(k, t) {
    e <- lu[[k]]
    if (!isTRUE(e$parsed)) return(NULL)
    decision_votes(e, term_court(t %% 100L), decided = e$decided)
  })) |>
  filter(!map_lgl(votes, is.null))
cat("\n================ Justice level ================\n")
cat("Cases with a parsed lineup:", nrow(jv), "of", nrow(case_d), "\n")

jb <- jv |>
  mutate(bench = map(parsed, tx_bench, parties_only = TRUE)) |>
  select(term, dkt, pw, votes, bench)
jd <- pmap_dfr(jb, function(term, dkt, pw, votes, bench) {
  b <- bench |> pivot_wider(id_cols = speaker, names_from = side, values_from = c(turns, words), values_fill = 0)
  for (c in c("turns_pet", "turns_resp", "words_pet", "words_resp")) if (!c %in% names(b)) b[[c]] <- 0
  votes |> filter(side %in% c("majority", "dissent")) |>
    transmute(term, dkt, justice = name, voted_pet = as.numeric(if_else(side == "majority", pw, 1 - pw))) |>
    left_join(b |> rename(justice = speaker), by = "justice") |>
    mutate(across(c(turns_pet, turns_resp, words_pet, words_resp), ~ coalesce(.x, 0)))
})
jd <- jd |> mutate(silent = turns_pet + turns_resp == 0,
                   lw = log((words_resp + 1) / (words_pet + 1)),
                   rule = as.numeric(words_pet <= words_resp))
spoke <- jd |> filter(!silent)
cat(sprintf("Justice-votes: %d (%d where the Justice spoke to at least one side)\n", nrow(jd), nrow(spoke)))
cat(sprintf("Where the Justice spoke: baseline (votes petitioner) %.3f | own-words rule %.3f\n",
            mean(spoke$voted_pet == 1), mean(spoke$rule == spoke$voted_pet)))
cat("\nBy Justice (where they spoke):\n")
print(spoke |> group_by(justice) |>
        summarise(votes = n(), pet_rate = round(mean(voted_pet), 3),
                  acc_baseline = round(pmax(mean(voted_pet), 1 - mean(voted_pet)), 3),
                  acc_own_words_rule = round(mean(rule == voted_pet), 3), .groups = "drop") |>
        arrange(desc(votes)), n = 20)

# The raw rule ignores that petitioners draw more words by construction (they
# go first and have rebuttal); a fitted intercept absorbs that. Leave-one-Term-
# out: each Justice's vote from their own imbalance and the whole bench's, with
# a per-Justice intercept, against that Justice's own petitioner rate.
jd2 <- jd |> left_join(case_d |> select(dkt, lw_case = lw), by = "dkt") |>
  mutate(lw_own = if_else(silent, 0, lw))
jl <- map_dfr(sort(unique(jd2$term)), function(t) {
  tr <- jd2[jd2$term != t, ]; te <- jd2[jd2$term == t & jd2$justice %in% tr$justice, ]
  m  <- glm(voted_pet ~ justice + lw_own + lw_case, data = tr, family = binomial)
  m0 <- glm(voted_pet ~ justice, data = tr, family = binomial)
  te |> mutate(p = predict(m, te, type = "response"), p0 = predict(m0, te, type = "response"))
})
cat(sprintf("\nLOTO, all %d Justice-votes: baseline (each Justice's own petitioner rate) acc %.3f, Brier %.4f | model acc %.3f, Brier %.4f\n",
            nrow(jl), mean((jl$p0 >= .5) == jl$voted_pet), mean((jl$p0 - jl$voted_pet)^2),
            mean((jl$p >= .5) == jl$voted_pet), mean((jl$p - jl$voted_pet)^2)))
print(summary(glm(voted_pet ~ justice + lw_own + lw_case, data = jd2, family = binomial))$coefficients[c("lw_own", "lw_case"), ] |> round(3))
cat("\nBy Justice, LOTO:\n")
print(jl |> group_by(justice) |>
        summarise(votes = n(), acc_base = round(mean((p0 >= .5) == voted_pet), 3),
                  acc_model = round(mean((p >= .5) == voted_pet), 3),
                  brier_gain_pct = round(100 * (1 - mean((p - voted_pet)^2) / mean((p0 - voted_pet)^2)), 1),
                  .groups = "drop") |> arrange(desc(brier_gain_pct)), n = 20)
cat("\nThomas-style silence: share of cases each Justice asked nothing\n")
print(jd |> group_by(justice) |> summarise(cases = n(), silent = round(mean(silent), 3), .groups = "drop") |>
        arrange(desc(silent)), n = 20)
saveRDS(list(cases = case_d |> select(-parsed), justice_votes = jd), "data-raw/transcript_backtest.rds")
