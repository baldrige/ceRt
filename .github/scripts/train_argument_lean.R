# Fit the post-argument lean models and write data/argument_lean.json.
#
# Reads data-raw/transcript_backtest.rds, which transcript_backtest.R writes
# (run transcript_parse.R, then transcript_backtest.R, then this). Fits on every
# decided argument it holds and stores plain coefficients, plus the
# leave-one-Term-out figures the reader page quotes, so the page never states a
# number this file did not measure. Re-run after a Term's decisions are in.
#
#   Rscript .github/scripts/train_argument_lean.R
#
# See R/argument_lean.R and docs/argument-transcripts.md.
suppressPackageStartupMessages({ library(tidyverse); library(jsonlite) })

bt <- readRDS("data-raw/transcript_backtest.rds")
d  <- bt$cases
jd <- bt$justice_votes |>
  left_join(d |> select(dkt, term, lw_case = lw), by = c("dkt", "term")) |>
  mutate(lw_own = if_else(silent, 0, lw)) |>
  filter(!is.na(lw_case))

# ---- leave-one-Term-out, for the figures the page quotes --------------------
terms <- sort(unique(d$term))
loto_c <- map_dfr(terms, function(t) {
  tr <- d[d$term != t, ]; te <- d[d$term == t, ]
  m <- glm(pw ~ lq + lw, data = tr, family = binomial)
  te |> mutate(p = predict(m, te, type = "response"), p0 = mean(tr$pw))
})
loto_j <- map_dfr(terms, function(t) {
  tr <- jd[jd$term != t, ]; te <- jd[jd$term == t & jd$justice %in% tr$justice, ]
  m  <- glm(voted_pet ~ justice + lw_own + lw_case, data = tr, family = binomial)
  m0 <- glm(voted_pet ~ justice, data = tr, family = binomial)
  te |> mutate(p = predict(m, te, type = "response"), p0 = predict(m0, te, type = "response"))
})
acc <- function(p, y) mean((p >= 0.5) == (y == 1))
brier <- function(p, y) mean((p - y)^2)
backtest <- list(
  terms = sprintf("OT%d–OT%d", min(terms), max(terms)),
  case_n = nrow(loto_c),
  case_base_acc = round(mean(loto_c$pw == 1), 3), case_model_acc = round(acc(loto_c$p, loto_c$pw), 3),
  case_base_brier = round(brier(loto_c$p0, loto_c$pw), 4), case_model_brier = round(brier(loto_c$p, loto_c$pw), 4),
  justice_n = nrow(loto_j),
  justice_base_acc = round(acc(loto_j$p0, loto_j$voted_pet), 3), justice_model_acc = round(acc(loto_j$p, loto_j$voted_pet), 3),
  justice_base_brier = round(brier(loto_j$p0, loto_j$voted_pet), 4), justice_model_brier = round(brier(loto_j$p, loto_j$voted_pet), 4))
print(backtest |> as_tibble() |> pivot_longer(everything(), values_transform = as.character), n = 20)

# ---- the served fit: every Term --------------------------------------------
mc <- glm(pw ~ lq + lw, data = d, family = binomial)
mj <- glm(voted_pet ~ justice + lw_own + lw_case, data = jd, family = binomial)
mj0 <- glm(voted_pet ~ justice, data = jd, family = binomial)
# An aliased coefficient would serve as exactly zero (the cert model's lesson,
# CLAUDE.md): refuse to write one.
for (m in list(mc, mj, mj0)) if (anyNA(coef(m))) stop("aliased coefficient: ", paste(names(which(is.na(coef(m)))), collapse = ", "))
offsets <- function(m) {
  cf <- coef(m); nm <- names(cf)[startsWith(names(cf), "justice")]
  ref <- setdiff(sort(unique(jd$justice)), sub("^justice", "", nm))
  as.list(c(setNames(rep(0, length(ref)), ref), setNames(unname(cf[nm]), sub("^justice", "", nm))))
}
out <- list(
  fitted = format(Sys.Date()), base_rate = round(mean(d$pw), 4),
  case = list(intercept = unname(coef(mc)[1]), lq = unname(coef(mc)["lq"]), lw = unname(coef(mc)["lw"])),
  justice = list(intercept = unname(coef(mj)[1]), offset = offsets(mj),
                 lw_own = unname(coef(mj)["lw_own"]), lw_case = unname(coef(mj)["lw_case"])),
  justice0 = list(intercept = unname(coef(mj0)[1]), offset = offsets(mj0)),
  backtest = backtest)
write_json(out, "data/argument_lean.json", auto_unbox = TRUE, pretty = TRUE, digits = 6)
cat("Wrote data/argument_lean.json: case n =", nrow(d), "| Justice votes n =", nrow(jd), "\n")
