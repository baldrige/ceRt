# The day-indexed cert model, run in SHADOW. Nothing here is shown to readers.
#
# The case page's headline is the v47 hybrid (docs/cert_model.md, "The case
# page's headline"): the petition-stage number until a petition is distributed,
# the at-risk P(granted ever) after. A day-indexed model -- one row per petition
# per pending day, with stage and age clock features -- beat it in a backtest,
# but an adversarial review cut the edge to -0.0025 log loss once the hybrid was
# given the same recalibration, with no gain before distribution and an
# over-forecast relist tail. So it runs here first, logged prospectively beside
# the number the page shows, until a Term of resolved petitions says whether it
# should replace it. The rule it has to meet is written down in advance in
# docs/cert_model.md ("The day model, in shadow").
#
# What changed from the prototype the review measured:
#   * a weighted Firth fit (firth_fit(weights =)), each petition weighing 1 in
#     total whatever its number of pending-day rows. The unpenalised prototype
#     drove pro se and court_below OTHER to -16;
#   * `stage` no longer calls a petition "after a conference" because a
#     response was requested or the SG invited -- see daily_stage(). 58% of the
#     prototype's post_conf rows were response requests;
#   * DISTRIBUTED entries with a long-form date ("September 25, 2017", 124 in
#     OT2017) are read;
#   * no resp_ext;
#   * per-stage Platt maps fitted on leave-one-Term-out predictions, and the
#     SAME per-stage recipe fitted for the hybrid, so the comparison is
#     like-for-like (the review's first finding).
#
# The artifact (data/cert_model_daily.rds) is deliberately NOT loaded by
# load_cert_models(): every case page's manifest key hashes that list, so adding
# it would re-render the back-catalog for a model no page shows.

DAILY_CLOCK_FEATURES <- c("stage", "age_b")
DAILY_FEATURES <- c(BASELINE_FEATURES, PROCESS_FEATURES, DAILY_CLOCK_FEATURES)
DAILY_STAGES <- c("pre_dist", "dist", "post_conf")
DAILY_GRID_DAYS <- 14L        # the training grid, and the shadow log's cadence
DAILY_GRID_MAX  <- 40L        # rows per petition, at most
DAILY_MODEL_PATH <- "data/cert_model_daily.rds"
HEADLINE_SHADOW_LOG <- "cases/headline_shadow.json"

# The conference named in each DISTRIBUTED entry, in either form the Clerk has
# used ("10/16/2026", "September 25, 2017"). conference_dates_from_events()
# reads only the first; the second is 1.5% of OT2017's entries.
.daily_conf_date <- function(txt) {
  m <- str_match(txt, "DISTRIBUTED for Conference of (\\d{1,2}/\\d{1,2}/\\d{4}|[A-Z][a-z]+ \\d{1,2}, \\d{4})")[, 2]
  suppressWarnings(lubridate::mdy(m))
}

# A docket's entries, parsed once: what daily_features_at() needs to place the
# petition on any day.
daily_events <- function(events) {
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events)) ||
      nrow(events) == 0)
    return(list(ed = as.Date(character()), txt = character(), relist = as.Date(character())))
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  ed <- suppressWarnings(lubridate::mdy(events$Date))
  ok <- !is.na(ed); txt <- txt[ok]; ed <- ed[ok]
  o <- order(ed); txt <- txt[o]; ed <- ed[o]
  cl <- tryCatch(classify_petition_events(events), error = function(e) NULL)
  list(ed = ed, txt = txt,
       is_dist = str_detect(txt, FUNNEL_PATTERNS$dist),
       conf = .daily_conf_date(txt),
       # What sends a distributed petition back to briefing rather than to a
       # conference: a called-for response or a CVSG. A reschedule moves the
       # conference without holding it.
       is_back = str_detect(txt, FUNNEL_PATTERNS$cfr) | str_detect(txt, FUNNEL_PATTERNS$cvsg),
       is_resched = str_detect(txt, FUNNEL_PATTERNS$resched),
       relist = if (is.null(cl)) as.Date(character()) else cl$relist_dates[[1]])
}

# Where the petition stands on day `as_of`, from entries strictly before it:
#   pre_dist  -- never distributed, or sent back to briefing (a response
#                requested, the SG invited) since its last distribution;
#   dist      -- distributed for a conference not yet held (or rescheduled);
#   post_conf -- the conference it was last distributed for has been held and
#                nothing has sent it back: awaiting the order list, relisted
#                for later, or held.
# The prototype's rule -- "any conference named in any DISTRIBUTED entry is
# past" -- read a petition pulled for a response as having survived a
# conference, which on 58% of its post_conf rows it had not.
daily_stage <- function(e, as_of) {
  vapply(as_of, function(a) {
    before <- e$ed < a
    di <- which(e$is_dist & before)
    if (!length(di)) return("pre_dist")
    last <- max(di)
    after <- seq_along(e$ed) > last & before
    if (any(e$is_back[after])) return("pre_dist")
    if (any(e$is_resched[after])) return("dist")
    cd <- e$conf[last]
    if (!is.na(cd) && cd < a) "post_conf" else "dist"
  }, character(1))
}

daily_age_b <- function(age) {
  as.character(cut(age, c(-Inf, 30, 60, 120, 240, Inf),
                   labels = c("0-30", "31-60", "61-120", "121-240", "241+")))
}

# The process features strictly before each date in `cut`, the same counts
# process_features() makes, vectorised over dates.
.daily_process_at <- function(e, cut) {
  cnt <- function(flag) { d <- e$ed[flag]; if (!length(d)) rep(0L, length(cut)) else colSums(outer(d, cut, "<")) }
  rd <- e$relist[!is.na(e$relist)]
  nrel <- if (!length(rd)) rep(0L, length(cut)) else colSums(outer(rd, cut, "<"))
  tibble(n_amicus_cert      = cnt(str_detect(e$txt, AMICUS_RX)),
         cvsg               = cnt(str_detect(e$txt, CVSG_RX)) > 0,
         response_requested = cnt(str_detect(e$txt, "^Response Requested")) > 0,
         response_filed     = cnt(str_detect(e$txt, "in opposition filed")) > 0,
         resp_waiver        = cnt(str_detect(e$txt, "^Waiver of right of respondent")) > 0,
         reply_filed        = cnt(str_detect(e$txt, "^Reply of (petitioner|applicant|appellant)")) > 0,
         n_relists          = as.integer(nrel))
}

# The date the published hybrid scores its at-risk number as of, on a day `as_of`
# (R/docket_page.R): the next conference still ahead, else the last one; NA
# before any distribution, when the page shows the petition-stage number. Only
# DISTRIBUTED entries already on the docket count.
.hybrid_cut <- function(e, as_of) {
  as.Date(vapply(as_of, function(a) {
    cd <- e$conf[e$is_dist & e$ed < a]; cd <- cd[!is.na(cd)]
    if (!length(cd)) return(NA_real_)
    ahead <- cd[cd >= a]
    as.numeric(if (length(ahead)) a else max(cd))
  }, numeric(1)), origin = "1970-01-01")
}

# ---- training frame ------------------------------------------------------------

# One row per resolved paid petition per pending day, on a DAILY_GRID_DAYS grid
# from the day after docketing to the day before the disposition (at most
# DAILY_GRID_MAX rows). Each row carries the day model's features as of that
# day, the published hybrid's at-risk features as of its own cut (`h_*`
# columns), and a weight of 1/rows so each petition counts once.
#
# `corpus` is the disposition corpus with counsel_tier attached;
# `archives` the term files it was assembled from.
assemble_daily_frame <- function(corpus, archives) {
  pet <- corpus |> filter(type == "paid", !is.na(label), !is.na(outcome_date))
  ev <- list()
  for (p in archives) {
    a <- readRDS(p); a <- a[a$dkt %in% pet$dkt, c("dkt", "events")]
    ev[a$dkt] <- a$events
  }
  pet <- pet |> filter(dkt %in% names(ev))
  message("Daily frame: ", nrow(pet), " resolved paid petitions")
  static <- setdiff(names(pet), c(names(.daily_process_at(daily_events(NULL), Sys.Date())),
                                  "n_dist", "has_cfr", "has_resp", "has_amicus"))
  rows <- purrr::map(seq_len(nrow(pet)), function(i) {
    d0 <- as.Date(pet$date[i]); od <- as.Date(pet$outcome_date[i])
    if (is.na(d0) || od - 1 < d0 + 1) return(NULL)
    A <- seq(d0 + 1, od - 1, by = DAILY_GRID_DAYS)
    A <- A[seq_len(min(length(A), DAILY_GRID_MAX))]
    e <- daily_events(ev[[pet$dkt[i]]])
    hc <- .hybrid_cut(e, A)
    h <- .daily_process_at(e, ifelse(is.na(hc), A, hc) |> as.Date(origin = "1970-01-01"))
    names(h) <- paste0("h_", names(h))
    bind_cols(tibble(dkt = pet$dkt[i], as_of = A, row_w = 1 / length(A),
                     stage = daily_stage(e, A), age_b = daily_age_b(as.numeric(A - d0)),
                     h_ever = !is.na(hc)),
              .daily_process_at(e, A), h)
  })
  bind_rows(rows) |> left_join(pet[, static], by = "dkt")
}

# model_frame() for the grid: it keeps one row per petition-day, so the docket
# is made unique for it and split back afterwards, and the row weight and the
# hybrid's columns are carried through by row id.
daily_model_frame <- function(frame, features, h = FALSE) {
  d <- frame; d$rid <- seq_len(nrow(d))
  if (h) {                                   # the hybrid's at-risk view of the row
    hc <- grep("^h_", names(d), value = TRUE)
    d[sub("^h_", "", hc)] <- d[hc]
  }
  m <- model_frame(d |> mutate(dkt = paste(dkt, rid, sep = "#")), features)
  m$rid <- as.integer(sub(".*#", "", m$dkt)); m$dkt <- sub("#.*", "", m$dkt)
  if ("stage" %in% features) m$stage <- factor(as.character(m$stage), levels = DAILY_STAGES)
  m$row_w <- frame$row_w[m$rid]
  m
}

# ---- weighted calibration ------------------------------------------------------

# Platt on the logit, weighted. Returns c(a, b); apply with .platt_ab().
.fit_platt_w <- function(y, p, w) {
  z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  unname(coef(suppressWarnings(glm(y ~ z, family = binomial(), weights = w))))
}
.platt_ab <- function(ab, p) {
  z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  plogis(ab[[1]] + ab[[2]] * z)
}
# The raw probability a shipped cert_model's Platt map was applied to: its
# inverse, so the hybrid's published number can be recalibrated from the raw
# scale its per-stage map was fitted on.
.platt_inverse <- function(cal, p) {
  co <- coef(cal); z <- qlogis(pmin(pmax(p, 1e-9), 1 - 1e-9))
  plogis((z - co[[1]]) / co[[2]])
}

# One map per key (stage, or component.stage), each fitted only if it has at
# least `min_pos` weighted grants; a key without one falls back to `fallback`.
.fit_stage_maps <- function(y, p, w, key, min_pos = 10) {
  maps <- list()
  for (k in sort(unique(key))) {
    s <- key == k
    if (sum(y[s]) >= min_pos) maps[[k]] <- .fit_platt_w(y[s], p[s], w[s])
  }
  maps
}
.apply_stage_maps <- function(maps, p, key, fallback = c(0, 1)) {
  vapply(seq_along(p), function(i) .platt_ab(maps[[key[i]]] %||% fallback, p[i]), numeric(1))
}

# ---- metrics -------------------------------------------------------------------

daily_ll <- function(y, p, w) { p <- pmin(pmax(p, 1e-4), 1 - 1e-4); -sum(w * (y * log(p) + (1 - y) * log(1 - p))) / sum(w) }
daily_brier <- function(y, p, w) sum(w * (p - y)^2) / sum(w)

# Paired, petition-clustered bootstrap of the difference in petition-weighted
# log loss and Brier between two forecast columns. Returns the estimate and a
# 95% interval for each.
daily_paired_boot <- function(d, a, b, B = 200L, seed = 20261020L) {
  set.seed(seed)
  ids <- unique(d$dkt); idx <- split(seq_len(nrow(d)), d$dkt)
  r <- replicate(B, {
    s <- unlist(idx[sample(ids, replace = TRUE)], use.names = FALSE); x <- d[s, ]
    c(daily_ll(x$y, x[[a]], x$w) - daily_ll(x$y, x[[b]], x$w),
      daily_brier(x$y, x[[a]], x$w) - daily_brier(x$y, x[[b]], x$w))
  })
  tibble(metric = c("log loss", "Brier"),
         est = c(daily_ll(d$y, d[[a]], d$w) - daily_ll(d$y, d[[b]], d$w),
                 daily_brier(d$y, d[[a]], d$w) - daily_brier(d$y, d[[b]], d$w)),
         lo = apply(r, 1, quantile, 0.025), hi = apply(r, 1, quantile, 0.975))
}

# ---- fit -----------------------------------------------------------------------

# Fit the day model and the per-stage maps for it and for the hybrid, from
# leave-one-Term-out predictions on the grid. `baseline` and `enhanced` are the
# shipped artifacts' specs (their features) -- the hybrid's components are
# refitted per fold here exactly as fit_cert_model() does, unpenalised by
# nothing it does not also do. Returns the artifact and the out-of-fold table
# it was judged on.
fit_daily_model <- function(frame, corpus, panel, complete) {
  mf_day  <- daily_model_frame(frame, DAILY_FEATURES)
  mf_base <- daily_model_frame(frame, BASELINE_FEATURES)
  mf_ever <- daily_model_frame(frame, ATRISK_FEATURES, h = TRUE)
  mf_c <- model_frame(corpus, BASELINE_FEATURES)
  mf_p <- model_frame(panel, ATRISK_FEATURES)
  message("Day model frame: ", nrow(mf_day), " petition-days, ",
          n_distinct(mf_day$dkt), " petitions, ", round(sum(mf_day$row_w * mf_day$label)), " grants")

  n <- nrow(frame)
  raw_day <- raw_base <- raw_ever <- rep(NA_real_, n)
  for (t in sort(unique(mf_day$term_year))) {
    message("  leave out OT", t)
    md <- fit_logit(mf_day[mf_day$term_year != t, ], DAILY_FEATURES, weights = "row_w")
    mb <- fit_logit(mf_c[mf_c$term_year != t, ], BASELINE_FEATURES)
    me <- fit_logit(mf_p[mf_p$term_year != t, ], ATRISK_FEATURES)
    te <- mf_day$term_year == t;  raw_day[mf_day$rid[te]]   <- predict(md, mf_day[te, ], type = "response")
    te <- mf_base$term_year == t; raw_base[mf_base$rid[te]] <- predict(mb, mf_base[te, ], type = "response")
    te <- mf_ever$term_year == t; raw_ever[mf_ever$rid[te]] <- predict(me, mf_ever[te, ], type = "response")
  }
  oof <- tibble(rid = seq_len(n), dkt = frame$dkt, term_year = frame$term_year,
                stage = frame$stage, h_ever = frame$h_ever, w = frame$row_w,
                y = frame$label, raw_day, raw_hyb = if_else(frame$h_ever, raw_ever, raw_base)) |>
    filter(!is.na(raw_day), !is.na(raw_hyb), !is.na(y)) |>
    mutate(hkey = paste(if_else(h_ever, "ever", "base"), stage, sep = "."))

  # Out-of-fold calibration, for the table: each Term mapped by maps fitted on
  # the other Terms, the day model per stage, the hybrid per component x stage.
  oof$day <- oof$hyb <- NA_real_
  for (t in unique(oof$term_year)) {
    tr <- oof$term_year != t; te <- !tr
    md <- .fit_stage_maps(oof$y[tr], oof$raw_day[tr], oof$w[tr], oof$stage[tr])
    mh <- .fit_stage_maps(oof$y[tr], oof$raw_hyb[tr], oof$w[tr], oof$hkey[tr])
    oof$day[te] <- .apply_stage_maps(md, oof$raw_day[te], oof$stage[te],
                                     fallback = .fit_platt_w(oof$y[tr], oof$raw_day[tr], oof$w[tr]))
    oof$hyb[te] <- .apply_stage_maps(mh, oof$raw_hyb[te], oof$hkey[te],
                                     fallback = .fit_platt_w(oof$y[tr], oof$raw_hyb[tr], oof$w[tr]))
  }

  final <- fit_logit(mf_day, DAILY_FEATURES, weights = "row_w")
  aliased <- names(which(is.na(coef(final))))
  if (length(aliased))
    stop("day model: aliased (NA) coefficient(s): ", paste(aliased, collapse = ", "))
  ct <- if (length(complete)) mf_day$term_year %in% complete else rep(TRUE, nrow(mf_day))
  structure(list(
    glm = strip_glm(final), features = DAILY_FEATURES, xlevels = final$xlevels,
    target = "grant",
    base_rate = sum(mf_day$row_w[ct] * mf_day$label[ct]) / sum(mf_day$row_w[ct]),
    xbar = colMeans(model.matrix(final)),
    stage_maps = .fit_stage_maps(oof$y, oof$raw_day, oof$w, oof$stage),
    stage_fallback = .fit_platt_w(oof$y, oof$raw_day, oof$w),
    hybrid_maps = .fit_stage_maps(oof$y, oof$raw_hyb, oof$w, oof$hkey),
    hybrid_fallback = .fit_platt_w(oof$y, oof$raw_hyb, oof$w),
    # The hybrid's maps act on the raw scale of the components they were fitted
    # beside. A retrain of either changes that scale, so the log refuses to
    # recalibrate against components it was not fitted with.
    components = c(baseline = paste(BASELINE_FEATURES, collapse = "+"),
                   enhanced = paste(ATRISK_FEATURES, collapse = "+")),
    fitted_on = format(Sys.Date()),
    oof = oof
  ), class = "daily_model")
}

load_daily_model <- function(path = DAILY_MODEL_PATH) {
  if (!file.exists(path)) return(NULL)
  m <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!inherits(m, "daily_model") || !setequal(m$features, DAILY_FEATURES) ||
      any(is.na(coef(m$glm)))) {
    warning("load_daily_model(): ", path, " is stale or unreadable; shadow log skipped.",
            call. = FALSE)
    return(NULL)
  }
  m
}

# ---- serve ---------------------------------------------------------------------

# The day model's raw and calibrated P(granted ever) for one live petition on
# `as_of`, with the features built exactly as score_case() builds them.
score_daily <- function(dm, caption, lower, parties, date, lower_date, related,
                        events, as_of, signals = NULL, counsel_index = NULL) {
  as_of <- as.Date(as_of)
  f <- petition_features(caption, lower, parties, date, lower_date, related)
  f$counsel_tier <- counsel_tier(f$counsel_key, date, counsel_index)
  f <- bind_cols(f, signal_features(signals))
  e <- daily_events(events)
  f <- bind_cols(f, .daily_process_at(e, as_of))
  f$relist_bucket <- relist_bucket(f$n_relists)
  f$amicus_bucket <- amicus_bucket(f$n_amicus_cert)
  f$stage <- daily_stage(e, as_of)
  f$age_b <- daily_age_b(as.numeric(as_of - as.Date(date)))
  for (v in intersect(names(dm$xlevels), names(f)))
    f[[v]] <- factor(as.character(f[[v]]), levels = dm$xlevels[[v]])
  beta <- coef(dm$glm)
  mm <- model.matrix(delete.response(terms(dm$glm)), data = f, xlev = dm$xlevels,
                     contrasts.arg = dm$glm$contrasts)
  raw <- plogis(sum(mm[1, names(beta)] * beta))
  st <- as.character(f$stage)
  list(raw = raw, prob = .platt_ab(dm$stage_maps[[st]] %||% dm$stage_fallback, raw),
       stage = st, age_b = as.character(f$age_b), hybrid_ever = !is.na(.hybrid_cut(e, as_of)))
}

# The headline the case page shows on `as_of` (R/docket_page.R, v47): the
# petition-stage number before any distribution, the at-risk P(granted ever)
# as of the next conference ahead (else the last) after, the SG rate once the
# SG has answered. `src` says which.
published_headline <- function(models, cx, events, as_of, signals = NULL) {
  par <- cx$parties[[1]]; rel <- cx$related %||% NA_character_
  base <- tryCatch(score_case(models$baseline, cx$caption, cx$lower, par, cx$date,
                              cx$lower_date, rel, signals = signals,
                              counsel_index = models$counsel_index)$prob,
                   error = function(e) NA_real_)
  cds <- conference_dates_from_events(events)
  if (!length(cds)) return(list(p = base, src = "base"))
  ahead <- cds[cds >= as_of]
  ac <- if (length(ahead)) min(ahead) else max(cds)
  s <- tryCatch(score_conference(models, cx$caption, cx$lower, par, cx$date, cx$lower_date,
                                 rel, events = events, as_of = ac, signals = signals),
                error = function(e) NULL)
  if (is.null(s) || is.na(s$p_grant_ever)) return(list(p = base, src = "base"))
  list(p = s$p_grant_ever, src = if (isTRUE(s[["sg_layer"]])) "sg" else "ever")
}

# ---- the shadow log ------------------------------------------------------------

read_headline_shadow <- function(site_dir) {
  p <- file.path(site_dir, HEADLINE_SHADOW_LOG)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) list())
}

# Append one entry per pending paid petition in `cases` whose last entry is at
# least DAILY_GRID_DAYS old (or which has none): the published headline, the
# hybrid recalibrated per stage, and the day model, all as of today. Keyed
# "<docket>@<date>", append-only, never rewritten -- the same rules as the
# forecast ledger (R/forecast_log.R), and union-merged on publish for the same
# reason. A petition's entries end when it is decided; scoring joins outcomes
# afterwards (score_headline_shadow()).
#
# Fields: dkt, as_of, stage, age_b; head (the published headline) and src
# (base / ever / sg); hyb (head recalibrated per component x stage); day and
# day_raw; dm (the day model's fit date).
append_headline_shadow <- function(site_dir, cases, models, dm, signals_map = NULL,
                                   as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  log <- read_headline_shadow(site_dir)
  last <- list()
  for (k in names(log)) {
    d <- log[[k]]$dkt; a <- as.Date(log[[k]]$as_of)
    if (is.null(last[[d]]) || a > last[[d]]) last[[d]] <- a
  }
  if (any(vapply(log, function(e) as.Date(e$as_of) > as_of, logical(1))))
    stop("headline shadow log holds entries dated after ", as_of, "; refusing to append")
  comp_ok <- identical(dm$components[["baseline"]], paste(models$baseline$features, collapse = "+")) &&
             identical(dm$components[["enhanced"]], paste(models$enhanced$features, collapse = "+"))
  if (!comp_ok) warning("append_headline_shadow(): the shipped models' features differ from ",
                        "those the hybrid maps were fitted beside; `hyb` left empty.", call. = FALSE)
  paid <- cases |> filter(funnel_case_type(dkt) == "paid")
  added <- 0L; considered <- 0L
  for (i in seq_len(nrow(paid))) {
    cx <- paid[i, ]; ev <- cx$events[[1]]
    cl <- tryCatch(classify_petition_events(ev), error = function(e) NULL)
    if (is.null(cl) || !identical(cl$outcome[[1]], "pending")) next
    if (is.na(as.Date(cx$date)) || as.Date(cx$date) >= as_of) next
    considered <- considered + 1L
    if (!is.null(last[[cx$dkt]]) && as_of - last[[cx$dkt]] < DAILY_GRID_DAYS) next
    sig <- signals_map[[cx$dkt]]
    h <- published_headline(models, cx, ev, as_of, signals = sig)
    d <- tryCatch(score_daily(dm, cx$caption, cx$lower, cx$parties[[1]], cx$date,
                              cx$lower_date, cx$related %||% NA_character_, ev, as_of,
                              signals = sig, counsel_index = models$counsel_index),
                  error = function(e) NULL)
    if (is.null(d) || is.na(h$p)) next
    hyb <- NA_real_
    if (comp_ok && h$src != "sg") {
      comp <- if (h$src == "ever") models$enhanced else models$baseline
      key <- paste(h$src, d$stage, sep = ".")
      hyb <- .platt_ab(dm$hybrid_maps[[key]] %||% dm$hybrid_fallback,
                       .platt_inverse(comp$calibrator, h$p))
    }
    log[[paste0(cx$dkt, "@", as_of)]] <- list(
      dkt = cx$dkt, as_of = format(as_of), stage = d$stage, age_b = d$age_b,
      head = round(h$p, 5), src = h$src, hyb = if (is.na(hyb)) NULL else round(hyb, 5),
      day = round(d$prob, 5), day_raw = round(d$raw, 5), dm = dm$fitted_on)
    added <- added + 1L
  }
  dir.create(dirname(file.path(site_dir, HEADLINE_SHADOW_LOG)), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(log, file.path(site_dir, HEADLINE_SHADOW_LOG), auto_unbox = TRUE, digits = NA)
  list(added = added, considered = considered, total = length(log))
}

# Score the shadow log against outcomes. `outcomes` is a tibble of dkt, outcome
# (granted / denied / gvr / dismissed / pending). Only entries for decided
# petitions count; SG-layer entries are left out (both forecasts there are the
# SG rate's). Each petition weighs 1 in total, as in training. Returns overall
# and per-stage log loss and Brier for the published headline, the
# recalibrated hybrid and the day model, and the paired bootstrap of day
# against hyb -- the comparison the rule in docs/cert_model.md is about.
score_headline_shadow <- function(log, outcomes) {
  if (!length(log)) return(NULL)
  d <- bind_rows(lapply(log, function(e) tibble(
    dkt = e$dkt, as_of = as.Date(e$as_of), stage = e$stage, src = e$src,
    head = e$head, hyb = e$hyb %||% NA_real_, day = e$day)))
  d <- d |> inner_join(outcomes |> select(dkt, outcome), by = "dkt") |>
    filter(!outcome %in% c("pending", "relisted"), src != "sg", !is.na(hyb)) |>
    mutate(y = as.integer(outcome == "granted")) |>
    group_by(dkt) |> mutate(w = 1 / n()) |> ungroup()
  if (!nrow(d)) return(list(n_petitions = 0L))
  tab <- function(x) tibble(petitions = n_distinct(x$dkt), granted = sum(x$w * x$y),
                            ll_head = daily_ll(x$y, x$head, x$w), ll_hyb = daily_ll(x$y, x$hyb, x$w),
                            ll_day = daily_ll(x$y, x$day, x$w),
                            br_head = daily_brier(x$y, x$head, x$w), br_hyb = daily_brier(x$y, x$hyb, x$w),
                            br_day = daily_brier(x$y, x$day, x$w))
  list(n_petitions = n_distinct(d$dkt), overall = tab(d),
       by_stage = d |> group_by(stage) |> group_modify(~ tab(.x)) |> ungroup(),
       boot = if (sum(d$y) >= 5) daily_paired_boot(d, "day", "hyb") else NULL,
       # The relist tail, where every model so far over-forecasts.
       tail = d |> filter(stage == "post_conf", day >= 0.2) |>
         summarise(petitions = n_distinct(dkt), forecast = weighted.mean(day, w),
                   observed = weighted.mean(y, w)))
}
