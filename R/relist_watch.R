# relist_watch.R ----------------------------------------------------------------
# Every petition the Court has looked at and not yet decided.
#
# A relist is the strongest publicly visible cert signal there is: the Justices
# had a petition in front of them, and instead of granting or denying it they put
# it back on the list for another look. Tracking them is a well-established genre.
# What is not available anywhere else is a CALIBRATED PROBABILITY beside the
# count, which the conference model already produces.
#
# TWO THINGS MAKE THE COUNTS HERE DIFFERENT FROM A NAIVE ONE.
#
# 1. True relists only. cert_funnel.R excludes redistributions that follow a
#    "Rescheduled" entry, a called-for response, or a CVSG -- those are
#    mechanical, not the Justices deferring. Pooled OT17-22, counting naively
#    overstates relists by ~55%, and the funnel published the naive number for two
#    weeks in 2026 by accident. `distribution_no` is NOT a substitute for
#    `n_relists`; it counts every redistribution.
#
# 2. The old cases are actually here. The weekly run range-fetches {T, T-1}, and
#    a heavily relisted petition is by definition an old one -- 19-333 was
#    relisted 12 times, 8 of them in a Term that window never loads. The targeted
#    pending fetch (R/pending_dockets.R) names those stragglers so they appear.
#    Without it this page would silently omit its most interesting rows, which is
#    the whole reason that machinery exists.
#
# WHAT COUNTS AS "STILL LIVE": no recognised disposition. That is the classifier's
# `pending`, with the caveat it always carries -- a petition whose disposition
# wording is unrecognised also reads as pending. Measured at 0.10% of a closed
# Term, so a handful of rows here are cases that are actually over.

suppressPackageStartupMessages({
  library(gt); library(gtExtras); library(tidyverse); library(htmltools)
})

local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f) else f
  }
  sys.source(find("page_style.R"), envir = globalenv())
  sys.source(find("interactive_theme.R"), envir = globalenv())
})

# One row per live, relisted petition, newest activity first.
#
# `dist` must be an UNFILTERED conference_distributions() tibble -- the whole
# history of each case, not a date-windowed slice. A case's relist count and its
# last/next conference are properties of the case, and slicing the frame by
# conference date would compute them from a fragment.
relist_watch_table <- function(dist, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  need <- c("dkt", "conf_date", "outcome", "n_relists")
  miss <- setdiff(need, names(dist))
  if (length(miss))
    stop("relist_watch_table(): dist is missing ", paste(miss, collapse = ", "),
         ". conference_distributions() must keep n_relists -- see the select() ",
         "in conference_dash.R.", call. = FALSE)

  dist |>
    filter(type != "app", outcome %in% "pending", !is.na(n_relists), n_relists >= 1) |>
    group_by(dkt) |>
    summarise(
      # Everything except conf_date is constant within a docket, so first() is
      # exact rather than a choice. The list-columns must be re-wrapped in list():
      # first() on a list-column returns the ELEMENT (a whole events tibble),
      # which summarise then tries to recycle to the group size.
      caption = first(caption), lower = first(lower), type = first(type),
      parties = list(first(parties)), events = list(first(events)),
      date = first(date), lower_date = first(lower_date),
      related = if ("related" %in% names(dist)) first(related) else NA_character_,
      n_relists = first(n_relists),
      last_conf = suppressWarnings(max(conf_date[conf_date <= as_of], na.rm = TRUE)),
      next_conf = suppressWarnings(min(conf_date[conf_date > as_of], na.rm = TRUE)),
      n_dist = n(), .groups = "drop") |>
    mutate(last_conf = as.Date(ifelse(is.finite(last_conf), last_conf, NA),
                               origin = "1970-01-01"),
           next_conf = as.Date(ifelse(is.finite(next_conf), next_conf, NA),
                               origin = "1970-01-01")) |>
    arrange(desc(n_relists), next_conf, desc(last_conf))
}

# Render /relists/index.html. Returns the path, or NULL when there is nothing
# live to show (a page reading "0 cases" is worse than no page).
relist_watch <- function(dist, out_dir, qp_map = NULL, models = NULL,
                         as_of = Sys.Date()) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  d <- relist_watch_table(dist, as_of = as_of)
  if (!nrow(d)) {
    message("relist_watch(): no live relisted petitions -- page not written.")
    return(invisible(NULL))
  }

  # Scored as of the NEXT conference where one is scheduled, else as of today.
  # Scoring a relisted case as of its last conference would answer a question
  # that has already been resolved -- the Court has acted on that one.
  p_grant <- rep(NA_real_, nrow(d)); p_gvr <- rep(NA_real_, nrow(d))
  p_ever <- rep(NA_real_, nrow(d)); held <- rep(FALSE, nrow(d))
  if (!is.null(models) && exists("score_conference") &&
      (!is.null(models$conference) || !is.null(models$enhanced))) {
    gd <- if (all(c("outcome", "outcome_date") %in% names(dist)))
      unique(dist$dkt[dist$outcome %in% "granted" & !is.na(dist$outcome_date) &
                      dist$outcome_date < as_of]) else character()
    for (i in seq_len(nrow(d))) {
      if (!identical(d$type[i], "paid")) next
      at <- if (!is.na(d$next_conf[i])) d$next_conf[i] else as_of
      s <- tryCatch(score_conference(
        models, d$caption[i], d$lower[i], d$parties[[i]], d$date[i],
        d$lower_date[i], d$related[i], events = d$events[[i]], as_of = at,
        conf_idx = d$n_dist[i] + 1L, granted_dockets = gd), error = function(e) NULL)
      if (!is.null(s)) {
        p_grant[i] <- s$p_grant_now; p_gvr[i] <- s$p_gvr_now
        p_ever[i] <- s$p_grant_ever; held[i] <- isTRUE(s$held)
      }
    }
  }

  pct <- function(p) ifelse(
    is.na(p), "—",
    ifelse(p >= 0.10, sprintf("%.0f%%", 100 * p),
    ifelse(p >= 0.01, sprintf("%.1f%%", 100 * p),
    ifelse(p >= 0.0001, sprintf("%.2f%%", 100 * p), "0%"))))
  fc_shade <- function(p, hi, cols) {
    out <- rep(GRANT_NA, length(p)); ok <- !is.na(p)
    if (any(ok)) {
      m <- grDevices::colorRamp(cols)(pmin(pmax(p[ok] / hi, 0), 1))
      out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
    }
    out
  }
  # Same cell shape as the conference reports, deliberately: a reader moving
  # between the two pages should read the same colours as the same thing.
  fc_cell <- function(g, e, v) {
    sub <- paste0(ifelse(is.na(g), "", paste0("next ", pct(g))),
                  ifelse(!is.na(g) & !is.na(v), "<br>", ""),
                  ifelse(is.na(v), "", paste0("GVR ", pct(v))))
    ifelse(is.na(g) & is.na(e) & is.na(v), "—",
      paste0("<span class='fc-here' style='background:", fc_shade(e, 1, GRANT_RAMP),
             "'>", pct(e), "</span>",
             ifelse(nzchar(sub), paste0("<span class='fc-sub'>", sub, "</span>"), "")))
  }

  qp_get <- function(dk) if (is.null(qp_map) || is.na(qp_map[dk])) NA_character_ else qp_map[[dk]]
  fmt_d <- function(x) ifelse(is.na(x), "—", format(x, "%b %e, %Y") |> str_squish())

  tbl <- tibble(
    Case = paste0("[", str_squish(strip_caption_roles(d$caption)), "]",
                  "(/cases/", d$dkt, ".html)",
                  "<br><span class='cdk'>No. ", d$dkt, "</span>"),
    Relists = d$n_relists,
    # "Held" is a different state from "relisted": a case held for a related
    # pending decision is waiting, not being reconsidered. hold_signal() already
    # distinguishes them and the distinction is the reader's main question.
    Status = ifelse(held, "Held", "Relisted"),
    Last = fmt_d(d$last_conf),
    Next = fmt_d(d$next_conf),
    Forecast = fc_cell(p_grant, p_ever, p_gvr),
    Court = ifelse(is.na(d$lower) | d$lower == "", "—", d$lower),
    Documents = map_chr(d$events, function(e)
      case_documents(e, c("Petition", "Appendix", "BIO", "Reply"))),
    QP = { q <- map_chr(d$dkt, qp_get); ifelse(is.na(q) | q == "", "—", q) })

  has_fc <- any(!is.na(p_ever)) || any(!is.na(p_grant))
  if (!has_fc) tbl <- select(tbl, -Forecast)
  for (col in c("QP", "Documents")) {
    if (col %in% names(tbl) && all(tbl[[col]] == "—")) tbl <- select(tbl, -all_of(col))
  }
  has_qp <- "QP" %in% names(tbl)
  left_cols <- match(intersect(c("Case", "Court", "Documents", "QP"), names(tbl)),
                     names(tbl))

  t <- tbl |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "Documents", "QP", "Forecast"))) |>
    data_color(columns = Status, method = "factor",
               palette = c("Relisted" = STATUS_FILL[["Scheduled"]],
                           "Held" = STATUS_FILL[["Granted"]])) |>
    cols_align("center", columns = everything()) |>
    cols_width(Case ~ px(230), Relists ~ px(76), Status ~ px(88),
               Last ~ px(112), Next ~ px(112), Court ~ px(160))
  if (has_qp) t <- t |> cols_label(QP = "Questions Presented") |> cols_width(QP ~ px(190))
  if (has_fc) t <- t |> cols_label(Forecast = "Grant forecast") |>
    cols_width(Forecast ~ px(120))

  n_held <- sum(tbl$Status == "Held")
  dek <- paste0(
    nrow(tbl), if (nrow(tbl) == 1) " petition" else " petitions",
    " the Justices have considered at least twice and not yet decided",
    if (n_held) paste0(", ", n_held, " of them apparently held for a related case") else "",
    " &mdash; sortable and filterable. Sorted by relist count.")

  footer <- paste0(
    "A <em>relist</em> is a redistribution with no intervening &ldquo;Rescheduled&rdquo; ",
    "entry, no call for a response and no CVSG &mdash; those redistributions are ",
    "mechanical rather than the Justices deferring, and counting them would ",
    "overstate relists by more than half. <em>Held</em> marks a petition that ",
    "looks to be waiting on a related granted case rather than being ",
    "reconsidered. ",
    if (has_fc) paste0(
      "<em>Grant forecast</em> leads with the estimate that the petition is granted ",
      "at <em>any</em> conference; beneath it, <em>next</em> is the estimate for its ",
      "next scheduled conference and <em>GVR</em> the companion summary-disposition ",
      "estimate. Paid petitions only. Estimates, not predictions about any case. ")
    else "",
    "A petition whose disposition wording the classifier does not recognise also ",
    "reads as live; that is about 0.1% of a completed Term.")

  scr_interactive(t, n_rows = nrow(tbl)) |>
    scr_write_page(
      file.path(out_dir, "index.html"),
      kicker = "Supreme Court of the United States",
      title = "Relist Watch",
      dek = dek, n_rows = nrow(tbl), left_cols = left_cols, footer = footer,
      leaf_max = 78, active = "/relists/",
      back = list(href = "/conferences/", label = "&larr; Conference reports"))

  invisible(file.path(out_dir, "index.html"))
}
