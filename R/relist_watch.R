# relist_watch.R ----------------------------------------------------------------
# Every petition the Court has relisted and not yet decided -- sorted so that what
# leads the page is what a reader wants to know this week.
#
# A relist is the strongest publicly visible cert signal there is: the Justices
# had a petition in front of them, and instead of granting or denying it they put
# it back on the list for another look. What is not available anywhere else is a
# CALIBRATED PROBABILITY beside the count, which the conference model produces.
#
# WHAT COUNTS AS A RELIST (classify_petition_events(), cert_funnel.R): a move to
# a LATER conference after the earlier one was held, with no "Rescheduled", call
# for a response or CVSG in between -- those redistributions are mechanical, and
# counting them overstated relists by ~55% (OT17-22). A second distribution for
# the same conference, or a move entered before the first conference met, is not
# a relist either.
#
# FOUR KINDS OF "LIVE", which the page separates (it used to sort them together
# by count, and the top of the page was all holds and leftovers -- Oct 2026: the
# first four rows were held cases "relisted" 9-21 times, the fifth a case decided
# in 2025):
#
#   Up next            relisted, and set for a coming conference -- the news.
#   Awaiting the SG    the Court has invited the Solicitor General; the case
#                      comes back when the brief does (the brief's date shown).
#   Held               re-distributed conference after conference while it waits
#                      on a related case (hold_signal(): 6+ relists, or a granted
#                      companion). Its count measures how long it has waited,
#                      not how hard it is being reconsidered, so it is shown as
#                      "N conferences since ...", below the relists.
#   No conference set  relisted before and not scheduled now -- a summer
#                      carry-over, usually.
#
# The old cases are actually here: a heavily relisted petition is by definition an
# old one, and the targeted pending fetch (R/pending_dockets.R) names those
# stragglers so the page does not silently omit its most interesting rows.
#
# "Still live" = no recognised disposition (the classifier's `pending`). A
# petition whose disposition wording is unrecognised also reads as pending --
# 0.10% of a closed Term; 23-402 and 25-243 were two such, until their wordings
# were added (Oct 2026).

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

RELIST_GROUPS <- c("Up next", "Awaiting the SG", "Held", "No conference set")

# What a case's docket says beyond the count: the conferences it has been
# relisted to, and the signals a reader weighs beside a relist.
relist_profile <- function(events) {
  none <- list(relist_confs = as.Date(character()), confs = as.Date(character()),
               cvsg = as.Date(NA), sg_brief = as.Date(NA), cfr = FALSE,
               record = FALSE, amici = 0L, last_dist = as.Date(NA))
  if (!is.data.frame(events) || !nrow(events)) return(none)
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  ed <- suppressWarnings(lubridate::mdy(events$Date))
  o <- order(ed); txt <- txt[o]; ed <- ed[o]
  cl <- tryCatch(classify_petition_events(events), error = function(e) NULL)
  is_dist <- str_detect(txt, FUNNEL_PATTERNS$dist)
  confs <- suppressWarnings(lubridate::mdy(
    str_match(txt[is_dist], "Conference of (\\d{1,2}/\\d{1,2}/\\d{4})")[, 2]))
  cv <- which(str_detect(txt, FUNNEL_PATTERNS$cvsg))
  cvsg <- if (length(cv)) ed[max(cv)] else as.Date(NA)
  sg <- which(str_detect(txt, regex("^Brief amicus curiae of (the )?United States", ignore_case = TRUE)))
  sg <- sg[!is.na(cvsg) & ed[sg] >= cvsg]
  list(
    relist_confs = if (is.null(cl)) as.Date(character()) else cl$relist_confs[[1]],
    confs = sort(unique(confs[!is.na(confs)])),
    cvsg = cvsg,
    sg_brief = if (length(sg)) ed[min(sg)] else as.Date(NA),
    cfr = any(str_detect(txt, FUNNEL_PATTERNS$cfr)),
    record = any(str_detect(txt, regex("^Record (is )?requested", ignore_case = TRUE))),
    amici = sum(str_detect(txt, FUNNEL_PATTERNS$amicus)),
    last_dist = if (any(is_dist)) max(ed[is_dist], na.rm = TRUE) else as.Date(NA))
}

# One row per live, relisted petition, in page order (group, then within it).
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

  # Granted before today, for hold_signal()'s companion tier.
  gd <- if (all(c("outcome", "outcome_date") %in% names(dist)))
    unique(dist$dkt[dist$outcome %in% "granted" & !is.na(dist$outcome_date) &
                    dist$outcome_date < as_of]) else character()

  d <- dist |>
    # The petition types, not "not an application": a motion docket (26M##)
    # is distributed too, and is no relisted petition.
    filter(type %in% c("paid", "ifp"), outcome %in% "pending", !is.na(n_relists), n_relists >= 1) |>
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
                               origin = "1970-01-01"))
  if (!nrow(d)) return(d)

  prof <- map(d$events, relist_profile)
  d$profile <- prof
  d$held <- map2_lgl(d$n_relists, d$related, function(n, r)
    isTRUE(if (exists("hold_signal")) hold_signal(n, r, gd) else n >= 6L))
  # Waiting on the SG: invited after the case's last distribution, so the
  # invitation is what it is waiting on.
  waiting_sg <- map_lgl(prof, function(p) !is.na(p$cvsg) && (is.na(p$last_dist) || p$cvsg >= p$last_dist))
  d$group <- case_when(
    !is.na(d$next_conf) & !d$held ~ "Up next",
    waiting_sg ~ "Awaiting the SG",
    d$held ~ "Held",
    TRUE ~ "No conference set")
  d |>
    mutate(group = factor(group, levels = RELIST_GROUPS)) |>
    arrange(group, next_conf, desc(n_relists), desc(last_conf))
}

# Render /relists/index.html. Returns the path, or NULL when there is nothing
# live to show (a page reading "0 cases" is worse than no page).
relist_watch <- function(dist, out_dir, qp_map = NULL, models = NULL,
                         as_of = Sys.Date(), signals_map = NULL, subject_map = NULL) {
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
  p_ever <- rep(NA_real_, nrow(d))
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
        conf_idx = d$n_dist[i] + 1L, granted_dockets = gd,
        signals = signals_map[[d$dkt[i]]]), error = function(e) NULL)
      if (!is.null(s)) {
        p_grant[i] <- s$p_grant_now; p_gvr[i] <- s$p_gvr_now; p_ever[i] <- s$p_grant_ever
      }
    }
  }
  # Within "Up next", the likeliest grants lead among equal relist counts: the
  # forecast is the column a reader came for once the count ties.
  ord <- order(d$group, d$next_conf, -d$n_relists, -ifelse(is.na(p_ever), -1, p_ever),
               na.last = TRUE)
  d <- d[ord, ]; p_grant <- p_grant[ord]; p_gvr <- p_gvr[ord]; p_ever <- p_ever[ord]

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
  md <- function(x) str_squish(format(x, "%b %e"))

  # The relist history a reader can read at a glance. A relisted case: the
  # conferences it has been at, last four ("Sep 28 -> Oct 9"). A held case: how
  # long it has been waiting -- its count is conferences, not reconsideration.
  hist_cell <- map_chr(seq_len(nrow(d)), function(i) {
    p <- d$profile[[i]]; n <- d$n_relists[i]
    cf <- p$confs
    if (identical(as.character(d$group[i]), "Held")) {
      since <- if (length(cf)) paste0(" since ", fmt_d(min(cf))) else ""
      return(paste0("<b>", length(cf), "</b><span class='fc-sub'>conferences", since, "</span>"))
    }
    shown <- tail(cf, 4)
    # A history across a New Year reads wrong without years ("Dec 12 -> Jun 29"
    # is seven months, not a week), so it carries a two-digit year throughout.
    lab <- if (length(unique(format(shown, "%Y"))) > 1)
      paste0(md(shown), " &rsquo;", format(shown, "%y")) else md(shown)
    trail <- if (length(cf) > 4) c("&hellip;", lab) else lab
    paste0("<b>", n, "</b><span class='fc-sub'>", paste(trail, collapse = " &rarr; "), "</span>")
  })

  signal_cell <- map_chr(seq_len(nrow(d)), function(i) {
    p <- d$profile[[i]]; s <- character()
    if (!is.na(p$cvsg)) s <- c(s, paste0("CVSG ", fmt_d(p$cvsg)))
    if (!is.na(p$sg_brief)) s <- c(s, paste0("SG brief ", fmt_d(p$sg_brief)))
    if (isTRUE(p$cfr)) s <- c(s, "Response requested")
    if (isTRUE(p$record)) s <- c(s, "Record requested")
    if (p$amici > 0) s <- c(s, paste0(p$amici, if (p$amici == 1) " amicus brief" else " amicus briefs"))
    if (!length(s)) "—" else paste(s, collapse = "<br>")
  })

  area <- if (is.null(subject_map)) rep(NA_character_, nrow(d)) else
    unname(vapply(d$dkt, function(k) { v <- subject_map[k]; if (length(v) && !is.na(v)) v else NA_character_ }, ""))

  tbl <- tibble(
    Case = paste0("[", str_squish(strip_caption_roles(d$caption)), "]",
                  "(/cases/", d$dkt, ".html)",
                  "<br><span class='cdk'>No. ", d$dkt,
                  ifelse(is.na(area), "", paste0(" &middot; ", area)), "</span>"),
    Status = as.character(d$group),
    # The NUMBER as data (reactable sorts on it), the history as display.
    Relists = d$n_relists,
    Next = fmt_d(d$next_conf),
    Signals = signal_cell,
    # The NUMBER, not the rendered cell -- reactable sorts on the underlying
    # data value, so a markup column sorts as text ("4%" after "12%"). The
    # markup is attached by fmt() below, which leaves the data numeric.
    Forecast = p_ever,
    Court = ifelse(is.na(d$lower) | d$lower == "", "—", d$lower),
    Documents = map_chr(d$events, function(e)
      case_documents(e, c("Petition", "Appendix", "BIO", "Reply"))),
    QP = { q <- map_chr(d$dkt, qp_get); ifelse(is.na(q) | q == "", "—", q) })

  fc_html <- fc_cell(p_grant, p_ever, p_gvr)
  has_fc <- any(!is.na(p_ever)) || any(!is.na(p_grant))
  if (!has_fc) tbl <- select(tbl, -Forecast)
  for (col in c("QP", "Documents", "Signals")) {
    if (col %in% names(tbl) && all(tbl[[col]] == "—")) tbl <- select(tbl, -all_of(col))
  }
  has_qp <- "QP" %in% names(tbl)
  left_cols <- match(intersect(c("Case", "Signals", "Court", "Documents", "QP"), names(tbl)),
                     names(tbl))

  t <- tbl |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "Signals", "Documents", "QP"))) |>
    fmt(columns = Relists, fns = function(x) hist_cell) |>
    data_color(columns = Status, method = "factor",
               palette = c("Up next" = STATUS_FILL[["Scheduled"]],
                           "Awaiting the SG" = STATUS_FILL[["Argued"]],
                           "Held" = STATUS_FILL[["Granted"]],
                           "No conference set" = STATUS_FILL[["Dismissed"]])) |>
    cols_label(Next = "Next conference") |>
    cols_align("center", columns = everything()) |>
    cols_width(Case ~ px(240), Status ~ px(104), Relists ~ px(132),
               Next ~ px(112), Court ~ px(150))
  if ("Signals" %in% names(tbl)) t <- t |> cols_width(Signals ~ px(150))
  if (has_qp) t <- t |> cols_label(QP = "Questions Presented") |> cols_width(QP ~ px(190))
  # fmt(), not fmt_markdown(): renders the cell to HTML for display while the
  # column's DATA stays the numeric probability reactable sorts on. fns is
  # called once with the whole column in row order.
  if (has_fc) t <- t |>
    fmt(columns = Forecast, fns = function(x) fc_html) |>
    cols_label(Forecast = "Grant forecast") |>
    cols_width(Forecast ~ px(120))

  # The dek says what the page holds this week, group by group, leading with
  # the news: how many were relisted for the next conference.
  n_g <- table(factor(d$group, levels = RELIST_GROUPS))
  nc <- suppressWarnings(min(d$next_conf[d$group == "Up next"], na.rm = TRUE))
  lead <- if (n_g[["Up next"]] > 0) paste0(
    n_g[["Up next"]], if (n_g[["Up next"]] == 1) " petition" else " petitions",
    " relisted and set for conference",
    if (is.finite(nc)) paste0(" (the next on ", fmt_d(as.Date(nc, origin = "1970-01-01")), ")") else "")
    else "No relisted petition is set for a coming conference"
  rest <- c(if (n_g[["Awaiting the SG"]]) paste0(n_g[["Awaiting the SG"]], " awaiting the Solicitor General"),
            if (n_g[["Held"]]) paste0(n_g[["Held"]], " apparently held for a related case"),
            if (n_g[["No conference set"]]) paste0(n_g[["No conference set"]], " relisted earlier with no conference set"))
  dek <- paste0(lead, if (length(rest)) paste0("; ", paste(rest, collapse = "; ")) else "",
                ". Sortable and filterable.")

  footer <- paste0(
    "<em>Relists</em> counts the times the Justices considered a petition and put it ",
    "off to a later conference; beneath it, the conferences it has been at. A ",
    "redistribution after a &ldquo;Rescheduled&rdquo; entry, a call for a response or ",
    "an invitation to the Solicitor General (CVSG) is mechanical and does not count, ",
    "nor does a second distribution for the same conference. ",
    "<em>Status</em>: <em>Up next</em>, relisted and set for a coming conference; ",
    "<em>Awaiting the SG</em>, invited to file and not yet back on a conference list; ",
    "<em>Held</em>, re-listed conference after conference while it apparently waits ",
    "on a related case &mdash; shown as conferences since it was first considered, ",
    "because that count measures waiting rather than reconsideration; ",
    "<em>No conference set</em>, relisted before and not now scheduled. ",
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
      title = "Relist Tracker",
      dek = dek, n_rows = nrow(tbl), left_cols = left_cols, footer = footer,
      leaf_max = 84, active = "/relists/",
      back = list(href = "/conferences/", label = "&larr; Conference reports"))

  invisible(file.path(out_dir, "index.html"))
}
