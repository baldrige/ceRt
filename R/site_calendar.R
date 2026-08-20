# site_calendar.R ---------------------------------------------------------------
# "Upcoming at the Court": the next conferences and argument days, in one
# chronological list on the landing page.
#
# WHY THIS IS A FILE ON DISK AND NOT A COMPUTATION.
#
# The landing page is written by the DAILY, which fetches only the current term.
# The arguments it would need to advertise are cases from earlier terms -- the
# October 2026 sitting is stocked with 25-xxx dockets -- so the daily cannot see
# them, and asking it to fetch ten terms to print six dates would be absurd.
#
# The pipelines that DO know each write a small manifest as they render, and the
# daily merges whatever it finds. Each owns its own data, a missing file costs
# only that event type, and neither pipeline has to know the landing page exists.
# Same shape as qp_cache.json and counsel_stats.json, which cross the same gap.
#
# Staleness is handled at READ time, not write time: the conferences workflow
# runs weekly, so a manifest can name a date that has since passed. read_upcoming()
# filters against today on every daily build, which is three times a day.

suppressPackageStartupMessages({ library(jsonlite) })

# One row per event: date, kind, label, detail, href.
.cal_df <- function(date = as.Date(character()), kind = character(),
                    label = character(), detail = character(), href = character())
  data.frame(date = as.Date(date), kind = kind, label = label,
             detail = detail, href = href, stringsAsFactors = FALSE)

#' Upcoming argument days from a build_argument_table() frame.
#'
#' Grouped by DATE, not by case: the Court hears one to three cases on a sitting
#' day and a reader wants the day, with what is on it. Only rows still awaiting
#' argument -- a date in the future with an argued_date already set would be a
#' case argued early, which does not happen, but filtering on status keeps the
#' list honest if the docket ever says otherwise.
upcoming_arguments <- function(tbl, as_of = Sys.Date()) {
  if (is.null(tbl) || !nrow(tbl) ||
      !all(c("scheduled_date", "caption", "term") %in% names(tbl)))
    return(.cal_df())
  d <- tbl[!is.na(tbl$scheduled_date) & tbl$scheduled_date >= as_of, , drop = FALSE]
  if ("argued_date" %in% names(d)) d <- d[is.na(d$argued_date), , drop = FALSE]
  if (!nrow(d)) return(.cal_df())
  d <- d[order(d$scheduled_date), , drop = FALSE]
  by <- split(d, d$scheduled_date)
  out <- lapply(names(by), function(k) {
    g <- by[[k]]
    first <- strip_caption_roles(g$caption[1])
    detail <- if (nrow(g) == 1L) first else
      sprintf("%s and %d other%s", first, nrow(g) - 1L, if (nrow(g) > 2L) "s" else "")
    .cal_df(as.Date(k), "argument",
            if (nrow(g) == 1L) "Argument" else sprintf("%d arguments", nrow(g)),
            detail, sprintf("arguments/arg_%d.html", g$term[1]))
  })
  do.call(rbind, out)
}

#' Upcoming conferences from a conference_distributions() frame.
#'
#' The count is what the page will actually show, so the two cannot disagree.
upcoming_conferences <- function(dist, as_of = Sys.Date()) {
  if (is.null(dist) || !nrow(dist) || !("conf_date" %in% names(dist))) return(.cal_df())
  d <- dist[!is.na(dist$conf_date) & dist$conf_date >= as_of, , drop = FALSE]
  if (!nrow(d)) return(.cal_df())
  n <- table(as.character(d$conf_date))
  dates <- as.Date(names(n))
  ord <- order(dates)
  .cal_df(dates[ord], "conference", "Conference",
          sprintf("%s case%s distributed", format(as.integer(n[ord]), big.mark = ","),
                  ifelse(as.integer(n[ord]) == 1L, "", "s")),
          sprintf("conferences/conf_%s.html", dates[ord]))
}

#' Write a manifest. Always writes, even empty: an absent file and an empty one
#' mean different things, and only one of them is "this pipeline ran and there
#' is nothing upcoming".
write_upcoming <- function(events, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- if (is.null(events) || !nrow(events)) .cal_df() else events
  df$date <- format(as.Date(df$date), "%Y-%m-%d")
  write_json(df, path, auto_unbox = TRUE, dataframe = "rows")
  invisible(path)
}

#' Merge every manifest found, drop anything already past, and return the next
#' `n` in date order. Unreadable or missing files are skipped rather than fatal:
#' this is one panel on a page whose job is publishing dockets.
read_upcoming <- function(paths, as_of = Sys.Date(), n = 6L) {
  rows <- list()
  for (p in paths) {
    if (!file.exists(p)) next
    j <- tryCatch(fromJSON(p, simplifyDataFrame = TRUE), error = function(e) NULL)
    if (is.null(j) || !is.data.frame(j) || !nrow(j)) next
    if (!all(c("date", "kind", "label", "detail", "href") %in% names(j))) next
    j$date <- as.Date(j$date)
    rows[[length(rows) + 1L]] <- j[, c("date", "kind", "label", "detail", "href")]
  }
  if (!length(rows)) return(.cal_df())
  all <- do.call(rbind, rows)
  all <- all[!is.na(all$date) & all$date >= as_of, , drop = FALSE]
  if (!nrow(all)) return(.cal_df())
  # A conference and an argument can fall on one day; conference first, because
  # it is the Court acting on the docket this site is about.
  all <- all[order(all$date, all$kind != "conference"), , drop = FALSE]
  utils::head(all, n)
}
