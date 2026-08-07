# feeds.R ----------------------------------------------------------------------
# Atom feeds, XML sitemaps and robots.txt for the published site.
#
# None of these existed: the site published 55k pages with no feed, no sitemap
# and nothing pointing a crawler at either.
#
# Two constraints drive the whole design, and both fail invisibly if ignored.
#
# 1. A FEED MUST BE BYTE-STABLE WHEN NOTHING HAPPENED.
#    The daily runs three times a day. If <updated> were the build time then
#    every reader would re-notify three times a day forever, and -- less
#    obviously -- publish_site.sh would stop short-circuiting on
#    `git diff --cached --quiet`, so gh-pages would gain a commit per run whose
#    only content is a timestamp. So every <updated> here is an EVENT date (the
#    date of the grant order, of the conference, of the dashboard), and the
#    feed-level <updated> is max() of the entries. A run that found nothing new
#    writes a byte-identical file and publishes nothing. That is also the test:
#    run the build twice and diff.
#
#    Entry order is fully determined (date descending, then id) for the same
#    reason -- a tie broken by list order would churn the file on re-render.
#
# 2. A SITEMAP HOLDS AT MOST 50,000 URLs.
#    cases/ is already past it (55,327 dockets at the last audit), so a single
#    sitemap.xml would be rejected whole rather than truncated. write_sitemaps()
#    emits an INDEX plus one child per term, mirroring the way
#    write_cases_index() already partitions the back catalogue into ot{NN}
#    browse pages. SITEMAP_MAX is set below the spec limit, and any group that
#    somehow exceeds it is chunked rather than silently over-filled.
#
# <lastmod> is deliberately ABSENT from case URLs. The obvious source -- file
# mtime -- is wrong here: the gh-pages checkout rewrites every mtime on every
# run, so it would tell crawlers that all 55k pages changed today, which is the
# opposite of the signal <lastmod> exists to give. The render manifest
# (cases/.manifest.json) stores a content hash, not a date, so it cannot supply
# one either. Dated pages carry their date in the filename and do get a real
# <lastmod>. Omitting the field is explicitly allowed; lying in it is not.
#
# No new dependencies: base R, plus lubridate::mdy() for docket dates (the same
# call cert_funnel.R makes, so the two cannot disagree about what a date is) and
# jsonlite for the grants cache.

SITE_URL <- "https://supremecourt.report"

# Below the 50,000 spec limit, with headroom: the cap is per file and a term
# that grew past it between runs should chunk, not fail.
SITEMAP_MAX <- 45000L

# ---- XML primitives -----------------------------------------------------------

# The five XML predefined entities. `&` first, or it would double-escape the
# ampersands the later substitutions introduce.
xml_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&apos;", x, fixed = TRUE)
}

# Strip control characters that are illegal in XML 1.0 even when escaped. Docket
# text is scraped, and 231 of 272,421 event rows carry an embedded CRLF; a bare
# control byte would make the feed unparseable for every reader at once.
xml_clean <- function(x) {
  x <- gsub("[\x01-\x08\x0b\x0c\x0e-\x1f]", "", as.character(x), perl = TRUE)
  gsub("[[:space:]]+", " ", x)
}

# RFC 3339, which Atom requires. Dates in this pipeline are days, not instants,
# so they are pinned to midnight UTC rather than given a fabricated time.
rfc3339 <- function(d) {
  d <- as.Date(d)
  ifelse(is.na(d), NA_character_, paste0(format(d, "%Y-%m-%d"), "T00:00:00Z"))
}

# ---- Atom ---------------------------------------------------------------------

# One Atom feed.
#
# `entries` is a data frame with columns id, title, link, updated (Date) and
# summary. It is sorted here, not by the caller: entry order is part of the
# byte-stability contract (see the header) and leaving it to four call sites
# would be four chances to get it wrong.
#
# Returns the path, or NULL (with a warning) when there is nothing to publish.
# An Atom feed must carry a feed-level <updated>, and with no entries there is no
# honest value for it -- writing an invented one would be the exact failure this
# file is built to avoid. A stale feed is better than an invalid one.
write_atom_feed <- function(entries, path, title, subtitle, self_path,
                            base = SITE_URL, max_entries = 50L) {
  if (is.null(entries) || !nrow(entries)) {
    warning("write_atom_feed(): no entries for ", basename(path), " -- not written.",
            call. = FALSE)
    return(invisible(NULL))
  }
  entries <- entries[!is.na(entries$updated), , drop = FALSE]
  # Drop entries dated in the future. Conference reports are published BEFORE the
  # conference they cover -- the 2026-09-28 long conference page existed on
  # 2026-08-07 -- so without this the newest entry, and with it the feed's own
  # <updated>, sits weeks ahead of today. Readers that sort by date pin such an
  # entry to the top permanently, and a feed claiming to have been updated in the
  # future is simply wrong.
  #
  # The alternative -- stamping those entries with the publication date instead --
  # would mean re-stamping them on every run, which is the churn this file exists
  # to prevent. So the entry appears on its own date and never moves. That costs
  # the "the long-conference list is up" announcement, which is a real loss and
  # the reason this is a comment rather than a silent filter.
  entries <- entries[entries$updated <= Sys.Date(), , drop = FALSE]
  if (!nrow(entries)) {
    warning("write_atom_feed(): no dated, non-future entries for ", basename(path),
            " -- not written.", call. = FALSE)
    return(invisible(NULL))
  }
  # Newest first; id breaks ties so the order cannot depend on input order.
  entries <- entries[order(entries$updated, entries$id, decreasing = TRUE), ,
                     drop = FALSE]
  entries <- utils::head(entries, max_entries)

  self <- paste0(base, self_path)
  items <- vapply(seq_len(nrow(entries)), function(i) paste0(
    "  <entry>\n",
    "    <title>", xml_escape(xml_clean(entries$title[i])), "</title>\n",
    "    <link href=\"", xml_escape(entries$link[i]), "\"/>\n",
    "    <id>", xml_escape(entries$id[i]), "</id>\n",
    "    <updated>", rfc3339(entries$updated[i]), "</updated>\n",
    if (!is.na(entries$summary[i]) && nzchar(entries$summary[i]))
      paste0("    <summary>", xml_escape(xml_clean(entries$summary[i])),
             "</summary>\n") else "",
    "  </entry>"), character(1))

  xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
    "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n",
    "  <title>", xml_escape(title), "</title>\n",
    "  <subtitle>", xml_escape(subtitle), "</subtitle>\n",
    "  <link rel=\"self\" href=\"", xml_escape(self), "\"/>\n",
    "  <link rel=\"alternate\" type=\"text/html\" href=\"", xml_escape(paste0(base, "/")), "\"/>\n",
    "  <id>", xml_escape(self), "</id>\n",
    # max() of the entries, never Sys.time() -- see the header.
    "  <updated>", rfc3339(max(entries$updated)), "</updated>\n",
    "  <author><name>Supreme Court Report</name></author>\n",
    paste(items, collapse = "\n"), "\n",
    "</feed>\n")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(xml), path, useBytes = TRUE)
  invisible(path)
}

# ---- entry builders -----------------------------------------------------------

.entries <- function(id, title, link, updated, summary) {
  data.frame(id = id, title = title, link = link,
             updated = as.Date(updated), summary = summary,
             stringsAsFactors = FALSE)
}

# The docket entry that granted the petition, for the entry summary. Taken from
# the events on the disposition date rather than re-run through the grant
# grammar: classify_petition_events() has already decided which date is the
# grant, and the point here is to quote the Court, not to re-classify it.
.grant_order_text <- function(events, on) {
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events)) ||
      is.na(on)) return(NA_character_)
  d <- suppressWarnings(lubridate::mdy(events$Date))
  hit <- which(!is.na(d) & d == on &
               grepl("granted", events[["Proceedings and Orders"]], ignore.case = TRUE))
  if (!length(hit)) return(NA_character_)
  txt <- xml_clean(events[["Proceedings and Orders"]][hit[1]])
  if (nchar(txt) > 500) paste0(substr(txt, 1, 497), "...") else txt
}

# ---- the grants cache ---------------------------------------------------------
#
# The grants feed cannot be built from the daily's fetch, and the first version of
# this file wrongly assumed it could.
#
# get_scotus_update() fetches `max(hi - 50, lo):hi` -- the trailing ~51 dockets of
# each bucket, ~153 cases. A petition is granted months after it is docketed, by
# which time its number is far outside that window, so a grant is almost never
# visible to the daily. Shipped that way, grants.xml was never written at all: the
# feed was structurally empty, not merely empty in August.
#
# Grants ARE visible wherever a full term is loaded -- render_conferences.R holds
# `combined` (current + prior term) and already classifies every petition. So the
# grant set is accumulated into a docket-keyed cache that any workflow holding
# full-term data can contribute to, and the daily reads it to build the feeds.
#
# cases/grants.json is a flat object keyed by docket, which is exactly the shape
# publish_site.sh's DERIVED union resolution (`jq -s '.[0] * .[1]'`) merges
# correctly -- so two workflows publishing concurrently union their grants rather
# than one clobbering the other. It is listed there for that reason.
GRANTS_CACHE <- "cases/grants.json"

read_grants_cache <- function(site_dir) {
  p <- file.path(site_dir, GRANTS_CACHE)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) {
    warning("read_grants_cache(): ", basename(p), " unreadable -- treating as empty.",
            call. = FALSE)
    list()
  })
}

# Merge every granted petition in `cases` into the cache and write it back.
# Returns the number of grants newly added.
#
# Existing keys are NOT overwritten. A grant's date and order text do not change,
# and leaving them alone means a workflow with a partial view of a term can only
# ever add to the record, never revise it downward.
update_grants_cache <- function(site_dir, cases, classify = NULL) {
  if (is.null(cases) || !nrow(cases)) return(0L)
  if (is.null(classify)) {
    if (!exists("classify_petition_events")) {
      warning("update_grants_cache(): classify_petition_events() not available.",
              call. = FALSE)
      return(0L)
    }
    classify <- get("classify_petition_events")
  }
  idx <- read_grants_cache(site_dir)
  added <- 0L
  for (i in seq_len(nrow(cases))) {
    dkt <- cases$dkt[i]
    if (!is.null(idx[[dkt]])) next
    cl <- tryCatch(classify(cases$events[[i]]), error = function(e) NULL)
    if (is.null(cl) || !identical(cl$outcome[[1]], "granted") ||
        is.na(cl$outcome_date[[1]])) next
    cap <- cases$caption[i]
    if (exists("strip_caption_roles")) cap <- get("strip_caption_roles")(cap)
    if (is.na(cap) || !nzchar(cap)) cap <- dkt
    idx[[dkt]] <- list(
      date = format(cl$outcome_date[[1]]),
      caption = cap,
      order = .grant_order_text(cases$events[[i]], cl$outcome_date[[1]]) %||% "")
    added <- added + 1L
  }
  if (added > 0L) {
    dir.create(dirname(file.path(site_dir, GRANTS_CACHE)), recursive = TRUE,
               showWarnings = FALSE)
    jsonlite::write_json(idx, file.path(site_dir, GRANTS_CACHE), auto_unbox = TRUE)
  }
  added
}

# Newly granted cases, newest first, from the cache.
grant_feed_entries <- function(site_dir, n = 50L, base = SITE_URL) {
  empty <- .entries(character(), character(), character(), as.Date(character()),
                    character())
  idx <- read_grants_cache(site_dir)
  if (!length(idx)) return(empty)
  dkt <- names(idx)
  href <- paste0(base, "/cases/", dkt, ".html")
  cap <- vapply(idx, function(g) g$caption %||% "", character(1), USE.NAMES = FALSE)
  cap[!nzchar(cap)] <- dkt[!nzchar(cap)]
  out <- .entries(
    id = href,
    title = paste0("Certiorari granted: ", cap, " (No. ", dkt, ")"),
    link = href,
    updated = as.Date(vapply(idx, function(g) g$date %||% NA_character_,
                             character(1), USE.NAMES = FALSE)),
    summary = vapply(idx, function(g) g$order %||% "", character(1),
                     USE.NAMES = FALSE))
  out <- out[!is.na(out$updated), , drop = FALSE]
  if (!nrow(out)) return(empty)
  # id breaks the tie, so which of several same-day grants survives the head()
  # does not depend on the order the cache happens to enumerate keys in.
  out <- out[order(out$updated, out$id, decreasing = TRUE), , drop = FALSE]
  out[seq_len(min(n, nrow(out))), , drop = FALSE]
}

# Entries for a directory of dated pages (conferences/conf_YYYY-MM-DD.html,
# dashboards/dash_YYYY-MM-DD.html). The date comes out of the FILENAME, which is
# the whole reason these pages are named that way -- no page has to be parsed and
# no mtime is consulted.
dated_page_entries <- function(dir, pattern, prefix, title_fmt, summary_fmt,
                               n = 50L, base = SITE_URL) {
  empty <- .entries(character(), character(), character(), as.Date(character()),
                    character())
  if (!dir.exists(dir)) return(empty)
  files <- list.files(dir, pattern = pattern)
  if (!length(files)) return(empty)
  d <- as.Date(sub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", files))
  keep <- !is.na(d)
  files <- files[keep]; d <- d[keep]
  if (!length(files)) return(empty)
  ord <- order(d, decreasing = TRUE)
  files <- utils::head(files[ord], n); d <- utils::head(d[ord], n)
  href <- paste0(base, prefix, files)
  pretty <- format(d, "%B %e, %Y")
  pretty <- gsub("  ", " ", pretty)     # %e pads single digits with a space
  .entries(id = href, title = sprintf(title_fmt, pretty), link = href,
           updated = d, summary = sprintf(summary_fmt, pretty))
}

# ---- sitemaps -----------------------------------------------------------------

.urlset <- function(urls, lastmod = NULL) {
  body <- vapply(seq_along(urls), function(i) paste0(
    "  <url>\n    <loc>", xml_escape(urls[i]), "</loc>\n",
    if (!is.null(lastmod) && !is.na(lastmod[i]))
      paste0("    <lastmod>", format(as.Date(lastmod[i]), "%Y-%m-%d"), "</lastmod>\n")
    else "",
    "  </url>"), character(1))
  paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
         "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n",
         paste(body, collapse = "\n"), "\n</urlset>\n")
}

# Every published page, as a sitemap index plus per-term children.
#
# Returns the child filenames written (the index itself excluded), so the caller
# and the audit can check the set rather than infer it.
write_sitemaps <- function(site_dir, base = SITE_URL, max_urls = SITEMAP_MAX) {
  rel <- function(...) file.path(site_dir, ...)
  children <- list()   # name -> list(urls =, lastmod =)

  # --- section and dated pages, all in one child ---
  pages <- c("/", "/about.html", "/methods.html")
  pages <- pages[vapply(pages, function(p) {
    f <- if (p == "/") "index.html" else sub("^/", "", p)
    file.exists(rel(f))
  }, logical(1))]
  page_urls <- paste0(base, pages)
  page_mod <- rep(as.Date(NA), length(page_urls))

  for (s in c("dashboards", "conferences", "arguments", "funnel", "cases")) {
    if (file.exists(rel(s, "index.html"))) {
      page_urls <- c(page_urls, paste0(base, "/", s, "/"))
      page_mod <- c(page_mod, as.Date(NA))
    }
  }
  # Dated leaves: the date is in the filename, so <lastmod> is real here.
  for (s in list(list("dashboards", "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$"),
                 list("conferences", "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$"))) {
    d <- rel(s[[1]])
    if (!dir.exists(d)) next
    f <- sort(list.files(d, pattern = s[[2]]))
    if (!length(f)) next
    page_urls <- c(page_urls, paste0(base, "/", s[[1]], "/", f))
    page_mod <- c(page_mod, as.Date(sub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", f)))
  }
  # Argument leaves are named by TERM (arg_2026.html), not by date. There is no
  # honest <lastmod> for them -- the page changes whenever a case in that Term is
  # set for argument -- so they get none, rather than a year-start date that
  # would tell crawlers the page has been untouched since January.
  arg <- sort(list.files(rel("arguments"), pattern = "^arg_\\d{4}\\.html$"))
  if (length(arg)) {
    page_urls <- c(page_urls, paste0(base, "/arguments/", arg))
    page_mod <- c(page_mod, rep(as.Date(NA), length(arg)))
  }
  # The per-term browse pages under cases/.
  ot <- sort(list.files(rel("cases"), pattern = "^ot\\d+\\.html$"))
  if (length(ot)) {
    page_urls <- c(page_urls, paste0(base, "/cases/", ot))
    page_mod <- c(page_mod, rep(as.Date(NA), length(ot)))
  }
  if (length(page_urls))
    children[["sitemap-pages.xml"]] <- list(urls = page_urls, lastmod = page_mod)

  # --- docket pages, one child per term (no <lastmod>; see the header) ---
  all_html <- list.files(rel("cases"), pattern = "\\.html$")
  dockets <- setdiff(all_html, c("index.html", grep("^ot\\d+\\.html$", all_html, value = TRUE)))
  if (length(dockets)) {
    term <- substr(dockets, 1, 2)
    for (tm in sort(unique(term))) {
      f <- sort(dockets[term == tm])
      urls <- paste0(base, "/cases/", f)
      # Chunk rather than over-fill. No term is near the cap today; this is here
      # so that if one ever is, the sitemap splits instead of being rejected.
      chunks <- split(urls, ceiling(seq_along(urls) / max_urls))
      for (k in seq_along(chunks)) {
        nm <- if (length(chunks) == 1) sprintf("sitemap-cases-ot%s.xml", tm)
              else sprintf("sitemap-cases-ot%s-%d.xml", tm, k)
        children[[nm]] <- list(urls = chunks[[k]], lastmod = NULL)
      }
    }
  }

  if (!length(children)) {
    warning("write_sitemaps(): nothing to map -- no sitemap written.", call. = FALSE)
    return(invisible(character()))
  }

  for (nm in names(children))
    writeLines(enc2utf8(.urlset(children[[nm]]$urls, children[[nm]]$lastmod)),
               rel(nm), useBytes = TRUE)

  # The index. No <lastmod> on the children either: it would be the build time,
  # which is the mtime problem again one level up.
  idx <- paste0(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
    "<sitemapindex xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n",
    paste(vapply(sort(names(children)), function(nm) paste0(
      "  <sitemap>\n    <loc>", xml_escape(paste0(base, "/", nm)),
      "</loc>\n  </sitemap>"), character(1)), collapse = "\n"),
    "\n</sitemapindex>\n")
  writeLines(enc2utf8(idx), rel("sitemap.xml"), useBytes = TRUE)

  invisible(sort(names(children)))
}

# robots.txt. Re-asserted on every run like CNAME and analytics.js, so no
# rebuild can silently drop it.
write_robots <- function(site_dir, base = SITE_URL) {
  txt <- paste0(
    "User-agent: *\n",
    "Allow: /\n",
    "\n",
    "Sitemap: ", base, "/sitemap.xml\n")
  writeLines(txt, file.path(site_dir, "robots.txt"), useBytes = TRUE)
  invisible(file.path(site_dir, "robots.txt"))
}

# ---- the site's feeds ---------------------------------------------------------

# Write /feed.xml and /grants.xml. `cases` is the current term's case tibble.
#
# Two feeds, because they answer different questions. The site feed carries
# everything the site published -- a grant, a conference report, a day's
# dashboard -- at roughly one or two entries a day. The grants feed carries only
# grants, which is a handful a month and is what most readers actually want to be
# told about.
write_site_feeds <- function(site_dir, base = SITE_URL) {
  grants <- grant_feed_entries(site_dir, base = base)

  confs <- dated_page_entries(
    file.path(site_dir, "conferences"), "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$",
    "/conferences/", "Conference of %s",
    "Petitions distributed for the Conference of %s, ranked by relists.",
    base = base)

  # Dashboards are capped well below the feed's own 50, deliberately. There is
  # one per docketing date, so an uncapped contribution would fill the entire
  # feed with the last fifty weekdays and push every grant and conference report
  # out of it -- a chronological feed in which the only thing visible is the
  # thing that happens most often.
  dash <- dated_page_entries(
    file.path(site_dir, "dashboards"), "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$",
    "/dashboards/", "Docket for %s",
    "Petitions and applications docketed on %s.", n = 20L, base = base)

  site <- rbind(grants, confs, dash)

  list(
    site = write_atom_feed(
      site, file.path(site_dir, "feed.xml"),
      title = "Supreme Court Report",
      subtitle = paste0("Certiorari grants, conference reports and daily ",
                        "docket dashboards from the U.S. Supreme Court."),
      self_path = "/feed.xml", base = base),
    grants = write_atom_feed(
      grants, file.path(site_dir, "grants.xml"),
      title = "Supreme Court Report: Certiorari Grants",
      subtitle = "Cases in which the Court has granted plenary review.",
      self_path = "/grants.xml", base = base))
}

# Which feeds the site actually has, as root-absolute paths.
#
# page_head() advertises only these. The first version advertised both
# unconditionally, and grants.xml turned out never to be written -- so every index
# page on the site carried a <link rel="alternate"> to a 404.
#
# Read at the START of a build, from the gh-pages checkout, so it reflects what
# the previous run published. A feed that appears for the first time this run is
# therefore advertised from the NEXT run onwards. That one-run lag is the price of
# never emitting a dangling link, and it is the right way round: a link to a feed
# that exists is always correct, a link to one that might exist is not.
site_feeds_present <- function(site_dir) {
  f <- c("/feed.xml", "/grants.xml")
  f[file.exists(file.path(site_dir, sub("^/", "", f)))]
}
