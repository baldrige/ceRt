# orders_list.R ------------------------------------------------------------------
# The Court's order lists: what it did on an order day, in the Court's own words.
# Spec and the measurements behind every rule here: docs/order-lists.md.
#
# WHAT THE COURT PUBLISHES. /orders/ordersofthecourt/NN lists one Term's order
# documents, newest first: the date, a kind ("Order List" on a Monday after a
# conference; "Miscellaneous Order" on any day the Court acts outside one), and
# a PDF. OT25 had 31 order lists and 76 miscellaneous orders. The PDFs have a
# text layer with a fixed grammar:
#
#   (ORDER LIST: 609 U.S.)
#   MONDAY, OCTOBER 6, 2025
#   ORDERS IN PENDING CASES            <- section heading, centred caps
#   25M1         DOE, JOHN V. ILLINOIS <- docket, then the caption in caps
#                   The motion for leave to file ... is denied.   <- order text,
#                                                                    indented prose
#   CERTIORARI -- SUMMARY DISPOSITIONS <- the GVRs
#   24-6543      CHANEY, DEVIN V. UNITED STATES
#   25-6218      NYANDORO, KENLEONE J. V. UNITED STATES
#                   The motions ... are granted. The judgments are vacated ...
#                                         <- one order text for a RUN of dockets
#   CERTIORARI GRANTED / CERTIORARI DENIED / HABEAS CORPUS DENIED /
#   MANDAMUS DENIED / REHEARINGS DENIED / ATTORNEY DISCIPLINE
#   25-904    )   LA UNION V. PAXTON   <- ")" brackets dockets sharing an order
#   26-5316    SILVIA, WILLIAM F. V. FLORIDA
#   (26A209)                           <- a related docket on its own line
#
# The list ends where any attached opinions begin ("SUPREME COURT OF THE UNITED
# STATES" at the margin, or a "THOMAS, J., dissenting" running head), and those
# are NOT parsed: they are the separate writings the docket pages already link.
#
# WHY IT IS WORTH HAVING. The docket JSON records the same orders as entries on
# each docket, and the site reads those. But the order list is the Court's own
# statement of a day's work, posted the morning it happens, and it is the one
# place the grants, the GVRs and the denials of a Monday sit together. It is
# also the audit source for the funnel's grammar: what the list says was
# granted is what was granted.
#
# WHO WRITES WHAT. The daily owns orders/: it fetches the listing for the
# current and prior Terms (two requests), downloads only the PDFs the manifest
# does not have, parses them, and renders. orders/orders.json is the manifest
# (one entry per document, append-only per key -- an order list never
# disappears); orders/data/<stem>.json holds each document's parsed entries, so
# a template change re-renders from disk without a re-download.

suppressPackageStartupMessages({ library(stringr); library(jsonlite); library(htmltools) })

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

ORDERS_LISTING_URL <- "https://www.supremecourt.gov/orders/ordersofthecourt/%s"
ORDERS_BASE        <- "https://www.supremecourt.gov"
ORDERS_DIR         <- "orders"
ORDERS_MANIFEST    <- "orders.json"
ORDERS_TEMPLATE_VERSION <- "o2"   # o2: captions through strip_caption_roles()
# The parser's version, stamped on each manifest entry. A document parsed by an
# older parser is fetched and parsed again (one request each, inside the run's
# ORDERS_MAX_NEW budget, so a bump spreads over a few runs).
#   p2: a prose line beginning with a docket number is text, not a new docket
ORDERS_PARSER_VERSION <- "p2"
# Landing-page panel: documents from the last N days, at most M of them.
ORDERS_PANEL_DAYS <- 21L
ORDERS_PANEL_MAX  <- 4L

# Section headings -> keys. `other` keeps the heading text as its label.
ORDERS_SECTIONS <- c(
  "ORDERS IN PENDING CASES"            = "pending",
  "CERTIORARI -- SUMMARY DISPOSITIONS" = "gvr",
  "CERTIORARI GRANTED"                 = "granted",
  "CERTIORARI DENIED"                  = "denied",
  "HABEAS CORPUS DENIED"               = "habeas",
  "MANDAMUS DENIED"                    = "mandamus",
  "PROHIBITION DENIED"                 = "prohibition",
  "REHEARINGS DENIED"                  = "rehearing",
  "ATTORNEY DISCIPLINE"                = "discipline")
ORDERS_SECTION_LABELS <- c(
  pending = "Orders in pending cases", gvr = "Summary dispositions",
  granted = "Certiorari granted", denied = "Certiorari denied",
  habeas = "Habeas corpus denied", mandamus = "Mandamus denied",
  prohibition = "Prohibition denied", rehearing = "Rehearings denied",
  discipline = "Attorney discipline", other = "Orders")
# The order the page shows sections in: what the Court did that matters most
# first. The Court's own order puts the pending-case housekeeping first.
ORDERS_SECTION_ORDER <- c("granted", "gvr", "pending", "other", "denied", "habeas",
                          "mandamus", "prohibition", "rehearing", "discipline")

# ---- the Terms to read ------------------------------------------------------------

# The listing is per Term, and a Term's page keeps filling until its successor
# opens in October. Current and prior, so nothing is missed across the boundary.
orders_terms <- function(as_of = Sys.Date()) {
  y <- as.integer(format(as_of, "%Y")) - as.integer(as.integer(format(as_of, "%m")) < 10L)
  sprintf("%02d", (y - 0:1) %% 100L)
}

# ---- the listing ------------------------------------------------------------------

.orders_get <- function(url, as = c("string", "raw")) {
  as <- match.arg(as)
  resp <- if (exists("scotus_perform") && exists("scotus_req"))
    scotus_perform(scotus_req(url))
  else
    httr2::req_perform(httr2::req_user_agent(httr2::request(url), "ceRt SCOTUS docketing dashboard (httr2)"))
  if (httr2::resp_status(resp) != 200L) stop("HTTP ", httr2::resp_status(resp), " for ", url)
  if (as == "raw") httr2::resp_body_raw(resp) else httr2::resp_body_string(resp)
}

.orders_df <- function(date = as.Date(character()), kind = character(), url = character(),
                       stem = character(), term = character(), label = character())
  data.frame(date = as.Date(date), kind = kind, url = url, stem = stem, term = term,
             label = label, stringsAsFactors = FALSE)

# One Term's listing page -> (date, kind, url, stem). The page is a run of
#   <div style="display:block"><span>09/04/26 &nbsp;</span>
#     <span><a href='/orders/courtorders/090426zor_22q3.pdf'>Order List</a></span></div>
# The stem is the PDF's name without the hash suffix ("090426zor"), which is
# the stable identity: the Court's file names carry a hash that could change on
# a re-post, and the date alone is not unique (two miscellaneous orders on one
# day are "072826zr1" and "072826zr2").
parse_orders_listing <- function(html, term = NA_character_) {
  m <- str_match_all(html, regex(paste0(
    "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s*(?:&nbsp;)?\\s*</span>\\s*<span[^>]*>\\s*",
    "<a[^>]+href=['\"]([^'\"]+\\.pdf)['\"][^>]*>([^<]*)</a>"), dotall = TRUE))[[1]]
  if (!nrow(m)) return(.orders_df())
  url <- ifelse(str_starts(m[, 3], "/"), paste0(ORDERS_BASE, m[, 3]), m[, 3])
  stem <- str_remove(basename(url), "_[A-Za-z0-9]+\\.pdf$")
  stem <- str_remove(stem, "\\.pdf$")
  # Three kinds. The listing also carries the April orders adopting amendments
  # to the Federal Rules ("Rules of Evidence", stem "frev26"): a document with
  # no dockets in it, kept for completeness under its own label and never on
  # the landing panel.
  label <- str_squish(m[, 4])
  kind <- ifelse(str_detect(label, regex("miscellaneous", ignore_case = TRUE)), "misc",
          ifelse(str_detect(label, regex("^order list", ignore_case = TRUE)), "list", "rules"))
  d <- suppressWarnings(lubridate::mdy(m[, 2]))
  out <- .orders_df(d, kind, url, tolower(stem), rep(term, length(d)), label)
  out[!is.na(out$date) & !duplicated(out$stem), , drop = FALSE]
}

fetch_orders_listing <- function(terms = orders_terms()) {
  parts <- lapply(terms, function(t) {
    url <- sprintf(ORDERS_LISTING_URL, t)
    tryCatch(parse_orders_listing(.orders_get(url), t), error = function(e) {
      cat("Orders listing", url, "unavailable:", conditionMessage(e), "\n"); .orders_df()
    })
  })
  out <- do.call(rbind, parts)
  if (is.null(out) || !nrow(out)) return(.orders_df())
  out <- out[!duplicated(out$stem), , drop = FALSE]
  out[order(out$date, out$stem, decreasing = TRUE), , drop = FALSE]
}

# ---- the parser ---------------------------------------------------------------------

.ORD_DOCKET_RX  <- "^\\s*(\\d{2}-\\d{1,5}|\\d{2}[AMO]\\d{1,4}|D-\\d{1,5})\\s*(\\))?\\s*(.*)$"
.ORD_RELATED_RX <- "^\\s*\\((\\d{2}[AMO-]\\d{1,5})\\)\\s*$"
.ORD_HEADING_RX <- "^[A-Z][A-Z .,&'-]{7,}$"
.ORD_STOP_RX    <- paste0("^\\s*(SUPREME COURT OF THE UNITED STATES\\s*$|Cite as:|",
                          "[A-Z]{3,}, (C\\. )?J\\., (dissenting|concurring))")
.ORD_FURNITURE_RX <- "^\\s*\\d{1,3}\\s*$|^\\s*\\(ORDER LIST:|^<<<PAGE \\d+>>>$"
.ORD_DATE_RX <- "^\\s*(MONDAY|TUESDAY|WEDNESDAY|THURSDAY|FRIDAY|SATURDAY|SUNDAY),\\s+([A-Z]+ \\d{1,2}, \\d{4})\\s*$"

.entries_df <- function() data.frame(section = character(), label = character(),
                                     dkt = character(), caption = character(),
                                     related = character(), text = character(),
                                     group = integer(), stringsAsFactors = FALSE)

#' Parse the text of one order document. `pages` is pdftools::pdf_text() output
#' (one string per page). Returns list(date, cite, entries), where `entries` is
#' one row per docket: section key, section label, docket, caption (the
#' Court's caps), related docket, the order text, and a group id shared by
#' dockets that one order text disposes of together.
parse_order_document <- function(pages) {
  lines <- unlist(strsplit(paste(pages, collapse = "\n"), "\n", fixed = TRUE))
  lines <- str_replace_all(lines, " ", " ")
  cite <- str_match(paste(head(lines, 6), collapse = " "), "\\(ORDER LIST:\\s*([^)]+)\\)")[1, 2]
  dm <- str_match(lines, .ORD_DATE_RX)
  date <- suppressWarnings(lubridate::mdy(dm[which(!is.na(dm[, 3]))[1], 3]))

  section <- "other"; label <- "Orders"
  entries <- list(); cur <- 0L; open <- integer(); group <- 0L
  last_kind <- "none"   # what the previous kept line was: docket / caption / text / other
  for (ln in lines) {
    if (str_detect(ln, .ORD_STOP_RX)) break
    if (!nzchar(str_trim(ln))) next
    if (str_detect(ln, .ORD_FURNITURE_RX) || !is.na(str_match(ln, .ORD_DATE_RX)[1, 1])) next
    t <- str_squish(ln)
    # A section heading: centred caps, no digits, and not a docket line.
    if (str_detect(t, .ORD_HEADING_RX) && !str_detect(ln, .ORD_DOCKET_RX) &&
        (t %in% names(ORDERS_SECTIONS) ||
         str_detect(t, "(GRANTED|DENIED|DISPOSITIONS|PENDING CASES|DISCIPLINE|DISMISSED)$"))) {
      section <- unname(ORDERS_SECTIONS[t]); if (is.na(section)) section <- "other"
      label <- if (section == "other") str_to_sentence(t) else unname(ORDERS_SECTION_LABELS[section])
      open <- integer(); last_kind <- "other"
      next
    }
    dk <- str_match(ln, .ORD_DOCKET_RX)
    # A docket line's remainder is a caption, in caps. A prose line that happens
    # to begin with a docket number -- "20-1531, No. 20-1778, and No. 20-1780
    # are granted", the wrapped tail of "The petitions for writs of certiorari
    # in No. 20-1530, No." -- carries lowercase words, and is text. ("McCARTHY"
    # and "DeBOSE" carry a lowercase letter, not a lowercase word.)
    if (!is.na(dk[1, 2]) && str_detect(dk[1, 4], "\\b[a-z]{3,}\\b")) dk[1, 2] <- NA_character_
    if (!is.na(dk[1, 2])) {
      # A run of docket lines shares whatever text follows; a docket that comes
      # after text starts a new run.
      if (identical(last_kind, "text")) open <- integer()
      cur <- cur + 1L; group <- if (length(open)) entries[[open[1]]]$group else group + 1L
      # The bracket that groups dockets sharing an order sits before OR after
      # the caption ("25-904    )   LA UNION V. PAXTON" / "25-390   FULL PLAY
      # GROUP, S.A. V. UNITED STATES, ET AL. )"); either way it is not caption.
      entries[[cur]] <- list(section = section, label = label, dkt = dk[1, 2],
                             caption = str_squish(str_remove(dk[1, 4], "\\s*\\)\\s*$")),
                             related = NA_character_, text = "", group = group)
      open <- c(open, cur); last_kind <- "docket"
      next
    }
    rel <- str_match(ln, .ORD_RELATED_RX)
    if (!is.na(rel[1, 2]) && cur > 0L) { entries[[cur]]$related <- rel[1, 2]; next }
    if (cur == 0L) next
    # A caption continuation is caps with no lowercase, directly under a docket
    # (or another continuation); anything with lowercase is the order's prose.
    if (last_kind %in% c("docket", "caption") && !str_detect(t, "[a-z]")) {
      # The grouping bracket can also be a line of its own under the caption.
      t2 <- str_squish(str_remove_all(t, "^\\)\\s*|\\s*\\)$"))
      if (nzchar(t2)) entries[[cur]]$caption <- str_squish(paste(entries[[cur]]$caption, t2))
      last_kind <- "caption"; next
    }
    for (i in if (length(open)) open else cur)
      entries[[i]]$text <- str_squish(paste(entries[[i]]$text, t))
    last_kind <- "text"
  }
  df <- if (length(entries)) do.call(rbind, lapply(entries, as.data.frame, stringsAsFactors = FALSE)) else .entries_df()
  # A single-Justice order (26A203, 21 Aug 2026: the Chief Justice's
  # administrative stay) is typeset as a caption page, not a list:
  #   Supreme Court of the United States / No. 26A203 / NATIONAL PARK SERVICE,
  #   ET AL., / Applicants / v. / NATIONAL TRUST ... / ORDER / IT IS ORDERED ...
  # One entry: the docket from the "No." line, the caption from the party lines
  # (role words dropped), the order from "ORDER" to the signature.
  if (!nrow(df)) {
    no <- str_match(lines, "^\\s*No\\.\\s*(\\d{2}[AMO-]\\d{1,5})\\s*$")
    k <- which(!is.na(no[, 2]))[1]
    if (!is.na(k)) {
      rest <- str_squish(lines[(k + 1):length(lines)])
      rest <- rest[nzchar(rest)]
      o <- which(str_detect(rest, "^(ORDER|IT IS ORDERED)"))[1]
      party <- if (!is.na(o) && o > 1) rest[seq_len(o - 1)] else character()
      party <- party[!str_detect(party, regex("^(applicants?|respondents?|petitioners?|appellants?|appellees?|plaintiffs?|defendants?)\\.?$", ignore_case = TRUE))]
      caption <- str_squish(str_replace_all(paste(party, collapse = " "), "\\s*,?\\s+v\\.\\s+", " V. "))
      caption <- str_remove(caption, ",\\s*$")
      body <- if (!is.na(o)) rest[o:length(rest)] else rest
      sig <- which(str_detect(body, "^/s/|^Dated this"))[1]
      if (!is.na(sig)) body <- body[seq_len(sig - 1)]
      body <- body[!str_detect(body, "^ORDER$")]
      if (is.na(date)) {
        dd <- str_match(paste(rest, collapse = " "), "Dated this (\\d{1,2})(?:st|nd|rd|th)?\\s*day of ([A-Za-z]+), (\\d{4})")
        if (!is.na(dd[1, 1])) date <- suppressWarnings(lubridate::mdy(paste(dd[1, 3], dd[1, 2], dd[1, 4])))
      }
      df <- data.frame(section = "other", label = "Order", dkt = no[k, 2], caption = caption,
                       related = NA_character_, text = str_squish(paste(body, collapse = " ")),
                       group = 1L, stringsAsFactors = FALSE)
    } else if (any(str_detect(lines, "^\\s*IT IS (FURTHER )?ORDERED"))) {
      # An order of the Court with no docket at all (17 Feb 2026: the revised
      # Rules of the Court take effect). One entry, no docket, the prose.
      rest <- str_squish(lines[!str_detect(lines, .ORD_FURNITURE_RX) & is.na(str_match(lines, .ORD_DATE_RX)[, 1])])
      rest <- rest[nzchar(rest) & !str_detect(rest, "^ORDER$")]
      df <- data.frame(section = "other", label = "Order", dkt = "", caption = "Order of the Court",
                       related = NA_character_, text = str_squish(paste(rest, collapse = " ")),
                       group = 1L, stringsAsFactors = FALSE)
    }
  }
  list(date = date, cite = if (is.na(cite)) NA_character_ else str_squish(cite), entries = df)
}

# Counts per section, in page order, as a named integer vector.
order_counts <- function(entries) {
  if (is.null(entries) || !nrow(entries)) return(integer())
  tab <- table(entries$section)
  keys <- c(intersect(ORDERS_SECTION_ORDER, names(tab)), setdiff(names(tab), ORDERS_SECTION_ORDER))
  setNames(as.integer(tab[keys]), keys)
}

# "5 granted · 7 GVRs · 214 denied": the counts a reader wants, in that order.
.COUNT_WORDS <- c(granted = "granted", gvr = "GVR", denied = "denied", habeas = "habeas denied",
                  mandamus = "mandamus denied", rehearing = "rehearings denied",
                  pending = "in pending cases", discipline = "discipline", other = "orders",
                  prohibition = "prohibition denied")
counts_line <- function(counts, sep = " · ", keys = names(.COUNT_WORDS)) {
  if (!length(counts)) return("")
  keys <- intersect(keys, names(counts))
  bits <- vapply(keys, function(k) {
    n <- counts[[k]]
    w <- .COUNT_WORDS[[k]]
    if (k == "gvr") w <- if (n == 1L) "GVR" else "GVRs"
    if (k == "other") w <- if (n == 1L) "order" else "orders"
    paste(format(n, big.mark = ","), w)
  }, character(1))
  paste(bits, collapse = sep)
}

# ---- the manifest -------------------------------------------------------------------

.orders_path <- function(site_dir, ...) file.path(site_dir, ORDERS_DIR, ...)

read_orders_manifest <- function(site_dir) {
  p <- .orders_path(site_dir, ORDERS_MANIFEST)
  if (!file.exists(p)) return(list())
  tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) {
    warning("read_orders_manifest(): ", basename(p), " unreadable -- treating as empty.", call. = FALSE)
    list()
  })
}

write_orders_manifest <- function(site_dir, idx) {
  p <- .orders_path(site_dir, ORDERS_MANIFEST)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  # Newest first, so a reader of the raw file sees the latest at the top and a
  # diff is small; the byte layout is a function of the content alone.
  d <- vapply(idx, function(x) x$date %||% "", character(1))
  idx <- idx[order(d, names(idx), decreasing = TRUE)]
  write_json(idx, p, auto_unbox = TRUE, na = "null", null = "null", pretty = FALSE)
  invisible(p)
}

# The page name for a document: the date, with the kind and a sequence for the
# rare second miscellaneous order on one day.
.orders_page <- function(stem, date, kind) {
  # A stem that is not the MMDDYY form (the rules orders: "frev26", "frbk26",
  # "frap26", three on one day) keeps its own name, or three documents would
  # share one page.
  if (!str_detect(stem, "^\\d{6}z")) return(paste0(format(as.Date(date), "%Y-%m-%d"), "-", stem, ".html"))
  suffix <- if (kind == "list") "" else paste0("-misc", str_extract(stem, "\\d+$") %|NA|% "")
  paste0(format(as.Date(date), "%Y-%m-%d"), suffix, ".html")
}
# The word for a document's kind, on its page, the index and the panel.
.ord_kind_word <- function(meta) switch(meta$kind %||% "list",
  misc = "Miscellaneous order", rules = meta$label %||% "Rules amendments", "Order list")
`%|NA|%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

#' Fetch the listings, download and parse every document the manifest does not
#' hold (newest first, at most `max_new`), write each one's entries beside the
#' manifest, and rewrite the manifest. Never fatal per document. Returns the
#' counts as a list.
update_orders <- function(site_dir, terms = orders_terms(), max_new = 250L) {
  idx <- read_orders_manifest(site_dir)
  listing <- fetch_orders_listing(terms)
  if (!nrow(listing)) return(invisible(list(listed = 0L, new = 0L, failed = 0L, total = length(idx))))
  # New documents, plus any the manifest holds with nothing parsed out of them:
  # a grammar the parser has since learned gets another look, at one request.
  empty <- names(idx)[vapply(idx, function(x)
    (identical(as.integer(x$n %||% 0L), 0L) || !identical(x$parser %||% "", ORDERS_PARSER_VERSION)) &&
      !identical(x$kind, "rules"), logical(1))]
  todo <- listing[!listing$stem %in% setdiff(names(idx), empty), , drop = FALSE]
  # Newest first, so the current Term's documents are re-parsed before the back
  # catalogue when a parser bump has to spread over runs.
  todo <- todo[order(todo$date, decreasing = TRUE), , drop = FALSE]
  if (nrow(todo) > max_new) {
    cat("Order documents: ", nrow(todo), " new, capped at ", max_new, " this run\n", sep = "")
    todo <- head(todo, max_new)
  }
  dir.create(.orders_path(site_dir, "data"), recursive = TRUE, showWarnings = FALSE)
  failed <- 0L
  for (i in seq_len(nrow(todo))) {
    r <- todo[i, ]
    doc <- tryCatch({
      raw <- .orders_get(r$url, "raw")
      tmp <- tempfile(fileext = ".pdf")
      writeBin(raw, tmp)
      pages <- pdftools::pdf_text(tmp); unlink(tmp)
      parse_order_document(pages)
    }, error = function(e) { cat("Order document", r$stem, "failed:", conditionMessage(e), "\n"); NULL })
    if (is.null(doc)) { failed <- failed + 1L; next }
    date <- if (!is.na(doc$date)) doc$date else r$date
    ent <- doc$entries
    write_json(ent, .orders_path(site_dir, "data", paste0(r$stem, ".json")),
               dataframe = "rows", auto_unbox = TRUE, na = "null")
    counts <- order_counts(ent)
    pick <- function(k) if (nrow(ent)) ent[ent$section == k, c("dkt", "caption"), drop = FALSE] else data.frame()
    idx[[r$stem]] <- list(
      date = format(date), kind = r$kind, label = r$label, url = r$url, term = r$term,
      parser = ORDERS_PARSER_VERSION,
      cite = doc$cite, page = .orders_page(r$stem, date, r$kind),
      n = nrow(ent), counts = as.list(counts),
      granted = pick("granted"), gvr = pick("gvr"),
      # A miscellaneous order names one or a few dockets; keep them for the panel.
      dockets = if (r$kind == "misc") head(ent[, c("dkt", "caption"), drop = FALSE], 6L) else data.frame())
  }
  write_orders_manifest(site_dir, idx)
  invisible(list(listed = nrow(listing), new = nrow(todo) - failed, failed = failed, total = length(idx)))
}

# ---- rendering ------------------------------------------------------------------------

.ord_esc <- function(x) htmltools::htmlEscape(x)
# "%e" pads the day with a space ("June  9"); squished for text that is not
# inside a tag where the browser would collapse it.
.ord_date <- function(d, fmt) str_squish(format(as.Date(d), fmt))

# The site's own caption for a docket where it has one, else the Court's caps.
.ord_captions <- function(site_dir) {
  p <- file.path(site_dir, "cases", "search.json")
  if (!file.exists(p)) return(list())
  caps <- tryCatch(as.list(fromJSON(p)), error = function(e) list())
  # search.json strips only ", Petitioners" and ", Respondents"; the landing
  # page's caption rule (strip_caption_roles, page_style.R) also drops
  # ", Applicants" and "et al." -- "National Park Service, et al., Applicants v."
  # was the first row this panel published.
  if (length(caps) && exists("strip_caption_roles"))
    caps <- as.list(setNames(get("strip_caption_roles")(unlist(caps, use.names = FALSE)), names(caps)))
  caps
}
.ord_case_link <- function(dkt, caption, caps, available, prefix = "../cases/") {
  cap <- caps[[dkt]] %||% caption
  if (is.null(cap) || is.na(cap) || !nzchar(cap)) cap <- dkt
  if (dkt %in% available) sprintf("<a href='%s%s.html'>%s</a>", prefix, dkt, .ord_esc(cap))
  else .ord_esc(cap)
}

.ORDERS_CSS <- "
  .osum{font-size:1.05rem;color:var(--ink-soft);margin:.2rem 0 1.2rem}
  .osum a.pdf{color:var(--link)}
  h2.osec{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.25rem;margin:1.8rem 0 .4rem;
    padding-bottom:.25rem;border-bottom:1px solid var(--rule)}
  h2.osec .cn{color:var(--faint);font-weight:400;font-size:.9rem;margin-left:.5rem}
  ol.olist{list-style:none;margin:0;padding:0}
  ol.olist li{padding:.45rem 0;border-bottom:1px solid var(--rule)}
  ol.olist .odk{font-family:'Newsreader',Georgia,serif;font-size:.85rem;color:var(--faint);
    letter-spacing:.02em;white-space:nowrap;margin-right:.6rem}
  ol.olist .ocap{font-family:'Fraunces',Georgia,serif;font-weight:600}
  ol.olist .ocap a{color:inherit;text-decoration:none}
  ol.olist .ocap a:hover{color:var(--accent);text-decoration:underline;text-underline-offset:3px}
  ol.olist .otext{display:block;margin:.25rem 0 0 0;color:var(--ink-soft);font-size:.95rem;line-height:1.45}
  ol.olist li.grp{border-bottom:0;padding-bottom:.1rem}
  ol.olist.dense li{padding:.22rem 0;border-bottom:0}
"

# One document's page. `available` is the set of docket numbers with a case page.
render_order_page <- function(site_dir, stem, meta, entries, caps, available) {
  date <- as.Date(meta$date)
  kind_word <- .ord_kind_word(meta)
  long_date <- .ord_date(date, "%A, %B %e, %Y")
  title <- paste0(kind_word, " — ", .ord_date(date, "%B %e, %Y"))
  counts <- order_counts(entries)
  cl <- counts_line(counts)
  summary <- paste0(
    "<p class='osum'>", if (nzchar(cl)) paste0(.ord_esc(cl), " &middot; ") else "",
    "<a class='pdf' href='", meta$url, "' target='_blank' rel='noopener'>The Court&rsquo;s PDF</a>",
    if (!is.null(meta$cite) && !is.na(meta$cite)) paste0(" &middot; ", .ord_esc(meta$cite)) else "",
    "</p>")
  secs <- unique(entries$section)
  secs <- c(intersect(ORDERS_SECTION_ORDER, secs), setdiff(secs, ORDERS_SECTION_ORDER))
  body <- vapply(secs, function(s) {
    e <- entries[entries$section == s, , drop = FALSE]
    lab <- if (s == "other") (e$label[1] %||% "Orders") else ORDERS_SECTION_LABELS[[s]]
    # A denial section is one sentence ("The petitions for writs of certiorari
    # are denied.") over hundreds of dockets: list them tight, and the sentence
    # prints once, under the last. Any section where the dockets carry their
    # own orders gets the roomier layout.
    dense <- s %in% c("denied", "habeas", "mandamus", "prohibition", "rehearing") &&
      length(unique(e$text)) <= 1L
    items <- vapply(seq_len(nrow(e)), function(i) {
      r <- e[i, ]
      # Text is shown once per group, under the last docket of the run.
      last_of_group <- i == nrow(e) || e$group[i + 1] != r$group
      txt <- if (nzchar(r$text) && last_of_group)
        paste0("<span class='otext'>", .ord_esc(r$text), "</span>") else ""
      rel <- if (!is.na(r$related)) paste0(" <span class='odk'>(", .ord_esc(r$related), ")</span>") else ""
      paste0("<li", if (!last_of_group) " class='grp'" else "", ">",
             if (nzchar(r$dkt)) paste0("<span class='odk'>", .ord_esc(r$dkt), "</span>") else "",
             "<span class='ocap'>", .ord_case_link(r$dkt, r$caption, caps, available), "</span>", rel,
             txt, "</li>")
    }, character(1))
    paste0("<h2 class='osec'>", .ord_esc(lab), "<span class='cn'>", nrow(e), "</span></h2>",
           "<ol class='olist", if (dense) " dense" else "", "'>", paste(items, collapse = ""), "</ol>")
  }, character(1))
  crumb <- list(href = paste0("/", ORDERS_DIR, "/"), label = "Orders")
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head(paste0(title, " — Supreme Court Report"),
              site_breadcrumb_jsonld(.ord_date(date, "%B %e, %Y"), crumb),
              extra_css = .ORDERS_CSS,
              description = paste0(kind_word, " of ", .ord_date(date, "%B %e, %Y"),
                                   if (nzchar(cl)) paste0(": ", cl) else "", "."),
              path = paste0("/", ORDERS_DIR, "/", meta$page)),
    "<body>", site_masthead(),
    "<main class='wrap' id='main'>",
    site_breadcrumb(.ord_date(date, "%B %e, %Y"), crumb),
    "<p class='kicker'>", kind_word, "</p><h1>", .ord_esc(long_date), "</h1>",
    "<hr class='brule'>", summary,
    paste(body, collapse = ""),
    "</main></body>\n</html>\n")
  out <- .orders_path(site_dir, meta$page)
  writeLines(enc2utf8(html), out, useBytes = TRUE)
  invisible(out)
}

#' Render orders/index.html and every document page whose template version is
#' behind (or that is missing). Incremental by version, like the docket pages.
render_orders <- function(site_dir) {
  idx <- read_orders_manifest(site_dir)
  if (!length(idx)) return(invisible(0L))
  caps <- .ord_captions(site_dir)
  available <- names(caps)[file.exists(file.path(site_dir, "cases", paste0(names(caps), ".html")))]
  # Page names are a function of (stem, date, kind), recomputed here so a change
  # to the naming rule reaches documents already in the manifest.
  for (stem in names(idx)) {
    p <- .orders_page(stem, idx[[stem]]$date, idx[[stem]]$kind %||% "list")
    if (!identical(idx[[stem]]$page, p)) { idx[[stem]]$page <- p; idx[[stem]]$rendered <- NULL }
  }
  n <- 0L
  for (stem in names(idx)) {
    meta <- idx[[stem]]
    out <- .orders_path(site_dir, meta$page)
    if (identical(meta$rendered %||% "", ORDERS_TEMPLATE_VERSION) && file.exists(out)) next
    ep <- .orders_path(site_dir, "data", paste0(stem, ".json"))
    entries <- if (file.exists(ep)) tryCatch(fromJSON(ep), error = function(e) NULL) else NULL
    if (is.null(entries) || !is.data.frame(entries)) entries <- .entries_df()
    if (!nrow(entries)) entries <- .entries_df()
    entries$related <- as.character(entries$related %||% NA_character_)
    entries$text <- as.character(entries$text %||% "")
    ok <- tryCatch({ render_order_page(site_dir, stem, meta, entries, caps, available); TRUE },
                   error = function(e) { message("order page ", stem, " failed: ", conditionMessage(e)); FALSE })
    if (ok) { idx[[stem]]$rendered <- ORDERS_TEMPLATE_VERSION; n <- n + 1L }
  }
  write_orders_manifest(site_dir, idx)
  # Only the daily writes orders/, so a page the manifest does not name is a
  # leftover of an old naming rule, and it goes.
  pages <- c("index.html", vapply(idx, function(x) x$page, character(1)))
  stray <- setdiff(list.files(.orders_path(site_dir), pattern = "\\.html$"), pages)
  if (length(stray)) { unlink(.orders_path(site_dir, stray)); message("orders: removed ", length(stray), " stray page(s)") }
  # The section index: one row per document, newest first.
  d <- vapply(idx, function(x) x$date, character(1))
  ord <- order(d, names(idx), decreasing = TRUE)
  items <- lapply(names(idx)[ord], function(stem) {
    m <- idx[[stem]]
    cl <- counts_line(unlist(m$counts))
    list(href = m$page,
         label = paste0(.ord_kind_word(m), " · ", .ord_date(m$date, "%A, %B %e, %Y")),
         meta = if (nzchar(cl)) cl else if (identical(m$kind, "rules")) "" else paste(m$n, "order(s)"))
  })
  styled_index_page(
    .orders_path(site_dir, "index.html"),
    title = "Orders — Supreme Court Report",
    kicker = "Supreme Court of the United States",
    heading = "Orders",
    dek = paste("Every order list and miscellaneous order the Court has issued, in the Court's",
                "own words: what it granted, what it sent back, what it turned away."),
    items = items, new_tab = FALSE,
    crumb = list(label = "Orders", section = NULL),
    path = paste0("/", ORDERS_DIR, "/"))
  message("orders: ", n, " page(s) rendered / ", length(idx), " document(s)")
  invisible(n)
}

# A (dkt, caption) block as it comes back from the manifest: a data.frame when
# written this run, a list of row-lists after the JSON round trip, or empty.
.rows_df <- function(x) {
  if (is.data.frame(x)) return(x)
  if (is.null(x) || !length(x)) return(data.frame(dkt = character(), caption = character(), stringsAsFactors = FALSE))
  data.frame(dkt = vapply(x, function(r) as.character(r$dkt %||% NA), character(1)),
             caption = vapply(x, function(r) as.character(r$caption %||% ""), character(1)),
             stringsAsFactors = FALSE)
}

# ---- the landing-page panel -------------------------------------------------------------

#' "Latest orders": the documents from the last ORDERS_PANEL_DAYS days, newest
#' first, at most ORDERS_PANEL_MAX. Each row names the day, links its page, and
#' says what happened: the counts, and the granted cases by name (each linked to
#' its docket page). NULL when there is nothing in the window, which is the
#' normal state for a stretch of the summer.
orders_panel <- function(site_dir, as_of = Sys.Date(), heading = "Latest orders",
                         note = "What the Court granted, sent back and turned away, from its order lists.",
                         days = ORDERS_PANEL_DAYS, max_rows = ORDERS_PANEL_MAX) {
  idx <- read_orders_manifest(site_dir)
  if (!length(idx)) return(NULL)
  d <- as.Date(vapply(idx, function(x) x$date, character(1)))
  kinds <- vapply(idx, function(x) x$kind %||% "list", character(1))
  keep <- which(!is.na(d) & d >= as.Date(as_of) - days & d <= as.Date(as_of) + 1L & kinds != "rules")
  if (!length(keep)) return(NULL)
  keep <- keep[order(d[keep], names(idx)[keep], decreasing = TRUE)]
  keep <- head(keep, max_rows)
  caps <- .ord_captions(site_dir)
  available <- names(caps)[file.exists(file.path(site_dir, "cases", paste0(names(caps), ".html")))]
  link <- function(dkt, cap) {
    label <- caps[[dkt]] %||% cap
    if (dkt %in% available) tags$a(href = paste0("cases/", dkt, ".html"), smarten(label)) else smarten(label)
  }
  rows <- lapply(keep, function(i) {
    m <- idx[[i]]; date <- d[i]
    counts <- unlist(m$counts)
    # The headline numbers where there are any; a list with no grants, GVRs or
    # denials (the September 4 list: rehearings, discipline, housekeeping) gets
    # the full breakdown instead, which is short in exactly that case.
    cl <- counts_line(counts, keys = c("granted", "gvr", "denied"))
    if (!nzchar(cl)) cl <- counts_line(counts)
    if (!nzchar(cl) && !is.null(m$n)) cl <- paste(m$n, if (m$n == 1) "order" else "orders")
    named <- .rows_df(if (identical(m$kind, "misc")) m$dockets else m$granted)
    linked <- if (nrow(named)) lapply(seq_len(nrow(named)), function(k) link(named$dkt[k], named$caption[k])) else list()
    bits <- list()
    if (nzchar(cl)) bits <- c(bits, list(cl))
    if (length(linked)) bits <- c(bits, list(tagList(
      if (!identical(m$kind, "misc")) tags$span(class = "dk", "Granted") else NULL,
      do.call(tagList, .interleave(linked, HTML(" &middot; "))))))
    tags$li(tags$div(
      class = "crow",
      tags$span(class = "cwhen", tags$span(class = "cdow", format(date, "%a")), format(date, "%b %e")),
      tags$span(class = "ctx",
                tags$a(class = "ckind", href = paste0(ORDERS_DIR, "/", m$page), .ord_kind_word(m)),
                tags$span(class = "cdet", do.call(tagList, .interleave(bits, HTML(" &middot; ")))))))
  })
  tags$section(
    class = "panel cal",
    tags$h2(heading),
    if (!is.null(note)) tags$p(class = "pnote", smarten(note)),
    tags$ol(class = "cal", rows),
    tags$p(class = "more", HTML(paste0("<a href='", ORDERS_DIR, "/'>All order lists &rarr;</a>"))))
}
