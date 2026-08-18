# scotus_dash_new.R -----------------------------------------------------------
# Daily Supreme Court docketing dashboard, powered by supremecourt.gov's
# per-docket JSON API instead of scraping the HTML docket pages:
#
#   https://www.supremecourt.gov/rss/cases/JSON/{docket}.json   (e.g. 26-9, 26A9)
#
# The JSON is the canonical source the HTML docket pages render from, so this
# replaces get_scotus_case()/extract_events()/parse_parties() with a single
# structured fetch. Benefits over the HTML scraper:
#   * no dependence on table/CSS layout; document URLs come straight from JSON
#   * real case type (Paid/IFP/Application) instead of positional inference
#   * IsCounselofRecord flag -> we show the actual counsel of record
#   * extra fields surfaced: capital-case flag, related cases, counsel email
#   * QP tries the petition's text layer first, OCR only as a fallback
#   * existence checks use HTTP 200/404 instead of an HTML error-string match

suppressPackageStartupMessages({
  library(gt)
  library(gtExtras)
  library(tidyverse)
  library(httr2)
  library(jsonlite)
  library(pdftools)
  library(htmltools)
})

# ---- small utilities --------------------------------------------------------

# NULL / empty coalesce (base R's %||% only guards NULL, not length-0).
`%|||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Safe column accessor for a data.frame that may lack the column.
df_col <- function(df, name) {
  if (name %in% names(df)) df[[name]] else rep(NA_character_, nrow(df))
}

# "July 2, 2026" -> Date
parse_scotus_date <- function(x) {
  suppressWarnings(as_date(x %|||% NA_character_, format = "%B %d, %Y"))
}

# Map the API's case type to the short codes used throughout the dashboard.
TYPE_MAP <- c(Paid = "paid", IFP = "ifp", Application = "app")

# ---- JSON docket API --------------------------------------------------------

json_url <- function(dkt) {
  paste0("https://www.supremecourt.gov/rss/cases/JSON/", dkt, ".json")
}

UA <- "ceRt SCOTUS docketing dashboard (httr2)"

# Outbound request rate, in requests per second, shared across EVERY call to
# supremecourt.gov in a run.
#
# Until 2026-07-30 there was no pacing at all. "One request at a time" (below,
# and in CLAUDE.md) meant not *concurrent* -- but sequential-at-full-speed is
# still roughly 3 req/s, perfectly regular and jitter-free, from a single IP.
# Akamai rate-limits on requests-per-second-per-IP, not on concurrency, so the
# fetcher was carefully avoiding a problem the WAF does not have while walking
# straight into the one it does. Five of twelve daily runs degraded in the four
# days to 2026-07-30.
#
# 2/s is deliberately below the ~3/s that a clean run was already sustaining.
# It costs a clean daily roughly 35 extra seconds (190 requests at 2/s ~= 95s
# against ~60s). A degraded run costs ten extra minutes AND publishes nothing,
# so the trade needs no further justification.
#
# Env-overridable so the rate can be tuned from a workflow without a code change
# -- lower it if throttling persists, raise it once we have evidence there is
# headroom. See docs/navigation.md for the observed failure pattern.
FETCH_RPS <- {
  v <- suppressWarnings(as.numeric(Sys.getenv("SCOTUS_FETCH_RPS", "2")))
  if (is.na(v) || v <= 0) 2 else v
}

# Treat 403/429/503 as transient: the Akamai CDN answers 403 when too many
# requests arrive at once, so retry those (a genuine missing docket is 404).
is_transient_status <- function(resp) {
  inherits(resp, "httr2_response") && resp_status(resp) %in% c(403, 429, 503)
}

# Pacing state, shared by EVERY supremecourt.gov call in the process. Shared is
# the point: the binary search that finds each bucket's highest docket fires ~35
# HEAD requests before the main fetch begins, and those are the first traffic a
# cold runner IP sends. Pacing the two independently would let the search spend
# the budget and hand the fetch an already-throttled server.
.fetch_state <- new.env(parent = emptyenv())
.fetch_state$last <- 0

# NOT httr2::req_throttle(). Its body ends with
#     the$throttle[[realm]] <- TokenBucket$new(capacity, fill_time_s)
# so it installs a FRESH, full token bucket every time it is called. That is
# fine when you build one request and reuse it, and useless here: a per-docket
# fetcher calls the request builder once per request, refilling the bucket each
# time, and the throttle never engages. Measured on httr2 1.2.0 -- 20 requests
# at a nominal capacity=2/fill_time_s=1 went out at ~800/s. It would have
# shipped, done nothing, and taught us that pacing does not help.
#
# Ten lines we own instead. Jitter is deliberate: a perfectly regular 2.000/s
# from a single IP is itself a bot signature, and it averages to the same rate.
scotus_pace <- function() {
  gap <- stats::runif(1, 0.75, 1.25) / FETCH_RPS
  wait <- gap - (as.numeric(Sys.time()) - .fetch_state$last)
  if (wait > 0) Sys.sleep(wait)
  .fetch_state$last <- as.numeric(Sys.time())
}

# One place that builds a supremecourt.gov request, and one that performs it, so
# pacing cannot be applied at some call sites and forgotten at others.
scotus_req <- function(url) {
  request(url) |>
    req_user_agent(UA) |>
    req_retry(max_tries = 5, is_transient = is_transient_status) |>
    req_error(is_error = \(resp) FALSE)
}
scotus_perform <- function(req) { scotus_pace(); req_perform(req) }

# Fetch, classify, and build one docket's case record. Returns list(case, failed):
# `case` is the tibble (NULL for a 404 or a parse failure); `failed` is TRUE only
# for a non-404 failure (throttling that survived retries, or a transport error),
# so callers can tell "docket absent" apart from "couldn't fetch it". Retries
# transient throttling (403/429/503) with backoff.
fetch_case_result <- function(dkt) {
  resp <- tryCatch(scotus_perform(scotus_req(json_url(dkt))), error = function(e) NULL)
  # `outcome` exists only to be counted. The old code collapsed every non-200,
  # non-404 result into failed=TRUE and discarded the status, which is why the
  # warning below could only say the server was "likely" throttling -- after
  # five degraded runs we still could not tell a 403 block from a 429 slow-down
  # from a transport error, and those want opposite responses (pause globally vs
  # pace down). It changes no control flow; `failed` keeps its exact meaning.
  if (is.null(resp)) return(list(case = NULL, failed = TRUE, outcome = "transport"))
  st <- resp_status(resp)
  if (st == 404) return(list(case = NULL, failed = FALSE, outcome = "absent"))
  if (st != 200) return(list(case = NULL, failed = TRUE,
                             outcome = paste0("http_", st)))
  j <- tryCatch(fromJSON(resp_body_string(resp), simplifyVector = TRUE),
                error = function(e) NULL)
  case <- tryCatch(build_case(j, dkt), error = function(e) NULL)
  # A 200 we could not parse is NOT counted as failed -- that is pre-existing
  # behaviour and changing it would move the fetch_is_degraded() gate, which is
  # a separate decision. But it is no longer invisible.
  list(case = case, failed = FALSE,
       outcome = if (is.null(case)) "parse" else "ok")
}

# Existence check via a cheap HEAD request (200 vs 404). Retries transient
# throttling: a 403/429/503 must NOT be misread as "docket absent", or the
# binary search would silently truncate the term.
docket_exists <- function(year, sep, n) {
  resp <- tryCatch(
    scotus_req(json_url(paste0(year, sep, n))) |>
      req_method("HEAD") |>
      scotus_perform(),
    error = function(e) NULL
  )
  !is.null(resp) && resp_status(resp) == 200
}

# Largest n for which {year}{sep}{n} exists in [lower, upper]. Docket numbers are
# assigned sequentially, so existence is monotonic and binary search is valid.
binary_search_max <- function(year, sep, lower, upper) {
  best <- lower - 1
  while (lower <= upper) {
    mid <- (lower + upper) %/% 2
    if (docket_exists(year, sep, mid)) {
      best <- mid
      lower <- mid + 1
    } else {
      upper <- mid - 1
    }
  }
  best
}

# ---- shaping the JSON into the dashboard's tibble ---------------------------

# Compose a one-line-per-attorney mailing address from the party record.
format_address <- function(df) {
  street <- str_replace_all(coalesce(df_col(df, "Address"), ""), "[\r\n]+", ", ")
  loc <- str_squish(str_c(
    coalesce(df_col(df, "City"), ""), ", ",
    coalesce(df_col(df, "State"), ""), " ",
    coalesce(df_col(df, "Zip"), "")
  ))
  loc <- str_remove(loc, "^,\\s*")
  out <- str_squish(str_c(street, ", ", loc))
  str_remove_all(out, "^,\\s*|,\\s*$")
}

# Heuristic pro se test: a party is self-represented when the counsel name is
# (essentially) the party's own name. Compare on multi-letter name tokens so
# middle initials / punctuation / "P." vs "Paul" don't defeat the match.
name_tokens <- function(x) {
  toks <- str_split(str_squish(str_replace_all(str_to_lower(x %|||% ""), "[^a-z ]", " ")), " ")[[1]]
  toks[nchar(toks) > 1]
}
is_pro_se <- function(party_name, attorney) {
  pn <- name_tokens(party_name)
  at <- name_tokens(attorney)
  if (length(pn) == 0 || length(at) == 0) return(FALSE)
  all(pn %in% at) || all(at %in% pn)
}

# One side's attorneys as a tidy tibble, counsel of record first.
build_party_side <- function(df, role) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(tibble())
  tibble(
    type = role,
    names = coalesce(df_col(df, "PartyName"), df_col(df, "Attorney"), ""),
    attys = coalesce(df_col(df, "Attorney"), ""),
    firm = df_col(df, "Title"), # firm / affiliation (e.g. "Clement & Murphy, PLLC")
    address = format_address(df),
    email = df_col(df, "Email"),
    counsel_of_record = coalesce(as.logical(df_col(df, "IsCounselofRecord")), FALSE)
  ) |>
    mutate(pro_se = purrr::map2_lgl(names, attys, is_pro_se)) |>
    arrange(desc(counsel_of_record))
}

build_parties <- function(j) {
  role <- function(header, default) str_remove(header %|||% default, "^Attorneys for ")
  bind_rows(
    build_party_side(j$Petitioner, role(j$AttorneyHeaderPetitioner, "Attorneys for Petitioner")),
    build_party_side(j$Respondent, role(j$AttorneyHeaderRespondent, "Attorneys for Respondent"))
  )
}

# Proceedings as a tibble with Date, `Proceedings and Orders`, and paired
# docs_N / links_N columns (document description + URL), matching the shape the
# render pipeline expects.
build_events <- function(po) {
  empty <- tibble(Date = character(), `Proceedings and Orders` = character())
  if (is.null(po) || !is.data.frame(po) || nrow(po) == 0) return(empty)
  has_links <- "Links" %in% names(po)
  purrr::map_dfr(seq_len(nrow(po)), function(i) {
    row <- tibble(
      Date = po$Date[i] %|||% NA_character_,
      `Proceedings and Orders` = po$Text[i] %|||% NA_character_
    )
    lk <- if (has_links) po$Links[[i]] else NULL
    if (is.data.frame(lk) && nrow(lk) > 0) {
      descs <- lk$Description
      urls <- lk$DocumentUrl
      row <- bind_cols(
        row,
        as_tibble(setNames(as.list(descs), paste0("docs_", seq_along(descs)))),
        as_tibble(setNames(as.list(urls), paste0("links_", seq_along(urls))))
      )
    }
    row
  })
}

# Direct URL of the petition PDF (for the question-presented extraction) -- or,
# on a direct appeal, of the jurisdictional statement that stands in for it. Same
# preference order and same rationale as find_opening_doc_url() in qp_extract.R,
# but against the RAW docket JSON (a Links data frame per row) rather than the
# flattened docs_*/links_* events tibble, so the traversal cannot be shared; only
# the patterns are.
find_petition_url <- function(po) {
  if (is.null(po) || !is.data.frame(po) || !("Links" %in% names(po))) return(NA_character_)
  by_desc <- function(rx) {
    for (i in seq_len(nrow(po))) {
      lk <- po$Links[[i]]
      if (is.data.frame(lk) && nrow(lk) > 0) {
        hit <- which(!is.na(lk$Description) & str_detect(lk$Description, rx))
        if (length(hit) > 0 && !is.na(lk$DocumentUrl[hit[1]])) return(lk$DocumentUrl[hit[1]])
      }
    }
    NA_character_
  }
  u <- by_desc(PETITION_DOC_RE); if (!is.na(u)) return(u)
  u <- by_desc(JS_DOC_RE);       if (!is.na(u)) return(u)
  # The appeal whose statement is described only as a generic "Main Document".
  if (!("Text" %in% names(po))) return(NA_character_)
  for (i in seq_len(nrow(po))) {
    t <- po$Text[i]
    if (is.na(t) || !str_detect(t, JS_FILED_RE)) next
    lk <- po$Links[[i]]
    if (is.data.frame(lk) && nrow(lk) > 0) {
      u <- lk$DocumentUrl[!is.na(lk$DocumentUrl)]
      if (length(u) > 0) return(u[1])
    }
  }
  NA_character_
}

# Build a case-record tibble from an already-parsed docket JSON (NULL if the
# JSON isn't a real docket). Kept separate from the fetch so the two concerns
# (network vs. parsing) stay independent and testable.
build_case <- function(j, dkt) {
  if (is.null(j) || is.null(j$CaseNumber)) return(NULL)

  # Petitioner / respondent titles -> "Pet. v. Resp.". Join with " v. " ONLY when
  # both sides are present: single-party captions ("In re __", original writs)
  # carry an empty/NA respondent, so unconditionally inserting " v. " left a
  # dangling "In re __ v." on every surface (dashboards + case pages).
  side <- function(x) { x <- x %|||% ""; if (length(x) == 0 || is.na(x)) "" else str_squish(x) }
  pet <- side(j$PetitionerTitle); resp <- side(j$RespondentTitle)
  caption <- if (nzchar(pet) && nzchar(resp)) str_c(pet, " v. ", resp) else str_c(pet, resp)
  lower_dkt <- str_remove_all(j$LowerCourtCaseNumbers %|||% NA_character_, "[()]")

  tibble(
    caption = caption,
    dkt = dkt,
    date = parse_scotus_date(j$DocketedDate),
    lower = j$LowerCourt %|||% NA_character_,
    lower_dkt = if (length(lower_dkt) == 0) NA_character_ else lower_dkt,
    lower_date = parse_scotus_date(j$LowerCourtDecision),
    type = unname(TYPE_MAP[j$sJsonCaseType %|||% ""]) %|||% NA_character_,
    capital = isTRUE(j$bCapitalCase),
    related = str_c(as.character(j$RelatedCaseNumber %|||% character()), collapse = ", "),
    petition_url = find_petition_url(j$ProceedingsandOrder),
    parties = list(build_parties(j)),
    events = list(build_events(j$ProceedingsandOrder))
  )
}

# Full case record for one docket (drop-in replacement for the HTML scraper).
get_scotus_case <- function(dkt) {
  fetch_case_result(dkt)$case
}

# Fetch a set of dockets into a case tibble, one request at a time. There is no
# bulk endpoint, and the Akamai WAF throttles concurrent/bursty clients (403), so
# sequential is both simpler and far more reliable than parallel fetching --
# fetch_case_result retries transient throttling, and a non-404 failure is
# surfaced via the n_failed / n_attempted attributes so callers can refuse to
# publish a degraded fetch.
fetch_cases <- function(dkts) {
  if (length(dkts) == 0) return(tibble())
  t0 <- Sys.time()
  results <- dkts |>
    map(\(d) tryCatch(fetch_case_result(d),
                      error = function(e) list(case = NULL, failed = TRUE,
                                               outcome = "transport")),
        .progress = TRUE)
  cases <- purrr::compact(purrr::map(results, "case"))
  n_failed <- sum(purrr::map_lgl(results, "failed"))

  # Say what actually happened, by status. Five degraded runs in four days were
  # diagnosed only as "likely throttling" because this tally did not exist; with
  # it, http_403 (an Akamai block, wants a global pause) is distinguishable from
  # http_429 (a rate limit, wants a lower rate) and from transport (a network
  # problem that no amount of pacing fixes).
  tally <- table(unlist(purrr::map(results, "outcome")))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("fetch: %d request(s) in %.0fs (%.1f/s effective, %.1f/s cap) -- %s",
                  length(dkts), secs,
                  length(dkts) / max(secs, 1e-9), FETCH_RPS,
                  paste(sprintf("%s %d", names(tally), as.integer(tally)),
                        collapse = ", ")))
  if (n_failed > 0) {
    warning(n_failed, " docket(s) unresolved after retries (",
            paste(sprintf("%s %d", names(tally), as.integer(tally)),
                  collapse = ", "), ").")
  }
  if (length(cases) == 0) return(tibble())
  result <- bind_rows(cases)
  attr(result, "n_attempted") <- length(dkts)
  attr(result, "n_failed") <- n_failed
  attr(result, "outcomes") <- tally      # for callers that want the breakdown
  result
}

# All recent cases for a term: the trailing ~50 dockets of each bucket
# (paid / IFP / applications), clamped so early-term buckets don't generate
# invalid docket numbers. This is the daily-job fetch.
get_scotus_update <- function(year) {
  paid <- binary_search_max(year, "-", 0, 2000)
  ifp <- binary_search_max(year, "-", 5001, 10000)
  apps <- binary_search_max(year, "A", 0, 2000)
  make_block <- function(hi, lo_bound, sep) {
    if (hi < lo_bound) return(character())
    paste0(year, sep, max(hi - 50, lo_bound):hi)
  }
  fetch_cases(c(
    make_block(paid, 1, "-"),
    make_block(ifp, 5001, "-"),
    make_block(apps, 1, "A")
  ))
}

# Every case in a term (the full range of each bucket). Needed for backfills and
# conference reports, which require the whole term rather than just recent
# dockets. Thousands of requests -- run it from a clean IP, not a throttled one.
get_scotus_term <- function(year) {
  paid <- binary_search_max(year, "-", 0, 2000)
  ifp <- binary_search_max(year, "-", 5001, 10000)
  apps <- binary_search_max(year, "A", 0, 2000)
  full_block <- function(hi, lo_bound, sep) {
    if (hi < lo_bound) return(character())
    paste0(year, sep, lo_bound:hi)
  }
  fetch_cases(c(
    full_block(paid, 1, "-"),
    full_block(ifp, 5001, "-"),
    full_block(apps, 1, "A")
  ))
}

# Did a fetch resolve enough dockets to trust for publishing? A cold run fails
# 0 dockets; a throttled run fails many. Refuse to publish above ~10% loss.
fetch_is_degraded <- function(ot, tol = 0.1) {
  nf <- attr(ot, "n_failed") %||% 0
  na <- attr(ot, "n_attempted") %||% nrow(ot)
  nf > 0 && nf > tol * max(na, 1)
}

# ---- shared modules ---------------------------------------------------------
# extract_qp() / get_qp() live in R/qp_extract.R and the page-presentation
# helpers (gtsave_titled, styled_index_page) in R/page_style.R -- both shared
# with the conference reports. Sourced relative to this file's location.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f)
    else f
  }
  sys.source(find("qp_extract.R"), envir = globalenv())
  sys.source(find("page_style.R"), envir = globalenv())
  sys.source(find("interactive_theme.R"), envir = globalenv())
})

# ---- render -----------------------------------------------------------------

scotus_dash <- function(range = today() - 1, year = "26",
                        out_dir = path.expand("~/public_html/dashboards"),
                        model = NULL, signals_map = NULL, counsel_index = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ot <- get_scotus_update(year = year)
  # Never overwrite a good dashboard with a throttle-degraded fetch.
  if (fetch_is_degraded(ot)) {
    warning("Degraded fetch (", attr(ot, "n_failed"), "/", attr(ot, "n_attempted"),
            " dockets unresolved -- server throttling); leaving existing dashboard ",
            "for ", range, " untouched.")
    return(invisible(NULL))
  }

  hits <- if (nrow(ot) == 0) ot else filter(ot, date == range)
  # Baseline (structural, pre-conference) P(grant) per paid docket. Defensive:
  # no model, non-paid, or a scoring error -> NA (rendered as an em dash).
  grant_map <- setNames(rep(NA_real_, nrow(hits)), hits$dkt)
  if (!is.null(model) && nrow(hits) > 0 && exists("score_case")) {
    ph <- hits |> filter(type == "paid")
    for (i in seq_len(nrow(ph))) {
      grant_map[ph$dkt[i]] <- tryCatch(
        score_case(model, ph$caption[i], ph$lower[i], ph$parties[[i]],
                   ph$date[i], ph$lower_date[i], ph$related[i],
                   signals = signals_map[[ph$dkt[i]]],
                   counsel_index = counsel_index)$prob,
        error = function(e) NA_real_)
    }
  }
  if (nrow(hits) == 0) {
    empty <- paste0(
      "<!DOCTYPE html><html lang='en'><head><script async src='/analytics.js'></script><meta charset='utf-8'>",
      "<meta name='viewport' content='width=device-width, initial-scale=1'>",
      "<link rel='icon' href='/favicon.svg' type='image/svg+xml'><link rel='alternate icon' href='/favicon.ico' sizes='any'>",
      "<title>The Daily Docket &mdash; ", format(range, "%B %d, %Y"), "</title>",
      "<link rel='preconnect' href='https://fonts.googleapis.com'>",
      "<link rel='stylesheet' href='", SCR_FONTS, "'>",
      "<style>", SCR_CSS, "</style></head><body><main class='wrap'>",
      "<p class='kicker'>Supreme Court of the United States</p>",
      "<h1>The Daily Docket</h1>",
      "<p class='dek'>No petitions or applications were docketed on ",
      format(range, "%B %d, %Y"), ".</p><hr class='brule'>",
      "<p class='back'><a href='index.html'>&larr; All daily dashboards</a></p>",
      "</main></body></html>")
    writeLines(enc2utf8(empty),
               file.path(out_dir, str_c("dash_", range, ".html")), useBytes = TRUE)
    return(invisible(NULL))
  }

  # Questions Presented (petition text layer first, OCR fallback). Wrap real QP
  # text in a collapsible <details> cell; show a bare em dash where unavailable
  # (so a missing QP isn't an empty expander).
  # Cache-backed so the QP is (a) extracted once, not re-fetched every run, and
  # (b) PERSISTED to dashboards/qp_cache.json, which render_dockets_for() reads so
  # a recent paid petition's docket page shows the same QP as the dashboard.
  qp_raw <- resolve_qps(hits$dkt, hits$petition_url,
                        cache_path = file.path(out_dir, "qp_cache.json"),
                        max_new = as.integer(Sys.getenv("QP_MAX_NEW", unset = "600")))
  qp_html <- qp_details(qp_raw)
  qps <- ifelse(is.na(qp_raw) | qp_raw == "" | qp_raw == "-", "—", qp_html)

  # One editorial row per docket. Grant stays NUMERIC so the column sorts by
  # value; Type/Grant get color scales, everything else is markdown/HTML.
  tbl <- tibble(
    Type = factor(hits$type, levels = c("paid", "ifp", "app"),
                  labels = c("Paid", "IFP", "Application")),
    # The docket number rides under the caption instead of holding open a column
    # of its own, as it does on the conference reports. It stays searchable --
    # the search box matches rendered cell text -- and it points at the same case
    # page the caption links to, so a column whose entire content was a duplicate
    # link target is gone.
    Case = sprintf(
      "<a href='../cases/%s.html' target='_blank'>%s</a><span class='cdk'>No. %s</span>",
      hits$dkt,
      strip_caption_roles(hits$caption),
      hits$dkt),
    Grant = unname(grant_map[hits$dkt]),
    Court = str_replace(coalesce(hits$lower, "—"),
              "^United States Court of Appeals for the (.+?Circuit)$", "\\1") |>
              str_trunc(30),
    Counsel = map_chr(hits$parties, petitioner_counsel_html),
    Documents = map_chr(hits$events, function(e)
                  case_documents(e, c("Petition", "Application", "Appendix"))),
    QP = qps
  ) |>
    # Group by type (Paid -> IFP -> Application), then by grant forecast within
    # each group so a paid petition with no forecast stays with the paid block
    # rather than sinking below the IFP/application rows.
    arrange(Type, desc(Grant))

  # Drop the Grant column entirely on days with no paid petitions (all NA).
  has_grant <- any(!is.na(tbl$Grant))
  if (!has_grant) tbl <- select(tbl, -Grant)
  # Clamped companion for the heat map; hidden before the table renders. See the
  # data_color() call below for why the real column cannot be shaded directly.
  # Clamped to the TOP of the shading domain, from palette.R, so the ceiling
  # cannot drift apart from the scale it is clamping to.
  if (has_grant) tbl$.grant_shade <- pmin(tbl$Grant, GRANT_DOMAIN[2])

  # Data cells that read better left-aligned (headers stay centered via CSS).
  left_cols <- match(intersect(c("Case", "Court", "Counsel", "Documents", "QP"),
                               names(tbl)), names(tbl))

  t <- tbl |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "Counsel", "Documents", "QP"))) |>
    data_color(columns = Type, method = "factor",
      palette = TYPE_CHIPS) |>
    cols_align("center", columns = everything()) |>
    cols_label(QP = "Questions Presented") |>
    # Type holds three short words and was sized by its header, not its data.
    # Case gains the 10px the docket line needs. Both match the conference
    # reports, which is the point -- the two tables show the same kind of row.
    cols_width(Case ~ px(230), Type ~ px(76), QP ~ px(190))
  if (has_grant) {
    t <- t |>
      fmt_percent(columns = Grant, decimals = 0) |>
      # NA grant (non-paid rows) displays as an em dash, not a literal "NA"; the
      # raw value stays numeric so the column still sorts by value.
      sub_missing(columns = Grant, missing_text = "—") |>
      # Shade from a CLAMPED copy, not from Grant itself. gt treats a value
      # outside `domain` as NA and hands it na_color, so a forecast above 60%
      # came out the same pale tone as a row with no forecast at all -- the
      # strongest petitions on the page were the ones that looked like blanks,
      # which is exactly backwards. Anything at or above the top of the domain
      # now takes the darkest stop. Grant itself is untouched, so the printed
      # percentage and the column's numeric sort are unaffected.
      data_color(columns = .grant_shade, target_columns = Grant,
                 palette = GRANT_RAMP,
                 domain = GRANT_DOMAIN, na_color = GRANT_NA) |>
      cols_hide(columns = .grant_shade) |>
      cols_label(Grant = "Grant forecast")
  }

  footer <- if (has_grant) paste0(
    "<em>Grant forecast</em> is a calibrated, pre-conference estimate of plenary ",
    "certiorari for paid petitions (base rate ~4%), from case structure alone ",
    "(who is involved, the court below, counsel). It sharpens once a case is ",
    "distributed for conference. An estimate, not a prediction about any case."
  ) else ""

  dek <- paste0(
    "Petitions and applications docketed <strong>", format(range, "%B %d, %Y"),
    "</strong> &mdash; sortable and filterable. ",
    if (has_grant) "Sort by <em>Grant forecast</em>, or expand" else "Expand",
    " a row&rsquo;s <em>Questions Presented</em>.")

  scr_interactive(t, n_rows = nrow(tbl)) |>
    scr_write_page(
      file.path(out_dir, str_c("dash_", range, ".html")),
      kicker = "Supreme Court of the United States",
      title = "The Daily Docket",
      dek = dek,
      n_rows = nrow(tbl), left_cols = left_cols, footer = footer,
      active = "/dashboards/",
      crumb = list(label = format(range, "%B %d, %Y"),
                   section = list(href = "/dashboards/", label = "Docket")),
      back = list(href = "index.html", label = "&larr; All daily dashboards"))
  invisible(file.path(out_dir, str_c("dash_", range, ".html")))
}

# Regenerate index.html for the daily-dashboard directory, listing every
# dash_YYYY-MM-DD.html newest-first.
dashboard_index <- function(out_dir = path.expand("~/public_html/dashboards")) {
  files <- list.files(out_dir, pattern = "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$")
  if (length(files) == 0) return(invisible(NULL))
  dates <- as.Date(str_extract(files, "\\d{4}-\\d{2}-\\d{2}"))
  ord <- order(dates, decreasing = TRUE)
  files <- files[ord]
  dates <- dates[ord]

  items <- purrr::map2(files, dates, function(f, d) {
    list(href = f, label = format(d, "%B %d, %Y"))
  })
  styled_index_page(
    file.path(out_dir, "index.html"),
    title = "Daily Petitions & Applications — SCOTUS",
    kicker = "Supreme Court of the United States",
    heading = "The Daily Docket",
    dek = "Every petition and application, the day it arrives.",
    items = items,
    # No back-link: the masthead's wordmark goes home and the active section is
    # marked, so a bespoke one here would be a third way of saying the same
    # thing. This link previously read "← All dashboards" and pointed at "/" --
    # copy-pasted verbatim into the conference and argument indexes, where the
    # label was simply wrong. That is what having no shared nav cost.
    active = "/dashboards/"
  )
  patch_prev_next(out_dir, "^dash_\\d{4}-\\d{2}-\\d{2}\\.html$", "day",
                  key   = function(f) as.Date(str_extract(f, "\\d{4}-\\d{2}-\\d{2}")),
                  label = function(f) format(as.Date(str_extract(f, "\\d{4}-\\d{2}-\\d{2}")),
                                             "%B %d, %Y"))
  invisible(file.path(out_dir, "index.html"))
}


scotus_dash(range = today(), year = "26")
