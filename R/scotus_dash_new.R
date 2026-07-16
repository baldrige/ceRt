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

# Treat 403/429/503 as transient: the Akamai CDN answers 403 when too many
# requests arrive at once, so retry those (a genuine missing docket is 404).
is_transient_status <- function(resp) {
  inherits(resp, "httr2_response") && resp_status(resp) %in% c(403, 429, 503)
}

# Fetch, classify, and build one docket's case record. Returns list(case, failed):
# `case` is the tibble (NULL for a 404 or a parse failure); `failed` is TRUE only
# for a non-404 failure (throttling that survived retries, or a transport error),
# so callers can tell "docket absent" apart from "couldn't fetch it". Retries
# transient throttling (403/429/503) with backoff.
fetch_case_result <- function(dkt) {
  resp <- tryCatch(
    request(json_url(dkt)) |>
      req_user_agent(UA) |>
      req_retry(max_tries = 5, is_transient = is_transient_status) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(list(case = NULL, failed = TRUE))
  st <- resp_status(resp)
  if (st == 404) return(list(case = NULL, failed = FALSE)) # legitimately absent
  if (st != 200) return(list(case = NULL, failed = TRUE)) # throttled / 5xx
  j <- tryCatch(fromJSON(resp_body_string(resp), simplifyVector = TRUE),
                error = function(e) NULL)
  list(case = tryCatch(build_case(j, dkt), error = function(e) NULL), failed = FALSE)
}

# Existence check via a cheap HEAD request (200 vs 404). Retries transient
# throttling: a 403/429/503 must NOT be misread as "docket absent", or the
# binary search would silently truncate the term.
docket_exists <- function(year, sep, n) {
  resp <- tryCatch(
    request(json_url(paste0(year, sep, n))) |>
      req_user_agent(UA) |>
      req_method("HEAD") |>
      req_retry(max_tries = 5, is_transient = is_transient_status) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform(),
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

# Direct URL of the petition PDF (for the question-presented extraction).
find_petition_url <- function(po) {
  if (is.null(po) || !is.data.frame(po) || !("Links" %in% names(po))) return(NA_character_)
  for (i in seq_len(nrow(po))) {
    lk <- po$Links[[i]]
    if (is.data.frame(lk) && nrow(lk) > 0) {
      hit <- which(!is.na(lk$Description) & str_detect(lk$Description, "^Petition"))
      if (length(hit) > 0) return(lk$DocumentUrl[hit[1]])
    }
  }
  NA_character_
}

# Build a case-record tibble from an already-parsed docket JSON (NULL if the
# JSON isn't a real docket). Kept separate from the fetch so the two concerns
# (network vs. parsing) stay independent and testable.
build_case <- function(j, dkt) {
  if (is.null(j) || is.null(j$CaseNumber)) return(NULL)

  caption <- str_squish(str_c(
    j$PetitionerTitle %|||% "", " v. ", j$RespondentTitle %|||% ""
  ))
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
  results <- dkts |>
    map(\(d) tryCatch(fetch_case_result(d),
                      error = function(e) list(case = NULL, failed = TRUE)),
        .progress = TRUE)
  cases <- purrr::compact(purrr::map(results, "case"))
  n_failed <- sum(purrr::map_lgl(results, "failed"))
  if (n_failed > 0) {
    warning(n_failed, " docket(s) unresolved after retries (server likely throttling).")
  }
  if (length(cases) == 0) return(tibble())
  result <- bind_rows(cases)
  attr(result, "n_attempted") <- length(dkts)
  attr(result, "n_failed") <- n_failed
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
# extract_qp_page2() / get_qp() live in R/qp_extract.R and the page-presentation
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
                        model = NULL, signals_map = NULL) {
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
                   signals = signals_map[[ph$dkt[i]]])$prob,
        error = function(e) NA_real_)
    }
  }
  if (nrow(hits) == 0) {
    empty <- paste0(
      "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
      "<meta name='viewport' content='width=device-width, initial-scale=1'>",
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
  qp_raw <- purrr::map_chr(hits$petition_url, get_qp)
  qp_html <- qp_details(qp_raw)
  qps <- ifelse(is.na(qp_raw) | qp_raw == "" | qp_raw == "-", "—", qp_html)

  # One editorial row per docket. Grant stays NUMERIC so the column sorts by
  # value; Type/Grant get color scales, everything else is markdown/HTML.
  tbl <- tibble(
    Type = factor(hits$type, levels = c("paid", "ifp", "app"),
                  labels = c("Paid", "IFP", "Application")),
    Case = sprintf(
      "<a href='https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/%s.html' target='_blank'>%s</a>",
      hits$dkt,
      str_squish(str_remove_all(hits$caption, ", Petitioners?|, Applicants?|, et al\\."))),
    Docket = hits$dkt,
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

  # Data cells that read better left-aligned (headers stay centered via CSS).
  left_cols <- match(intersect(c("Case", "Court", "Counsel", "Documents", "QP"),
                               names(tbl)), names(tbl))

  t <- tbl |>
    gt() |>
    fmt_markdown(columns = any_of(c("Case", "Counsel", "Documents", "QP"))) |>
    data_color(columns = Type, method = "factor",
      palette = c("Paid" = "#e4e7d8", "IFP" = "#efe1cd", "Application" = "#dfe4ea")) |>
    cols_align("center", columns = everything()) |>
    cols_label(QP = "Questions Presented") |>
    cols_width(Case ~ px(220), QP ~ px(190))
  if (has_grant) {
    t <- t |>
      fmt_percent(columns = Grant, decimals = 0) |>
      # NA grant (non-paid rows) displays as an em dash, not a literal "NA"; the
      # raw value stays numeric so the column still sorts by value.
      sub_missing(columns = Grant, missing_text = "—") |>
      data_color(columns = Grant, palette = c("#f3ecdd", "#e8c9a0", "#c8794f", "#8a2b2b"),
                 domain = c(0, 0.6), na_color = "#f7f1e4") |>
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
    back = list(href = "../", label = "← All dashboards")
  )
  invisible(file.path(out_dir, "index.html"))
}


scotus_dash(range = today(), year = "26")
