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
    empty_html <- sprintf(
      '<!DOCTYPE html><html><head><meta charset="utf-8"><title>%s</title></head><body style="font-family:sans-serif;padding:2em"><h2>No petitions or applications docketed on %s.</h2></body></html>',
      format(range, "%B %d, %Y"), format(range, "%B %d, %Y")
    )
    writeLines(empty_html, file.path(out_dir, str_c("dash_", range, ".html")))
    return(invisible(NULL))
  }

  table1 <- ot |>
    filter(date == range) |>
    mutate(caption = caption |>
      str_replace_all("\\s+", " ") |>
      str_remove(", Petitioners?") |>
      str_remove(", Applicants?") |>
      str_remove_all(", et al.") |>
      str_replace("\\s*v\\.\\s+", "\n\nv.\n\n") |>
      str_trim()) |>
    # Surface capital-case flag and related cases in the caption cell.
    mutate(caption = if_else(capital, str_c(caption, "\n\n`CAPITAL CASE`"), caption)) |>
    mutate(caption = if_else(related != "", str_c(caption, "\n\n*Related:* ", related), caption)) |>
    mutate(lower = str_replace(
      lower, "^United States Court of Appeals for the (.+?Circuit)", "\\1"
    )) |>
    unnest_longer(col = events) |>
    unnest(col = events, names_sep = "_") |>
    rowwise() |>
    mutate(doc_links = list(purrr::map2_chr(
      c_across(starts_with("events_docs_")),
      c_across(starts_with("events_links_")),
      ~ if (!is.na(.x) && !is.na(.y)) sprintf("[[%s](%s)]", .x, .y) else ""
    ))) |>
    unnest_wider(col = doc_links, names_sep = "_", simplify = TRUE) |>
    unnest_longer(col = parties) |>
    unnest(col = parties, names_sep = "_") |>
    filter(str_detect(parties_type, "Petitioner|Applicant|Appellant")) |>
    # After unite, each row's doc_links is a single string for that one
    # proceeding; the real per-docket aggregation is the group_by() collapse
    # below. (Do NOT str_c(collapse=) here: unite drops rowwise grouping, so a
    # collapse would concatenate every row into every row.)
    unite(col = doc_links, starts_with("doc_links"), sep = " ") |>
    mutate(events = doc_links) |>
    mutate(events = str_remove_all(
      events, "\\[\\[Motion for Leave to Proceed in Forma Pauperis\\]\\(\\S+\\)\\] "
    )) |>
    mutate(events = str_remove_all(events, "\\[\\[Certificate of Word Count\\]\\(\\S+\\)\\]")) |>
    mutate(events = str_remove_all(events, "\\[\\[Proof of Service\\]\\(\\S+\\)\\]")) |>
    mutate(events = str_remove_all(events, "\\[\\[Other\\]\\(.+\\)\\]")) |>
    ungroup() |>
    group_by(dkt) |>
    mutate(events = str_squish(str_c(events, collapse = " "))) |>
    slice(1) |>
    ungroup() |>
    # Group rows by type (paid -> ifp -> app), numeric docket order within each.
    arrange(
      factor(type, levels = c("paid", "ifp", "app")),
      as.integer(str_extract(dkt, "\\d+$"))
    ) |>
    # Counsel of record's name with their firm beneath it (no address/email).
    mutate(parties_attys = if_else(
      !is.na(parties_firm) & parties_firm != "",
      str_c(parties_attys, "  \n", parties_firm),
      parties_attys
    )) |>
    # Grant forecast cell (dkt is still raw here), from the baseline model.
    mutate(grant = unname(grant_map[dkt]),
           grant = if_else(is.na(grant), "—", str_c("**", round(100 * grant), "%**"))) |>
    mutate(dkt = str_c(
      "[", dkt,
      "](https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/",
      dkt, ".html)"
    )) |>
    mutate(lower_date = format(lower_date, format = "%B %d, %Y")) |>
    # paste0 (not str_c) so a missing field stringifies to "NA" and is rewritten
    # to "—" below. str_c would propagate NA and blank the whole cell -- e.g.
    # applications have no LowerCourtDecision, so lower_date is NA.
    mutate(lower = paste0(
      lower, ", No. ", lower_dkt, "\n\n", "Judgment: ", lower_date, "\n\n"
    )) |>
    mutate(lower = str_replace_all(lower, " NA", " —")) |>
    rename(pro_se = parties_pro_se) |>
    select(type, caption, dkt, grant, lower, parties_attys,
           events, pro_se, petition_url)

  qps <- purrr::map_chr(table1$petition_url, get_qp)
  has_grant <- any(table1$grant != "—")
  table1 <- table1 |> mutate(qps = qp_details(qps)) |> select(-petition_url)
  if (!has_grant) table1 <- table1 |> select(-grant)

  labels <- list(caption = "Caption", dkt = "Docket No", grant = "Grant forecast",
                 lower = "Court Below", parties_attys = "Petitioner's Counsel",
                 events = "Recent Filings", qps = "QP")
  labels <- labels[names(labels) %in% names(table1)]

  tbl <- table1 |>
    gt() |>
    fmt_markdown(columns = any_of(c("caption", "lower", "dkt", "grant",
                                    "parties_attys", "events", "qps"))) |>
    tab_header(title = paste0(
      "Petitions and applications docketed on ", format(range, "%B %d, %Y")
    )) |>
    gt_theme_nytimes() |>
    cols_label(.list = labels) |>
    data_color(
      columns = c(pro_se),
      target_columns = "parties_attys",
      method = "factor",
      palette = c("white", "gray"),
      domain = c(TRUE, FALSE)
    ) |>
    cols_width(
      dkt ~ px(80),
      events ~ px(150)
    ) |>
    cols_align(align = "left", columns = c(caption)) |>
    cols_align(align = "center", columns = c(type:events)) |>
    # Fixed per-type fills: paid = green, ifp = orange, app = blue.
    tab_style(style = cell_fill(color = "#B2DF8A"),
              locations = cells_body(columns = type, rows = type == "paid")) |>
    tab_style(style = cell_fill(color = "#FDBF6F"),
              locations = cells_body(columns = type, rows = type == "ifp")) |>
    tab_style(style = cell_fill(color = "#A6CEE3"),
              locations = cells_body(columns = type, rows = type == "app")) |>
    cols_hide(columns = c(pro_se))
  if (has_grant) {
    tbl <- tbl |> tab_source_note(gt::md(paste0(
      "*Grant forecast* is a calibrated, pre-conference estimate of plenary ",
      "certiorari for paid petitions (base rate ~4%), from case structure alone ",
      "(who is involved, the court below, counsel). It sharpens once a case is ",
      "distributed for conference. An estimate, not a prediction about any case.")))
  }
  gtsave_titled(tbl, str_c("dash_", range, ".html"), path = out_dir,
                title = paste0("Petitions & applications — ",
                               format(range, "%B %d, %Y")))
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
