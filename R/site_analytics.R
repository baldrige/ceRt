# site_analytics.R ------------------------------------------------------------
# "Most-read cases" for the landing page, sourced from Google Analytics.
#
# The site is static -- no server, no request logs -- so the only record of what
# anyone actually reads is the GA4 property every page already reports to (see
# analytics.js, Measurement ID G-R7NQL34H3X). Note that the Measurement ID is
# public and effectively WRITE-only: it lets a page send a hit, not read the
# data back. Reading needs two secrets, neither of which is the Measurement ID:
#
#   GA4_PROPERTY_ID   the numeric property id (Admin -> Property details),
#                     e.g. "493812756" -- NOT the "G-..." measurement id
#   GA4_SA_KEY        a Google Cloud service-account JSON key, verbatim, whose
#                     client_email has been granted Viewer on that property
#
# If either is absent, or the API call fails, top_viewed_cases() returns zero
# rows and the landing page omits the block. That is deliberate. This is one
# decorative panel on a pipeline whose actual job is publishing dockets every
# morning; a lapsed credential must not take the daily down with it. The failure
# is loud in the workflow log rather than silent -- but it is not fatal.
#
# Contrast the conference "Granted here" column, where the opposite call is
# correct: there an empty result means published numbers are wrong, so it fails
# the build. The difference is whether the degraded page states something false.
# An absent panel says nothing; an empty forecast column says "no grants".

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

GA4_TOKEN_URL <- "https://oauth2.googleapis.com/token"
GA4_DATA_URL  <- "https://analyticsdata.googleapis.com/v1beta"
GA4_SCOPE     <- "https://www.googleapis.com/auth/analytics.readonly"

# Mint a short-lived access token from a service-account key using the JWT-bearer
# flow: sign a claim set with the account's private key, trade it for a token.
# No user interaction, which is what makes this work unattended in CI.
ga4_access_token <- function(key) {
  # httr2's jwt_* helpers are thin wrappers over jose, and openssl reads the PEM.
  # Name them here: a bare "could not find function" out of a wrapper is a far
  # worse thing to meet in a CI log than a sentence saying which install is short.
  for (p in c("jose", "openssl"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("GA4: package '", p, "' is required to sign the service-account ",
           "assertion.", call. = FALSE)
  stopifnot(is.list(key), !is.null(key$client_email), !is.null(key$private_key))
  now <- as.integer(Sys.time())
  claim <- httr2::jwt_claim(
    iss   = key$client_email,
    aud   = GA4_TOKEN_URL,
    iat   = now,
    exp   = now + 3600L,
    scope = GA4_SCOPE
  )
  assertion <- httr2::jwt_encode_sig(claim, openssl::read_key(key$private_key))
  resp <- request(GA4_TOKEN_URL) |>
    req_body_form(
      grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion  = assertion
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()
  tok <- resp_body_json(resp)$access_token
  if (is.null(tok) || !nzchar(tok)) stop("GA4: token endpoint returned no access_token")
  tok
}

# One runReport call: page views per /cases/ path over the trailing `days` days.
# Ends at "yesterday" so the window is always whole days -- ending at "today"
# would mix a partial day in and make the ranking wobble by hour of build.
ga4_case_report <- function(property_id, token, days = 30L, limit = 200L) {
  body <- list(
    dateRanges = list(list(startDate = paste0(days, "daysAgo"), endDate = "yesterday")),
    dimensions = list(list(name = "pagePath")),
    metrics    = list(list(name = "screenPageViews"), list(name = "totalUsers")),
    dimensionFilter = list(filter = list(
      fieldName    = "pagePath",
      stringFilter = list(matchType = "BEGINS_WITH", value = "/cases/")
    )),
    orderBys = list(list(desc = TRUE, metric = list(metricName = "screenPageViews"))),
    limit    = limit
  )
  resp <- request(sprintf("%s/properties/%s:runReport", GA4_DATA_URL, property_id)) |>
    req_auth_bearer_token(token) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_retry(max_tries = 3) |>
    req_perform()
  ga4_rows_to_df(resp_body_json(resp)$rows)
}

# Same query with no path filter: what GA sees across the whole site. Only used
# to explain a zero /cases/ result, so it asks for views alone.
ga4_all_paths <- function(property_id, token, days = 30L, limit = 10L) {
  body <- list(
    dateRanges = list(list(startDate = paste0(days, "daysAgo"), endDate = "yesterday")),
    dimensions = list(list(name = "pagePath")),
    metrics    = list(list(name = "screenPageViews")),
    orderBys   = list(list(desc = TRUE, metric = list(metricName = "screenPageViews"))),
    limit      = limit
  )
  resp <- request(sprintf("%s/properties/%s:runReport", GA4_DATA_URL, property_id)) |>
    req_auth_bearer_token(token) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_retry(max_tries = 2) |>
    req_perform()
  ga4_rows_to_df(resp_body_json(resp)$rows, users = FALSE)
}

# An empty report is a zero-row frame, never NULL: callers use NULL to mean "the
# request failed", and conflating the two is what let a silent empty result pass
# for a working one.
ga4_rows_to_df <- function(rows, users = TRUE) {
  if (is.null(rows) || !length(rows))
    return(data.frame(path = character(), views = integer(),
                      users = integer(), stringsAsFactors = FALSE))
  out <- data.frame(
    path  = vapply(rows, function(r) r$dimensionValues[[1]]$value, character(1)),
    views = as.integer(vapply(rows, function(r) r$metricValues[[1]]$value, character(1))),
    stringsAsFactors = FALSE)
  out$users <- if (users)
    as.integer(vapply(rows, function(r) r$metricValues[[2]]$value, character(1)))
  else NA_integer_
  out
}

# "/cases/24-1122.html?utm_source=x#top" -> "24-1122". GA reports the path as
# requested, so the same page arrives under several spellings whenever anything
# appends a query string; collapsing them before ranking keeps a shared link
# from splitting one popular case across three rows.
case_docket_from_path <- function(path) {
  p <- sub("[?#].*$", "", path)
  ifelse(grepl("^/cases/[^/]+\\.html$", p), sub("^/cases/(.+)\\.html$", "\\1", p), NA_character_)
}

# The caption lives in each page's own <title>: "Robin Root v. Jeremy Howard,
# Warden &mdash; No. 24-1122". Read it off disk rather than keeping a parallel
# index -- the gh-pages checkout already holds every case page, and a title read
# cannot drift out of sync with what the page says.
case_caption <- function(site_dir, docket) {
  f <- file.path(site_dir, "cases", paste0(docket, ".html"))
  if (!file.exists(f)) return(NA_character_)
  head_txt <- paste(readLines(f, n = 40L, warn = FALSE), collapse = " ")
  m <- regmatches(head_txt, regexpr("<title>[^<]*</title>", head_txt))
  if (!length(m)) return(NA_character_)
  t <- sub("</title>$", "", sub("^<title>", "", m))
  t <- sub("\\s*&mdash;\\s*No\\..*$", "", t)          # drop the trailing docket no.
  t <- gsub("&amp;", "&", t, fixed = TRUE)
  t <- gsub("&#39;", "'", t, fixed = TRUE)
  t <- gsub("&quot;", '"', t, fixed = TRUE)
  t <- gsub("&lt;", "<", t, fixed = TRUE); t <- gsub("&gt;", ">", t, fixed = TRUE)
  trimws(t)
}

# The top `n` case pages by views over the trailing `days` days, as a data frame
# of docket / caption / views / href. Zero rows means "render nothing": no
# credentials, no data yet, or an API error -- all three are non-fatal.
top_viewed_cases <- function(site_dir, n = 5L, days = 30L) {
  none <- data.frame(docket = character(), caption = character(),
                     views = integer(), users = integer(), href = character(),
                     stringsAsFactors = FALSE)

  prop <- Sys.getenv("GA4_PROPERTY_ID", "")
  raw  <- Sys.getenv("GA4_SA_KEY", "")
  if (!nzchar(prop) || !nzchar(raw)) {
    message("top_viewed_cases(): GA4_PROPERTY_ID / GA4_SA_KEY not set -- ",
            "skipping the most-read panel.")
    return(none)
  }
  if (grepl("^G-", prop)) {
    warning("top_viewed_cases(): GA4_PROPERTY_ID looks like a Measurement ID (",
            prop, "). It must be the NUMERIC property id -- skipping panel.",
            call. = FALSE)
    return(none)
  }

  # Token and report are separate steps with separate failure meanings: a bad
  # token is a credential problem, a failed report is an access or property-id
  # problem. Keeping the token in scope also lets the zero-row branch below run
  # its diagnostic query without minting a second one.
  tok <- tryCatch({
    key <- jsonlite::fromJSON(raw, simplifyVector = TRUE)
    ga4_access_token(key)
  }, error = function(e) {
    warning("top_viewed_cases(): could not authenticate to GA4 (",
            conditionMessage(e), ") -- skipping the most-read panel.", call. = FALSE)
    NULL
  })
  if (is.null(tok)) return(none)

  df <- tryCatch(ga4_case_report(prop, tok, days = days), error = function(e) {
    warning("top_viewed_cases(): GA4 query failed (", conditionMessage(e),
            ") -- skipping the most-read panel.", call. = FALSE)
    NULL
  })
  # Authorised, but nothing came back. This returned silently in the first cut,
  # which is precisely the wrong behaviour: an empty result and a broken
  # credential then looked identical in the log, and the actual cause (case pages
  # were not loading analytics.js at all, so /cases/ had never reported a view)
  # took a log-archaeology session to find. Say what happened, and say what GA
  # *does* see, so the next zero is self-diagnosing.
  if (is.null(df)) return(none)             # request failed; already warned
  if (!nrow(df)) {
    message("top_viewed_cases(): GA4 returned no /cases/ rows for the last ",
            days, " days. Querying all paths to show what it does see...")
    seen <- tryCatch(ga4_all_paths(prop, tok, days = days, limit = 10L),
                     error = function(e) NULL)
    if (is.null(seen) || !nrow(seen)) {
      message("  ...no page views at ALL in the window. Either the property is ",
              "not the one behind analytics.js, or nothing is reporting to it.")
    } else {
      message("  ...top paths GA does see (none of them under /cases/):")
      for (i in seq_len(nrow(seen)))
        message(sprintf("    %6d  %s", seen$views[i], seen$path[i]))
      message("  If /cases/ pages are missing here, check they load ",
              "/analytics.js (added to the docket template in v14).")
    }
    return(none)
  }

  df$docket <- case_docket_from_path(df$path)
  df <- df[!is.na(df$docket), , drop = FALSE]
  if (!nrow(df)) {
    message("top_viewed_cases(): GA rows matched /cases/ but none parsed to a ",
            "docket -- all were directory or malformed paths.")
    return(none)
  }

  # Collapse the query-string variants, then re-rank on the combined totals.
  agg <- stats::aggregate(cbind(views, users) ~ docket, data = df, FUN = sum)
  agg <- agg[order(-agg$views, agg$docket), , drop = FALSE]

  # A page can be in GA and gone from the site (a docket renumbered, a stale
  # bookmark). Linking it would publish a 404, so drop before taking the top n.
  agg$caption <- vapply(agg$docket, function(d) case_caption(site_dir, d), character(1))
  agg <- agg[!is.na(agg$caption) & nzchar(agg$caption), , drop = FALSE]
  if (!nrow(agg)) return(none)

  out <- utils::head(agg, n)
  out$href <- paste0("cases/", out$docket, ".html")
  message(sprintf("top_viewed_cases(): %d case pages in GA over %dd; top %d: %s",
                  nrow(agg), days, nrow(out),
                  paste(sprintf("%s (%d views / %d users)", out$docket, out$views, out$users),
                        collapse = ", ")))
  rownames(out) <- NULL
  out[, c("docket", "caption", "views", "users", "href")]
}
