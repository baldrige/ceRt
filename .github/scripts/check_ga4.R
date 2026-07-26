#!/usr/bin/env Rscript
# Verify a GA4 service-account credential locally, before wiring it into CI.
#
#   Rscript .github/scripts/check_ga4.R <property-id> <key.json> [site-dir]
#
# In the daily build a bad credential is deliberately non-fatal: the Most-Read
# panel is dropped and the docket publish carries on (see R/site_analytics.R).
# That is the right call for a decorative block, but it makes CI a poor place to
# debug a credential, because "it didn't work" and "it isn't configured yet"
# look identical from the outside. This script is the opposite: it does one
# thing, says exactly which step failed, and names the fix.
#
# The key file is a live credential. Keep it out of the repo -- run it straight
# from ~/Downloads and delete it once the secret is set.

suppressPackageStartupMessages({library(httr2); library(jsonlite)})
source("R/site_analytics.R")

a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 2) {
  cat("usage: Rscript .github/scripts/check_ga4.R <property-id> <key.json> [site-dir]\n")
  quit(status = 2)
}
prop <- a[1]; keyfile <- a[2]; site <- if (length(a) > 2) a[3] else NA_character_

say  <- function(...) cat(sprintf(...), "\n", sep = "")
# Pastes its arguments verbatim -- callers interpolate with sprintf() themselves,
# so a multi-line explanation is just several strings and needs no format string.
fail <- function(...) { cat("\nFAILED: ", paste0(...), "\n", sep = ""); quit(status = 1) }

if (grepl("^G-", prop))
  fail(sprintf("'%s' is the Measurement ID, not the Property ID.\n", prop),
       "  The Property ID is numeric. GA4 -> Admin -> Property details,\n",
       "  top right, labelled PROPERTY ID.")
if (!grepl("^[0-9]+$", prop)) fail(sprintf("Property ID '%s' is not numeric.", prop))
if (!file.exists(keyfile))    fail(sprintf("No key file at '%s'.", keyfile))

key <- tryCatch(jsonlite::fromJSON(keyfile), error = function(e)
  fail(sprintf("'%s' is not valid JSON (%s).", keyfile, conditionMessage(e))))
for (f in c("client_email", "private_key", "type"))
  if (is.null(key[[f]]))
    fail(sprintf("Key file has no '%s' field -- is it a service-account JSON key?", f))
if (!identical(key$type, "service_account"))
  fail(sprintf("Key type is '%s', expected 'service_account'.", key$type))

say("Service account : %s", key$client_email)
say("Property        : %s", prop)
say("")

# --- step 1: mint a token -----------------------------------------------------
say("1. Exchanging the signed assertion for an access token...")
tok <- tryCatch(ga4_access_token(key), error = function(e) {
  m <- conditionMessage(e)
  if (grepl("invalid_grant|400", m))
    fail(sprintf("Google rejected the assertion (%s).\n", trimws(m)),
         "  The signature was well-formed but Google would not honour it: the\n",
         "  service account or key no longer exists, the key was disabled, or\n",
         "  this machine's clock is skewed. Check Google Cloud -> IAM & Admin\n",
         "  -> Service Accounts -> Keys.")
  fail(sprintf("Could not get a token: %s", trimws(m)))
})
say("   ok -- token acquired.")

# --- step 2: run the report ---------------------------------------------------
say("2. Querying pagePath views for /cases/ over the last 30 days...")
df <- tryCatch(ga4_case_report(prop, tok, days = 30L), error = function(e) {
  m <- conditionMessage(e)
  if (grepl("403", m))
    fail("403 from the Data API.\n",
         "  Either the API is not enabled, or the service account has no access:\n",
         "  (a) console.cloud.google.com -> APIs & Services -> Library ->\n",
         "      'Google Analytics Data API' -> Enable\n",
         "  (b) analytics.google.com -> Admin -> Property access management ->\n",
         sprintf("      add %s as Viewer", key$client_email))
  if (grepl("404", m))
    fail(sprintf("404 -- property %s does not exist, or this account cannot see it.", prop))
  if (grepl("400", m))
    fail(sprintf("400 from the Data API (%s).\n", trimws(m)),
         "  Check the Property ID is the numeric one for the right property.")
  fail(sprintf("Query failed: %s", trimws(m)))
})

if (is.null(df) || !nrow(df)) {
  say("   ok -- authorised, but GA returned no /cases/ rows for the window.")
  say("")
  say("Credential works. No data yet: either the property saw no case-page")
  say("traffic in the last 30 days, or this is the wrong property. The panel")
  say("will stay hidden until there is traffic.")
  quit(status = 0)
}
say("   ok -- %d rows.", nrow(df))

# --- step 3: what the landing page would actually show -------------------------
df$docket <- case_docket_from_path(df$path)
other <- sum(df$views[is.na(df$docket)])
df <- df[!is.na(df$docket), , drop = FALSE]
agg <- stats::aggregate(cbind(views, users) ~ docket, data = df, FUN = sum)
agg <- agg[order(-agg$views, agg$docket), , drop = FALSE]
say("")
say("Top case pages, last 30 days (views / unique users):")
for (i in seq_len(min(10L, nrow(agg)))) {
  cap <- if (!is.na(site)) case_caption(site, agg$docket[i]) else NA_character_
  say("  %2d. %-10s %6d / %-6d %s", i, agg$docket[i], agg$views[i], agg$users[i],
      if (is.na(cap)) "" else substr(cap, 1, 60))
}
if (other > 0)
  say("\nNote: %d views sat on non-case paths (incl. GA's '(other)' bucket).", other)
if (is.na(site))
  say("\nPass a gh-pages checkout as the 3rd argument to resolve captions and to\nsee which of these would be dropped as no longer on the site.")
say("\nCredential works. Set the secrets with:")
say("  gh secret set GA4_PROPERTY_ID --body '%s'", prop)
say("  gh secret set GA4_SA_KEY < %s", keyfile)
