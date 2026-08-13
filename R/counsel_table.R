# counsel_table.R ---------------------------------------------------------------
# The Counsel Table: who files cert petitions, whose petitions get a second look,
# and whose get granted -- computed over the whole committed archive.
#
# WHAT AN "ADVOCATE" IS HERE, AND WHAT IT IS NOT.
#
# A row is a `counsel_key()` -- first and last name token, lowercased, generational
# suffixes stripped. That is a NAME, not a verified identity. It deliberately
# merges "Neal K. Katyal" and "Neal Kumar Katyal", and it therefore also merges two
# different lawyers who happen to share a first and last name. Every row publishes
# the name strings that keyed together so a reader can see the merge and judge it.
# There is no way to do better from docket text alone: 0 of 8,989 archive petitions
# carry a firm, so firm matching can disambiguate new filings and can never clean
# the back catalogue.
#
# It counts PETITIONER'S COUNSEL OF RECORD ONLY. An advocate who argues a case
# they did not petition in does not appear at all, and a respondent's counsel --
# which is most of what the elite Supreme Court bar actually does -- is invisible
# here. This is a cert-petition table, not a Supreme Court bar table.
#
# WHY THE GRANT TABLE IS SPLIT, AND WHERE THE SPLIT IS MADE.
#
# Pooled, the top of a grant-rate table is entirely government: measured over
# OT2017-OT2024, cases filed for the United States are granted at 48.6%, for a
# State at 14.5%, and for a private party at 4.6%. Publishing one ranking would
# put the Solicitor General's office on top and read as a finding about advocacy
# when it is a finding about who the client is.
#
# The split is made at the CASE, by who the petitioner is -- not at the advocate.
# An advocate-level cut would have to pick a threshold, and there is none to pick:
# `gov_share` has 245 of 286 qualifying advocates at exactly zero and the rest
# strung out in a gradient, thick with former state solicitors general now in
# private practice. Splitting the cases needs no threshold and lets an advocate
# appear on both boards, which is what actually happened to several of them.
#
# `petitioner_gov_side()` is ~100% precise (1 apparent false positive in 547
# hand-checked private-bar petitions, and that one is a real government filing)
# but only ~82% recall: the docket's party field carries bare officer names
# ("Merrick B. Garland"), abbreviations ("FCC", "Att'y Gen."), source typos
# ("Securites and Exchange Commission", "Homeland Secuirty") and offices with no
# fixed written form ("Office of the United States Trustee"). Recall is the
# dangerous direction -- every miss moves a government case onto the PRIVATE
# board -- so COUNSEL_GOV_MAX_PRIVATE guards that side and only that side.
#
# ADDING THE CAPTION TO THE CLASSIFIER IS A TRAP, MEASURED. It lifts recall from
# 82% to 90% and collapses precision from 99.8% to 81%, because a private
# petitioner SUING the government has the government in their caption: 50 of Raed
# Gonzalez's 51 immigration petitions read as federal filings. The party field
# names the petitioner; the caption names both sides.

suppressPackageStartupMessages({library(tidyverse); library(htmltools)})

local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f) else f
  }
  for (f in c("cert_funnel.R", "cert_model.R", "page_style.R"))
    if (!exists(switch(f, cert_funnel.R = "classify_petitions",
                          cert_model.R = "counsel_key", page_style.R = "page_head")))
      sys.source(find(f), envir = globalenv())
})

# An advocate must clear this to appear anywhere on the page. 73% of advocates in
# the archive filed exactly one petition; without a floor the rate tables are a
# small-sample generator and the volume tables have a 4,000-row tail of noise.
COUNSEL_MIN_CASES <- 5L

# A rate needs more evidence than a count, so the three boards that publish one
# take a higher floor. This is measured, not taste: at a floor of five, eleven of
# the twenty-five rows on the relist board were the same advocate-shaped row --
# 3 relisted of 5 filed, a 60% share and a 23.1% lower bound, eleven times. The
# ranking was real and told the reader nothing. At eight the largest tie in the
# top twenty-five is two rows, and ten or twelve buy nothing further.
#
# The grant boards get the same floor for the same reason one step removed: a
# Wilson bound shrinks a small sample, it does not rescue one. At five, 3 grants
# in 5 (a 23.1% bound) outranked 8 in 23 -- still the small-sample artefact the
# bound is there to prevent, just a milder one. It costs the government board
# four rows, including a 6-of-7 that would have led it.
COUNSEL_MIN_RATE_CASES <- 8L

# How many rows each leaderboard publishes. A leaderboard, not a database dump:
# the full qualifying set is ~280 advocates and the page says so.
COUNSEL_TOP_N <- 25L

# An advocate whose filings are at least this fraction government is kept OFF the
# private-bar board entirely, rather than appearing there on whatever petitions
# the party grammar failed to recognise.
#
# This is the 82% recall showing up where it does damage. Split purely by
# petition, the top row of the private board was Elizabeth Prelogar at 6 grants in
# 7 petitions (86%) -- all seven of which are government filings the grammar
# missed. The cut is safe rather than finely tuned: among the top of that board
# the government share runs 92%, 44%, then 3%, 2%, 0%, 0%, so anything from 5% to
# 40% draws the same line. A quarter is the round number inside that gap.
#
# It costs a few genuine private-practice rows -- former state solicitors general
# whose government filings are historical. That is the right direction to err: an
# advocate wrongly left off a board is invisible, an advocate wrongly on top of it
# is a published claim that is false.
COUNSEL_GOV_MAX_PRIVATE <- 0.25

# ---- who the petitioner is -----------------------------------------------------

COUNSEL_STATES <- c(state.name, "District of Columbia", "Puerto Rico", "Guam",
                    "Virgin Islands", "American Samoa", "Northern Mariana Islands")
.state_rx <- paste0("\\b(", paste(COUNSEL_STATES, collapse = "|"), ")\\b")

# Federal petitioner. Loose where the docket is inconsistent (`Secu\\w+` covers
# both "Security" and the archive's "Secuirty"; `Federal \\w+` covers the whole
# alphabet of commissions) and strict where looseness would misfire: "ex rel."
# is a qui tam relator suing in the government's name, and the relator's own
# counsel files it.
FED_PARTY_RX <- regex(paste0(
  "^United States( of America)?\\b(?!.*\\bex rel)",
  "|\\bUnited States\\b.*\\b(Department|Agency|Bureau|Office|Service|Trustee|",
    "Commission|Board|Attorney|Marshal|Postal|Patent)\\b",
  "|\\bPresident (of|for) the United States\\b",
  "|\\bUnited States Trustee\\b|\\bTrustee, Region\\b",
  "|^Department of \\w+",
  "|\\b(Securit\\w+ and Exchange|National Labor Relations Board|",
    "Environmental Protection|Federal \\w+|Internal Revenue|Social Security|",
    "Patent and Trademark|Equal Employment Opportunity|",
    "Consumer Financial Protection|Food and Drug|Centers for Medicare|",
    "Immigration and Customs|Citizenship and Immigration|Homeland Secu\\w+|",
    "Transportation Security|Office of Personnel Management|Postal Service|",
    "General Services Administration|Agency for International Development|",
    "Small Business Administration|Surface Transportation Board|",
    "Merit Systems Protection Board|National Transportation Safety Board)\\b",
  "|\\b(FCC|FEC|SEC|NLRB|EPA|FTC|FDIC|IRS|USPTO|ICE|USCIS|DHS|HHS|TSA|OPM|CFPB)\\b",
  "|\\bSecretary of (State|Commerce|Labor|Defense|Education|Energy|Agriculture|",
    "Transportation|Veterans Affairs|Homeland Secu\\w+|the Interior|the Treasury|",
    "Health and Human Service|the Army|the Navy|the Air Force)",
  "|\\bCommissioner of (Internal Revenue|Social Security)\\b",
  # A federal officer's title, EXCEPT where a State is named alongside it -- an
  # "Attorney General" is federal, an "Attorney General of Mississippi" is not.
  "|\\b(Attorney General|Att'y Gen|Solicitor General|Surgeon General|",
    "Postmaster General)\\b(?!.*", .state_rx, ")",
  "|\\bActing (Attorney General|Secretary|Commissioner|Director|Administrator)\\b(?!.*",
    .state_rx, ")"),
  ignore_case = TRUE)

STATE_PARTY_RX <- regex(paste0(
  "^(", paste(COUNSEL_STATES, collapse = "|"), ")(,? et al\\.?)?$",
  "|", .state_rx, ".{0,60}\\b(Attorney General|Secretary|Commissioner|Director|",
    "Board|Department|Cabinet|Division|Bureau|Agency|Warden|Governor|State)\\b",
  "|\\b(Attorney General|Secretary|Commissioner|Director|Governor|Warden|",
    "Department of Corrections)\\b.{0,60}", .state_rx),
  ignore_case = TRUE)

# "us" / "state" / "private" for a petitioner party name. Vectorised.
#
# Reads the PARTY, never the caption. See the header note: the caption names both
# sides, so a private petitioner challenging a federal rule scans as the
# government filing it is the opposite of.
petitioner_gov_side <- function(party) {
  p <- coalesce(as.character(party), "")
  ifelse(str_detect(p, FED_PARTY_RX), "us",
  ifelse(str_detect(p, STATE_PARTY_RX), "state", "private"))
}

# ---- ranking a rate ------------------------------------------------------------

# Wilson score lower bound. Ranking on a raw rate is a small-sample generator:
# at a 5-petition floor the best raw grant rate in the archive is 82% on eleven
# petitions, which would top a table over an advocate with 24 grants in 100. The
# lower bound needs no arbitrary second cutoff and demotes exactly what it should.
# The raw rate is always published beside it -- the bound is the ORDER, the rate
# is the claim.
wilson_lower <- function(successes, n, z = 1.96) {
  ok <- !is.na(n) & n > 0
  out <- rep(NA_real_, length(n))
  s <- successes[ok]; m <- n[ok]
  p <- s / m; d <- 1 + z^2 / m
  out[ok] <- (p + z^2 / (2 * m) - z * sqrt(p * (1 - p) / m + z^2 / (4 * m^2))) / d
  out
}

# ---- assembly ------------------------------------------------------------------

# One row per paid petition with a named, non-self-representing petitioner's
# counsel. This is the slow step (~4 minutes over ~49k dockets), which is why the
# published page reads a committed JSON and only recomputes on a fingerprint miss.
counsel_petitions <- function(paths) {
  rows <- map_dfr(paths, function(p) {
    x <- readRDS(p)
    cls <- classify_petitions(x)
    cls |> left_join(x[, intersect(c("dkt", "parties", "lower"), names(x))], by = "dkt")
  })
  rows |>
    filter(type == "paid") |>
    mutate(
      # counsel_key() and petitioner_*() take ONE parties frame, not a column.
      # Called on the column they silently return the first row's answer for every
      # petition -- the shape of bug that shipped elite_counsel dead for months.
      counsel = map_chr(parties, ~ tryCatch(str_squish(petitioner_counsel(.x)),
                                            error = function(e) "")),
      pro_se  = map_lgl(parties, ~ tryCatch(petitioner_pro_se(.x),
                                            error = function(e) FALSE)),
      party   = map_chr(parties, ~ tryCatch(str_squish(petitioner_side(.x)$party),
                                            error = function(e) "")),
      counsel_key = map_chr(counsel, counsel_key),
      side = petitioner_gov_side(party)) |>
    filter(nzchar(counsel_key), !pro_se) |>
    select(-parties)
}

# Collapse companion petitions: one row per (advocate, caption) rather than per
# docket number.
#
# A single dispute is often docketed as several petitions -- one per patent, per
# consolidated appeal, per petitioner. Counted as petitions, the top of the relist
# board was "10 filed, 10 relisted, 100%", which is eight identical *Paice LLC v.
# Ford* petitions docketed across three days plus two identical *KIP CR* ones:
# two disputes, each redistributed once, presented as an advocate whose every
# filing got a second look.
#
# The effect is small in aggregate (1.3% of petitions, 49 of 286 qualifying
# advocates) and decisive at the top of a rate board, which is where a
# leaderboard is read. Companion petitions do not share a docketing DATE, so the
# caption alone is the key.
#
# The collapsed row keeps the strongest outcome and the highest relist count: the
# Court disposes of companions together, so this is what happened to the dispute.
counsel_cases <- function(pet) {
  pet |>
    mutate(caption = str_squish(coalesce(caption, ""))) |>
    group_by(counsel_key, caption) |>
    summarise(
      n_petitions = n(),
      dkt = first(dkt), counsel = first(counsel), side = first(side),
      lower = first(lower), date = suppressWarnings(min(date, na.rm = TRUE)),
      n_relists = suppressWarnings(max(n_relists, na.rm = TRUE)),
      outcome = if (any(outcome == "granted")) "granted"
                else if (any(outcome == "gvr")) "gvr"
                else if (all(outcome == "pending")) "pending"
                else first(outcome[outcome != "pending"]),
      .groups = "drop") |>
    mutate(n_relists = if_else(is.finite(n_relists), n_relists, NA_integer_),
           date = as.Date(ifelse(is.finite(date), date, NA), origin = "1970-01-01"))
}

# The display form of a merged key: the variant that appears most often, ties
# broken by the longest string ("Neal Kumar Katyal" over "Neal Katyal") so the
# fuller name wins a coin-flip.
.display_name <- function(v) {
  v <- str_squish(v[nzchar(v)])
  if (!length(v)) return(NA_character_)
  tb <- sort(table(v), decreasing = TRUE)
  top <- names(tb)[tb == tb[[1]]]
  top[which.max(nchar(top))]
}

# One row per advocate, over the collapsed case frame. `min_cases` is applied by
# the caller so the unfiltered frame can report how much tail it dropped.
counsel_aggregate <- function(cases) {
  cases |>
    group_by(counsel_key) |>
    summarise(
      name = .display_name(counsel),
      variants = list(sort(unique(str_squish(counsel[nzchar(counsel)])))),
      cases = n(),
      petitions = sum(n_petitions),
      resolved = sum(outcome != "pending"),
      granted = sum(outcome == "granted"),
      gvr = sum(outcome == "gvr"),
      # A case the Justices looked at twice. TRUE relists only -- the funnel
      # grammar excludes reschedules, called-for-response and CVSG
      # redistributions, which pooled would overstate relists by ~55%.
      relisted = sum(n_relists >= 1, na.rm = TRUE),
      n_us = sum(side == "us"),
      n_state = sum(side == "state"),
      courts = n_distinct(lower[!is.na(lower) & nzchar(lower)]),
      first = suppressWarnings(min(date, na.rm = TRUE)),
      last = suppressWarnings(max(date, na.rm = TRUE)),
      .groups = "drop") |>
    mutate(
      gov_share = (n_us + n_state) / cases,
      relist_share = relisted / cases,
      relist_lo = wilson_lower(relisted, cases),
      across(c(first, last), ~ as.Date(ifelse(is.finite(.x), .x, NA),
                                       origin = "1970-01-01")))
}

# One row per (advocate, pool), where a pool is "government" or "private" by the
# CLIENT, not by the advocate. Ranked on that pool's resolved cases alone.
#
# Splitting the cases rather than the advocates is what removes the arbitrary
# threshold: gov_share has no natural gap -- 245 qualifying advocates sit at
# exactly zero and the rest form a gradient thick with former state solicitors
# general now in private practice -- so any advocate-level cut would be a
# judgement call dressed as a measurement. An advocate who did both appears on
# both boards, which is what actually happened.
# Every pool column is named pool_* so the join with the per-advocate frame cannot
# silently shadow a total: `granted` on both sides would leave the board's own
# count winning under the name the whole-career count uses everywhere else.
counsel_boards <- function(cases, agg, min_cases = COUNSEL_MIN_RATE_CASES) {
  cases |>
    filter(outcome != "pending") |>
    mutate(pool = if_else(side == "private", "private", "government")) |>
    group_by(counsel_key, pool) |>
    summarise(pool_n = n(), pool_granted = sum(outcome == "granted"),
              .groups = "drop") |>
    filter(pool_n >= min_cases) |>
    mutate(pool_rate = pool_granted / pool_n,
           pool_lo = wilson_lower(pool_granted, pool_n)) |>
    left_join(agg, by = "counsel_key") |>
    # The recall guard: see COUNSEL_GOV_MAX_PRIVATE.
    filter(pool == "government" | gov_share < COUNSEL_GOV_MAX_PRIVATE) |>
    arrange(pool, desc(pool_lo), desc(pool_rate))
}

# Everything data/counsel_stats.json holds, as R objects.
#
# Split from counsel_petitions() so the four-minute classify pass can be done once
# and the assembly exercised against its output repeatedly -- the tables here are
# ranked, and a ranking is not something to eyeball for the first time in CI.
counsel_stats_from <- function(pet, terms, fingerprint,
                               min_cases = COUNSEL_MIN_CASES,
                               min_rate_cases = COUNSEL_MIN_RATE_CASES,
                               top_n = COUNSEL_TOP_N) {
  cases <- counsel_cases(pet)
  agg <- counsel_aggregate(cases)
  q <- agg |> filter(cases >= min_cases)
  qr <- agg |> filter(cases >= min_rate_cases)
  boards <- counsel_boards(cases, agg, min_cases = min_rate_cases)

  cols <- function(d) d |>
    transmute(name, key = counsel_key,
              # n_variants FIRST: transmute() evaluates in order, and taking it
              # after the collapse would run lengths() over a character vector and
              # return 1 for every row -- which silently suppressed the merged
              # spellings on every row of the published page.
              n_variants = lengths(variants),
              variants = map_chr(variants, ~ paste(.x, collapse = " / ")),
              cases, petitions, resolved, granted, gvr, relisted,
              n_us, n_state, courts, gov_share, relist_share, relist_lo,
              first = as.character(first), last = as.character(last))
  take <- function(d, n = top_n) cols(head(d, n))
  take_board <- function(p) {
    d <- boards |> filter(pool == p) |> head(top_n)
    bind_cols(cols(d), select(d, pool_n, pool_granted, pool_rate, pool_lo))
  }

  # Case-level base rates, which are the reason the grant board is split at all.
  # Published on the page: 4.6% reads as "low" only once the 48.6% is beside it.
  by_side <- cases |> filter(outcome != "pending") |> group_by(side) |>
    summarise(cases = n(), granted = sum(outcome == "granted"),
              rate = mean(outcome == "granted"), .groups = "drop")

  list(
    filings = take(q |> arrange(desc(cases), desc(granted))),
    # Published as a SHARE, not a count. Raw relist count correlates with filing
    # volume at 0.81, so a count column re-ranks the filings table under a
    # different heading; the share is a different claim. Ties on the bound break
    # toward the advocate with more cases behind it.
    relists = take(qr |> arrange(desc(relist_lo), desc(cases), desc(relist_share))),
    grants_private = take_board("private"),
    grants_government = take_board("government"),
    by_side = by_side,
    totals = list(
      petitions = nrow(pet),
      cases = nrow(cases),
      advocates = nrow(agg),
      qualifying = nrow(q),
      qualifying_rate = nrow(qr),
      one_off = sum(agg$cases == 1),
      board_private = sum(boards$pool == "private"),
      board_government = sum(boards$pool == "government"),
      min_cases = min_cases,
      min_rate_cases = min_rate_cases,
      top_n = top_n,
      terms = terms,
      as_of = as.character(suppressWarnings(max(pet$date, na.rm = TRUE)))),
    fingerprint = fingerprint)
}

compute_counsel_stats <- function(paths, min_cases = COUNSEL_MIN_CASES,
                                  min_rate_cases = COUNSEL_MIN_RATE_CASES,
                                  top_n = COUNSEL_TOP_N) {
  yrs <- as.integer(str_extract(basename(paths), "\\d{4}"))
  counsel_stats_from(
    counsel_petitions(paths),
    terms = paste0("OT", paste(range(yrs), collapse = "–")),
    fingerprint = counsel_stats_fingerprint(paths),
    min_cases = min_cases, min_rate_cases = min_rate_cases, top_n = top_n)
}

# A digest of everything data/counsel_stats.json is a function of, so a committed
# copy can be checked against the code that would produce it today.
#
# Same shape and same reason as funnel_baseline_fingerprint(): that file went
# stale for two weeks and the site published a relist count 2.5x too high, and
# the change that invalidated it was inside a FUNCTION BODY, not a constant. So
# the bodies are deparsed and digested, not just the tunables.
counsel_stats_fingerprint <- function(paths) {
  body_of <- function(f) if (exists(f)) paste(deparse(get(f)), collapse = "\n") else ""
  digest::digest(list(
    logic = lapply(c("classify_petition_events", "classify_petitions",
                     "counsel_key", "petitioner_side", "petitioner_counsel",
                     "petitioner_pro_se", "petitioner_gov_side",
                     "counsel_petitions", "counsel_cases", "counsel_aggregate",
                     "counsel_stats_from",
                     "counsel_boards", "wilson_lower",
                     ".display_name"), body_of),
    grammar = list(COUNSEL_SUFFIXES, as.character(FED_PARTY_RX),
                   as.character(STATE_PARTY_RX), COUNSEL_STATES,
                   COUNSEL_MIN_CASES, COUNSEL_TOP_N, COUNSEL_GOV_MAX_PRIVATE),
    archives = unname(tools::md5sum(sort(paths)))))
}

# ---- rendering -----------------------------------------------------------------

# Leaderboard styling. Lives here rather than in INDEX_CSS because nothing else
# on the site publishes a ranked table, and every colour is a token or a
# fill_palette() substitution -- audit_site.R fails the build on a six-digit hex
# written anywhere but palette.R.
COUNSEL_CSS <- "
  /* Wider than the 40rem index measure, and exactly SITE_NAV_MAX, so the
     masthead rule lands flush on the content column rather than floating wide
     of it. */
  .wrap.wide{max-width:54rem}
  .clede{font-size:1.02rem;line-height:1.62;max-width:36rem;margin:1.4rem 0 0}
  .clede p{margin:0 0 1rem}
  .over{font:600 .74rem/1 'Newsreader',Georgia,serif;letter-spacing:.2em;
    text-transform:uppercase;color:var(--link,@link@);margin:3rem 0 .5rem;
    display:flex;align-items:center;gap:.7rem}
  .over::after{content:'';flex:1;border-top:1px solid var(--rule,@rule@)}
  .cnote2{color:var(--faint,@faint@);font-size:.9rem;font-style:italic;
    margin:.2rem 0 1rem;max-width:38rem}
  /* Tables scroll inside their own box; the page body never scrolls sideways. */
  .ctwrap{overflow-x:auto;margin:0 0 .6rem}
  table.ctab{border-collapse:collapse;width:100%;min-width:34rem;
    font-variant-numeric:tabular-nums}
  table.ctab th{font:600 .68rem/1.3 'Newsreader',Georgia,serif;letter-spacing:.14em;
    text-transform:uppercase;color:var(--accent,@accent@);text-align:right;
    padding:.45rem .5rem;border-bottom:2px solid var(--ink,@ink@);white-space:nowrap}
  table.ctab th.l,table.ctab td.l{text-align:left}
  table.ctab td{padding:.5rem .5rem;border-bottom:1px solid var(--rule,@rule@);
    text-align:right;font-size:.98rem;vertical-align:baseline}
  table.ctab tbody tr:hover{background:rgba(@accent:rgb@,.05)}
  table.ctab .rk{color:var(--faint,@faint@);font-size:.85rem;width:2.2rem;
    text-align:right;padding-right:.1rem}
  table.ctab .nm{font-family:'Fraunces',Georgia,serif;font-weight:600;
    font-size:1.02rem;line-height:1.2;color:var(--ink,@ink@)}
  /* The merged spellings, published under every name that merged more than one.
     This is the page's answer to the same-name problem: show the merge and let
     the reader judge it. */
  table.ctab .alias{display:block;font-size:.8rem;font-style:italic;
    color:var(--faint,@faint@);line-height:1.35;margin-top:.1rem}
  .chip{display:inline-block;font:600 .6rem/1 'Newsreader',Georgia,serif;
    letter-spacing:.12em;text-transform:uppercase;padding:.2rem .35rem;
    border:1px solid var(--rule,@rule@);color:var(--ink-soft,@ink-soft@);
    border-radius:2px;vertical-align:.1em;margin-left:.35rem;white-space:nowrap}
  /* The bar is the ranking made visible; the number beside it is the claim.
     The track is a FIXED width so two bars are comparable -- sized off the
     remaining flex space, they would encode the width of the number beside them
     as much as the value. */
  .bar{display:flex;align-items:center;justify-content:flex-end;gap:.45rem}
  .bar span.tr{display:block;width:3.4rem;flex:none;height:.5rem;
    background:rgba(@accent:rgb@,.12);border-radius:1px}
  .bar i{display:block;height:.5rem;background:var(--accent,@accent@);opacity:.55;
    min-width:1px;border-radius:1px}
  .bar b{font-weight:600;min-width:3rem;text-align:right}
  .bar .lo{color:var(--faint,@faint@);font-weight:400;font-size:.85rem;
    min-width:3rem;text-align:right}
  /* Base-rate callout: the three numbers that justify splitting the grant board. */
  .rates{display:flex;flex-wrap:wrap;gap:1.6rem;margin:1.2rem 0 1.4rem;
    padding:1.1rem 1.3rem;background:var(--panel,@panel@);
    border:1px solid var(--rule,@rule@);border-left:4px solid var(--accent,@accent@)}
  .rates div{flex:1;min-width:7rem}
  .rates .big{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:2rem;
    line-height:1;color:var(--accent,@accent@)}
  .rates .lab{font:600 .66rem/1.3 'Newsreader',Georgia,serif;letter-spacing:.14em;
    text-transform:uppercase;color:var(--ink-soft,@ink-soft@);margin-top:.35rem}
  .rates .sub{color:var(--faint,@faint@);font-size:.82rem;font-style:italic;
    margin-top:.15rem}
  .method{margin:2.6rem 0 0;font-size:.92rem;line-height:1.6;
    color:var(--ink-soft,@ink-soft@);max-width:40rem}
  .method h2{font:600 .74rem/1 'Newsreader',Georgia,serif;letter-spacing:.2em;
    text-transform:uppercase;color:var(--accent,@accent@);margin:0 0 .6rem}
  .method p{margin:0 0 .8rem}
  @media(max-width:640px){
    table.ctab .alias{display:none}
    .rates{gap:1rem;padding:.9rem 1rem}
    .rates .big{font-size:1.6rem}
  }
" |> fill_palette()

.cn <- function(x) format(x, big.mark = ",", trim = TRUE)
.cpct <- function(x, digits = 1) if (is.na(x)) "&mdash;" else
  paste0(formatC(100 * x, format = "f", digits = digits), "%")

# One leaderboard. `spec` is a list of columns: list(label=, align=, cell=) where
# cell(row_index) returns raw HTML for the cell.
.counsel_table <- function(d, spec) {
  if (is.null(d) || !nrow(d)) return(tags$p(class = "cnote2", "No qualifying advocates."))
  head_cells <- paste0(vapply(spec, function(s) sprintf(
    "<th class='%s'>%s</th>", if (identical(s$align, "l")) "l" else "", s$label),
    character(1)), collapse = "")
  body <- vapply(seq_len(nrow(d)), function(i) paste0(
    "<tr><td class='rk'>", i, "</td>",
    paste0(vapply(spec, function(s) sprintf("<td class='%s'>%s</td>",
      if (identical(s$align, "l")) "l" else "", s$cell(i)), character(1)),
      collapse = ""),
    "</tr>"), character(1))
  HTML(paste0("<div class='ctwrap'><table class='ctab'><thead><tr><th></th>",
              head_cells, "</tr></thead><tbody>",
              paste(body, collapse = ""), "</tbody></table></div>"))
}

# The advocate cell: display name, the merged spellings beneath it, and a chip
# when most of their filings were for a government.
.name_cell <- function(d, i, chip = TRUE) {
  alias <- if (!is.na(d$n_variants[i]) && d$n_variants[i] > 1)
    paste0("<span class='alias'>", htmlEscape(d$variants[i]), "</span>") else ""
  g <- if (chip && !is.na(d$gov_share[i]) && d$gov_share[i] >= 0.5)
    "<span class='chip'>Government</span>" else ""
  paste0("<span class='nm'>", htmlEscape(d$name[i]), g, "</span>", alias)
}

# A proportional bar plus its number. `lo` prints a muted second figure (the
# Wilson floor) after the rate it ranks on.
.bar_cell <- function(value, max_value, digits = 0, lo = NULL) {
  w <- if (is.na(value) || is.na(max_value) || max_value <= 0) 0 else
    round(100 * value / max_value)
  paste0("<span class='bar'><span class='tr'><i style='width:", max(w, 1),
         "%'></i></span><b>", .cpct(value, digits), "</b>",
         if (!is.null(lo)) paste0("<span class='lo'>", .cpct(lo, digits), "</span>") else "",
         "</span>")
}

# Render /counsel/index.html from a compute_counsel_stats() list. Returns the path.
render_counsel_page <- function(stats, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tot <- stats$totals
  as_tib <- function(x) if (is.null(x) || !length(x)) tibble() else as_tibble(x)
  filings <- as_tib(stats$filings); relists <- as_tib(stats$relists)
  priv <- as_tib(stats$grants_private); govt <- as_tib(stats$grants_government)
  side <- as_tib(stats$by_side)
  rate_of <- function(s) { r <- side$rate[side$side == s]; if (length(r)) r[[1]] else NA_real_ }
  n_of <- function(s) { r <- side$cases[side$side == s]; if (length(r)) r[[1]] else NA_integer_ }

  sec <- function(over, heading, note, tbl) tagList(
    tags$p(class = "over", over), tags$h2(heading),
    if (!is.null(note)) tags$p(class = "cnote2", HTML(smarten(note))) else NULL, tbl)

  # ---- table 1: volume
  t_filings <- .counsel_table(filings, list(
    list(label = "Advocate", align = "l", cell = function(i) .name_cell(filings, i)),
    list(label = "Cases", cell = function(i) .cn(filings$cases[i])),
    list(label = "Relisted", cell = function(i) .cn(filings$relisted[i])),
    list(label = "Granted", cell = function(i) .cn(filings$granted[i]))))

  # ---- table 2: relists, as a share
  rmax <- if (nrow(relists)) max(relists$relist_share, na.rm = TRUE) else 1
  t_relists <- .counsel_table(relists, list(
    list(label = "Advocate", align = "l", cell = function(i) .name_cell(relists, i)),
    list(label = "Cases", cell = function(i) .cn(relists$cases[i])),
    list(label = "Relisted", cell = function(i) .cn(relists$relisted[i])),
    list(label = "Share", cell = function(i)
      .bar_cell(relists$relist_share[i], rmax, 0, lo = relists$relist_lo[i]))))

  grant_tbl <- function(d) {
    if (!nrow(d)) return(.counsel_table(d, list()))
    gmax <- max(d$pool_rate, na.rm = TRUE)
    .counsel_table(d, list(
      list(label = "Advocate", align = "l", cell = function(i) .name_cell(d, i, chip = FALSE)),
      list(label = "Cases", cell = function(i) .cn(d$pool_n[i])),
      list(label = "Granted", cell = function(i) .cn(d$pool_granted[i])),
      list(label = "Rate", cell = function(i)
        .bar_cell(d$pool_rate[i], gmax, 1, lo = d$pool_lo[i]))))
  }

  body <- tags$body(
    HTML(site_masthead(active = "/counsel/")),
    tags$main(
      id = "main", class = "wrap wide",
      tags$p(class = "kicker", "Supreme Court of the United States"),
      tags$h1("The Counsel Table"),
      tags$hr(class = "brule"),
      tags$p(class = "dek", smarten(paste0(
        "Who files certiorari petitions, whose petitions get a second look, and ",
        "whose get granted — across ", tot$terms, "."))),
      tags$div(class = "clede", tagList(
        tags$p(HTML(smarten(paste0(
          "Every paid petition in the archive with a named petitioner's counsel of ",
          "record: ", .cn(tot$petitions), " petitions from ", .cn(tot$advocates),
          " advocates, filed in ", .cn(tot$cases), " separate cases. The bar is ",
          "long-tailed — ", .cn(tot$one_off), " advocates (",
          .cpct(tot$one_off / tot$advocates, 0), ") appear exactly once — so ",
          "an advocate needs at least ", tot$min_cases,
          " cases to appear below, and ", .cn(tot$qualifying), " clear that bar. ",
          "The two tables that publish a <em>rate</em> ask for ", tot$min_rate_cases,
          ", which ", .cn(tot$qualifying_rate), " reach: at five, eleven of the ",
          "twenty-five relist rows were the same three-of-five advocate.")))),
        tags$p(HTML(smarten(paste0(
          "This counts petitioner's counsel of record and nothing else. An advocate ",
          "who argued a case they did not petition in does not appear, and ",
          "respondents' counsel — which is most of what the Supreme Court bar ",
          "actually does — is invisible here."))))
      )),

      sec("Volume", "Most cases filed",
          paste0("Companion petitions are counted once: a single dispute is often ",
                 "docketed as several petitions, and counting each one credits an ",
                 "advocate with eight filings for one case."),
          t_filings),

      sec("A second look", "Most often relisted",
          paste0("A <em>relist</em> is a redistribution with no intervening ",
                 "&ldquo;Rescheduled&rdquo; entry, no call for a response and no ",
                 "CVSG — those are mechanical rather than the Justices ",
                 "deferring. Published as a share, not a count: the raw count ",
                 "correlates with filing volume at 0.81 and would simply re-rank ",
                 "the table above. Ordered by the lower bound (the muted figure), ",
                 "not the share."),
          t_relists),

      tags$p(class = "over", "Certiorari granted"),
      tags$h2("Whose petitions are granted"),
      tags$div(class = "rates", tagList(
        tags$div(tags$div(class = "big", .cpct(rate_of("us"), 1)),
                 tags$div(class = "lab", "For the United States"),
                 tags$div(class = "sub", paste0(.cn(n_of("us")), " cases"))),
        tags$div(tags$div(class = "big", .cpct(rate_of("state"), 1)),
                 tags$div(class = "lab", "For a State"),
                 tags$div(class = "sub", paste0(.cn(n_of("state")), " cases"))),
        tags$div(tags$div(class = "big", .cpct(rate_of("private"), 1)),
                 tags$div(class = "lab", "For a private party"),
                 tags$div(class = "sub", paste0(.cn(n_of("private")), " cases"))))),
      tags$p(class = "cnote2", HTML(smarten(paste0(
        "Which is why there are two boards rather than one. A pooled grant ranking ",
        "would be the Solicitor General's office at the top, and that measures who ",
        "the client is, not how the petition was written. The split is made case by ",
        "case, so an advocate who did both work appears on both boards.")))),

      tags$h3("For a private client"),
      tags$p(class = "cnote2", HTML(smarten(paste0(
        "Ranked by the lower bound of a 95% confidence interval on the grant rate, ",
        "with the rate itself shown in bold. Ranking on the raw rate instead puts ",
        "3 grants in 10 above 22 in 94 — a higher number on a tenth of the ",
        "evidence.")))),
      grant_tbl(priv),

      tags$h3("For a government"),
      tags$p(class = "cnote2", HTML(smarten(paste0(
        "Federal and state governments together — ", tot$board_government,
        " advocates clear the minimum on government cases alone.")))),
      grant_tbl(govt),

      tags$section(class = "method", tagList(
        tags$h2("How to read this"),
        tags$p(HTML(smarten(paste0(
          "<strong>An advocate here is a name, not a verified identity.</strong> ",
          "Rows are keyed on a first and last name, so &ldquo;Neal K. Katyal&rdquo; ",
          "and &ldquo;Neal Kumar Katyal&rdquo; merge — and so would two ",
          "different lawyers who share both names. Every row that merged more than ",
          "one spelling prints them beneath the name so the merge is visible. The ",
          "dockets carry no firm, so there is nothing else to disambiguate on.")))),
        tags$p(HTML(smarten(paste0(
          "Whether a petition was filed for a government is read from the ",
          "petitioner's name, never from the case caption — a private ",
          "petitioner <em>suing</em> the government has the government in their ",
          "caption. That test almost never calls a private petition governmental, ",
          "but it misses roughly one government petition in six, so an advocate who ",
          "files for a government in at least a quarter of their cases is kept off ",
          "the private board entirely rather than appearing on it with whatever the ",
          "test failed to catch.")))),
        tags$p(HTML(smarten(paste0(
          "Paid petitions only, from ", tot$terms, ", through ",
          format(as.Date(tot$as_of), "%B %e, %Y") |> str_squish(),
          ". Self-represented petitioners are excluded. Pending petitions count ",
          "toward cases filed and relists but not toward a grant rate."))))
      )),
      tags$p(class = "back", HTML("<a href='/'>&larr; Supreme Court Report</a>"))
    ))

  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head("The Counsel Table — Supreme Court Report",
              jsonld = site_breadcrumb_jsonld("The Counsel Table"),
              extra_css = COUNSEL_CSS), "\n",
    as.character(body), "\n</html>\n")
  out <- file.path(out_dir, "index.html")
  writeLines(enc2utf8(smarten_html(html)), out, useBytes = TRUE)
  invisible(out)
}
