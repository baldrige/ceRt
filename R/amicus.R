# amicus.R ----------------------------------------------------------------------
# The amicus page (/counsel/amicus.html): who files amicus briefs at the Court,
# at which stage, on which side, beside whom, and who writes them -- computed
# over the committed archives, never fetched at render time.
#
# A BRIEF IS A DOCKET ENTRY, READ BY THE CASE PAGES' OWN GRAMMAR.
#
# Every number here starts from brief_cover() (R/docket_page.R), the classifier
# that colours the case pages' timeline dots, called with the same grant date
# (docket_granted_on()) and the same Rule 37 split (resp_merits_brief_on()). So
# a case page and this page cannot disagree about whether an entry is an amicus
# brief, whether it came before or after the grant, or which side it was filed
# on. Fix a classification there and it is fixed here; the fingerprint below
# digests those bodies, so the committed stats go stale with them.
#
# WHAT THE SIDE IS, AND WHAT IT IS NOT.
#
# The docket rarely says which side an amicus supports. Rule 37 does, by the
# calendar: briefs for the petitioner OR FOR NEITHER PARTY are due 7 days after
# the petitioner's merits brief, briefs for the respondent 7 days after the
# respondent's. Measured on 6,766 merits amici in the argued-case refresh, where
# the PDF filename happens to name a side, the timing agrees on 97% of
# respondent-side briefs. The petitioner bucket is the weak one: a sixth of the
# briefs in it whose filenames say anything say "neither party". So the page
# never calls that bucket "for the petitioner", and it never publishes a "backed
# the winner" rate -- for a neither-party brief there is no winner to back. What
# it publishes instead is a fact about the CASE: when this filer's brief came in
# on the petitioner's schedule, how often the judgment below fell; on the
# respondent's, how often it stood. The exact side is on the brief's cover and
# nowhere in the docket.
#
# WHO THE FILER IS.
#
# The lead name on the entry ("Brief amici curiae of Cato Institute, et al.
# filed."). "et al." hides the co-signers -- for a multistate brief, every State
# but the first -- and the full list is on the PDF cover, which this page does
# not read. So a filer is credited with the briefs it LED, and "West Virginia,
# et al." is West Virginia's. Names are keyed with amicus_key() ("The Cato
# Institute" and "Cato Institute, et al." are one filer) and, as on the Counsel
# Table, every merged spelling is published beneath the name.
#
# THE UNITED STATES IS NOT ON THE FILER BOARDS. At the cert stage its amicus
# brief is the answer to a CVSG -- the Court asked for it -- and on the merits it
# is the Solicitor General's office, whose record is a different population for
# the reason the Counsel Table's grant board is split. It gets its own panel.

suppressPackageStartupMessages({library(tidyverse); library(htmltools)})

local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
  find <- function(f) {
    if (!is.na(here) && file.exists(file.path(here, f))) file.path(here, f)
    else if (file.exists(file.path("R", f))) file.path("R", f) else f
  }
  for (f in c("counsel_table.R", "docket_page.R", "argument_lean.R"))
    if (!exists(switch(f, counsel_table.R = "counsel_archive_paths",
                          docket_page.R = "brief_cover",
                          argument_lean.R = "judgment_of")))
      sys.source(find(f), envir = globalenv())
})

# ---- inputs --------------------------------------------------------------------

# A re-fetch of every archived docket that carries an amicus brief, with the
# docket JSON's "Other" block kept as an `amici` column (build_amici()). Written
# by refetch-amici.yml. Optional: without it the page still renders, from the
# archives alone, with no counsel boards and the merits stage of the most recent
# docket years thin (see .github/scripts/refetch_amici.R).
AMICUS_REFRESH <- "data-raw/amici_refresh.rds"

# MOST CURRENT FIRST: amicus_briefs() keeps the first copy of a docket it meets,
# exactly as counsel_petitions() does.
amicus_input_paths <- function(dir = "data-raw") {
  refresh <- file.path(dir, basename(AMICUS_REFRESH))
  arg <- file.path(dir, basename(COUNSEL_ARG_REFRESH))
  c(refresh[file.exists(refresh)], arg[file.exists(arg)], counsel_archive_paths(dir))
}

# The docket years the page covers: the Terms of counsel_archive_paths(), read
# off the docket number ("24-1287" is docketed in OT2024). Rows from a refresh
# layer for any other year are dropped rather than published as a partial year.
amicus_docket_years <- function(paths) {
  b <- basename(paths)
  yrs <- c(as.integer(str_match(b, "^ot_(\\d{4})\\.rds$")[, 2]),
           2000L + as.integer(str_match(b, "^snapshot_(\\d{2})\\.rds$")[, 2]))
  yrs <- yrs[!is.na(yrs)]
  if (!length(yrs)) integer() else seq(min(yrs), max(yrs))
}

# ---- the entry ------------------------------------------------------------------

# An amicus brief, in either of the docket's two phrasings (see brief_cover()):
# "Brief amicus curiae of X filed." and the e-filing "Amicus brief of X
# submitted." The case pages' grammar decides what counts; this pattern only
# finds the candidates and extracts the name.
AMICUS_ENTRY_RX <- regex("^\\s*(brief\\s+amic(us|i)\\s+curiae|amicus\\s+brief)\\b", ignore_case = TRUE)
AMICUS_PREFIX_RX <- regex(paste0("^\\s*(brief\\s+amic(us|i)\\s+curiae|amicus\\s+brief)",
                                 "\\s+(of|for|by)\\s+"), ignore_case = TRUE)
# Everything after the name: "filed", "submitted", a stated side, the Clerk's
# "(Distributed)" / "VIDED." / "(Corrected)".
AMICUS_SUFFIX_RX <- regex(paste0("(\\s+in\\s+(partial\\s+)?support\\s+of\\b.*$)",
                                 "|(\\s+(filed|submitted)\\b.*$)"), ignore_case = TRUE)

amicus_filer <- function(text) {
  t <- str_squish(str_replace_all(coalesce(text, ""), "<[^>]*>", ""))
  ok <- str_detect(t, AMICUS_PREFIX_RX)
  t <- str_remove(t, AMICUS_PREFIX_RX)
  t <- str_squish(str_remove(t, AMICUS_SUFFIX_RX))
  t <- str_remove(t, "[.,;:]+$")
  if_else(ok, t, "")
}

# The filer key: case, punctuation, a leading "The", a trailing corporate suffix
# and "et al." do not distinguish filers. Measured on the archive, the raw
# strings split the Chamber of Commerce four ways and Cato three.
amicus_key <- function(x) {
  k <- tolower(coalesce(x, ""))
  k <- str_replace_all(k, "[‘’`]", "'")
  k <- str_remove(k, ",?\\s*\\bet\\.?\\s+al\\b.*$")
  k <- str_replace_all(k, "&", " and ")
  k <- str_remove_all(k, "'")
  k <- str_replace_all(k, "[^a-z0-9 ]", " ")
  k <- str_remove(str_squish(k), "^the\\s+")
  k <- str_remove(k, "\\s+(inc|llc|llp|corp|corporation|co|ltd)$")
  str_squish(k)
}

# A State, or a list of States, and nothing else: "Texas, et al.", "State of
# Ohio and 18 Other States", "States of Tennessee, Arizona Arkansas ...". Tested
# by deleting every State name and the words that join them and asking whether
# anything is left -- so "Washington Legal Foundation" (leaves "legal
# foundation") and "Members of Congress" are not States. Longest names first, so
# "West Virginia" is not read as "Virginia".
.AMICUS_STATE_NAMES <- COUNSEL_STATES[order(-nchar(COUNSEL_STATES))]
.amicus_state_rx <- regex(paste0("\\b(", paste(.AMICUS_STATE_NAMES, collapse = "|"), ")\\b"),
                          ignore_case = TRUE)
amicus_is_state <- function(key) {
  rest <- str_remove_all(key, regex(paste0("\\b(", paste(tolower(.AMICUS_STATE_NAMES), collapse = "|"), ")\\b")))
  # "Alabama and 17 Additional States"; "West Virginia, 27 Other States & the
  # Governor of Kentucky" -- a governor joining the States' brief is still the
  # States' brief.
  rest <- str_remove_all(rest, "\\b(states?|commonwealths?|of|and|the|other|additional|governors?|\\d+)\\b")
  str_detect(key, regex(paste0("\\b(", paste(tolower(.AMICUS_STATE_NAMES), collapse = "|"), ")\\b"))) &
    !nzchar(str_squish(rest))
}
# Every State the entry itself names. Only an explicit list counts: "Texas, et
# al." names one, and the others are on the cover.
amicus_states_named <- function(filer) {
  m <- str_extract_all(filer, .amicus_state_rx)
  lapply(m, function(s) {
    s <- unique(str_to_title(s))
    # str_to_title() lower-cases the "of" in "District Of Columbia" back
    s <- str_replace(s, " Of ", " of ")
    s[s %in% COUNSEL_STATES]
  })
}

AMICUS_US_KEYS <- c("united states", "united states of america")
amicus_kind <- function(key) case_when(
  key %in% AMICUS_US_KEYS ~ "us",
  amicus_is_state(key) ~ "state",
  TRUE ~ "other")

# ---- assembly -------------------------------------------------------------------

# One row per amicus brief, plus (as attribute "petitions") one row per paid or
# IFP petition with its outcome and whether a non-federal amicus brief reached it
# before any grant -- the denominator for the front-door panel.
#
# This is the slow step (classify_petitions() over every archive, ~4 minutes),
# which is why the page reads a committed JSON.
amicus_briefs <- function(paths) {
  years <- amicus_docket_years(counsel_archive_paths(dirname(paths[1])))
  seen <- character()
  pets <- list(); out <- list()
  for (p in paths) {
    x <- readRDS(p)
    # From the docket number, as classify_petitions() does: the older archives
    # carry no `type` column, and the number is authoritative in every era.
    x <- x[funnel_case_type(x$dkt) %in% PETITION_TYPES & !x$dkt %in% seen, , drop = FALSE]
    yr <- 2000L + suppressWarnings(as.integer(str_match(x$dkt, "^(\\d{2})-")[, 2]))
    x <- x[!is.na(yr) & yr %in% years, , drop = FALSE]
    if (!nrow(x)) next
    seen <- c(seen, x$dkt)
    cls <- classify_petitions(x) |> select(dkt, type, outcome)
    has_amici <- "amici" %in% names(x)
    for (i in seq_len(nrow(x))) {
      ev <- x$events[[i]]
      if (!is.data.frame(ev) || !nrow(ev)) next
      po <- coalesce(ev[["Proceedings and Orders"]], "")
      hit <- which(str_detect(po, AMICUS_ENTRY_RX))
      if (!length(hit)) next
      ed <- suppressWarnings(lubridate::mdy(ev$Date))
      granted_on <- docket_granted_on(ev)
      sides <- tryCatch(.brief_caption_sides(x$caption[i]), error = function(e) NULL)
      rbo <- resp_merits_brief_on(ev, granted_on, sides)
      jd <- if (!is.na(granted_on)) judgment_of(ev) else NA_character_
      fell <- petitioner_won(jd)
      # A DIG, a Rule 46 dismissal or a moot case is decided, with no side
      # winning: not "undecided" in the by-year table.
      dismissed <- argument_disposition(jd) %in% "dismissed"
      # The Court called for the Solicitor General's views: the United States'
      # cert-stage brief that follows is the answer, not a volunteer.
      cvsg <- any(str_detect(po, FUNNEL_PATTERNS$cvsg))
      rows <- map(hit, function(k) {
        cv <- brief_cover(po[k], granted_on, ed[k], rbo, sides)
        if (is.null(cv)) return(NULL)
        col <- str_match(cv$color, "c-([a-z]+)")[, 2]
        if (!col %in% c("cream", "lgreen", "dgreen")) return(NULL)
        tibble(dkt = x$dkt[i], caption = x$caption[i], date = ed[k],
               stage = if (col == "cream") "cert" else "merits",
               side = case_when(col == "lgreen" ~ "pet", col == "dgreen" ~ "resp",
                                TRUE ~ NA_character_),
               vided = str_detect(po[k], regex("\\bVIDED\\b", ignore_case = TRUE)),
               filer = amicus_filer(po[k]), fell = fell, dismissed = dismissed, cvsg = cvsg)
      })
      b <- bind_rows(rows)
      if (!nrow(b)) next
      am <- if (has_amici) x$amici[[i]] else NULL
      b$counsel <- NA_character_; b$firm <- NA_character_
      if (is.data.frame(am) && nrow(am)) {
        m <- amicus_match_counsel(b$filer, am)
        b$counsel <- m$counsel; b$firm <- m$firm
      }
      b$has_block <- is.data.frame(am)
      out[[length(out) + 1]] <- b
    }
    pets[[length(pets) + 1]] <- cls
  }
  briefs <- bind_rows(out)
  if (!nrow(briefs)) return(structure(briefs, petitions = bind_rows(pets)))
  briefs <- briefs |>
    filter(nzchar(filer)) |>
    mutate(term = 2000L + as.integer(str_match(dkt, "^(\\d{2})-")[, 2]),
           key = amicus_key(filer), kind = amicus_kind(key)) |>
    # "Brief amicus curiae of Court-appointed amicus curiae ..." is the lawyer
    # the Court appointed to defend the judgment below, in one of the three
    # forms its brief takes -- not a filer. Anchored, so "Third Circuit
    # Court-Appointed Amicus Curiae" (a lower court's appointee, filing as an
    # ordinary amicus) stays.
    filter(!str_detect(key, "^court appointed amic")) |>
    # One brief filed in consolidated cases is docketed on each, marked "VIDED".
    # Counted once: same filer, same day, same stage and side.
    distinct(key, date, stage, side, vided_grp = if_else(vided, "v", dkt), .keep_all = TRUE) |>
    select(-vided_grp)
  pet <- bind_rows(pets) |> distinct(dkt, .keep_all = TRUE)
  cert_dk <- unique(briefs$dkt[briefs$stage == "cert" & briefs$kind != "us"])
  pet$cert_amicus <- pet$dkt %in% cert_dk
  structure(briefs, petitions = pet)
}

# Counsel of record and firm for each filer, from the docket's "Other" block.
#
# The block names each amicus PARTY as its counsel typed it, which is rarely the
# docket entry's wording: "Bipartisan Former Chairs of the Federal Trade
# Commission (FTC)" for "Brief amici curiae of Bipartisan Former Chairs of the
# FTC". Exact key first, then party_score()'s distinctive-word overlap (the case
# pages' party matcher); a tie or no overlap is no match, never a guess.
amicus_match_counsel <- function(filers, am) {
  am <- am[nzchar(coalesce(am$party, "")), , drop = FALSE]
  na <- list(counsel = rep(NA_character_, length(filers)), firm = rep(NA_character_, length(filers)))
  if (!nrow(am)) return(na)
  # One row per party: its counsel of record, else its first-listed attorney.
  am <- am |>
    mutate(pkey = amicus_key(party)) |>
    arrange(pkey, desc(counsel_of_record)) |>
    distinct(pkey, .keep_all = TRUE)
  fk <- amicus_key(filers)
  pick <- vapply(seq_along(fk), function(i) {
    if (!nzchar(fk[i])) return(NA_integer_)
    ex <- which(am$pkey == fk[i])
    if (length(ex)) return(ex[1])
    s <- vapply(am$pkey, function(n) party_score(fk[i], n), numeric(1))
    if (max(s) <= 0 || sum(s == max(s)) > 1) return(NA_integer_)
    which.max(s)
  }, integer(1))
  list(counsel = str_squish(am$attys[pick]),
       firm = str_squish(am$firm[pick]))
}

# ---- tunables -------------------------------------------------------------------

# Rows per board, and the floors under the boards that publish a rate -- the
# Counsel Table's, for the Counsel Table's reasons (wilson_lower(), and
# COUNSEL_MIN_RATE_CASES on why five is too few).
AMICUS_TOP_N <- 25L
AMICUS_MIN_RATE <- COUNSEL_MIN_RATE_CASES
# Allies and opponents: only filers with this many merits briefs are paired, and
# a pair must share this many cases. Below that the board is coincidence -- two
# filers who each appear in thirty cases will meet in one or two by chance.
AMICUS_PAIR_FILER_MIN <- 10L
AMICUS_PAIR_MIN <- 4L
# Firm-field values that name no firm, matched against the keyed firm.
AMICUS_GENERIC_FIRM_RX <- "^(attorneys? at law|n a|na|none|self|solo practitioner|private practice)$"
# A government's own law office. Matched on the TITLE, not the client: a State
# that retains Consovoy McCarthy is a firm's client like any other (keying on
# the client cost that firm 16 briefs), but "Office of the Attorney General"
# is the State itself.
# Keyed text, so apostrophes are gone ("attorney generals office") and the
# docket's own typos ride along ("attomey general").
AMICUS_GOV_FIRM_RX <- paste0(
  "\\b(solicitors? generals?|att[o]?[rm]n?eys? generals?|department of (justice|law)|",
  "law department|office of (the )?general counsel|",
  "(county|city|district|parish) attorneys?|corporation counsel)\\b|",
  "^(state|commonwealth|city|county|district|people) of\\b")

# ---- statistics -----------------------------------------------------------------

# The display name (the commonest spelling) and the merged spellings printed
# beneath it. Spellings that differ only in case or a leading "The" are one
# spelling here: the first render listed the Chamber of Commerce eight times,
# "Chamber Of Commerce Of The ..." among them, which shows the reader nothing
# about the merge. At most four are printed, with a count of the rest.
AMICUS_MAX_VARIANTS <- 4L
.amicus_display <- function(key, filer) {
  tibble(key, nm = str_squish(str_remove(filer, ",?\\s*\\bet\\.?\\s+al\\b.*$"))) |>
    # Curly and straight apostrophes too: "America's Future" twice, otherwise.
    mutate(form = str_remove(str_replace_all(tolower(nm), "[‘’`']", ""), "^the\\s+")) |>
    count(key, form, nm) |>
    group_by(key, form) |>
    arrange(desc(n), nm, .by_group = TRUE) |>
    summarise(nm = first(nm), n = sum(n), .groups = "drop") |>
    group_by(key) |>
    arrange(desc(n), nm, .by_group = TRUE) |>
    summarise(name = first(nm), n_variants = n(),
              variants = paste0(paste(head(nm, AMICUS_MAX_VARIANTS), collapse = " · "),
                                if (n() > AMICUS_MAX_VARIANTS)
                                  paste0(" · +", n() - AMICUS_MAX_VARIANTS, " more") else ""),
              .groups = "drop")
}

# The judgment's side, per case: did the judgment below fall (reversed or
# vacated, in whole or in part) or stand (affirmed)? NA when not yet decided,
# dismissed, or never granted.
.amicus_merits_record <- function(b) {
  b |>
    filter(stage == "merits", !is.na(fell)) |>
    group_by(key) |>
    summarise(pet_n = sum(side == "pet"), pet_fell = sum(side == "pet" & fell == 1),
              resp_n = sum(side == "resp"), resp_stood = sum(side == "resp" & fell == 0),
              .groups = "drop")
}

amicus_stats_from <- function(briefs, fingerprint, terms,
                              top_n = AMICUS_TOP_N, min_rate = AMICUS_MIN_RATE) {
  pet <- attr(briefs, "petitions")
  disp <- .amicus_display(briefs$key, briefs$filer)
  named <- function(d) d |> left_join(disp, by = "key")

  # ---- by docket year
  undecided <- briefs |>
    filter(stage == "merits") |> distinct(term, dkt, fell, dismissed) |>
    group_by(term) |> summarise(undecided = sum(is.na(fell) & !dismissed), .groups = "drop")
  by_term <- briefs |>
    group_by(term) |>
    summarise(briefs = n(), cert = sum(stage == "cert"),
              merits_pet = sum(stage == "merits" & side == "pet"),
              merits_resp = sum(stage == "merits" & side == "resp"),
              merits_cases = n_distinct(dkt[stage == "merits"]),
              filers = n_distinct(key), .groups = "drop") |>
    left_join(undecided, by = "term") |>
    mutate(undecided = coalesce(undecided, 0L),
           per_case = if_else(merits_cases > 0, (merits_pet + merits_resp) / merits_cases, NA_real_)) |>
    arrange(term)

  nonfed <- briefs |> filter(kind != "us")
  # ---- the volume board: organisations and people (States have their own)
  filers <- nonfed |>
    filter(kind == "other") |>
    group_by(key) |>
    summarise(briefs = n(), cert = sum(stage == "cert"),
              merits_pet = sum(stage == "merits" & side == "pet"),
              merits_resp = sum(stage == "merits" & side == "resp"),
              terms = n_distinct(term), .groups = "drop") |>
    arrange(desc(briefs), key) |>
    head(top_n) |> named()

  # ---- the front door: cert-stage briefs on PAID petitions the Court has
  # disposed of. Paid only: an IFP petition almost never draws an amicus and is
  # granted at a fraction of the paid rate, so pooling them set 13.2% with
  # support against 0.6% without -- a comparison of dockets, not of support.
  decided <- pet |> filter(type == "paid", !outcome %in% c("pending", NA))
  door_base <- decided |>
    group_by(cert_amicus) |>
    summarise(n = n(), granted = sum(outcome == "granted"), .groups = "drop") |>
    mutate(rate = granted / n)
  door <- nonfed |>
    filter(stage == "cert") |>
    distinct(key, dkt) |>
    inner_join(decided |> select(dkt, outcome), by = "dkt") |>
    group_by(key) |>
    summarise(n = n(), granted = sum(outcome == "granted"), .groups = "drop") |>
    filter(n >= min_rate) |>
    mutate(rate = granted / n, lo = wilson_lower(granted, n)) |>
    arrange(desc(lo), desc(n)) |>
    head(top_n) |> named()
  door_qualifying <- nonfed |> filter(stage == "cert") |> distinct(key, dkt) |>
    inner_join(decided |> select(dkt), by = "dkt") |> count(key) |>
    filter(n >= min_rate) |> nrow()

  # ---- the merits record, as facts about the case
  record <- .amicus_merits_record(nonfed |> filter(kind == "other")) |>
    mutate(n = pet_n + resp_n) |>
    filter(n >= min_rate) |>
    arrange(desc(n), key) |>
    head(top_n) |> named()
  case_base <- briefs |> filter(stage == "merits", !is.na(fell)) |> distinct(dkt, fell)

  # ---- the United States
  us <- briefs |> filter(kind == "us")
  us_rec <- .amicus_merits_record(us)
  us_out <- list(
    cert = sum(us$stage == "cert"),
    cert_invited = sum(us$stage == "cert" & us$cvsg),
    merits = sum(us$stage == "merits"),
    merits_pet = sum(us$stage == "merits" & us$side == "pet"),
    merits_resp = sum(us$stage == "merits" & us$side == "resp"),
    pet_n = sum(us_rec$pet_n), pet_fell = sum(us_rec$pet_fell),
    resp_n = sum(us_rec$resp_n), resp_stood = sum(us_rec$resp_stood))

  # ---- the States: briefs each led, and the States each explicit list named
  st <- nonfed |> filter(kind == "state")
  lead <- st |>
    mutate(lead = map_chr(amicus_states_named(filer), ~ if (length(.x)) .x[1] else NA_character_)) |>
    filter(!is.na(lead))
  joined <- st |> mutate(s = amicus_states_named(filer)) |>
    filter(lengths(s) > 1) |> select(s) |> unnest(s) |> count(s, name = "named")
  states <- lead |>
    group_by(state = lead) |>
    summarise(led = n(), cert = sum(stage == "cert"),
              merits_pet = sum(stage == "merits" & side == "pet"),
              merits_resp = sum(stage == "merits" & side == "resp"), .groups = "drop") |>
    arrange(desc(led), state) |>
    head(top_n)

  # ---- allies and opponents, on the merits, between frequent filers
  freq <- nonfed |> filter(stage == "merits") |> count(key) |>
    filter(n >= AMICUS_PAIR_FILER_MIN) |> pull(key)
  m <- nonfed |> filter(stage == "merits", key %in% freq) |>
    mutate(case = str_squish(tolower(caption))) |>
    distinct(case, side, key)
  pairs_of <- function(a, b, same) {
    j <- inner_join(a, b, by = "case", suffix = c("1", "2"), relationship = "many-to-many")
    j <- if (same) j |> filter(side1 == side2, key1 < key2) else
      j |> filter(side1 == "pet", side2 == "resp")
    j |> distinct(case, key1, key2) |> count(key1, key2, name = "cases") |>
      filter(cases >= AMICUS_PAIR_MIN) |> arrange(desc(cases), key1, key2) |> head(top_n)
  }
  label <- function(d) d |>
    left_join(disp |> select(key1 = key, name1 = name), by = "key1") |>
    left_join(disp |> select(key2 = key, name2 = name), by = "key2")
  allies <- label(pairs_of(m, m, TRUE))
  opponents <- label(pairs_of(m, m, FALSE))

  # ---- who writes them: the "Other" block, where the refresh carried it
  cov <- briefs |> filter(has_block %in% TRUE)
  firm_key <- function(f) str_squish(str_remove(amicus_key(f), "\\s+(pllc|llp|llc|pc|p c|lp)$"))
  wr <- cov |> filter(!is.na(counsel), nzchar(counsel), kind != "us") |>
    mutate(ck = map_chr(counsel, counsel_key), fk = firm_key(firm),
           # Counsel who work FOR the filer -- Cato's own lawyers on Cato's
           # brief -- are the filer, not outside counsel. Same word test as the
           # case pages' party matcher.
           in_house = map2_lgl(fk, key, ~ nzchar(.x) && (.x == .y || party_score(.y, .x) >= 2)))
  # Not firms: a solo lawyer's title in the firm field ("Attorney at Law", 80
  # rows in the first refresh, merged into one row that ranked 18th), and a
  # State's own lawyers ("Solicitor General", "Office of the Attorney General"),
  # who are its government, kept off as the United States is.
  firms <- wr |> filter(nzchar(coalesce(fk, "")), !in_house,
                        !str_detect(fk, AMICUS_GENERIC_FIRM_RX),
                        !str_detect(fk, AMICUS_GOV_FIRM_RX)) |>
    group_by(fk) |>
    summarise(firm = names(sort(table(firm), decreasing = TRUE))[1],
              briefs = n(), filers = n_distinct(key),
              merits = sum(stage == "merits"), .groups = "drop") |>
    arrange(desc(briefs), fk) |> head(top_n) |> select(-fk)
  counsel <- wr |> filter(nzchar(ck)) |>
    group_by(ck) |>
    summarise(name = names(sort(table(counsel), decreasing = TRUE))[1],
              firm = names(sort(table(firm), decreasing = TRUE))[1] %||% NA_character_,
              briefs = n(), filers = n_distinct(key),
              merits = sum(stage == "merits"), .groups = "drop") |>
    arrange(desc(briefs), ck) |> head(top_n) |> select(-ck)

  list(
    totals = list(
      terms = terms,
      term_from = min(briefs$term), term_to = max(briefs$term),
      briefs = nrow(briefs), cases = n_distinct(briefs$dkt),
      filers = n_distinct(nonfed$key),
      one_off = sum(table(nonfed$key) == 1),
      cert = sum(briefs$stage == "cert"), merits = sum(briefs$stage == "merits"),
      merits_cases = n_distinct(briefs$dkt[briefs$stage == "merits"]),
      state_briefs = nrow(st), us_briefs = nrow(us),
      door_with_n = door_base$n[door_base$cert_amicus] %||% NA,
      door_with_rate = door_base$rate[door_base$cert_amicus] %||% NA,
      door_without_n = door_base$n[!door_base$cert_amicus] %||% NA,
      door_without_rate = door_base$rate[!door_base$cert_amicus] %||% NA,
      door_qualifying = door_qualifying,
      case_fell_rate = if (nrow(case_base)) mean(case_base$fell == 1) else NA_real_,
      cases_decided = nrow(case_base),
      states_named = nrow(joined),
      block_briefs = nrow(cov), block_share = nrow(cov) / nrow(briefs),
      matched = sum(!is.na(cov$counsel) & nzchar(coalesce(cov$counsel, ""))),
      in_house = sum(wr$in_house),
      top_n = top_n, min_rate = min_rate,
      pair_filer_min = AMICUS_PAIR_FILER_MIN, pair_min = AMICUS_PAIR_MIN,
      as_of = as.character(suppressWarnings(max(briefs$date, na.rm = TRUE)))),
    by_term = by_term, filers = filers, door = door, record = record,
    us = us_out, states = states, allies = allies, opponents = opponents,
    firms = firms, counsel = counsel,
    fingerprint = fingerprint)
}

compute_amicus_stats <- function(paths = amicus_input_paths()) {
  amicus_stats_from(amicus_briefs(paths),
                    fingerprint = amicus_stats_fingerprint(paths),
                    terms = counsel_terms(counsel_archive_paths(dirname(paths[1]))))
}

# Everything data/amicus_stats.json is a function of: the archives' bytes and the
# BODIES of every function on the path -- counsel_stats_fingerprint()'s rule, for
# its reason (a stale committed summary is indistinguishable from a fresh one).
amicus_stats_fingerprint <- function(paths = amicus_input_paths()) {
  body_of <- function(f) if (exists(f)) paste(deparse(get(f)), collapse = "\n") else ""
  digest::digest(list(
    logic = lapply(c("amicus_input_paths", "amicus_docket_years", "amicus_filer",
                     "amicus_key", "amicus_is_state", "amicus_states_named", "amicus_kind",
                     "amicus_briefs", "amicus_match_counsel", ".amicus_display",
                     ".amicus_merits_record", "amicus_stats_from",
                     # the case pages' grammar, which decides what a brief is
                     "brief_cover", "resp_merits_brief_on", "docket_granted_on",
                     ".brief_caption_sides", "party_score", "party_side",
                     "judgment_of", "argument_disposition", "petitioner_won",
                     "classify_petition_events", "classify_petitions", "funnel_case_type",
                     "counsel_key", "wilson_lower", "counsel_archive_paths",
                     "counsel_terms"), body_of),
    grammar = list(as.character(AMICUS_ENTRY_RX), as.character(AMICUS_PREFIX_RX),
                   as.character(AMICUS_SUFFIX_RX), AMICUS_US_KEYS, COUNSEL_STATES,
                   as.character(JUDGMENT_ENTRY_RX), as.character(PARTIAL_DISMISSAL_RX),
                   JUDGMENT_RULES, PETITION_TYPES, FUNNEL_PATTERNS, GRANT_FORMS,
                   AMICUS_TOP_N, AMICUS_MIN_RATE, AMICUS_PAIR_FILER_MIN, AMICUS_PAIR_MIN,
                   AMICUS_MAX_VARIANTS, AMICUS_GENERIC_FIRM_RX, AMICUS_GOV_FIRM_RX,
                   AMICUS_REFRESH, COUNSEL_SUFFIXES),
    archives = unname(tools::md5sum(sort(paths)))))
}

# ---- rendering -------------------------------------------------------------------

# The Counsel Table's stylesheet and table helpers, so the two pages of the
# section read as one. Only what is new is added here.
AMICUS_CSS <- paste0(COUNSEL_CSS, "
  .bcrumb{margin:.7rem 0 1.2rem}
  table.ctab .pair{display:block;line-height:1.3}
  table.ctab .vs{display:block;font-size:.78rem;font-style:italic;
    color:var(--faint,@faint@);margin:.1rem 0}
" |> fill_palette())

.amicus_rec <- function(k, n) if (is.null(n) || is.na(n) || n == 0) "&mdash;" else
  paste0("<span class='rec'>", .cn(k), "<span class='of'> of ", .cn(n), "</span></span>")

# Render /counsel/amicus.html from a compute_amicus_stats() list. Returns the path.
render_amicus_page <- function(stats, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tot <- stats$totals
  as_tib <- function(x) if (is.null(x) || !length(x)) tibble() else as_tibble(x)
  filers <- as_tib(stats$filers); by_term <- as_tib(stats$by_term)
  door <- as_tib(stats$door); record <- as_tib(stats$record)
  states <- as_tib(stats$states); allies <- as_tib(stats$allies)
  opponents <- as_tib(stats$opponents); firms <- as_tib(stats$firms)
  counsel <- as_tib(stats$counsel); us <- stats$us
  num <- function(x) if (is.null(x) || !length(x) || is.na(x)) NA_real_ else as.numeric(x)

  sec <- function(over, heading, note, tbl) tagList(
    tags$p(class = "over", over), tags$h2(heading),
    if (!is.null(note)) tags$p(class = "cnote2", HTML(smarten(note))) else NULL, tbl)
  note <- function(x) tags$p(class = "cnote2", HTML(smarten(x)))
  callout <- function(...) tags$div(class = "rates", tagList(...))
  fig <- function(big, lab, sub) tags$div(tags$div(class = "big", HTML(big)),
                                          tags$div(class = "lab", lab),
                                          tags$div(class = "sub", sub))

  t_filers <- .counsel_table(filers, list(
    list(label = "Filer", align = "l", cell = function(i) .name_cell(filers, i, chip = FALSE)),
    list(label = "Briefs", cell = function(i) .cn(filers$briefs[i])),
    list(label = "Cert", cell = function(i) .cn(filers$cert[i])),
    list(label = "Pet./neither", cell = function(i) .cn(filers$merits_pet[i])),
    list(label = "Resp.", cell = function(i) .cn(filers$merits_resp[i])),
    list(label = "Terms", cell = function(i) .cn(filers$terms[i]))))

  t_terms <- if (!nrow(by_term)) NULL else {
    head_cells <- c("Docketed", "Briefs", "Cert", "Pet./neither", "Resp.", "Cases", "Per case")
    rows <- vapply(seq_len(nrow(by_term)), function(i) {
      r <- by_term[i, ]
      flag <- if (r$undecided > 0) paste0("<span class='alias'>", .cn(r$undecided),
                                          " undecided</span>") else ""
      paste0("<tr><td class='l'><span class='nm'>OT", r$term, "</span>", flag, "</td>",
             "<td>", .cn(r$briefs), "</td><td>", .cn(r$cert), "</td>",
             "<td>", .cn(r$merits_pet), "</td><td>", .cn(r$merits_resp), "</td>",
             "<td>", .cn(r$merits_cases), "</td>",
             "<td>", if (is.na(r$per_case)) "&mdash;" else formatC(r$per_case, format = "f", digits = 1),
             "</td></tr>")
    }, character(1))
    HTML(paste0("<div class='ctwrap'><table class='ctab'><thead><tr>",
                paste0("<th class='", c("l", rep("", 6)), "'>", head_cells, "</th>", collapse = ""),
                "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"))
  }

  t_door <- if (!nrow(door)) NULL else {
    dmax <- max(door$rate, na.rm = TRUE)
    .counsel_table(door, list(
      list(label = "Filer", align = "l", cell = function(i) .name_cell(door, i, chip = FALSE)),
      list(label = "Petitions", cell = function(i) .cn(door$n[i])),
      list(label = "Granted", cell = function(i) .cn(door$granted[i])),
      list(label = "Rate", cell = function(i) .bar_cell(door$rate[i], dmax, 0, lo = door$lo[i]))))
  }

  t_record <- .counsel_table(record, list(
    list(label = "Filer", align = "l", cell = function(i) .name_cell(record, i, chip = FALSE)),
    list(label = "Pet./neither: fell", cell = function(i) .amicus_rec(record$pet_fell[i], record$pet_n[i])),
    list(label = "Resp.: stood", cell = function(i) .amicus_rec(record$resp_stood[i], record$resp_n[i]))))

  pair_tbl <- function(d, sep) {
    if (!nrow(d)) return(note("No pair clears the floor."))
    .counsel_table(d, list(
      list(label = "Filers", align = "l", cell = function(i) paste0(
        "<span class='pair'><span class='nm'>", htmlEscape(d$name1[i]), "</span>",
        "<span class='vs'>", sep, "</span>",
        "<span class='nm'>", htmlEscape(d$name2[i]), "</span></span>")),
      list(label = "Cases", cell = function(i) .cn(d$cases[i]))))
  }

  t_states <- .counsel_table(states, list(
    list(label = "State", align = "l", cell = function(i)
      paste0("<span class='nm'>", htmlEscape(states$state[i]), "</span>")),
    list(label = "Led", cell = function(i) .cn(states$led[i])),
    list(label = "Cert", cell = function(i) .cn(states$cert[i])),
    list(label = "Pet./neither", cell = function(i) .cn(states$merits_pet[i])),
    list(label = "Resp.", cell = function(i) .cn(states$merits_resp[i]))))

  has_counsel <- isTRUE(num(tot$block_briefs) > 0) && nrow(firms) > 0
  t_firms <- if (!has_counsel) NULL else .counsel_table(firms, list(
    list(label = "Firm", align = "l", cell = function(i)
      paste0("<span class='nm'>", htmlEscape(firms$firm[i]), "</span>")),
    list(label = "Briefs", cell = function(i) .cn(firms$briefs[i])),
    list(label = "Merits", cell = function(i) .cn(firms$merits[i])),
    list(label = "Clients", cell = function(i) .cn(firms$filers[i]))))
  t_counsel <- if (!has_counsel || !nrow(counsel)) NULL else .counsel_table(counsel, list(
    list(label = "Counsel of record", align = "l", cell = function(i) paste0(
      "<span class='nm'>", htmlEscape(counsel$name[i]), "</span>",
      if (!is.na(counsel$firm[i]) && nzchar(counsel$firm[i]))
        paste0("<span class='alias'>", htmlEscape(counsel$firm[i]), "</span>") else "")),
    list(label = "Briefs", cell = function(i) .cn(counsel$briefs[i])),
    list(label = "Merits", cell = function(i) .cn(counsel$merits[i])),
    list(label = "Clients", cell = function(i) .cn(counsel$filers[i]))))

  span <- paste0("cases docketed OT", tot$term_from, "–OT", tot$term_to)
  body <- tags$body(
    HTML(site_masthead(active = "/counsel/")),
    tags$main(
      id = "main", class = "wrap wide",
      HTML(site_breadcrumb("Amicus filers", list(href = "/counsel/", label = "Counsel"))),
      tags$p(class = "kicker", "Supreme Court of the United States · The Counsel Table"),
      tags$h1("Friends of the Court"),
      tags$hr(class = "brule"),
      tags$p(class = "dek", smarten(paste0(
        "Who files amicus briefs, at which stage, on which side, beside whom — and who ",
        "writes them — across ", span, "."))),
      tags$div(class = "clede", tagList(
        tags$p(HTML(smarten(paste0(
          .cn(tot$briefs), " amicus briefs in ", .cn(tot$cases), " cases, from ",
          .cn(tot$filers), " filers other than the United States. ", .cn(tot$cert),
          " came in at the petition stage, asking the Court to take a case; ",
          .cn(tot$merits), " came after a grant, in ", .cn(tot$merits_cases),
          " cases. As at the lectern, the field is long-tailed: ", .cn(tot$one_off),
          " filers (", .cpct(tot$one_off / tot$filers, 0), ") appear exactly once.")))),
        tags$p(HTML(smarten(paste0(
          "Every brief here is classified by the same rules that colour the timeline on ",
          "each case page, so the two always agree. <a href='/counsel/'>The Counsel ",
          "Table</a> covers the parties&rsquo; own lawyers."))))
      )),

      sec("Volume", "Most briefs filed",
          paste0("Organisations and individuals; the States and the United States have ",
                 "panels of their own below. A filer is credited with the briefs it ",
                 "<em>led</em> — the name the docket gives before &ldquo;et al.&rdquo; — ",
                 "and every spelling that merged into a row is printed beneath it. ",
                 "<em>Pet./neither</em> and <em>Resp.</em> are merits-stage briefs by ",
                 "side; see the note on sides at the foot of the page."),
          t_filers),

      sec("By docket year", "The flow, Term by Term",
          paste0("Cases are dated by the Term they were docketed, not argued: a case ",
                 "granted in January is usually briefed and argued the following Term. ",
                 "<em>Per case</em> is merits briefs per case that drew any. The most ",
                 "recent years are still being briefed, and the undecided count says ",
                 "how far."),
          t_terms),

      tags$p(class = "over", "At the front door"),
      tags$h2("Amicus support before a grant"),
      callout(
        fig(.cpct(num(tot$door_with_rate), 1), "Granted, with amicus support",
            paste0(.cn(tot$door_with_n), " decided petitions")),
        fig(.cpct(num(tot$door_without_rate), 1), "Granted, without",
            paste0(.cn(tot$door_without_n), " decided petitions"))),
      note(paste0(
        "Paid petitions that drew a cert-stage amicus brief from anyone but the United ",
        "States, against those that drew none. <strong>This is not an effect.</strong> ",
        "Amici file where review already looks possible, and a petition worth an ",
        "amicus brief is usually worth a grant for the same reasons; the site&rsquo;s ",
        "<a href='/methods.html'>cert model</a> counts cert-stage amici as one signal ",
        "among many. The board below asks a narrower question: of the petitions each ",
        "filer supported, how many did the Court take?")),
      if (!is.null(t_door)) tagList(
        t_door,
        note(paste0(
          "Filers with at least ", tot$min_rate, " cert-stage briefs on paid petitions the ",
          "Court has acted on — ", .cn(tot$door_qualifying), " of them. Ordered by ",
          "the lower bound of a 95% interval (the muted figure), not the rate.")))
      else note("No filer clears the floor."),

      sec("On the merits", "When the brief came in, how the case came out",
          paste0("Not a win rate. A brief filed on the petitioner&rsquo;s schedule ",
                 "supports the petitioner <em>or neither party</em>, and the docket ",
                 "does not say which, so this table does not score the filer. It ",
                 "reports the case: for briefs on the petitioner&rsquo;s schedule, ",
                 "how often the judgment below fell; on the respondent&rsquo;s, how ",
                 "often it stood. Read it against the Court itself, which reversed or ",
                 "vacated in ", .cpct(num(tot$case_fell_rate), 0), " of the ",
                 .cn(tot$cases_decided), " decided cases here that drew merits amici. ",
                 "Filers with ", tot$min_rate, " or more briefs in decided cases."),
          t_record),

      tags$p(class = "over", "The United States"),
      tags$h2("The Solicitor General as amicus"),
      callout(
        fig(.cn(us$cert), "At the petition stage",
            paste0(.cn(us$cert_invited), " after the Court called for its views")),
        fig(.amicus_rec(us$pet_fell, us$pet_n), "Pet./neither: judgment fell", "decided cases"),
        fig(.amicus_rec(us$resp_stood, us$resp_n), "Resp.: judgment stood", "decided cases")),
      note(paste0(
        "Kept off every board above. At the petition stage its brief is the answer to ",
        "a call for the views of the Solicitor General — the Court asked for it — and ",
        "on the merits it is the office that also chooses which cases the government ",
        "joins at all. ", .cn(us$merits), " merits briefs: ", .cn(us$merits_pet),
        " on the petitioner&rsquo;s schedule, ", .cn(us$merits_resp),
        " on the respondent&rsquo;s.")),

      tags$p(class = "over", "Coalitions"),
      tags$h2("Allies and opponents"),
      note(paste0(
        "Pairs of frequent filers — ", tot$pair_filer_min, " or more merits briefs ",
        "each — that met in at least ", tot$pair_min, " cases. Companion cases count ",
        "once. A brief on the petitioner&rsquo;s schedule may support neither party, ",
        "so an &ldquo;opponent&rdquo; pairing is a pairing of schedules, not always ",
        "of positions.")),
      tags$h3("Most often on the same side"),
      pair_tbl(allies, "with"),
      tags$h3("Most often on opposite sides"),
      pair_tbl(opponents, "petitioner&rsquo;s schedule &middot; respondent&rsquo;s schedule"),

      sec("The States", "Briefs the States led",
          paste0(.cn(tot$state_briefs), " briefs were filed by States, nearly always ",
                 "together. Each is credited to the State the docket names first — the ",
                 "co-signers of &ldquo;West Virginia, et al.&rdquo; are on the brief&rsquo;s ",
                 "cover, not the docket."),
          t_states),

      tags$p(class = "over", "Who writes them"),
      tags$h2("Counsel for the amici"),
      if (has_counsel) tagList(
        note(paste0(
          "From the counsel of record the Court lists for each amicus party, matched ",
          "to ", .cn(tot$matched), " of ", .cn(tot$block_briefs), " briefs. Lawyers ",
          "who work for the filer itself — an institute&rsquo;s own litigators on ",
          "the institute&rsquo;s brief — are not outside counsel and are left off the ",
          "firm table (", .cn(tot$in_house), " briefs), as are the States&rsquo; own ",
          "solicitors general and a solo lawyer&rsquo;s &ldquo;Attorney at ",
          "Law&rdquo;, which names no firm. The counsel table below keeps everyone.")),
        tags$h3("Firms"), t_firms,
        if (!is.null(t_counsel)) tagList(tags$h3("Counsel of record"), t_counsel) else NULL)
      else note(paste0(
        "Not yet published. The Court lists each amicus party&rsquo;s counsel and firm ",
        "in the docket, and this page reads them from a re-fetch of the archived ",
        "dockets that has not yet run.")),

      tags$section(class = "method", tagList(
        tags$h2("How to read this"),
        tags$p(HTML(smarten(paste0(
          "<strong>Sides come from the calendar.</strong> The docket rarely says which ",
          "side an amicus supports. Rule 37 does, by deadline: briefs supporting the ",
          "petitioner or neither party are due a week after the petitioner&rsquo;s ",
          "merits brief, briefs supporting the respondent a week after the ",
          "respondent&rsquo;s. Where a brief&rsquo;s file name happens to state its ",
          "side, the deadline agrees on 97% of respondent-side briefs; about one in six ",
          "on the petitioner&rsquo;s schedule turns out to support neither party.")))),
        tags$p(HTML(smarten(paste0(
          "<strong>A filer is a name.</strong> Rows are keyed on the docket&rsquo;s ",
          "spelling with case, punctuation, a leading &ldquo;The&rdquo; and a ",
          "corporate suffix set aside. Two different filers with the same name ",
          "would merge; one filer that changes its name would split.")))),
        tags$p(HTML(smarten(paste0(
          "Paid and in forma pauperis petitions docketed OT", tot$term_from, "–OT",
          tot$term_to, ", with briefs through ",
          str_squish(format(as.Date(tot$as_of), "%B %e, %Y")), ". Briefs on emergency ",
          "applications and in original actions are not counted. A brief filed in ",
          "consolidated cases counts once. Rejected filings, motions for leave and the ",
          "Court-appointed amicus defending a judgment below are not amicus briefs here."))))
      )),
      tags$p(class = "back", HTML("<a href='/counsel/'>&larr; The Counsel Table</a>"))
    ))

  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head("Friends of the Court — Supreme Court Report",
              jsonld = site_breadcrumb_jsonld("Amicus filers",
                                              list(href = "/counsel/", label = "Counsel")),
              extra_css = AMICUS_CSS,
              description = paste(
                "Who files amicus briefs at the Supreme Court: the most active filers,",
                "the petitions they supported, the coalitions they form, the States,",
                "and the lawyers who write the briefs."),
              path = "/counsel/amicus.html"), "\n",
    as.character(body), "\n</html>\n")
  out <- file.path(out_dir, "amicus.html")
  writeLines(enc2utf8(smarten_html(html)), out, useBytes = TRUE)
  invisible(out)
}
