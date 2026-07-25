# cert_model.R -----------------------------------------------------------------
# A calibrated model of the probability that a paid petition for certiorari is
# GRANTED plenary review. Phase 0 assembles a labeled corpus and computes
# leakage-safe features; Phase 1 fits an interpretable logistic model, evaluates
# it out-of-time, calibrates it, and explains each prediction as a set of cues.
#
# Design decisions (see the feature-notes README for the rationale):
#   * Target      : outcome == "granted" (plenary merits grant). GVRs, Rule 46
#                   dismissals, and still-pending petitions are EXCLUDED from
#                   training -- the model contrasts clean grants against clean
#                   denials, which is what "likelihood of cert" means to a
#                   reader. At inference it scores any petition.
#   * Segment     : PAID petitions only. IFP grant rates are ~0.1%, a different
#                   regime; pooling lets the easy IFP negatives wash out the
#                   paid signal. IFP is modeled separately (or not surfaced).
#   * Leakage     : every process feature (relists, cert-stage amicus, CVSG,
#                   response events) is snapshotted STRICTLY BEFORE the decision
#                   date. This is essential: merits-stage amicus briefs are
#                   filed AFTER a grant, so counting them over the whole docket
#                   would let the model "learn" that grants cause amicus briefs.
#   * Two tiers   : the BASELINE model (daily dashboards) uses only structural
#                   features known at the petition stage; the ENHANCED model
#                   (conference reports, Phase 2) adds the process features.
#
# Labels and the relist/amicus/CFR machinery come from classify_petitions() in
# cert_funnel.R, which must be sourced before this file is used.

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---- A. party / entity typing -------------------------------------------------

# The petitioner and respondent captions carry most of the entity signal. We
# split the caption on " v. " and classify each side into a coarse type; raw
# party names are far too high-cardinality to use directly.
US_FED_RX <- str_c(
  "\\bUnited States\\b",
  # NOTE: the bare "Secretary|Commissioner|Administrator|..." alternative that
  # used to live here has moved to BARE_FED_TITLE_RX, which classify_entity()
  # applies only AFTER the state patterns. Tested first, it typed state officers
  # and private estate administrators as the federal government.
  "|\\bAttorney General of the United States\\b",
  "|\\b(National Labor Relations Board|NLRB|Securities and Exchange Commission|SEC",
  "|Environmental Protection Agency|EPA|Federal (Trade|Communications|Energy|Election|Maritime|Deposit) ",
  "|FTC|FCC|FDIC|Internal Revenue Service|Commissioner of Internal Revenue",
  "|Food and Drug Administration|Immigration and Naturalization|Citizenship and Immigration",
  "|Federal Bureau|Department of (Justice|Labor|State|Defense|Homeland Security|Health",
  "|Education|Agriculture|Commerce|Treasury|the Interior|Transportation|Veterans|Energy|Housing))\\b"
)
STATE_RX <- str_c(
  "\\b(State|Commonwealth) of [A-Z]",
  "|\\bPeople of the State\\b",
  "|^(State|Commonwealth|People) (of|ex rel)\\b",
  "|^(City|County|Town|Village|Borough|Township|Parish|Board of Education) of\\b",
  "|\\bGovernor of\\b|\\bAttorney General of [A-Z]",
  "|\\bDepartment of Corrections\\b|\\bWarden\\b|\\bSheriff\\b",
  "|\\b(School District|Board of Education|Housing Authority|Transit Authority)\\b"
)
BUSINESS_RX <- str_c(
  "\\b(Inc|L\\.?L\\.?C|Corp|Corporation|Co|Company|Ltd|L\\.?P|LLP|N\\.A|PLLC",
  "|Bank|Bancorp|Group|Holdings|Industries|Systems|Technologies|Pharmaceuticals",
  "|Laboratories|Airlines|Insurance|Mutual|Partners|Associates|Enterprises",
  "|Motors|Financial|Capital|Services|Communications|Networks|Solutions)\\b\\.?"
)

# A caption side that is nothing but a State's name ("Texas", "New York") -- the
# single largest source of mistyping. 901 paid petitions had a respondent that is
# a bare state name and were typed `individual`.
STATE_NAMES <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
  "Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois",
  "Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland",
  "Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana",
  "Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York",
  "North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
  "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
  "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming",
  "District of Columbia","Puerto Rico","Guam")
BARE_STATE_RX <- str_c("^(", str_c(STATE_NAMES, collapse = "|"), ")$")

# A state officer or agency: an office title carrying a State's name, either way
# round ("Arkansas Secretary of State", "Connecticut Commissioner of Revenue
# Services", "Tennessee Commissioner of Transportation", "Attorney General of
# Texas"). Without this the bare-title branch below claims them for the federal
# government, which is a +2.89 log-odds error on the petitioner side.
.states_alt <- str_c(STATE_NAMES, collapse = "|")
.offices <- str_c("Secretary|Commissioner|Attorney General|Administrator|Director",
                  "|Treasurer|Auditor|Comptroller|Superintendent|Board|Department",
                  "|Division|Bureau|Agency|Authority|Council|Commission")
STATE_OFFICER_RX <- str_c(
  "\\b(", .states_alt, ")\\b[^,]{0,40}\\b(", .offices, ")\\b",
  "|\\b(", .offices, ")\\b[^,]{0,30} of (the )?\\b(", .states_alt, ")\\b")

# Federal office titles that are NOT self-identifying: "Attorney General",
# "Secretary", "Commissioner" name state officers as often as federal ones
# ("Arkansas Secretary of State", "Connecticut Commissioner of Revenue
# Services"). They are therefore tested AFTER the state patterns, not before --
# the old single-pass order sent 126 of the 412 sides it typed on a bare title
# to the wrong government (30.6%), including 48 private estate administrators.
BARE_FED_TITLE_RX <- str_c(
  "\\b(Attorney General|Secretary|Commissioner|Administrator|Comptroller",
  "|Postmaster General|Director|Acting Secretary|Acting Attorney General)\\b")
# ...and these are never a government office at all.
NOT_AN_OFFICER_RX <- str_c(
  "(?i)\\bAdministrat(or|rix) (of|for) the Estate\\b|(?i)\\bAdministrator Ad Litem\\b",
  "|(?i)\\bPersonal Representative\\b|(?i)\\bSpecial Administrator\\b")

classify_entity <- function(name) {
  name <- name %||% ""
  name[is.na(name)] <- ""
  nm <- str_squish(name)
  # Order is load-bearing. Unambiguous federal first ("United States", named
  # agencies); then state/local, which claims the ambiguous titles when they
  # carry a state marker; then bare federal titles; then business; then people.
  dplyr::case_when(
    str_detect(nm, US_FED_RX)                          ~ "us_fed",
    str_detect(nm, STATE_RX) | str_detect(nm, BARE_STATE_RX) |
      str_detect(nm, STATE_OFFICER_RX)                 ~ "state_local",
    str_detect(nm, NOT_AN_OFFICER_RX)                  ~ "individual",
    str_detect(nm, BARE_FED_TITLE_RX)                  ~ "us_fed",
    str_detect(nm, BUSINESS_RX)                        ~ "business",
    nzchar(nm)                                         ~ "individual",
    TRUE                                               ~ "other"
  )
}

# petitioner / respondent name strings from a caption ("A, B v. C").
caption_sides <- function(caption) {
  caption <- caption %||% ""
  parts <- str_split_fixed(caption, "\\s+v\\.?\\s+", 2)
  list(pet = str_squish(parts[, 1]), resp = str_squish(parts[, 2]))
}

# ---- B. court below -----------------------------------------------------------

CIRCUIT_WORDS <- c(First="CA1", Second="CA2", Third="CA3", Fourth="CA4",
                   Fifth="CA5", Sixth="CA6", Seventh="CA7", Eighth="CA8",
                   Ninth="CA9", Tenth="CA10", Eleventh="CA11")

# Bucket the LowerCourt string into a modest set of levels: the 13 federal
# courts of appeals, a pooled STATE bucket, other federal courts, and OTHER.
court_bucket <- function(lower) {
  lower <- lower %||% ""
  lower[is.na(lower)] <- ""
  out <- rep("OTHER", length(lower))
  for (w in names(CIRCUIT_WORDS)) {
    hit <- str_detect(lower, str_c("\\b", w, " Circuit\\b"))
    out[hit] <- CIRCUIT_WORDS[[w]]
  }
  out[str_detect(lower, "District of Columbia Circuit")] <- "CADC"
  out[str_detect(lower, "Federal Circuit")]              <- "CAFED"
  # state courts: "Supreme Court of X", "Court of Appeals of X", "X Court of
  # Appeal(s)", "Superior Court", "Court of Criminal Appeals", etc. Anything not
  # a federal court of appeals but clearly a state tribunal.
  is_state <- str_detect(lower, str_c(
    "Supreme (Court|Judicial Court) of |Court of Appeals? of |Court of Criminal Appeals",
    "|Superior Court|Appellate (Court|Division)|Supreme Court, Appellate",
    "|Court of Special Appeals|Commonwealth Court|District Court of Appeal"))
  out[out == "OTHER" & is_state] <- "STATE"
  # De-pooled from the old single FED_OTHER level, which pooled three populations
  # that behave nothing alike. A split refit: CAAF +0.97 (SE 0.79 -- statistically
  # indistinguishable from a state court) against US district courts +6.57, whose
  # rows are cert-before-judgment cases the Court takes at extraordinary rates.
  # Pooled, they shared a +3.22 coefficient -- the largest court term in the model
  # -- so every military-justice petition published ~9.6% where ~1.6-2.2% is honest.
  out[out == "OTHER" & str_detect(lower, "Court of Appeals for the Armed Forces")] <- "CAAF"
  # Anchored to the federal name on purpose: a bare "District Court" test routed
  # state trial courts ("District Court of Colorado, Denver County", "38th
  # Judicial District Court of Louisiana, Cameron Parish") into the federal
  # bucket and handed them the cert-before-judgment coefficient.
  out[out == "OTHER" & str_detect(lower, "(?i)\\b(United States|U\\.? ?S\\.?) District Court")] <- "USDC"
  out[out == "OTHER" & str_detect(lower, "Court of Federal Claims|Tax Court|Court of International Trade")] <- "FED_SPEC"
  # Whatever still looks like a county/parish trial court is a state court.
  out[out == "OTHER" & str_detect(lower, "(?i)\\b(County|Parish)\\b")] <- "STATE"
  # Anything left is genuinely OTHER -- overwhelmingly "In re __" original writs
  # with no court below. That level is now kept (not lumped into STATE); see
  # model_frame().
  out
}

# ---- C. counsel (elite Supreme Court bar) -------------------------------------

# A curated set of firms and named advocates with well-above-baseline cert
# success. Matched against the petitioner's counsel-of-record firm and attorney
# name. Not exhaustive; extend as the data warrants.
# Firms with well-above-baseline cert success. NOT CURRENTLY REACHED: the term
# archives carry no firm column, so petitioner_counsel() returns an attorney
# name only, and matching firms at serve time but never in training is precisely
# the train/serve asymmetry that killed this feature once already. Kept for the
# day the archives are re-fetched with the live parties schema.
ELITE_FIRM_RX <- str_c(
  "Clement & Murphy|Gibson,? Dunn|Jones Day|Williams & Connolly|Mayer Brown",
  "|Hogan Lovells|Kirkland & Ellis|Latham & Watkins|Sidley Austin|Munger,? Tolles",
  "|WilmerHale|Wilmer Cutler|O'Melveny|Arnold & Porter|MoloLamken|Paul,? Weiss",
  "|Sullivan & Cromwell|Cravath|Covington|Susman Godfrey|Orrick|Morrison & Foerster",
  "|Vinson & Elkins|King & Spalding|Quinn Emanuel|Skadden|Dechert|Gupta Wessler",
  "|Consovoy|Cooper & Kirk|Boies Schiller|Bancroft")

# Named advocates, who move between firms. Common surnames MUST carry a given
# name: matching bare "Goldstein|Clement|Phillips|Fisher|Gupta" collided with
# unrelated counsel on 34 of 346 archive hits (9.8%), none of which won a grant
# -- e.g. a Philadelphia criminal-defense "Zak Taylor Goldstein" scoring as Tom
# Goldstein. Distinctive surnames stay bare.
ELITE_ADVOCATE_RX <- str_c(
  "\\b(Thomas C\\.?|Tom) Goldstein\\b",
  "|\\bPaul (D\\.? )?Clement\\b",
  "|\\bCarter (G\\.? )?Phillips\\b",
  "|\\b(Jeffrey L\\.?|Jeffrey|Thomas M\\.?|Thomas) Fisher\\b",
  "|\\bDeepak Gupta\\b",
  "|\\bKatyal\\b|\\bWaxman\\b|\\bShanmugam\\b|\\bGornstein\\b|\\bDreeben\\b")

ELITE_COUNSEL_RX <- ELITE_ADVOCATE_RX

# Pull the petitioner's counsel-of-record firm + attorney from the parties
# tibble (may be empty). Returns "" when unknown.
# Petitioner's counsel-of-record NAME. Two parties shapes exist in this pipeline
# and both must work:
#   live JSON (build_party_side, scotus_dash_new.R): type, names, attys, firm,
#       counsel_of_record  -- one row per attorney, CoR flagged by a logical
#   archives (data-raw/ot_*.rds):  party, names, attys  -- one row per side, no
#       firm, and the CoR marker is embedded in the attorney string:
#       "Lauren M. Burke    Counsel of Record, Second Attorney"
# Reading only the live shape returned "" for all 48,985 archive dockets, which
# made elite_counsel constant FALSE across the whole training corpus, aliased its
# coefficient to NA, and silently zeroed the feature at serve time.
#
# Deliberately NAME-ONLY, not firm + name. The archives carry no firm column, so
# a firm-matching cue could fire at serve time on a pattern the model had never
# seen in training -- exactly the train/serve asymmetry that caused the original
# bug. Firm matching can return once the archives carry firms.
# The petitioner's row(s), the counsel-of-record name, and the party's own name,
# resolved from either parties shape. Shared by petitioner_counsel() and
# petitioner_pro_se() so the two can never disagree about which row is which.
petitioner_side <- function(parties) {
  none <- list(counsel = "", party = "")
  if (!is.data.frame(parties) || nrow(parties) == 0) return(none)
  role <- if ("type" %in% names(parties)) parties$type
          else if ("party" %in% names(parties)) parties$party else NULL
  if (is.null(role)) return(none)
  role <- as.character(role); role[is.na(role)] <- ""
  p <- parties[str_detect(role, regex("petition|appellant|applicant",
                                      ignore_case = TRUE)), , drop = FALSE]
  if (nrow(p) == 0 || !("attys" %in% names(p))) return(none)
  atts <- vapply(seq_len(nrow(p)), function(k) {
    a <- p$attys[[k]]
    if (is.null(a) || length(a) == 0) "" else
      paste(unlist(a, use.names = FALSE), collapse = ", ")
  }, character(1))
  atts[is.na(atts)] <- ""
  i <- if ("counsel_of_record" %in% names(p)) which(p$counsel_of_record %in% TRUE)
       else which(str_detect(atts, regex("counsel of record", ignore_case = TRUE)))
  i <- if (length(i)) i[[1]] else 1L
  pn <- if ("names" %in% names(p)) {
    v <- p$names[[i]]
    if (is.null(v) || length(v) == 0) "" else paste(unlist(v, use.names = FALSE), collapse = ", ")
  } else ""
  list(counsel = str_squish(str_remove(atts[i],
         regex("\\s*Counsel of Record.*$", ignore_case = TRUE))),
       party = str_squish(pn %||% ""))
}

# Self-represented petitioner: the counsel of record is (essentially) the party.
# Mirrors is_pro_se() in scotus_dash_new.R, reimplemented here because the term
# archives carry no `pro_se` column and cert_model.R does not source the fetcher.
# 0 grants in 3,068 paid pro se petitions across all eight terms, so the fitted
# effect needs the Firth penalty to stay finite.
.name_tokens <- function(x) {
  t <- str_split(str_squish(str_replace_all(str_to_lower(x %||% ""), "[^a-z ]", " ")), " ")[[1]]
  t[nchar(t) > 1]
}
petitioner_pro_se <- function(parties) {
  s <- petitioner_side(parties)
  pn <- .name_tokens(s$party); at <- .name_tokens(s$counsel)
  if (length(pn) == 0 || length(at) == 0) return(FALSE)
  all(pn %in% at) || all(at %in% pn)
}

petitioner_counsel <- function(parties) {
  petitioner_side(parties)$counsel
}

# ---- C2. counsel track record -------------------------------------------------

# A matching key for an advocate: first and last name tokens only. Middle names
# and initials are inconsistent across dockets for the same person ("Neal K.
# Katyal" and "Neal Kumar Katyal" both appear in the archives), and a full-string
# key would treat them as two advocates.
counsel_key <- function(name) {
  t <- .name_tokens(name)
  if (length(t) == 0) return("")
  if (length(t) == 1) return(t[[1]])
  paste(t[[1]], t[[length(t)]])
}

# An expanding-window record of what each advocate had done BEFORE a given date.
# This replaces the fixed list of ~13 famous names that elite_counsel matched:
# "counsel who has filed here before, and won" is the signal that list was a poor
# proxy for. Grant rate by tier: new 1.8% / some 3.8% / vet 6.3% / won 21.4%.
#
# Leakage discipline is the whole game here. `dates` are docketing dates, so a
# petition only ever counts advocates' EARLIER filings; `grant_dates` are
# disposition dates, so a grant only counts once it has actually happened. The
# difference is not academic -- scoring against the full sample instead lifts
# forward AUC from 0.882 to 0.963, which is the signature of leakage, not skill.
build_counsel_index <- function(corpus) {
  corpus |>
    filter(type == "paid", !is.na(counsel_key), nzchar(counsel_key)) |>
    group_by(counsel_key) |>
    summarise(
      dates = list(sort(as.Date(date[!is.na(date)]))),
      grant_dates = list(sort(as.Date(
        outcome_date[outcome == "granted" & !is.na(outcome_date)]))),
      .groups = "drop")
}

# Tier for one (advocate, as-of date) pair, counting strictly before as_of.
counsel_tier <- function(keys, as_of, index) {
  if (is.null(index) || nrow(index) == 0)
    stop("counsel_tier(): no counsel index supplied. Scoring without it would ",
         "silently rate every advocate as first-time and quietly revert the ",
         "model to its pre-2026-07 performance.", call. = FALSE)
  keys <- as.character(keys)
  keys[is.na(keys) | !nzchar(keys)] <- NA_character_
  j <- match(keys, index$counsel_key)      # NA for unknown; `[[` would error
  as_of <- as.Date(as_of)
  if (length(as_of) == 1L) as_of <- rep(as_of, length(keys))
  vapply(seq_along(keys), function(i) {
    if (is.na(j[i]) || is.na(as_of[i])) return("new")
    n_prior <- sum(index$dates[[j[i]]] < as_of[i])
    n_won   <- sum(index$grant_dates[[j[i]]] < as_of[i])
    if (n_won > 0) "won" else if (n_prior >= 5) "vet" else if (n_prior >= 1) "some" else "new"
  }, character(1))
}


# ---- D. feature extraction ----------------------------------------------------

# Structural features known at the petition stage (no docket-development
# signal). One row of predictors from one case record.
petition_features <- function(caption, lower, parties, date, lower_date, related) {
  sides <- caption_sides(caption)
  counsel <- petitioner_counsel(parties)
  # NA-safe: missing/blank counsel makes str_detect() return NA, which drops the
  # whole row in model.matrix (score_case then fails with "subscript out of
  # bounds"). Absence of counsel data means "not elite", not unknown.
  elite <- str_detect(counsel, ELITE_COUNSEL_RX); elite <- !is.na(elite) & elite
  pet_type <- classify_entity(sides$pet)
  resp_type <- classify_entity(sides$resp)
  d_gap <- suppressWarnings(as.integer(as.Date(date) - as.Date(lower_date)))
  g <- if (length(d_gap) == 0 || is.na(d_gap)) NA_integer_ else d_gap
  # Days from the judgment below to docketing. Rule 13 gives 90 days, so filing
  # fast is a deliberate choice and a strong cue: petitions docketed within 60
  # days are granted at 22.0% against ~3% past 90 days, and that 2.8% of rows
  # holds 13.9% of all grants. Entered as a HINGE rather than a linear term or a
  # spline -- the effect is a cliff at ~120 days, a linear term would extrapolate
  # the 2,400-day tail, and a spline reads 42% at gap 0. Winsorised at 400 days;
  # a missing date is imputed to 160 (i.e. no boost) and flagged, because
  # model_frame() ends in drop_na() and would otherwise silently delete the row.
  gap_c <- if (is.na(g)) 160 else pmin(pmax(g, 0), 400)
  tibble(
    pet_type       = pet_type,
    resp_type      = resp_type,
    us_petitioner  = pet_type == "us_fed",
    us_respondent  = resp_type == "us_fed",
    business_pet   = pet_type == "business",
    court_below    = court_bucket(lower),
    elite_counsel  = elite,
    # Not a model feature itself -- the key the counsel index is looked up by.
    # counsel_tier is attached afterwards, once the index exists.
    counsel_key    = counsel_key(counsel),
    pro_se         = petitioner_pro_se(parties),
    days_lower_gap = g,
    gap_fast       = pmax(0, 120 - gap_c) / 30,
    gap_na         = is.na(g),
    # NA-safe: nzchar(NA) is TRUE, which silently made this a constant on the
    # historical archives (they carry no `related` column).
    related_present = !is.na(related) && nzchar(str_squish(related %||% ""))
  )
}

# Process features, snapshotted STRICTLY BEFORE `as_of` (the decision date in
# training; a conference date at inference). Counts are leakage-safe by date.
CVSG_RX  <- "Solicitor General is invited"
AMICUS_RX <- "^Brief (amicus|amici) curiae"
process_features <- function(events, as_of) {
  empty <- tibble(n_amicus_cert = 0L, cvsg = FALSE,
                  response_requested = FALSE, response_filed = FALSE,
                  resp_waiver = FALSE, reply_filed = FALSE)
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events)) ||
      nrow(events) == 0) return(empty)
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  edate <- suppressWarnings(lubridate::mdy(events$Date))
  keep <- !is.na(edate) & edate < as_of              # strictly before the decision
  txt <- txt[keep]
  tibble(
    n_amicus_cert      = sum(str_detect(txt, AMICUS_RX)),
    cvsg               = any(str_detect(txt, CVSG_RX)),
    response_requested = any(str_detect(txt, "^Response Requested")),
    response_filed     = any(str_detect(txt, "in opposition filed")),
    # The respondent declining to respond at all. `response_filed == FALSE` was
    # conflating two completely different postures: a waived response is granted
    # at 0.05% (3 of 5,455), a silent docket with no waiver at 4.4% (73 of 1,655).
    resp_waiver        = any(str_detect(txt, "^Waiver of right of respondent")),
    # The petitioner's reply brief -- the single strongest process signal found
    # anywhere in this review, and stable in all eight terms: P(grant | reply)
    # 11.9% vs 0.62% without. Counsel who expect to be relisted write a reply.
    reply_filed        = any(str_detect(txt, "^Reply of (petitioner|applicant|appellant)"))
  )
}

# Relists enter the model as a BUCKET, not a linear count: the grant rate is
# non-monotonic in relists, and a linear term would extrapolate a 20-relist hold
# to ~99%. Bucketing gives each level its own effect and caps the ambiguous tail.
#
# Rates on the current basis (OT2017-24, paid, resolved):
#     0: 1.3%   1: 42.2%   2: 30.8%   3-4: 26.4%   5+: 15.4%
#
# Three separate corrections had to land before those numbers meant anything, and
# every one of them moves this table -- so do NOT quote it without checking which
# basis you are on. train_cert_model.R reprints it at every retrain; trust that
# over this comment.
#   * relists counted strictly before the disposition (assemble_term()), not over
#     the whole docket -- ~6% of petitions, almost all denials, pick up a
#     post-denial rehearing redistribution that used to count as a relist;
#   * the full resolved denominator (set_target()), not granted-or-denied only;
#   * a CVSG redistribution is mechanical, not a relist (classify_petition_events()).
# For reference, the figures published before 2026-07 -- none of the three
# applied -- were 1.3/20.0/43.8/36.2/18.6%.
relist_bucket <- function(n) {
  as.character(cut(n, breaks = c(-1, 0, 1, 2, 4, Inf),
                   labels = c("0", "1", "2", "3-4", "5+")))
}

# Cert-stage amicus gets the same treatment, and for the same reason. The raw
# count entered the logit linearly at +0.205 per brief on a distribution that is
# 82% zeros and concave in the log-odds, so a 25-amicus petition collected
# +5.13 log-odds from this term alone and saturated the forecast. The effect is
# really a step at "any amicus at all" and then a slow rise: observed grant rate
# 1.9% at 0 briefs, 11.4% at 1, 17.0% at 2, 17.1% at 3-4, 22.7% at 5-9. Bucketing
# beats the linear form decisively (AIC 2335 vs 2374; LR 44.95, p=9.5e-10) and,
# unlike log1p or sqrt, each level is nameable in the cue explainer.
amicus_bucket <- function(n) {
  as.character(cut(n, breaks = c(-1, 0, 1, 2, 4, Inf),
                   labels = c("0", "1", "2", "3-4", "5+")))
}

# "Hold" detector. A petition deferred conference after conference -- far beyond
# the 1-3 relists of a case under active grant consideration -- is being HELD,
# typically pending a lead case on the same question. Empirically (OT2017-24,
# paid, >=6 relists) a held petition resolves ~15% granted, ~20% GVR'd, ~65%
# denied: a hold predicts DEFERRAL, most often toward a GVR, not a plenary grant.
# So `held` is exposed as a status flag and a GVR-risk signal, NOT as something
# that raises the grant probability. Leakage-safe: pass relists counted < as_of.
#
# Two tiers: (1) serial relisting >= threshold; (2) the definitional signal --
# the petition is expressly linked (docket "Vide, NN-NNN" companion references)
# to a case already GRANTED as of the conference. Tier 2 is the sharper signal
# for companion grants; it needs the `related` field (live JSON pipeline only --
# the training archives lack it) and the set of dockets granted so far, which a
# conference renderer has on hand.
hold_signal <- function(n_relists, related = NULL, granted_dockets = character(),
                        threshold = 6L) {
  linked <- related_companions(related)
  linked_grant <- length(intersect(linked, granted_dockets)) > 0
  (n_relists >= threshold) | linked_grant
}

# Companion docket numbers referenced in a petition's `related` field (e.g.
# "Vide, 25-566" -> "25-566"). The Court uses "Vide" (Latin: see) to cross-link
# companion petitions processed together.
related_companions <- function(related) {
  if (is.null(related) || length(related) == 0 || is.na(related[1]) ||
      !nzchar(related[1])) return(character())
  unique(str_extract_all(related[1], "\\b\\d{2}-\\d{1,5}\\b")[[1]])
}

# ---- E. corpus assembly -------------------------------------------------------

# Read one term's case tibble, classify every petition, recover the party/court
# columns the classifier drops, and attach structural + process features. The
# process features are snapshotted as of each petition's decision date so the
# same corpus serves both the baseline (structural) and enhanced (all) models.
assemble_term <- function(path) {
  cases <- readRDS(path)
  cls <- classify_petitions(cases)                    # dkt,type,date,caption,outcome,...
  # recover parties / lower / lower_date / related / events by docket
  extra <- cases |>
    transmute(dkt,
              lower = if ("lower" %in% names(cases)) lower else NA_character_,
              lower_date = if ("lower_date" %in% names(cases)) lower_date else as.Date(NA),
              related = if ("related" %in% names(cases)) related else NA_character_,
              parties = if ("parties" %in% names(cases)) parties else vector("list", n()),
              events = events)
  df <- cls |> left_join(extra, by = "dkt")

  # Distribution counts must be snapshotted on the SAME clock as inference.
  # classify_petitions() returns WHOLE-DOCKET counts, which include
  # redistributions entered AFTER the disposition: a denied petition whose
  # counsel files for rehearing picks up a fresh "DISTRIBUTED for Conference"
  # weeks later. That entry is a consequence of the denial, so it can never
  # exist on a live petition -- but score_case() counts relists strictly before
  # its as-of date, so training and serving were reading different quantities.
  # Measured over OT2017-24: 695 of 11,368 paid decided petitions (6.1%) were
  # affected, 685 of them denials, which pushed 671 zero-relist denials into the
  # one-relist training cell and halved its apparent grant rate (45.1% -> 20.0%).
  # Count strictly before the disposition, mirroring score_case()'s `< as_of`.
  snap <- function(dates, as_of) {
    if (length(dates) == 0) return(0L)
    if (is.na(as_of)) return(sum(!is.na(dates)))   # still pending: everything so far
    sum(!is.na(dates) & dates < as_of)
  }
  df$n_relists <- purrr::map2_int(df$relist_dates, df$outcome_date, snap)
  df$n_dist    <- purrr::map2_int(df$dist_dates,   df$outcome_date, snap)

  feats <- purrr::pmap_dfr(
    list(df$caption, df$lower, df$parties, df$date, df$lower_date, df$related),
    petition_features)
  proc <- purrr::map2_dfr(
    df$events, df$outcome_date,
    function(ev, od) process_features(ev, if (is.na(od)) as.Date("2999-01-01") else od))

  bind_cols(
    df |> select(dkt, type, term, caption, date, outcome, outcome_date,
                 n_dist, n_relists, has_cfr, has_resp, has_amicus),
    feats, proc)
}

# ---- E2. the at-risk (petition x conference) panel ----------------------------

# One row per (paid petition, conference it was distributed for), with the
# process features snapshotted as of THAT conference and the label set to the
# petition's EVENTUAL outcome.
#
# This is the frame the conference-stage models must be fitted on. The
# disposition frame assemble_corpus() builds answers "given the Court acts on
# this petition at this conference, is it a grant?" -- which is not the question
# a conference reader is asking, and whose answer is ~2.6x too low at a
# petition's first conference (published 1.6% against 4.1% eventually granted).
# The serving code already produces exactly these rows: conference_dash.R scores
# every distributed petition as-of the conference date. So this aligns the
# training unit with the serving unit and nothing downstream changes --
# score_case(), score_disposition() and both renderers are untouched.
#
# Rows dated after the petition's own disposition are dropped (they are rehearing
# redistributions), as are still-pending petitions, which have no eventual label.
assemble_at_risk_term <- function(path) {
  cases <- readRDS(path) |> mutate(type = funnel_case_type(dkt)) |> filter(type == "paid")
  if (nrow(cases) == 0) return(NULL)
  col <- function(nm, def) if (nm %in% names(cases)) cases[[nm]] else rep(def, nrow(cases))
  lower <- col("lower", NA_character_); lower_date <- col("lower_date", as.Date(NA))
  related <- col("related", NA_character_)
  parties <- if ("parties" %in% names(cases)) cases$parties else vector("list", nrow(cases))
  term <- str_extract(cases$dkt, "^\\d{2}")

  purrr::map_dfr(seq_len(nrow(cases)), function(i) {
    ev <- cases$events[[i]]
    cl <- classify_petition_events(ev)
    if (cl$outcome[1] == "pending") return(NULL)
    od <- cl$outcome_date[1]
    cd <- conference_dates_from_events(ev)
    if (!is.na(od)) cd <- cd[cd <= od]
    if (length(cd) == 0) return(NULL)
    rd <- cl$relist_dates[[1]]
    # Structural features are fixed for the petition; parse them once.
    f <- petition_features(cases$caption[i], lower[i], parties[[i]],
                           cases$date[i], lower_date[i], related[i])
    n_cd <- length(cd)
    purrr::map_dfr(seq_along(cd), function(k) {
      d <- cd[k]
      bind_cols(
        # `conf_outcome` is what happened AT this conference -- the competing-risks
        # response. `cd` is already truncated at the disposition, so every
        # conference but the last one ended in a relist by construction.
        tibble(conf_outcome = if (k < n_cd) "relisted" else cl$outcome[1]),
        tibble(dkt = cases$dkt[i], type = "paid", term = term[i],
               # `date` is the petition's DOCKETING date, carried so downstream
               # as-of features (counsel_tier) resolve against the right clock;
               # `conf_date` is this row's conference.
               date = as.Date(cases$date[i]),
               conf_date = d, conf_idx = k,
               outcome = cl$outcome[1], outcome_date = od,
               n_relists = sum(!is.na(rd) & rd < d)),
        f, process_features(ev, d))
    })
  })
}

# Assemble the panel across terms and label it. Same shape as assemble_corpus(),
# so model_frame() / fit_cert_model() / loto_predict() all work unchanged.
assemble_at_risk <- function(paths) {
  message("Assembling at-risk panel from ", length(paths), " term file(s)...")
  panel <- purrr::map_dfr(paths, function(p) { message("  ", basename(p)); assemble_at_risk_term(p) })
  panel |>
    left_join(load_petition_signals(), by = "dkt") |>
    mutate(across(c(dissent_below, dissent_argued, enbanc_dissent, split_argued),
                  ~ coalesce(.x, FALSE)),
           term_year = 2000L + as.integer(term), granted = outcome == "granted") |>
    set_target("grant")
}

# The petition-derived Rule 10 signals (data-raw/petition_signals.json), keyed by
# docket, produced by the enrich-petitions workflow. Returns an empty (but typed)
# tibble when the layer is absent, so a join still creates the columns.
PETITION_SIGNALS_PATH <- "data-raw/petition_signals.json"
load_petition_signals <- function(path = PETITION_SIGNALS_PATH) {
  empty <- tibble(dkt = character(), dissent_below = logical(),
                  dissent_argued = logical(), enbanc_dissent = logical(),
                  split_argued = logical())
  if (!file.exists(path)) return(empty)
  j <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  if (length(j) == 0) return(empty)
  purrr::imap_dfr(j, function(s, dk) tibble(
    dkt = dk, dissent_below = isTRUE(s$dissent_below),
    dissent_argued = isTRUE(s$dissent_argued),
    enbanc_dissent = isTRUE(s$enbanc_dissent),
    split_argued = isTRUE(s$split_argued)))
}

# Assemble the full labeled corpus across term files. Adds the binary label,
# term-year, and the petition-derived Rule 10 signals (missing/unresolved ->
# FALSE, so no rows drop at fit time). Keeps every decided-or-pending row; model
# fitting does the grant/deny filtering so callers can inspect the distribution.
assemble_corpus <- function(paths) {
  message("Assembling corpus from ", length(paths), " term file(s)...")
  corpus <- purrr::map_dfr(paths, function(p) {
    message("  ", basename(p)); assemble_term(p)
  })
  corpus |>
    left_join(load_petition_signals(), by = "dkt") |>
    mutate(across(c(dissent_below, dissent_argued, enbanc_dissent, split_argued),
                  ~ coalesce(.x, FALSE)),
           term_year = 2000L + as.integer(term), granted = outcome == "granted") |>
    set_target("grant")
}

# Set the binary `label` for a modeling target, leaving all other rows NA so
# model fitting drops them:
#   "grant" -> granted (1) vs denied (0)               [the cert model]
#   "gvr"   -> GVR (1) vs granted|denied (0)           [the companion GVR-risk model]
set_target <- function(corpus, target = c("grant", "gvr")) {
  target <- match.arg(target)
  # Every resolved petition is a valid negative. Dropping GVRs and Rule 46
  # dismissals made the published number P(grant | grant or deny) while the page
  # presented it as P(grant), and the distortion was profile-dependent: GVRs
  # concentrate in the relisted buckets, so the exclusion multiplier ran 1.02 at
  # zero relists but 1.28 at 3-4 -- inflating the forecast most precisely where
  # the conference report tells readers to look. Only a petition the Court has
  # not yet acted on is genuinely unlabelled.
  resolved <- c("granted", "denied", "gvr", "dismissed")
  corpus$label <- if (target == "grant")
    dplyr::case_when(corpus$outcome == "granted" ~ 1L,
                     corpus$outcome %in% resolved ~ 0L, TRUE ~ NA_integer_)
  else
    dplyr::case_when(corpus$outcome == "gvr" ~ 1L,
                     corpus$outcome %in% resolved ~ 0L, TRUE ~ NA_integer_)
  corpus
}

# ---- F. metrics (no external deps) --------------------------------------------

# Rank-based ROC-AUC (Mann-Whitney). NA if a class is empty.
auc_roc <- function(y, p) {
  pos <- p[y == 1]; neg <- p[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  r <- rank(c(pos, neg))
  (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
}

# Average precision (area under precision-recall), the imbalance-aware metric.
average_precision <- function(y, p) {
  if (sum(y == 1) == 0) return(NA_real_)
  o <- order(p, decreasing = TRUE); y <- y[o]
  tp <- cumsum(y == 1); fp <- cumsum(y == 0)
  prec <- tp / (tp + fp); rec <- tp / sum(y == 1)
  drec <- diff(c(0, rec))
  sum(prec * drec)
}

binary_metrics <- function(y, p) {
  ok <- !is.na(y) & !is.na(p)
  y <- y[ok]; p <- p[ok]
  list(n = length(y), n_pos = sum(y == 1), base_rate = mean(y == 1),
       auc = auc_roc(y, p), ap = average_precision(y, p),
       brier = mean((p - y)^2))
}

# Calibration table: bin predictions and compare mean predicted vs observed.
calibration_table <- function(y, p, bins = 10) {
  ok <- !is.na(y) & !is.na(p); y <- y[ok]; p <- p[ok]
  br <- unique(quantile(p, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  g <- cut(p, breaks = br, include.lowest = TRUE)
  tibble(y, p, g) |> group_by(g) |>
    summarise(n = n(), pred = mean(p), obs = mean(y), .groups = "drop")
}

# ---- G. model fit, out-of-time evaluation, calibration ------------------------

# Feature groups. Structural = known from case identity (entity type is carried
# by the pet_type/resp_type factors, which include a "us_fed" level, so the
# standalone us_petitioner/business_pet logicals are omitted -- they duplicate a
# level and would alias a coefficient to NA; related_present is omitted until the
# archives carry a `related` column). Petition-signal = the Rule 10 cues parsed
# from the petition PDF (petition_signals.R). Process = the leakage-safe docket-
# development signals (relists bucketed, not linear).
# `elite_counsel` is deliberately NOT here. It shipped dead for months (constant
# FALSE in training, NA coefficient, zero contribution at serve time), and once
# petitioner_counsel() was repaired a joint test showed it is worth -0.0006 AUC
# against a counsel track-record feature that subsumes it: matching a fixed list
# of ~13 famous names recovers a fraction of "counsel who has done this before".
# petitioner_counsel() stays -- counsel_tier is built on it -- and so does the
# aliased-coefficient guard in fit_cert_model(), which is the durable lesson.
STRUCTURAL_FEATURES <- c("pet_type", "resp_type", "court_below",
                         "pro_se", "gap_fast", "gap_na")

# Counsel track record is a PETITION-STAGE feature only. Measured on the at-risk
# panel it helps the baseline (AUC 0.8512 -> 0.8630, AP 0.2739 -> 0.3127) and
# very slightly HURTS the conference model (0.8748 -> 0.8723, AP 0.3617 ->
# 0.3584): by the time a petition has been relisted and drawn a reply brief, the
# docket has already revealed what counsel experience was proxying for. Same
# reasoning the Rule 10 signals are baseline-only. Keeping it out of the
# conference tier also means the conference renderer needs no counsel index.
COUNSEL_FEATURES <- c("counsel_tier")
PETITION_SIGNAL_FEATURES <- c("dissent_below", "split_argued")
PROCESS_FEATURES <- c("relist_bucket", "amicus_bucket", "cvsg",
                      "response_requested", "response_filed",
                      "resp_waiver", "reply_filed")

# The BASELINE (daily / petition-stage) model adds the Rule 10 signals to the
# structural set -- both are knowable the day a petition is docketed, and the
# dissent/split cues are the biggest lever a structural model has. The ENHANCED
# (conference-stage) model instead adds the process signals; it deliberately
# does NOT use the petition signals (they add ~no lift once relists/amicus exist,
# and the conference renderer doesn't parse petition PDFs, so leaving them out
# avoids a train/serve mismatch).
BASELINE_FEATURES <- c(STRUCTURAL_FEATURES, COUNSEL_FEATURES, PETITION_SIGNAL_FEATURES)
ENHANCED_FEATURES <- c(STRUCTURAL_FEATURES, PROCESS_FEATURES)

# Reference levels for the categorical predictors, chosen so a cue's log-odds
# reads against an intuitive baseline: a private individual party, a state
# court below, a petition not yet relisted.
FACTOR_REFERENCES <- list(pet_type = "individual", resp_type = "individual",
                          court_below = "STATE", relist_bucket = "0",
                          amicus_bucket = "0", counsel_tier = "new")

# Training frame: paid, decided as grant or deny, complete predictors. Residual
# levels that would separate the likelihood (the "OTHER" court bucket has zero
# grants; the empty-caption entity "other" is a handful of cases) are lumped
# into a neighboring low-rate level so glm coefficients stay finite. The raw
# extractors keep the granular levels; only model fitting lumps.
# REMOVED: lump_model_levels(). It folded `court_below == "OTHER"` into STATE and
# the entity "other" levels into "individual", ostensibly to keep glm coefficients
# finite. It did real damage instead. STATE is the *reference* level, so an
# "OTHER" court contributed exactly 0 log-odds -- an "In re __" mandamus petition
# (0 grants in 312) was scored as an ordinary state-court case and published at
# ~10.4% where ~3.5% is honest, across 1,721 live-served rows. Likewise
# resp_type "other" (1 grant in 221, the same "In re" petitions with no named
# respondent) was scored as "individual", the highest-rate respondent class.
# Aggregate AUC cannot see any of this -- 300-odd rows carrying one grant have no
# leverage -- which is why it survived. Separation is now handled where it
# belongs, in the estimator: see firth_fit().
model_frame <- function(corpus, features) {
  df <- corpus |>
    filter(type == "paid", !is.na(label)) |>
    mutate(relist_bucket = relist_bucket(n_relists),   # bucketed from the raw counts
           amicus_bucket = amicus_bucket(n_amicus_cert))
  for (v in names(FACTOR_REFERENCES)) if (v %in% features)
    df[[v]] <- relevel(factor(df[[v]]), ref = FACTOR_REFERENCES[[v]])
  df |>
    select(dkt, term_year, label, all_of(features)) |>
    tidyr::drop_na()
}

# Firth-penalised logistic (Jeffreys prior), hand-rolled in base R: the CI runner
# has no logistf/brglm2, and the deployed artifact must stay a plain glm so
# strip_glm() / model.matrix() / score_features() keep working untouched.
#
# This exists because lump_model_levels() is gone (see model_frame()). Once
# `court_below == "OTHER"` is a real level it is a genuine 0-grants-in-312 cell
# -- "In re __" mandamus petitions with no lower court -- and an unpenalised glm
# drives its coefficient to -19 with an SE of ~4,000. Firth gives a finite
# -3.5 (SE 1.7). Un-lumping WITHOUT Firth is measurably worse than the status quo
# (LOTO calibration slope 0.927 vs 0.947); with it, aggregate metrics are
# unchanged (LOTO AUC 0.9305 vs 0.9301) and the separated cell becomes honest.
#
# Score equation: U_j = sum_i [y_i - p_i + h_i(1/2 - p_i)] x_ij, where h is the
# diagonal of the weighted hat matrix. Validated against logistf to ~2e-8.
firth_fit <- function(X, y, maxit = 100L, tol = 1e-10) {
  b <- rep(0, ncol(X))
  for (it in seq_len(maxit)) {
    eta <- as.vector(X %*% b)
    p <- plogis(eta)
    w <- pmax(p * (1 - p), .Machine$double.eps)
    R <- tryCatch(chol(crossprod(X * w, X)), error = function(e) NULL)
    if (is.null(R)) stop("firth_fit: information matrix is not positive definite")
    Q <- backsolve(R, t(X * sqrt(w)), transpose = TRUE)
    h <- colSums(Q^2)                                  # hat diagonal
    U <- crossprod(X, y - p + h * (0.5 - p))
    step <- as.vector(backsolve(R, backsolve(R, U, transpose = TRUE)))
    # Step-halve if the update overshoots -- separated cells can take huge first
    # steps before the penalty bites.
    if (max(abs(step)) > 10) step <- step * (10 / max(abs(step)))
    b <- b + step
    if (max(abs(step)) < tol) break
  }
  list(coefficients = b, vcov = chol2inv(R), iter = it)
}

fit_logit <- function(train, features, firth = TRUE) {
  form <- reformulate(features, response = "label")
  m <- suppressWarnings(glm(form, data = train, family = binomial()))
  if (!firth) return(m)
  keep <- !is.na(coef(m))        # aliased columns: fit_cert_model errors on these
  X <- model.matrix(m)[, keep, drop = FALSE]
  ff <- firth_fit(X, m$y)
  cf <- coef(m); cf[keep] <- ff$coefficients
  m$coefficients <- cf
  # Keep the Firth information matrix: it is the right basis for a published
  # interval, and it is p-by-p (a few KB) rather than n-by-p.
  m$firth_vcov <- ff$vcov
  m$firth <- TRUE
  m
}

# Drop the n-sized components (model frame, residuals, fitted values) and the
# captured environments a glm carries, so a saved model stays small and doesn't
# serialize the training data. Point prediction + model.matrix (for the cue
# explainer) still work from coefficients / terms / xlevels.
strip_glm <- function(m) {
  m$model <- NULL; m$residuals <- NULL; m$fitted.values <- NULL
  m$effects <- NULL; m$weights <- NULL; m$prior.weights <- NULL
  m$y <- NULL; m$linear.predictors <- NULL; m$na.action <- NULL; m$data <- NULL
  # The deployed scoring path computes the linear predictor from the design
  # matrix and coefficients directly (see score_features), never predict.glm,
  # so the n-by-p qr factorization is dead weight -- drop it. This is the bulk
  # of a fitted glm's size.
  m$qr <- NULL
  # Re-root (not empty) the formula/terms environments: model.frame() evaluates
  # in them and needs base functions, but we don't want to retain the original
  # (data-carrying) calling environment. baseenv() gives both.
  attr(m$terms, ".Environment") <- baseenv()
  if (!is.null(m$formula)) attr(m$formula, ".Environment") <- baseenv()
  m
}

# Leave-one-term-out predictions: each term is scored by a model trained on the
# OTHER terms, giving an out-of-time prediction for every petition. Used both to
# report honest performance and to fit the calibrator.
loto_predict <- function(mf, features) {
  terms <- sort(unique(mf$term_year))
  preds <- rep(NA_real_, nrow(mf))
  for (t in terms) {
    tr <- mf[mf$term_year != t, ]; te <- which(mf$term_year == t)
    m <- fit_logit(tr, features)
    preds[te] <- predict(m, newdata = mf[te, ], type = "response")
  }
  preds
}

# Platt scaling: recalibrate raw probabilities via a 1-D logistic on the logit.
# The logit is precomputed into a plain column `z` so the fitted formula is
# `y ~ z` (no namespaced function), which keeps it safe to strip_glm().
fit_platt <- function(y, p) {
  z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  glm(y ~ z, family = binomial(), data = data.frame(y = y, z = z))
}
# Out-of-fold calibrated predictions, for REPORTING only. fit_cert_model used to
# fit the Platt map on the out-of-term predictions and then score the very same
# rows, which pins the reported calibration slope to 1.000 and the intercept to
# 0.000 by construction -- the table could not detect miscalibration even in
# principle. Here each term is calibrated by a map fitted on the other terms.
# The DEPLOYED calibrator is still fitted on all terms; only the honesty of the
# published diagnostics changes.
loto_platt <- function(label, raw, term_year) {
  out <- rep(NA_real_, length(raw))
  for (t in unique(term_year)) {
    te <- term_year == t
    if (sum(label[!te] == 1, na.rm = TRUE) < 2) next   # need both classes to fit
    out[te] <- apply_platt(fit_platt(label[!te], raw[!te]), raw[te])
  }
  out
}

apply_platt <- function(cal, p) {
  z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  co <- coef(cal)                       # intercept + slope on z; computed
  as.numeric(plogis(co[[1]] + co[[2]] * z))   # directly, so no qr needed
}

# Fit the deployable model: refit on ALL terms, and fit a Platt calibrator on
# out-of-term predictions so the stored probabilities are honest. Returns the
# glm, the calibrator, the feature set, the training base rate, and the
# out-of-time metrics.
# Terms complete enough to contribute an honest base rate. A term still resolving
# supplies mostly fast denials and few slow grants (median days to disposition:
# denied 70, granted 128, GVR 193), so it drags the pooled rate down. OT2024 was
# 23.8% pending at its snapshot against 0.0-1.7% for every closed term. This
# matters beyond training because describe_forecast() prints the base rate to
# readers and every published `lift` divides by it. Same 1.5% rule the funnel
# baselines already use.
complete_terms <- function(corpus, max_pending = 0.015) {
  paid <- corpus |> filter(type == "paid")
  # A frame with no pending rows AT ALL cannot answer this question -- the
  # at-risk panel drops pending petitions by construction, so every term would
  # look complete and the censoring guard would silently pass. Say so rather
  # than return a confidently wrong answer.
  if (nrow(paid) == 0 || !any(paid$outcome == "pending")) {
    warning("complete_terms(): this frame contains no pending petitions, so term ",
            "completeness cannot be assessed from it. Pass `complete =` explicitly ",
            "(e.g. complete_terms(<the disposition corpus>)).", call. = FALSE)
    return(character(0))
  }
  paid |>
    group_by(term_year) |>
    summarise(pending = mean(outcome == "pending"), .groups = "drop") |>
    filter(pending <= max_pending) |>
    pull(term_year)
}

fit_cert_model <- function(corpus, features = BASELINE_FEATURES, target = "grant",
                           complete = NULL) {
  mf <- model_frame(corpus, features)
  # Fit on every term; measure the base rate only on the complete ones. `complete`
  # must be supplied when `corpus` is the at-risk panel, which has no pending rows
  # to infer completeness from.
  ct <- if (!is.null(complete)) complete else suppressWarnings(complete_terms(corpus))
  br <- if (length(ct)) mf$term_year %in% ct else rep(TRUE, nrow(mf))
  if (!any(br)) br <- rep(TRUE, nrow(mf))
  base_rate <- mean(mf$label[br])
  message("Base rate: ", sprintf("%.3f%%", 100 * base_rate), " over ",
          length(ct), " complete term(s); ", sum(!br), " row(s) from incomplete ",
          "terms excluded from the rate (but kept in the fit)")
  message("Model frame [", target, "]: ", nrow(mf), " paid petitions, ",
          sum(mf$label), " positives (", round(100 * mean(mf$label), 2), "%)")
  raw <- loto_predict(mf, features)
  cal <- fit_platt(mf$label, raw)                 # the deployed calibrator
  cal_oof <- loto_platt(mf$label, raw, mf$term_year)   # honest, for reporting
  metrics_raw <- binary_metrics(mf$label, raw)
  metrics_cal <- binary_metrics(mf$label, cal_oof)
  final <- fit_logit(mf, features)
  # A feature that is constant in the training frame aliases to an NA coefficient
  # and then contributes exactly zero at serve time, silently -- which is how
  # `elite_counsel` shipped dead in all three models for months while being
  # documented as a driver. Fail the build instead.
  # Mean design row, stored so score_features() can centre its cue attribution on
  # the population rather than on the reference level. p numbers, a few hundred
  # bytes.
  xbar <- colMeans(model.matrix(final))
  aliased <- names(which(is.na(coef(final))))
  if (length(aliased))
    stop("aliased (NA) coefficient(s) -- feature is constant or collinear in the ",
         "training frame, and would contribute nothing at serve time: ",
         paste(aliased, collapse = ", "))
  structure(list(
    glm = strip_glm(final), calibrator = strip_glm(cal), features = features,
    target = target, xlevels = final$xlevels, base_rate = base_rate, xbar = xbar,
    metrics = metrics_raw, metrics_calibrated = metrics_cal,
    calibration = calibration_table(mf$label, cal_oof),
    loto = tibble(dkt = mf$dkt, term_year = mf$term_year,
                  label = mf$label, pred = cal_oof)
  ), class = "cert_model")
}

# A strictly forward-looking check: train on all terms before the last, test on
# the last term (the closest thing to real deployment).
forward_eval <- function(corpus, features = BASELINE_FEATURES) {
  mf <- model_frame(corpus, features)
  last <- max(mf$term_year)
  tr <- mf[mf$term_year < last, ]; te <- mf[mf$term_year == last, ]
  m <- fit_logit(tr, features)
  p <- predict(m, newdata = te, type = "response")
  c(list(test_term = last), binary_metrics(te$label, p))
}

# ---- G2. discrete-time competing risks (the conference model) ------------------

# What happens to a petition AT a given conference is a four-way choice, not two
# independent binaries: {granted, gvr, denied, relisted}. Modelling it that way
# fixes two things the enhanced+gvr pair cannot.
#
#   1. Coherence. p_grant and p_gvr came from separately-fitted, separately-
#      Platt-scaled binaries with different denominators, and summed above 1 on
#      real published rows (140 of 16,333 before the at-risk retarget, 49 after,
#      max 1.36 -- i.e. an implied negative P(denied)). A softmax cannot do that.
#   2. It gives BOTH published quantities from one fit: the per-conference hazard
#      ("what happens on Friday") and, by forward recursion over future
#      conferences, the cumulative ("will this ever be granted").
#
# `denied` is the reference level. nnet is a base-recommended package, so it is
# already on the CI runner -- no new dependency.
# NOTE the conditioning, because this is the same move that made the old grant
# model publish P(grant | grant or deny) under a label that said P(grant):
# `dismissed` is NOT one of the four risks, so these probabilities are
# conditional on the petition not being dismissed. That is 84 of 16,333 panel
# rows (0.5%), and a Rule 46 dismissal is a settlement rather than a judgment on
# certiorari, so excluding it is defensible -- but it is a choice, not a fact,
# and it belongs in the methods note.
CONF_LEVELS <- c("denied", "relisted", "granted", "gvr")
CONF_FEATURES <- c("conf_f", "phase", STRUCTURAL_FEATURES, PROCESS_FEATURES)

# Where in the Term a conference falls. The September "long conference" clears a
# summer's backlog and the late-June conferences clean up before recess; both
# behave nothing like an ordinary sitting. This matters for the HAZARD (when the
# Court acts) even though it was measured null for the terminal outcome, which is
# why it appears here and not in ENHANCED_FEATURES.
conference_phase <- function(d) {
  m <- as.integer(format(as.Date(d), "%m"))
  dplyr::case_when(is.na(m) ~ "fall", m == 9 ~ "long", m %in% c(10, 11) ~ "fall",
                   m %in% c(12, 1, 2) ~ "winter", m %in% c(3, 4, 5) ~ "spring",
                   m == 6 ~ "june", TRUE ~ "summer")
}

conf_model_frame <- function(panel) {
  panel |>
    filter(type == "paid", conf_outcome %in% CONF_LEVELS) |>
    mutate(relist_bucket = relist_bucket(n_relists),
           amicus_bucket = amicus_bucket(n_amicus_cert),
           conf_f = factor(pmin(conf_idx, 5L)),
           phase  = relevel(factor(conference_phase(conf_date)), ref = "fall"),
           y = factor(conf_outcome, levels = CONF_LEVELS)) |>
    { \(d) { for (v in names(FACTOR_REFERENCES)) if (v %in% names(d))
               d[[v]] <- relevel(factor(d[[v]]), ref = FACTOR_REFERENCES[[v]]); d } }() |>
    # n_relists is not a model term, but conference_cumulative() needs it to
    # advance relist_bucket as it rolls forward, so it is carried alongside.
    select(dkt, term_year, y, conf_idx, n_relists, all_of(CONF_FEATURES)) |>
    tidyr::drop_na()
}

# Drop the n-sized components a multinom carries, mirroring strip_glm(). Verified
# to leave predictions bit-identical.
strip_multinom <- function(m) {
  for (s in c("fitted.values", "residuals", "weights", "model", "Hessian", "data"))
    m[[s]] <- NULL
  attr(m$terms, ".Environment") <- baseenv()
  m
}

fit_conference_model <- function(panel) {
  mf <- conf_model_frame(panel)
  form <- reformulate(CONF_FEATURES, response = "y")
  fit <- nnet::multinom(form, data = mf, trace = FALSE, maxit = 300)
  structure(list(model = strip_multinom(fit), features = CONF_FEATURES,
                 levels = CONF_LEVELS, xlevels = fit$xlevels,
                 n = nrow(mf), rates = prop.table(table(mf$y))),
            class = "conf_model")
}

# Per-conference hazards for a frame of conference rows: an n-by-4 matrix.
conference_hazards <- function(model, newdata) {
  for (v in intersect(names(model$xlevels), names(newdata)))
    newdata[[v]] <- factor(newdata[[v]], levels = model$xlevels[[v]])
  p <- predict(model$model, newdata = newdata, type = "probs")
  if (is.null(dim(p))) p <- matrix(p, nrow = 1)
  # predict.multinom takes its column names from $fitted.values, which
  # strip_multinom() drops -- so the matrix comes back unnamed. The column order
  # is the fitted level order, which is CONF_LEVELS; assert that rather than
  # trust it silently, because a mislabelled column would swap "granted" for
  # "denied" on a public page without erroring anywhere.
  stopifnot(ncol(p) == length(model$levels),
            identical(model$model$lev, model$levels))
  colnames(p) <- model$levels
  stopifnot(max(abs(rowSums(p) - 1)) < 1e-6)
  p
}

# Cumulative P(eventual grant / GVR) by rolling the hazards forward over future
# conferences: at each step the petition either resolves or is relisted, and if
# relisted it comes back one conference later with one more relist. Covariates
# other than the conference counters are held fixed -- we cannot know a future
# amicus brief, and assuming none is the conservative choice.
conference_cumulative <- function(model, newdata, horizon = 20L) {
  if (!all(c("conf_idx", "n_relists") %in% names(newdata)))
    stop("conference_cumulative(): newdata needs conf_idx and n_relists to roll ",
         "the conference counters forward.", call. = FALSE)
  n <- nrow(newdata)
  alive <- rep(1, n); cum_g <- rep(0, n); cum_v <- rep(0, n)
  nd <- newdata
  for (step in seq_len(horizon)) {
    h <- conference_hazards(model, nd)
    cum_g <- cum_g + alive * h[, "granted"]
    cum_v <- cum_v + alive * h[, "gvr"]
    alive <- alive * h[, "relisted"]
    if (max(alive) < 1e-4) break
    nd$conf_idx <- nd$conf_idx + 1L
    nd$conf_f <- factor(pmin(nd$conf_idx, 5L), levels = levels(factor(1:5)))
    nd$n_relists <- nd$n_relists + 1L
    nd$relist_bucket <- relist_bucket(nd$n_relists)
  }
  list(p_grant = cum_g, p_gvr = cum_v, p_unresolved = alive)
}

# ---- H. scoring + cue-breakdown explainer -------------------------------------

# Score a feature row with a fitted cert_model, returning the calibrated
# probability and the raw log-odds contribution of each predictor (relative to
# the model's reference level), sorted by magnitude -- the "which cues fired"
# explanation shown to the reader.
score_features <- function(model, newrow) {
  beta <- coef(model$glm)
  beta[is.na(beta)] <- 0            # aliased terms contribute nothing (as predict.glm)
  # Build the one-row design matrix once; the linear predictor and the per-cue
  # contributions both come from it, so we never call predict.glm (and the
  # stored model needs no qr).
  mm <- model.matrix(delete.response(terms(model$glm)), data = newrow,
                     xlev = model$xlevels, contrasts.arg = model$glm$contrasts)
  x <- mm[1, names(beta), drop = TRUE]
  eta <- sum(x * beta)
  raw <- plogis(eta)                                 # logit link inverse
  prob <- apply_platt(model$calibrator, raw)

  # Wald interval on the linear predictor, pushed through the link and the same
  # calibrator as the point estimate. This costs nothing: a Firth fit stores its
  # penalised information matrix, and a plain glm keeps $R (p-by-p, a few KB)
  # through strip_glm() -- so no retrain and no larger artifact. It matters
  # because the pages publish a bare integer percent: the measured two-level
  # bootstrap interval around a 46% estimate runs roughly [22%, 70%], and around
  # 0.7% it is under a point wide. One number cannot convey both.
  V <- model$glm$firth_vcov
  if (is.null(V) && !is.null(model$glm$R))
    V <- tryCatch(chol2inv(model$glm$R), error = function(e) NULL)
  se_eta <- NA_real_; ci <- c(NA_real_, NA_real_)
  if (!is.null(V) && is.matrix(V) && nrow(V) == length(x)) {
    se_eta <- sqrt(max(0, as.numeric(crossprod(x, V %*% x))))
    ci <- apply_platt(model$calibrator, plogis(eta + c(-1.96, 1.96) * se_eta))
  }
  # Cue contributions are centred on the POPULATION MEAN design row, not on the
  # reference level. Against the reference (a private individual, a state court
  # below) every federal case showed a large positive cue for its own circuit
  # merely for not being a state case -- 68.9% of the explanations that were
  # shown led with a circuit dummy, and describe_forecast() had to suppress the
  # driver list entirely below 0.85x lift to stop it reading as a grant signal on
  # unremarkable cases. Centred, a contribution is positive only when the factor
  # is genuinely above average, and the contributions sum to logit(p) minus the
  # mean linear predictor, so they net to ~0 for an average petition.
  nonint <- names(beta) != "(Intercept)"
  xb <- model$xbar
  ctr <- if (!is.null(xb) && all(names(beta) %in% names(xb))) xb[names(beta)] else 0 * beta
  contrib <- (x[nonint] - ctr[nonint]) * beta[nonint]
  # Report only factors this petition ACTUALLY HAS. Centring makes every term
  # nonzero, including absent ones -- a petition that is not self-represented
  # picks up (0 - 0.255) * -4.67 = +1.19, which is true ("not being pro se
  # helps") but renders as "the model weights this up for a self-represented
  # petitioner", the exact opposite of the case's facts. FORECAST_CUE_PHRASES
  # names the factor and takes direction from the sign, so a cue may only be
  # shown when the factor is present. Magnitudes stay centred, so a cue now fires
  # only when the factor is genuinely above or below the population average
  # rather than merely different from the reference level.
  present <- x[nonint] != 0
  contrib <- contrib[present]
  contrib <- contrib[is.finite(contrib) & contrib != 0]
  cues <- tibble(term = names(contrib), log_odds = as.numeric(contrib)) |>
    arrange(desc(abs(log_odds)))
  list(prob = as.numeric(prob), raw = as.numeric(raw),
       base_rate = model$base_rate, lift = as.numeric(prob) / model$base_rate,
       se_eta = se_eta, ci_low = ci[[1]], ci_high = ci[[2]],
       cues = cues)
}

# ---- forecast description (plain-English cue read) ----------------------------
# Human phrase for each model cue term. model.matrix names a factor level as
# "<var><level>" (reference level omitted), a logical as "<var>TRUE", a numeric
# as "<var>". Each phrase names ONLY the factor; the direction (raises vs lowers
# the forecast) comes from the sign of the cue's log-odds, so one phrase serves
# both. Covers the BASELINE cues (structural + petition signals) and the ENHANCED
# process cues, so the describer works for either model.
FORECAST_CUE_PHRASES <- c(
  "pet_typeus_fed"       = "a federal-government petitioner",
  "pet_typestate_local"  = "a state or local-government petitioner",
  "pet_typebusiness"     = "a business petitioner",
  "resp_typeus_fed"      = "a federal-government respondent",
  "resp_typestate_local" = "a state or local-government respondent",
  "resp_typebusiness"    = "a business respondent",
  "court_belowCA1"  = "a First Circuit decision below",
  "court_belowCA2"  = "a Second Circuit decision below",
  "court_belowCA3"  = "a Third Circuit decision below",
  "court_belowCA4"  = "a Fourth Circuit decision below",
  "court_belowCA5"  = "a Fifth Circuit decision below",
  "court_belowCA6"  = "a Sixth Circuit decision below",
  "court_belowCA7"  = "a Seventh Circuit decision below",
  "court_belowCA8"  = "an Eighth Circuit decision below",
  "court_belowCA9"  = "a Ninth Circuit decision below",
  "court_belowCA10" = "a Tenth Circuit decision below",
  "court_belowCA11" = "an Eleventh Circuit decision below",
  "court_belowCADC"  = "a D.C. Circuit decision below",
  "court_belowCAFED" = "a Federal Circuit decision below",
  "court_belowCAAF"  = "the Court of Appeals for the Armed Forces below",
  "court_belowUSDC"  = "a federal district court below (certiorari before judgment)",
  "court_belowFED_SPEC" = "a specialised federal court below",
  "court_belowOTHER" = "no court below (an original writ)",
  "counsel_tiersome"  = "counsel who has filed here before",
  "counsel_tiervet"   = "counsel with five or more prior petitions here",
  "counsel_tierwon"   = "counsel who has won certiorari before",
  "pro_seTRUE"        = "a self-represented petitioner",
  "gap_fast"          = "a petition filed soon after the judgment below",
  "dissent_belowTRUE" = "a dissent in the court below (flagged in the petition)",
  "split_arguedTRUE"  = "a circuit split argued in the petition",
  "relist_bucket1"   = "one relist",
  "relist_bucket2"   = "two relists",
  "relist_bucket3-4" = "three or four relists",
  "relist_bucket5+"  = "five or more relists",
  "amicus_bucket1"   = "one cert-stage amicus brief",
  "amicus_bucket2"   = "two cert-stage amicus briefs",
  "amicus_bucket3-4" = "three or four cert-stage amicus briefs",
  "amicus_bucket5+"  = "five or more cert-stage amicus briefs",
  "cvsgTRUE"               = "a call for the Solicitor General's views (CVSG)",
  "response_requestedTRUE" = "a requested response",
  # Phrased neutrally on purpose. Once reply_filed is in the model these two cues
  # change sign -- conditional on a reply having been filed, a bare opposition
  # brief is the weaker posture -- and describe_forecast() renders direction from
  # the coefficient's sign, so the wording has to read sensibly either way.
  "response_filedTRUE"     = "an opposition brief on the docket",
  "resp_waiverTRUE"        = "the respondent waiving its right to respond",
  "reply_filedTRUE"        = "a reply brief from the petitioner"
)

# Turn a score_features() result into one model-faithful sentence: the forecast's
# lift over the base rate, then the factors the model weights up and down (biggest
# |log-odds| first, up to `top` each; cues below `eps` are dropped as negligible).
# Deterministic -- no model call, no network. Guardrails baked into the wording:
# probability + lift, never yes/no; "the model weights this up/down" (the cues are
# correlational weights, not causes); a lean, not a verdict. `include_prob` adds a
# leading "an N% forecast" for standalone use (off by default, since on the docket
# page the number is shown right above it).
describe_forecast <- function(score, top = 3L, eps = 0.05, include_prob = FALSE,
                              retrospective = FALSE) {
  if (is.null(score) || is.null(score$cues) || is.na(score$prob %||% NA_real_)) return("")
  pctd <- function(p) sprintf("%.1f%%", 100 * p)        # base rate (1 dp)
  pcti <- function(p) sprintf("%d%%", round(100 * p))   # forecast (integer)
  mult <- function(l) { r <- round(l, 1)
    if (abs(r - round(r)) < .05) sprintf("%d", round(r)) else sprintf("%.1f", r) }
  join <- function(x) { n <- length(x)
    if (n == 0) "" else if (n == 1) x[1]
    else if (n == 2) paste(x, collapse = " and ")
    else paste0(paste(x[-n], collapse = ", "), ", and ", x[n]) }
  cap1 <- function(s) if (nzchar(s)) paste0(toupper(substring(s, 1, 1)), substring(s, 2)) else s
  # On a decided case the note is a retrospective: past tense + "before the
  # decision" frame. On a pending case it's a live read (present tense).
  wv  <- if (retrospective) "weighted" else "weights"
  pre <- if (retrospective) "before the decision, " else ""

  lift <- score$lift; base <- pctd(score$base_rate)

  # Below the base rate, DON'T list drivers. The per-cue log-odds are measured
  # against a very-low-grant reference profile (a private party, a state court
  # below), so almost any federal case shows a large positive "up" cue for its
  # circuit of origin -- which reads as a grant signal on a case the model
  # actually rates as unremarkable. Say that plainly instead.
  if (!is.na(lift) && lift <= 0.85) {
    lead <- paste0(pre, "well below the ", base, " base rate")
    if (include_prob) lead <- paste0(pre, pcti(score$prob), " — well below the ", base, " base rate")
    return(paste0(cap1(lead), ", with no standout signals pointing toward a grant."))
  }

  # At or above the base rate, name the real drivers (biggest |log-odds| first).
  lift_ph <- if (is.na(lift)) "" else
    if (lift >= 1.5)       sprintf("about %s× the %s base rate", mult(lift), base)
    else if (lift >= 1.15) sprintf("modestly above the %s base rate", base)
    else                   sprintf("roughly the %s base rate", base)   # 0.85 < lift < 1.15
  lead <- paste0(pre, if (include_prob && nzchar(lift_ph))
                        paste0(pcti(score$prob), " — ", lift_ph) else lift_ph)
  if (nzchar(lead)) lead <- paste0(cap1(lead), ".")

  cu <- score$cues
  cu <- cu[is.finite(cu$log_odds) & abs(cu$log_odds) >= eps, , drop = FALSE]
  ph <- function(terms) { v <- unname(FORECAST_CUE_PHRASES[terms]); v[!is.na(v)] }
  up <- ph(head(dplyr::arrange(dplyr::filter(cu, log_odds > 0), dplyr::desc(log_odds))$term, top))
  dn <- ph(head(dplyr::arrange(dplyr::filter(cu, log_odds < 0), log_odds)$term, top))

  drv <- if (length(up) && length(dn))
      sprintf(" The model %s this up for %s, and down for %s.", wv, join(up), join(dn))
    else if (length(up)) sprintf(" The model %s this up for %s.", wv, join(up))
    else if (length(dn)) sprintf(" The model %s this down for %s.", wv, join(dn))
    else if (retrospective) " No single factor stood out." else " No single factor stands out."
  paste0(lead, drv)
}

# Convenience: score a raw case record (caption/lower/parties/...) at a given
# as-of date. Structural features always apply; process features are included
# only if the model uses them.
score_case <- function(model, caption, lower, parties, date, lower_date,
                       related, events = NULL, as_of = Sys.Date(), signals = NULL,
                       counsel_index = NULL) {
  f <- petition_features(caption, lower, parties, date, lower_date, related)
  # Counsel track record, as of this petition's docketing date. counsel_tier()
  # errors rather than defaulting when the index is missing -- see the note there.
  if ("counsel_tier" %in% model$features)
    f$counsel_tier <- counsel_tier(f$counsel_key, date, counsel_index)
  # Petition-derived Rule 10 signals: supplied by the caller (which fetched/parsed
  # the petition PDF) or defaulted to FALSE (absence) when unavailable at inference.
  for (nm in c("dissent_below", "dissent_argued", "enbanc_dissent", "split_argued"))
    f[[nm]] <- if (!is.null(signals) && !is.null(signals[[nm]])) isTRUE(signals[[nm]]) else FALSE
  if (any(PROCESS_FEATURES %in% model$features)) {
    f <- bind_cols(f, process_features(events, as.Date(as_of)))
    # Relists strictly before the as-of date, via the audited relist grammar in
    # classify_petition_events() (cert_funnel.R must be sourced). A grant at this
    # conference would be its final relist, so counting < as_of is correct.
    nrel <- 0L
    cl <- tryCatch(classify_petition_events(events), error = function(e) NULL)
    if (!is.null(cl)) {
      rd <- cl$relist_dates[[1]]
      nrel <- sum(!is.na(rd) & rd < as.Date(as_of))
    }
    f$n_relists <- nrel
    f$relist_bucket <- relist_bucket(nrel)
    f$amicus_bucket <- amicus_bucket(f$n_amicus_cert)
  }
  # Coerce factor columns to the model's known levels. No lumping here any more:
  # training keeps "OTHER"/"other" as real levels, so serving must too, or the
  # two paths disagree on what an "In re" petition is.
  for (v in intersect(names(model$xlevels), names(f)))
    f[[v]] <- factor(f[[v]], levels = model$xlevels[[v]])
  score_features(model, f)
}

# Combined disposition read for a petition at a conference: calibrated P(grant)
# and P(GVR) from the two models plus the hold flag. `granted_dockets` is the set
# of dockets already granted as of the conference (enables the companion
# "Vide"-linkage hold tier); a conference renderer has this set on hand. This is
# the interface the dashboards call.
score_disposition <- function(grant_model, gvr_model, caption, lower, parties,
                              date, lower_date, related, events, as_of,
                              granted_dockets = character(), counsel_index = NULL) {
  g <- score_case(grant_model, caption, lower, parties, date, lower_date,
                  related, events = events, as_of = as_of,
                  counsel_index = counsel_index)
  v <- score_case(gvr_model, caption, lower, parties, date, lower_date,
                  related, events = events, as_of = as_of,
                  counsel_index = counsel_index)
  cl <- tryCatch(classify_petition_events(events), error = function(e) NULL)
  nrel <- if (is.null(cl)) 0L else {
    rd <- cl$relist_dates[[1]]; sum(!is.na(rd) & rd < as.Date(as_of))
  }
  list(p_grant = g$prob, p_gvr = v$prob, n_relists = nrel,
       held = hold_signal(nrel, related, granted_dockets),
       lift = g$lift, grant_cues = g$cues)
}

# The two numbers a conference report publishes, each from the model that
# actually wins that target on a like-for-like rolling-origin comparison:
#
#   p_grant_now   -- "what happens to this petition on Friday". Competing-risks
#                    hazard: AUC 0.920 / AP 0.259 / Brier 0.0248, mean 2.87%
#                    against 2.94% observed. The binary reads 7.77% here, i.e.
#                    2.6x too high, because it is not a per-conference quantity.
#   p_grant_ever  -- "will this petition ever be granted". At-risk binary:
#                    AUC 0.870 / AP 0.397 / Brier 0.0580, mean 7.77% against
#                    7.81%. Fitting that target directly beats rolling the
#                    hazards forward (AP 0.397 vs 0.363), so the forward
#                    recursion is NOT used in production.
#
# Deliberately not a single model doing both: measured, each is worse at the
# other's job, and in opposite directions.
score_conference <- function(models, caption, lower, parties, date, lower_date,
                             related, events, as_of, conf_idx = NULL,
                             granted_dockets = character()) {
  as_of <- as.Date(as_of)
  f <- petition_features(caption, lower, parties, date, lower_date, related)
  f <- bind_cols(f, process_features(events, as_of))
  cl <- tryCatch(classify_petition_events(events), error = function(e) NULL)
  rd <- if (is.null(cl)) as.Date(character()) else cl$relist_dates[[1]]
  nrel <- sum(!is.na(rd) & rd < as_of)
  if (is.null(conf_idx)) {
    cd <- conference_dates_from_events(events)
    conf_idx <- max(1L, sum(cd <= as_of))
  }
  f$n_relists    <- nrel
  f$relist_bucket <- relist_bucket(nrel)
  f$amicus_bucket <- amicus_bucket(f$n_amicus_cert)
  f$conf_idx <- as.integer(conf_idx)
  f$conf_f   <- as.character(pmin(f$conf_idx, 5L))
  f$phase    <- conference_phase(as_of)

  out <- list(p_grant_now = NA_real_, p_gvr_now = NA_real_, p_denied_now = NA_real_,
              p_relist_now = NA_real_, p_grant_ever = NA_real_, n_relists = nrel,
              conf_idx = f$conf_idx, held = hold_signal(nrel, related, granted_dockets))
  if (!is.null(models$conference)) {
    h <- tryCatch(conference_hazards(models$conference, f), error = function(e) NULL)
    if (!is.null(h)) {
      out$p_grant_now  <- h[1, "granted"];  out$p_gvr_now    <- h[1, "gvr"]
      out$p_denied_now <- h[1, "denied"];   out$p_relist_now <- h[1, "relisted"]
    }
  }
  if (!is.null(models$enhanced)) {
    g <- tryCatch(score_features(models$enhanced,
           { d <- f; for (v in intersect(names(models$enhanced$xlevels), names(d)))
               d[[v]] <- factor(as.character(d[[v]]), levels = models$enhanced$xlevels[[v]]); d }),
           error = function(e) NULL)
    if (!is.null(g)) { out$p_grant_ever <- g$prob; out$cues <- g$cues; out$lift <- g$lift }
  }
  # P(granted at this conference) <= P(granted at some conference) is an identity,
  # not a preference -- but the two numbers come from two independently fitted
  # models, so they can cross. Measured on a real long-conference render: 3 of
  # 232 paid rows (1.3%), always by a hair. A crossing is definitionally a model
  # error, so clamp to the constraint rather than publish a row that contradicts
  # itself. Recorded on the result so a caller can count how often it fires.
  if (!is.na(out$p_grant_now) && !is.na(out$p_grant_ever) &&
      out$p_grant_now > out$p_grant_ever) {
    out$clamped <- TRUE
    out$p_grant_now <- out$p_grant_ever
  } else out$clamped <- FALSE
  out
}

# Load the trained models from `dir`; returns a named list with whichever exist
# (baseline / enhanced / gvr). Never errors -- a render pipeline can call this and
# simply omit the forecast column when the artifacts are absent or unreadable.
load_cert_models <- function(dir = "data") {
  want <- c(baseline   = "cert_model_baseline.rds",
            enhanced   = "cert_model_enhanced.rds",
            gvr        = "cert_model_gvr.rds",
            conference = "cert_model_conference.rds")
  out <- list()
  for (nm in names(want)) {
    f <- file.path(dir, want[[nm]])
    if (file.exists(f)) {
      m <- tryCatch(readRDS(f), error = function(e) NULL)
      if (!is.null(m)) out[[nm]] <- m
    }
  }
  # Validate each artifact against what the code expects. A stale .rds whose
  # feature set has drifted does not error -- it scores with the wrong design
  # matrix, or aliases a term to zero, and the only symptom is that the numbers
  # are quietly wrong. Drop it loudly instead; the renderers already omit the
  # column when a model is absent, which makes the failure visible.
  expect <- list(baseline = list(f = BASELINE_FEATURES, t = "grant"),
                 enhanced = list(f = ENHANCED_FEATURES, t = "grant"),
                 gvr      = list(f = ENHANCED_FEATURES, t = "gvr"))
  for (nm in names(expect)) {
    m <- out[[nm]]
    if (is.null(m)) next
    bad <- character()
    if (!inherits(m, "cert_model")) bad <- c(bad, "not a cert_model")
    if (!setequal(m$features %||% character(), expect[[nm]]$f))
      bad <- c(bad, sprintf("feature set drifted (artifact has %s)",
                            paste(setdiff(m$features, expect[[nm]]$f), collapse = "/")))
    if (!identical(m$target %||% "", expect[[nm]]$t)) bad <- c(bad, "wrong target")
    if (!is.null(m$glm) && any(is.na(coef(m$glm)))) bad <- c(bad, "aliased coefficient")
    if (length(bad)) {
      warning("load_cert_models(): dropping ", want[[nm]], " -- ",
              paste(bad, collapse = "; "), ". Retrain with train_cert_model.R.",
              call. = FALSE)
      out[[nm]] <- NULL
    }
  }

  # The counsel index is a separate artifact, and a model that needs it is worse
  # than useless without it: counsel_tier would read "new" for every advocate,
  # quietly reverting the model to its pre-2026-07 performance with no error
  # anywhere. Drop such a model loudly instead -- the renderers already omit the
  # forecast column when a model is absent, so the failure becomes visible.
  ci <- file.path(dir, "counsel_index.rds")
  out$counsel_index <- if (file.exists(ci)) tryCatch(readRDS(ci), error = function(e) NULL) else NULL
  if (is.null(out$counsel_index)) {
    needs <- names(which(vapply(out[names(want)], function(m)
      !is.null(m) && "counsel_tier" %in% (m$features %||% character()), logical(1))))
    for (nm in needs) {
      warning("load_cert_models(): ", want[[nm]], " uses counsel_tier but ",
              ci, " is missing; dropping the model rather than scoring every ",
              "advocate as first-time.", call. = FALSE)
      out[[nm]] <- NULL
    }
  }
  out
}

# A calibrated P(grant) percentage cell for a gt table, e.g. 0.42 -> "42%".
# NA (non-paid / unmodeled) renders as an em dash.
fmt_prob_cell <- function(p) ifelse(is.na(p), "—", paste0(round(100 * p), "%"))
