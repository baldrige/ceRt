# sg_views.R ---------------------------------------------------------------------
# What the Solicitor General recommended, after the Court called for the views of
# the United States (CVSG). See docs/sg-recommendation-spec.md.
#
# The model's only CVSG input is the flag `cvsg`: it knows the Court asked, never
# what the SG answered. Once the brief is filed every CVSG'd petition reads the
# same blended number -- 26-49 read 28.4% whichever way the SG came down -- and
# that number is the average of two very different populations.
#
# This file finds the SG's brief on a docket, reads its Conclusion and labels the
# recommendation. The labels are hand-checked (data-raw/sg_recommendations.json,
# `source = "hand"`); a regex label is a proposal, not a measurement.

suppressPackageStartupMessages({ library(stringr); library(tibble) })

CVSG_ENTRY_RX <- "Solicitor General is invited"

# The SG's CVSG brief is filed as an amicus brief of the United States. Seen in
# OT2017-OT2025: "of United States", "of United States of America", a lowercase
# "United states", and one "United Stares" (17-1530) -- hence the [rt] and the
# case-insensitive match.
SG_BRIEF_RX <- regex("^Brief (amicus curiae|amici curiae|for the) (of )?(the )?United Sta[rt]es", ignore_case = TRUE)

SG_RECS <- c("grant", "grant_limited", "deny", "gvr", "hold", "dismiss", "other")

# How a recommendation reads in a sentence ("The Solicitor General recommends %s").
SG_REC_WORDS <- c(grant = "a grant", grant_limited = "a limited grant", deny = "denial",
                  gvr = "a GVR", hold = "a hold", dismiss = "dismissal",
                  other = "another disposition")

# The committed, hand-checked labels, and the cache the conferences run adds to
# as new briefs are filed (resolve_sg_views()).
SG_VIEWS_PATH  <- "data-raw/sg_recommendations.json"
SG_VIEWS_CACHE <- file.path("conferences", "sg_views_cache.json")

# The layer scores three groups, not seven: hold, GVR, dismiss and other are 11
# rows between them in OT2017-OT2024, too few to estimate apart.
sg_rec_group <- function(rec) {
  dplyr::case_when(is.na(rec) ~ NA_character_,
                   rec %in% c("grant", "grant_limited") ~ "grant",
                   rec == "deny" ~ "deny", TRUE ~ "other")
}

# The first United States brief AFTER the CVSG entry. Only the first: a grant can
# be followed by a second US brief at the merits stage, and reading that one would
# be leakage (the same tripwire as amicus side; docs/cert_model_review_2026-07.md).
# Returns NULL when there is no CVSG or no brief yet.
find_sg_brief <- function(events) {
  if (!is.data.frame(events) || !nrow(events)) return(NULL)
  txt <- events[["Proceedings and Orders"]]; txt[is.na(txt)] <- ""
  cv <- which(str_detect(txt, CVSG_ENTRY_RX))
  if (!length(cv)) return(NULL)
  i <- which(str_detect(txt, SG_BRIEF_RX))
  i <- i[i > cv[1]]
  if (!length(i)) return(list(cvsg_date = events$Date[cv[1]], filed = NA_character_,
                              url = NA_character_, entry = NA_character_))
  # The first entry can lack a link: 17-290's brief was docketed "to be
  # corrected and reprinted" and the PDF hangs off the corrected brief's entry a
  # week later. Take the first linked entry within 30 days of the first; the
  # filing date stays the first entry's. Never later than that -- the next US
  # brief on 17-290 is its merits brief, four months on.
  d <- suppressWarnings(lubridate::mdy(events$Date))
  url <- NA_character_
  for (k in i[!is.na(d[i]) & d[i] - d[i[1]] <= 30]) {
    url <- sg_main_document(events[k, , drop = FALSE])
    if (!is.na(url)) break
  }
  list(cvsg_date = events$Date[cv[1]], filed = events$Date[i[1]],
       url = url, entry = txt[i[1]])
}

# The brief's own PDF, not its certificate of service. Link order varies (the
# certificate is first about as often as not), so go by the Document_n label,
# falling back to the one link that is not a certificate. The archives name the
# label columns Document_n, the snapshots docs_n.
sg_main_document <- function(row) {
  dn <- grep("^(Document|docs)_", names(row), value = TRUE)
  for (d in dn) {
    if (identical(str_to_lower(trimws(row[[d]] %||% "")), "main document")) {
      l <- row[[sub("^(Document|docs)_", "links_", d)]]
      if (!is.null(l) && !is.na(l) && nzchar(l)) return(l)
    }
  }
  ln <- unlist(row[grep("^links_", names(row))]); ln <- ln[!is.na(ln) & nzchar(ln)]
  ln <- ln[!str_detect(ln, regex("certificate|proof", ignore_case = TRUE))]
  if (length(ln)) ln[1] else NA_character_
}

# The Conclusion: from the last CONCLUSION heading to the signature block. A
# brief whose heading pdftools did not keep falls back to the last 3,000
# characters, which is where the Conclusion is in every brief seen.
sg_conclusion <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_character_)
  hits <- str_locate_all(text, "(?m)^\\s*CONCLUSION\\s*$")[[1]]
  body <- if (nrow(hits)) substring(text, hits[nrow(hits), "end"] + 1L)
          else substring(text, max(1L, nchar(text) - 3000L))
  # Stop at the signature: "Respectfully submitted." and the SG's name follow.
  cut <- str_locate(body, regex("Respectfully submitted", ignore_case = TRUE))[1, "start"]
  if (!is.na(cut)) body <- substring(body, 1L, cut - 1L)
  body <- str_squish(body)
  # Rejoin words pdftools split at a line break ("grant- ed", "cer- tiorari").
  # Letters only: "19- 351" is a docket number and must stay apart.
  str_replace_all(body, "([a-z])- ([a-z])", "\\1\\2")
}

# Label one docket's recommendation from the Conclusion text. Returns the label
# and the sentence it rests on. Multi-docket briefs name the docket; when the
# Conclusion mentions this docket's number, only the sentences naming it count.
sg_recommendation <- function(concl, dkt = NA_character_) {
  if (is.na(concl) || !nzchar(concl))
    return(list(rec = NA_character_, quote = NA_character_))
  # Sentences end at . or ; followed by a capital -- never "No. 22-555", which
  # names a docket mid-sentence.
  sents <- str_split(concl, "(?<=[.;])\\s+(?=[A-Z])")[[1]]
  sents <- sents[nzchar(sents)]
  # A conditional cross-petition is the RESPONDENT'S petition. "The petition
  # should be granted and the conditional cross-petition denied" is a grant of
  # this one, so the cross-petition's clause is dropped before reading -- unless
  # it is the only thing said, as in 22-393, which IS the cross-petition.
  # Only when what is left still recommends something: in 18-447, "The petition
  # and the conditional cross-petition ... should be denied", both are denied
  # and stripping would leave "The petition."
  strip_cross <- function(x) {
    y <- str_replace(x, regex(",? and the conditional cross-petition[^.;]*", ignore_case = TRUE), "")
    ifelse(str_detect(y, regex("grant|den(y|ied)|\\bheld\\b|\\bhold\\b|vacate|dismiss", ignore_case = TRUE)), y, x)
  }
  # Multi-docket briefs: keep the sentences, then the clauses, that name this
  # docket. 18-260/18-268 share one sentence with a grant for one and a hold for
  # the other.
  if (!is.na(dkt)) {
    mine <- str_detect(sents, fixed(dkt))
    if (any(mine)) {
      sents <- sents[mine]
      cl <- unlist(str_split(sents, ",? and (?=the petition)|;\\s*"))
      if (sum(str_detect(cl, fixed(dkt))) >= 1 && length(cl) > length(sents))
        sents <- cl[str_detect(cl, fixed(dkt))]
    } else sents <- strip_cross(sents)
  } else sents <- strip_cross(sents)
  rx <- list(
    gvr     = "vacate[sd]?\\b.*remand|grant(ed)?,? (the petition, )?vacate",
    hold    = "\\bheld\\b.*pending|should hold the petition|hold the petitions?\\b.*pending",
    dismiss = "should be dismissed|dismiss the (petition|appeal)",
    grant_limited = "grant(ed)?\\b.*(limited to|as to)|with respect to question",
    grant   = "should be granted|should grant|grant the petition|be granted",
    deny    = "should be denied|should deny|deny the petition|be denied"
  )
  # The first sentence that recommends anything wins; an "In the alternative"
  # fallback is read only when nothing precedes it (21-260: deny, alternatively GVR).
  alt <- str_detect(sents, regex("^(In the alternative|Alternatively)", ignore_case = TRUE))
  for (s in c(sents[!alt], sents[alt])) {
    for (lab in names(rx))
      if (str_detect(s, regex(rx[[lab]], ignore_case = TRUE)))
        return(list(rec = lab, quote = str_trunc(s, 300)))
  }
  list(rec = "other", quote = str_trunc(paste(sents, collapse = " "), 300))
}

# ---- serving -------------------------------------------------------------------

.read_views <- function(p) {
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) {
    warning("sg views: ", p, " unreadable -- ignored.", call. = FALSE); list() })
}

# The committed labels, plus the on-site cache when `site_dir` is given. The
# committed file wins for any docket it holds: those labels are the reviewed ones.
load_sg_views <- function(site_dir = NULL, path = SG_VIEWS_PATH) {
  v <- .read_views(path)
  if (!is.null(site_dir)) {
    cache <- .read_views(file.path(site_dir, SG_VIEWS_CACHE))
    for (d in setdiff(names(cache), names(v))) v[[d]] <- cache[[d]]
  }
  v
}

# A brief's URL without its scheme: the archives hold http:// links and a fresh
# fetch https:// ones for the same file.
.sg_url_key <- function(u) sub("^https?://", "", u %||% NA_character_)

# What the SG said on this docket, if the brief was filed before `as_of`. NULL
# when there is no CVSG, no brief yet, or the brief postdates `as_of`; `rec` is NA
# when the brief is filed but not labelled (not yet read, or unreadable), and a
# caller must then fall back to the model rather than guess.
#
# Keyed on the brief's URL, not the docket, so score_conference() -- which is
# handed events, not a docket number -- can find it.
sg_view_for <- function(events, views, as_of = NULL) {
  b <- find_sg_brief(events)
  if (is.null(b) || is.na(b$filed)) return(NULL)
  filed <- suppressWarnings(lubridate::mdy(b$filed))
  if (is.na(filed) || (!is.null(as_of) && !is.na(as_of) && filed >= as.Date(as_of))) return(NULL)
  key <- .sg_url_key(b$url)
  hit <- if (!is.na(key) && length(views))
    Find(function(e) identical(.sg_url_key(e$url), key), views) else NULL
  list(rec = hit$rec %||% NA_character_, group = sg_rec_group(hit$rec %||% NA_character_),
       filed = filed, url = b$url)
}

# The layer: P(granted ever) for a petition the SG has answered, by recommendation.
# The measured result (docs/sg-recommendation-spec.md) is that once the SG has
# spoken the at-risk model's score carries no information -- slope -0.16 +/- 0.24
# on its logit, AUC 0.41 out of time -- so the rate by recommendation REPLACES it
# rather than adjusting it. Smoothed (k + 1) / (n + 2), on resolved petitions
# only; a Rule 46 dismissal is not a ruling and is left out.
#
# With `panel` (the at-risk panel, one row per petition x conference), also the
# per-conference split -- what the Court does at a given conference -- by
# recommendation and by whether it is the first conference after the brief or a
# later one. The conference model's hazards are no better than the at-risk
# score on these petitions: 25-828 (SG: grant) read 78% denied at its first
# conference after the brief, where 8% of such petitions were denied and 67%
# relisted -- the Court relists an SG-recommended grant once before granting it.
# See sg_hazard().
fit_sg_layer <- function(views, corpus, panel = NULL) {
  d <- tibble(dkt = names(views),
              group = sg_rec_group(vapply(views, function(e) e$rec %||% NA_character_, ""))) |>
    dplyr::inner_join(dplyr::select(corpus, dkt, outcome), by = "dkt") |>
    dplyr::filter(!is.na(group), outcome %in% c("granted", "denied", "gvr"))
  r <- d |> dplyr::group_by(group) |>
    dplyr::summarise(n = dplyr::n(), k = sum(outcome == "granted"), .groups = "drop")
  if (!all(c("grant", "deny") %in% r$group))
    stop("fit_sg_layer(): need resolved petitions under both grant and deny recommendations.")
  out <- list(rates = setNames((r$k + 1) / (r$n + 2), r$group),
              n = setNames(r$n, r$group), k = setNames(r$k, r$group))
  if (!is.null(panel)) out <- c(out, .fit_sg_hazard(views, panel))
  structure(out, class = "sg_layer")
}

SG_HAZARD_OUTCOMES <- c("granted", "gvr", "denied", "relisted")
SG_HAZARD_PRIOR <- 4   # pseudo-counts each level borrows from the one above it

# The per-conference split on SG-answered petitions, as a table keyed
# "<group>.<phase>" (phase "first" or "later") of shares over
# SG_HAZARD_OUTCOMES, with the row counts behind each. Each cell is shrunk
# toward its group's split over both phases, and each group toward all SG rows,
# by SG_HAZARD_PRIOR pseudo-counts: the "other" recommendation has a dozen rows.
# A conference that dismissed the petition (Rule 46) is not a ruling and is
# left out, as in the rates.
.fit_sg_hazard <- function(views, panel) {
  v <- tibble(dkt = names(views),
              filed = as.Date(vapply(views, function(e) e$filed %||% NA_character_, "")),
              group = sg_rec_group(vapply(views, function(e) e$rec %||% NA_character_, "")))
  d <- panel |> dplyr::select(dkt, conf_date, conf_outcome) |>
    dplyr::inner_join(v, by = "dkt") |>
    dplyr::filter(!is.na(group), !is.na(filed), as.Date(conf_date) > filed) |>
    dplyr::arrange(dkt, conf_date) |> dplyr::group_by(dkt) |>
    dplyr::mutate(phase = if_else(dplyr::row_number() == 1L, "first", "later")) |>
    dplyr::ungroup() |> dplyr::filter(conf_outcome %in% SG_HAZARD_OUTCOMES)
  cnt <- function(x) vapply(SG_HAZARD_OUTCOMES, function(o) sum(x == o), numeric(1))
  shrink <- function(k, prior) (k + SG_HAZARD_PRIOR * prior) / (sum(k) + SG_HAZARD_PRIOR)
  all_sg <- cnt(d$conf_outcome) / nrow(d)
  hazard <- list(); hazard_n <- c()
  for (g in unique(d$group)) {
    q_g <- shrink(cnt(d$conf_outcome[d$group == g]), all_sg)
    for (ph in c("first", "later")) {
      x <- d$conf_outcome[d$group == g & d$phase == ph]
      key <- paste(g, ph, sep = ".")
      hazard[[key]] <- shrink(cnt(x), q_g)
      hazard_n[[key]] <- length(x)
    }
  }
  list(hazard = hazard, hazard_n = unlist(hazard_n))
}

# The per-conference split for a petition the SG has answered, scored for the
# conference held on `as_of`: "first" when no conference fell between the brief
# and it. NULL when the layer has no split for the recommendation (an artifact
# from before the split was fitted, or an unlabelled brief).
sg_hazard <- function(layer, view, events, as_of) {
  key_g <- view$group
  if (is.null(layer$hazard) || is.na(key_g)) return(NULL)
  cds <- tryCatch(conference_dates_from_events(events), error = function(e) as.Date(character()))
  between <- sum(cds > view$filed & cds < as.Date(as_of))
  layer$hazard[[paste(key_g, if (between == 0) "first" else "later", sep = ".")]]
}

# Label the SG briefs filed since the last run: any CVSG'd paid docket in `cases`
# whose brief is neither in the committed file nor the on-site cache. The PDF is
# read through fetch_petition_text() (R/petition_signals.R, same pacing). A failed
# download is not cached, so it is retried next run; an unparseable Conclusion is
# cached with rec = null, and the page shows the model's number until the label
# is added to the committed file by hand.
resolve_sg_views <- function(cases, site_dir, max_new = 25L) {
  if (is.null(cases) || !nrow(cases) || !"events" %in% names(cases)) return(invisible(0L))
  views <- load_sg_views(site_dir)
  known <- vapply(views, function(e) .sg_url_key(e$url), "")
  cp <- file.path(site_dir, SG_VIEWS_CACHE)
  cache <- .read_views(cp)
  n_new <- 0L
  for (i in seq_len(nrow(cases))) {
    dkt <- cases$dkt[i]
    if (!identical(funnel_case_type(dkt), "paid")) next
    b <- tryCatch(find_sg_brief(cases$events[[i]]), error = function(e) NULL)
    if (is.null(b) || is.na(b$url) || .sg_url_key(b$url) %in% known) next
    if (n_new >= max_new) break
    text <- fetch_petition_text(b$url)
    if (is.na(text)) next
    r <- sg_recommendation(sg_conclusion(text), dkt)
    cache[[dkt]] <- list(cvsg = format(lubridate::mdy(b$cvsg_date)),
                         filed = format(lubridate::mdy(b$filed)), url = b$url,
                         rec = r$rec, quote = r$quote, source = "regex", sg_v = 1L)
    known <- c(known, .sg_url_key(b$url))
    n_new <- n_new + 1L
    message("SG brief read: ", dkt, " -> ", r$rec %||% "unparsed")
  }
  if (n_new) {
    dir.create(dirname(cp), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(cache[order(names(cache))], cp, auto_unbox = TRUE, pretty = TRUE, na = "null")
  }
  invisible(n_new)
}
