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
  for (f in c("counsel_table.R", "docket_page.R", "argument_lean.R", "term_range.R"))
    if (!exists(switch(f, counsel_table.R = "counsel_archive_paths",
                          docket_page.R = "brief_cover",
                          argument_lean.R = "judgment_of",
                          term_range.R = "term_of_docket")))
      sys.source(find(f), envir = globalenv())
})

# ---- inputs --------------------------------------------------------------------

# A re-fetch of every archived docket that carries an amicus brief, with the
# docket JSON's "Other" block kept as an `amici` column (build_amici()). Written
# by refetch-amici.yml. Optional: without it the page still renders, from the
# archives alone, with no counsel boards and the merits stage of the most recent
# docket years thin (see .github/scripts/refetch_amici.R).
AMICUS_REFRESH <- "data-raw/amici_refresh.rds"

# The extracted briefs, committed beside the stats by make_amicus_stats.R under
# the same fingerprint: the Term-range pages are computed from it, in seconds
# each, rather than from a 13-minute extraction.
AMICUS_BRIEFS_PATH <- "data/amicus_briefs.rds"

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
      argued <- argued_dates_of(ev)
      rows <- map(hit, function(k) {
        cv <- brief_cover(po[k], granted_on, ed[k], rbo, sides)
        if (is.null(cv)) return(NULL)
        col <- str_match(cv$color, "c-([a-z]+)")[, 2]
        if (!col %in% c("cream", "lgreen", "dgreen")) return(NULL)
        # The Term the brief belongs to (R/term_range.R): the Term docketed at
        # the petition stage, the Term argued on the merits.
        tm <- if (col == "cream") term_of_docket(x$dkt[i]) else term_of_merits_brief(ed[k], argued)
        tibble(dkt = x$dkt[i], caption = x$caption[i], date = ed[k], term = tm,
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
    filter(nzchar(filer), !is.na(term)) |>
    mutate(term_docketed = term_of_docket(dkt),
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
  pet$term <- term_of_docket(pet$dkt)
  structure(.amicus_writers(briefs), petitions = pet)
}

# Who wrote each brief, keyed: counsel, firm, and whether the counsel works for
# the filer (Cato's own lawyers on Cato's brief are the filer, not outside
# counsel; same word test as the case pages' party matcher). Per brief and the
# same in every Term range, so worked out once here -- it was over half the
# time of each amicus_stats_from() call.
.amicus_firm_key <- function(f) str_squish(str_remove(amicus_key(f), "\\s+(pllc|llp|llc|pc|p c|lp)$"))
.amicus_writers <- function(b) {
  has <- !is.na(b$counsel) & nzchar(coalesce(b$counsel, ""))
  b$ck <- NA_character_; b$fk <- NA_character_; b$in_house <- NA
  b$ck[has] <- map_chr(b$counsel[has], counsel_key)
  b$fk[has] <- .amicus_firm_key(b$firm[has])
  b$in_house[has] <- map2_lgl(b$fk[has], b$key[has],
                              ~ nzchar(coalesce(.x, "")) && (.x == .y || party_score(.y, .x) >= 2))
  b
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

# ---- the coalition network -------------------------------------------------------
# The allies table's pairs, all of them rather than the top 25: every frequent
# filer (AMICUS_PAIR_FILER_MIN merits briefs) joined to every other it met on
# the same schedule in at least AMICUS_PAIR_MIN cases. Groups are Louvain
# communities and the picture a force layout, both computed here in base R --
# igraph is not on the CI runners, and a layout fixed at build time means the
# page draws the same graph for every reader and with no script at all.

# Louvain's local-moving phase, repeated on the aggregated graph until no node
# moves. Deterministic: nodes are visited in the order given, ties go to the
# community they are already in. Returns a membership vector.
.amicus_louvain <- function(W) {
  memb <- seq_len(nrow(W)); A <- W
  repeat {
    k <- rowSums(A); m2 <- sum(A); comm <- seq_len(nrow(A)); moved_any <- FALSE
    repeat {
      moved <- FALSE
      for (i in seq_len(nrow(A))) {
        ci <- comm[i]; comm[i] <- 0L
        lev <- factor(comm, levels = unique(c(comm, ci)))
        tot <- tapply(k, lev, sum); kin <- tapply(A[i, ], lev, sum)
        cand <- union(ci, unique(comm[A[i, ] > 0 & comm > 0]))
        gain <- vapply(cand, function(c) coalesce(kin[[as.character(c)]], 0) -
                         coalesce(tot[[as.character(c)]], 0) * k[i] / m2, 0)
        best <- cand[which.max(gain)]
        if (best != ci) moved <- moved_any <- TRUE
        comm[i] <- best
      }
      if (!moved) break
    }
    comm <- match(comm, unique(comm))
    memb <- comm[memb]
    if (!moved_any || max(comm) == nrow(A)) break
    G <- outer(comm, seq_len(max(comm)), "==") * 1
    A <- t(G) %*% A %*% G
  }
  memb
}

# Fruchterman-Reingold, seeded on a circle in group order so each community
# starts together, with a weak pull to the centre. Returns an n x 2 matrix
# scaled into [0, 1] on both axes.
.amicus_fr <- function(W, group, iters = 700L, spread = 2) {
  n <- nrow(W)
  if (n == 1) return(matrix(0.5, 1, 2))
  ord <- order(group, -rowSums(W))
  a <- numeric(n); a[ord] <- 2 * pi * (seq_len(n) - 1) / n
  x <- 0.5 + 0.4 * cos(a); y <- 0.5 + 0.4 * sin(a)
  k <- spread * sqrt(1 / n); w <- sqrt(W / max(W))
  for (it in seq_len(iters)) {
    t <- 0.08 * (1 - it / iters) + 0.002
    dx <- outer(x, x, "-"); dy <- outer(y, y, "-")
    d <- sqrt(dx^2 + dy^2) + 1e-4
    f <- k^2 / d - w * d^2 / k
    diag(f) <- 0
    mx <- rowSums(f * dx / d) - 0.4 * (x - 0.5)
    my <- rowSums(f * dy / d) - 0.4 * (y - 0.5)
    len <- sqrt(mx^2 + my^2) + 1e-9
    step <- pmin(len, t)
    x <- x + mx / len * step; y <- y + my / len * step
  }
  rng <- function(v) (v - min(v)) / max(diff(range(v)), 1e-9)
  cbind(rng(x), rng(y))
}

# Lay out each connected piece on its own. Laid out together, a piece with no
# tie to the rest (the IP bar's four associations) is pushed by repulsion alone
# to the far corner and squeezes everything else into what is left. The
# largest piece fills the panel's left `main` share; the others stack, largest
# first, in the column to its right.
.amicus_layout <- function(W, group, main = 0.82) {
  n <- nrow(W); comp <- rep(0L, n); cc <- 0L
  for (s in seq_len(n)) if (!comp[s]) {
    cc <- cc + 1L; q <- s; comp[s] <- cc
    while (length(q)) {
      v <- q[1]; q <- q[-1]
      nb <- which(W[v, ] > 0 & !comp); comp[nb] <- cc; q <- c(q, nb)
    }
  }
  sizes <- tabulate(comp); ord <- order(-sizes)
  xy <- matrix(0, n, 2)
  big <- which(comp == ord[1])
  p <- .amicus_fr(W[big, big, drop = FALSE], group[big])
  xy[big, ] <- cbind(p[, 1] * if (length(ord) > 1) main else 1, p[, 2])
  rest <- ord[-1]
  if (length(rest)) {
    # Each small piece gets a band of the right-hand column in proportion to its size.
    h <- sizes[rest] / sum(sizes[rest]); top <- c(0, cumsum(h))[seq_along(rest)]
    for (r in seq_along(rest)) {
      ix <- which(comp == rest[r])
      p <- .amicus_fr(W[ix, ix, drop = FALSE], group[ix], iters = 300L)
      span <- min(h[r] * 0.8, 0.22)
      xy[ix, 1] <- main + 0.06 + p[, 1] * (1 - main - 0.06)
      xy[ix, 2] <- top[r] + (h[r] - span) / 2 + p[, 2] * span
    }
  }
  xy
}

# Up to this many groups get a hue of their own; any beyond share --net-other.
AMICUS_NET_GROUPS <- 4L

amicus_network <- function(m, disp, pair_min = AMICUS_PAIR_MIN) {
  j <- inner_join(m, m, by = "case", suffix = c("1", "2"), relationship = "many-to-many") |>
    filter(side1 == side2, key1 < key2) |> distinct(case, key1, key2) |>
    count(key1, key2, name = "cases") |> filter(cases >= pair_min)
  if (nrow(j) < 2) return(NULL)
  keys <- sort(unique(c(j$key1, j$key2)))
  W <- matrix(0, length(keys), length(keys))
  W[cbind(match(j$key1, keys), match(j$key2, keys))] <- j$cases
  W <- W + t(W)
  comm <- .amicus_louvain(W)
  strength <- rowSums(W)
  # Groups ranked by their total strength, so the largest takes --net-1.
  rank <- order(-tapply(strength, comm, sum))
  group <- match(comm, rank)
  xy <- .amicus_layout(W, group)
  briefs <- m |> count(key, name = "briefs")
  nodes <- tibble(key = keys, strength = strength, group = group,
                  x = round(xy[, 1], 4), y = round(xy[, 2], 4)) |>
    left_join(disp |> select(key, name), by = "key") |>
    left_join(briefs, by = "key") |>
    mutate(name = coalesce(name, key))
  groups <- nodes |> group_by(group) |>
    summarise(n = n(), lead = paste(head(name[order(-strength)], 3), collapse = "; "),
              .groups = "drop") |> arrange(group)
  edges <- j |> transmute(i = match(key1, keys) - 1L, j = match(key2, keys) - 1L, cases)
  list(nodes = nodes, edges = edges, groups = groups,
       modularity = .amicus_modularity(W, comm))
}

.amicus_modularity <- function(W, comm) {
  k <- rowSums(W); m2 <- sum(W)
  round(sum((W - outer(k, k) / m2) * outer(comm, comm, "==")) / m2, 3)
}

amicus_stats_from <- function(briefs, fingerprint, terms,
                              top_n = AMICUS_TOP_N, min_rate = AMICUS_MIN_RATE,
                              pair_filer_min = AMICUS_PAIR_FILER_MIN,
                              pair_min = AMICUS_PAIR_MIN) {
  pet <- attr(briefs, "petitions")
  if (!"in_house" %in% names(briefs)) briefs <- .amicus_writers(briefs)
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
    filter(n >= pair_filer_min) |> pull(key)
  m <- nonfed |> filter(stage == "merits", key %in% freq) |>
    mutate(case = str_squish(tolower(caption))) |>
    distinct(case, side, key)
  pairs_of <- function(a, b, same) {
    j <- inner_join(a, b, by = "case", suffix = c("1", "2"), relationship = "many-to-many")
    j <- if (same) j |> filter(side1 == side2, key1 < key2) else
      j |> filter(side1 == "pet", side2 == "resp")
    j |> distinct(case, key1, key2) |> count(key1, key2, name = "cases") |>
      filter(cases >= pair_min) |> arrange(desc(cases), key1, key2) |> head(top_n)
  }
  label <- function(d) d |>
    left_join(disp |> select(key1 = key, name1 = name), by = "key1") |>
    left_join(disp |> select(key2 = key, name2 = name), by = "key2")
  allies <- label(pairs_of(m, m, TRUE))
  opponents <- label(pairs_of(m, m, FALSE))
  network <- amicus_network(m, disp, pair_min = pair_min)

  # ---- who writes them: the "Other" block, where the refresh carried it.
  # Counsel, firm and in-house are per brief (.amicus_writers()).
  cov <- briefs |> filter(has_block %in% TRUE)
  wr <- cov |> filter(!is.na(counsel), nzchar(counsel), kind != "us")
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
      pair_filer_min = pair_filer_min, pair_min = pair_min,
      as_of = as.character(suppressWarnings(max(briefs$date, na.rm = TRUE)))),
    by_term = by_term, filers = filers, door = door, record = record,
    us = us_out, states = states, allies = allies, opponents = opponents,
    network = network, firms = firms, counsel = counsel,
    fingerprint = fingerprint)
}

compute_amicus_stats <- function(paths = amicus_input_paths(), briefs = amicus_briefs(paths)) {
  span <- amicus_term_span(briefs)
  amicus_stats_range(briefs, span[1], span[2], fingerprint = amicus_stats_fingerprint(paths))
}

# The Terms the briefs cover: the first docketed Term to the latest Term any
# brief belongs to, which is the Term under way once its cases are briefed.
amicus_term_span <- function(briefs) range(briefs$term, na.rm = TRUE)

# The page's statistics over Terms `from`..`to` (R/term_range.R): the briefs that
# belong to those Terms, the petitions docketed in them, and every floor scaled
# to the range's length. The full span is a range like any other, so the
# canonical page and a range page cannot be computed two ways.
amicus_stats_range <- function(briefs, from, to, fingerprint = NA_character_) {
  pet <- attr(briefs, "petitions")
  b <- briefs[!is.na(briefs$term) & briefs$term >= from & briefs$term <= to, , drop = FALSE]
  attr(b, "petitions") <- pet[!is.na(pet$term) & pet$term >= from & pet$term <= to, , drop = FALSE]
  n <- to - from + 1L
  st <- amicus_stats_from(b, fingerprint = fingerprint, terms = term_range_label(from, to),
                          min_rate = scale_floor(AMICUS_MIN_RATE, n, lo = 3L),
                          pair_filer_min = scale_floor(AMICUS_PAIR_FILER_MIN, n, lo = 3L),
                          pair_min = scale_floor(AMICUS_PAIR_MIN, n, lo = 2L))
  st$totals$term_from <- from; st$totals$term_to <- to; st$totals$n_terms <- n
  span <- amicus_term_span(briefs)
  st$totals$span_from <- span[1]; st$totals$span_to <- span[2]
  st
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
                     ".amicus_merits_record", "amicus_stats_from", "amicus_network",
                     ".amicus_louvain", ".amicus_fr", ".amicus_layout", ".amicus_modularity",
                     ".amicus_writers", ".amicus_firm_key", "amicus_stats_range",
                     "amicus_term_span", "compute_amicus_stats",
                     # which Term a brief belongs to, and the scaled floors
                     "term_of_docket", "term_of_date", "term_of_merits_brief",
                     "argued_dates_of", "scale_floor", "term_range_label",
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
                   AMICUS_NET_GROUPS, as.character(ARGUED_ENTRY_RX), TERM_FLOOR_SPAN,
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
  .an-stage{position:relative;background:var(--panel,@panel@);border:1px solid var(--rule,@rule@);margin:.4rem 0 .6rem}
  .an-stage svg{display:block;width:100%;height:auto}
  .an-e{stroke:@faint@;stroke-opacity:.16;stroke-linecap:round}
  .an-e.g1{stroke:@net-1@;stroke-opacity:.3}.an-e.g2{stroke:@net-2@;stroke-opacity:.3}
  .an-e.g3{stroke:@net-3@;stroke-opacity:.3}.an-e.g4{stroke:@net-4@;stroke-opacity:.3}
  .an-n circle{stroke:@panel@;stroke-width:1.5}
  .an-n.g1 circle{fill:@net-1@}.an-n.g2 circle{fill:@net-2@}.an-n.g3 circle{fill:@net-3@}
  .an-n.g4 circle{fill:@net-4@}.an-n.g0 circle{fill:@net-other@}
  .an-n{cursor:default;outline:none}
  .an-l{font-size:11.5px;fill:@ink@;pointer-events:none;paint-order:stroke;
    stroke:@panel@;stroke-width:3px;stroke-linejoin:round}
  .an-hot .an-e{stroke-opacity:.04}.an-hot .an-e.on{stroke-opacity:.75}
  .an-hot .an-n{opacity:.25}.an-hot .an-n.on,.an-hot .an-n.nb{opacity:1}
  .an-hot .an-l{opacity:.2}.an-hot .an-l.on{opacity:1}
  .an-n.on circle{stroke:@ink@;stroke-width:2}
  .an-tip{position:absolute;pointer-events:none;background:@ink@;color:@paper@;font-size:.8rem;
    line-height:1.35;padding:.35rem .6rem;border-radius:2px;max-width:17rem;opacity:0;
    transition:opacity .12s}
  .an-tip.on{opacity:1}.an-tip b{font-weight:600}
  .an-legend{display:flex;flex-wrap:wrap;gap:.4rem 1.4rem;font-size:.84rem;color:var(--ink-soft,@ink-soft@);
    margin:.2rem 0 .9rem}
  .an-legend i{display:inline-block;width:.75rem;height:.75rem;border-radius:50%;margin-right:.4rem;
    vertical-align:-.05rem}
  .an-legend .g1 i{background:@net-1@}.an-legend .g2 i{background:@net-2@}
  .an-legend .g3 i{background:@net-3@}.an-legend .g4 i{background:@net-4@}
  .an-legend .g0 i{background:@net-other@}
  @media (prefers-reduced-motion:reduce){.an-tip{transition:none}}
" |> fill_palette())

# The page's only script: hover or tap a filer to light its partners, with the
# names and counts in a tip. The graph itself is drawn at build time, so with
# no script the reader still has the picture, the legend and the table below.
AMICUS_NETWORK_JS <- "
(function(){
  var st=document.getElementById('an-stage'); if(!st) return;
  var svg=st.querySelector('svg'), tip=document.getElementById('an-tip');
  var nodes=[].slice.call(svg.querySelectorAll('.an-n')), edges=[].slice.call(svg.querySelectorAll('.an-e'));
  function esc(s){return String(s).replace(/[&<>\"]/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;'}[c];});}
  var byI={}; nodes.forEach(function(n){byI[n.getAttribute('data-i')]=n;});
  var labs={}; [].slice.call(svg.querySelectorAll('.an-l')).forEach(function(l){labs[l.getAttribute('data-i')]=l;});
  function lit(i){var l=labs[i]; if(l) l.classList.add('on');}
  var adj={}; edges.forEach(function(e){var a=e.getAttribute('data-i'),b=e.getAttribute('data-j'),c=+e.getAttribute('data-c');
    (adj[a]=adj[a]||[]).push([b,c,e]); (adj[b]=adj[b]||[]).push([a,c,e]);});
  function clear(){svg.classList.remove('an-hot'); nodes.forEach(function(n){n.classList.remove('on','nb');});
    edges.forEach(function(e){e.classList.remove('on');});
    for(var k in labs) labs[k].classList.remove('on'); tip.classList.remove('on');}
  function place(n){var r=st.getBoundingClientRect(), b=n.querySelector('circle').getBoundingClientRect();
    var x=b.left+b.width/2-r.left, y=b.top-r.top-8; tip.style.left=Math.max(4,Math.min(x-tip.offsetWidth/2,r.width-tip.offsetWidth-4))+'px';
    tip.style.top=Math.max(4,y-tip.offsetHeight)+'px';}
  function show(n){clear(); var i=n.getAttribute('data-i'), nb=(adj[i]||[]).slice().sort(function(p,q){return q[1]-p[1];});
    svg.classList.add('an-hot'); n.classList.add('on'); lit(i);
    nb.forEach(function(p){p[2].classList.add('on'); byI[p[0]].classList.add('nb'); lit(p[0]);});
    var h='<b>'+esc(n.getAttribute('data-n'))+'</b><br>'+n.getAttribute('data-b')+' merits briefs; same side as '+nb.length+' frequent filers';
    nb.slice(0,3).forEach(function(p){h+='<br>'+esc(byI[p[0]].getAttribute('data-n'))+': '+p[1]+' cases';});
    tip.innerHTML=h; tip.classList.add('on'); place(n);}
  nodes.forEach(function(n){n.addEventListener('mouseenter',function(){show(n);});
    n.addEventListener('focus',function(){show(n);});
    n.addEventListener('click',function(ev){ev.stopPropagation(); if(n.classList.contains('on')) clear(); else show(n);});});
  svg.addEventListener('mouseleave',clear); document.addEventListener('click',clear);
  nodes.forEach(function(n){n.addEventListener('blur',clear);});
})();
"

# A filer's name for a label: the docket's own words, less a leading "The" and
# a corporate tail. The tip carries the name in full.
.amicus_short_name <- function(x) {
  x |> str_remove("^The\\s+") |> str_remove(",?\\s+(Inc|L\\.?L\\.?C|Corp)\\.?$") |> str_squish()
}

# The coalition network as inline SVG. Positions come from amicus_network();
# this sizes the circles and edges, chooses which filers get a printed label
# (the strongest, where a label fits without covering another label or a
# circle) and leaves the rest to the tip.
.amicus_network_svg <- function(net, W = 860, H = 600, labels = 30L) {
  nodes <- as_tibble(net$nodes); edges <- as_tibble(net$edges)
  mx <- 70; my <- 34
  px <- mx + nodes$x * (W - 2 * mx); py <- my + nodes$y * (H - 2 * my)
  b <- coalesce(as.numeric(nodes$briefs), 0)
  r <- 3.5 + 9 * sqrt(b / max(b, 1))
  g <- ifelse(nodes$group <= AMICUS_NET_GROUPS, nodes$group, 0L)
  cmax <- max(edges$cases); cmin <- min(edges$cases)
  ew <- 0.5 + 3.5 * sqrt((edges$cases - cmin) / max(cmax - cmin, 1))
  same <- g[edges$i + 1] == g[edges$j + 1] & g[edges$i + 1] > 0
  eorder <- order(edges$cases)
  e_svg <- paste0("<line class='an-e", ifelse(same, paste0(" g", g[edges$i + 1]), ""), "' x1='",
                  round(px[edges$i + 1], 1), "' y1='", round(py[edges$i + 1], 1), "' x2='",
                  round(px[edges$j + 1], 1), "' y2='", round(py[edges$j + 1], 1),
                  "' stroke-width='", round(ew, 2), "' data-i='", edges$i, "' data-j='", edges$j,
                  "' data-c='", edges$cases, "'/>")[eorder]

  # Labels: strongest first; try below, above, right, left of the circle.
  short <- .amicus_short_name(nodes$name)
  lines_of <- function(s) {
    if (nchar(s) <= 24) return(s)
    w <- strsplit(s, " ")[[1]]; cut <- which.min(abs(cumsum(nchar(w) + 1) - nchar(s) / 2))
    c(paste(w[seq_len(cut)], collapse = " "), paste(w[-seq_len(cut)], collapse = " "))
  }
  boxes <- list(); lab <- rep("", nrow(nodes))
  hits <- function(bx) {
    for (o in boxes) if (!(bx[1] + bx[3] < o[1] || o[1] + o[3] < bx[1] ||
                           bx[2] + bx[4] < o[2] || o[2] + o[4] < bx[2])) return(TRUE)
    # A label may cross a small circle (it has a halo and sits above the
    # nodes), but never one of the large ones, which are the filers it names.
    cx <- pmax(bx[1], pmin(px, bx[1] + bx[3])); cy <- pmax(bx[2], pmin(py, bx[2] + bx[4]))
    any((sqrt((cx - px)^2 + (cy - py)^2) < r + 1)[big])
  }
  big <- r >= quantile(r, 0.7)
  # The strongest filers, and each group's strongest, so no community goes unnamed.
  lead_of <- vapply(split(seq_len(nrow(nodes)), nodes$group),
                    function(ix) ix[which.max(nodes$strength[ix])], 1L)
  for (k in unique(c(lead_of, head(order(-nodes$strength), labels)))) {
    ln <- lines_of(short[k]); w <- max(nchar(ln)) * 6.2; h <- length(ln) * 13
    cands <- list(c(px[k] - w / 2, py[k] + r[k] + 3, "middle"),
                  c(px[k] - w / 2, py[k] - r[k] - 3 - h, "middle"),
                  c(px[k] + r[k] + 4, py[k] - h / 2, "start"),
                  c(px[k] - r[k] - 4 - w, py[k] - h / 2, "end"))
    for (cd in cands) {
      bx <- c(as.numeric(cd[1]), as.numeric(cd[2]), w, h)
      if (bx[1] < 2 || bx[1] + w > W - 2 || bx[2] < 2 || bx[2] + h > H - 2 || hits(bx)) next
      boxes[[length(boxes) + 1]] <- bx
      tx <- switch(cd[3], middle = bx[1] + w / 2, start = bx[1], end = bx[1] + w)
      lab[k] <- paste0("<text class='an-l' data-i='", k - 1L, "' x='", round(tx, 1),
                       "' text-anchor='", cd[3], "'>",
                       paste0("<tspan x='", round(tx, 1), "' y='", round(bx[2] + 10 + 13 * (seq_along(ln) - 1), 1),
                              "'>", htmlEscape(ln), "</tspan>", collapse = ""), "</text>")
      break
    }
  }
  n_svg <- paste0("<g class='an-n g", g, "' tabindex='0' data-i='", seq_len(nrow(nodes)) - 1L,
                  "' data-n='", htmlEscape(nodes$name, attribute = TRUE), "' data-b='", b,
                  "'><title>", htmlEscape(nodes$name), "</title><circle cx='", round(px, 1),
                  "' cy='", round(py, 1), "' r='", round(r, 1), "'/></g>")
  # Larger circles first, so a small one is never hidden beneath them; the
  # labels go in a layer of their own above every circle.
  n_svg <- n_svg[order(-r)]
  paste0("<div class='an-stage' id='an-stage'><svg viewBox='0 0 ", W, " ", H,
         "' role='img' aria-label='Network of frequent amicus filers that file on the same side'>",
         paste(e_svg, collapse = ""), paste(n_svg, collapse = ""),
         "<g class='an-labels'>", paste(lab[nzchar(lab)], collapse = ""), "</g>",
         "</svg><div class='an-tip' id='an-tip'></div></div>")
}

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
  net <- stats$network
  has_net <- !is.null(net) && length(net$nodes) && NROW(as_tib(net$nodes)) > 1
  if (has_net) {
    net$nodes <- as_tib(net$nodes); net$edges <- as_tib(net$edges)
    net_groups <- as_tib(net$groups)
  }

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
    head_cells <- c("Term", "Briefs", "Cert", "Pet./neither", "Resp.", "Cases", "Per case")
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

  rng <- .term_nav(tot)
  counsel_href <- .term_href(rng, "index.html", "/counsel/")
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
        "writes them — across ", tot$terms, "."))),
      if (!is.null(rng)) HTML(term_picker_html("amicus.html", rng$from, rng$to, rng$span)),
      tags$div(class = "clede", tagList(
        tags$p(HTML(smarten(paste0(
          .cn(tot$briefs), " amicus briefs in ", .cn(tot$cases), " cases, from ",
          .cn(tot$filers), " filers other than the United States. ", .cn(tot$cert),
          " came in at the petition stage, asking the Court to take a case; ",
          .cn(tot$merits), " came after a grant, in ", .cn(tot$merits_cases),
          " cases. As at the lectern, the field is long-tailed: ", .cn(tot$one_off),
          " filers (", .cpct(tot$one_off / tot$filers, 0), ") appear exactly once.",
          if (isTRUE(rng$part)) paste0(
            " Every floor on the page is scaled to the range. Over all ", rng$span_n,
            " Terms a filer needs ", AMICUS_MIN_RATE, " decided cases to be rated and ",
            AMICUS_PAIR_FILER_MIN, " merits briefs to be paired; over ",
            term_range_label(rng$from, rng$to), " it needs ", tot$min_rate, " and ",
            tot$pair_filer_min, ".") else "")))),
        tags$p(HTML(smarten(paste0(
          "Every brief here is classified by the same rules that colour the timeline on ",
          "each case page, so the two always agree. <a href='", counsel_href, "'>The Counsel ",
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

      sec("By Term", "The flow, Term by Term",
          paste0("Each brief belongs to the Term in which it was filed for: a ",
                 "petition-stage brief to the Term the case was docketed, a merits ",
                 "brief to the Term the case was argued. A case granted in January is ",
                 "usually briefed and argued the following Term, so its two kinds of ",
                 "brief land a row apart. <em>Cases</em> and <em>per case</em> count ",
                 "the cases that drew merits briefs. The Term under way is still being ",
                 "briefed, and the undecided count says how far.",
                 .term_coverage_note(rng, "merits briefs")),
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
      if (has_net) tagList(
        tags$h3("The network"),
        note(paste0(
          "Every pair of those filers that met on the same schedule in ", tot$pair_min,
          " or more cases, not only the strongest ", nrow(allies), ". ",
          .cn(nrow(net$nodes)), " filers and ", .cn(nrow(net$edges)), " pairings. ",
          "A circle&rsquo;s size is the filer&rsquo;s merits briefs and a line&rsquo;s ",
          "weight the cases the two shared. Colours are communities found in the ",
          "pairings alone (Louvain; modularity ", formatC(num(net$modularity), format = "f", digits = 2),
          "), named here by their most connected members: the page infers who files ",
          "together, not why. Hover or tap a filer for its partners.")),
        HTML(.amicus_network_svg(net)),
        HTML(paste0("<div class='an-legend'>", paste0(
          "<span class='g", ifelse(net_groups$group <= AMICUS_NET_GROUPS, net_groups$group, 0L), "'><i></i>",
          htmlEscape(map_chr(net_groups$lead, function(s)
            paste(.amicus_short_name(head(strsplit(s, "; ")[[1]], 2)), collapse = ", "))),
          " &middot; ", net_groups$n, " filers</span>", collapse = ""), "</div>")),
        tags$script(HTML(AMICUS_NETWORK_JS))),
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
          "Briefs in paid and in forma pauperis cases belonging to ", tot$terms,
          " — at the petition stage by the Term the case was docketed, on the merits ",
          "by the Term it was argued — through ",
          str_squish(format(as.Date(tot$as_of), "%B %e, %Y")), ". The archive begins ",
          "with the cases docketed in OT2017. Briefs on emergency ",
          "applications and in original actions are not counted. A brief filed in ",
          "consolidated cases counts once. Rejected filings, motions for leave and the ",
          "Court-appointed amicus defending a judgment below are not amicus briefs here."))))
      )),
      tags$p(class = "back", HTML(paste0("<a href='", counsel_href, "'>&larr; The Counsel Table</a>")))
    ))

  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head(paste0("Friends of the Court", if (isTRUE(rng$part)) paste0(", ", tot$terms) else "",
                     " — Supreme Court Report"),
              jsonld = site_breadcrumb_jsonld("Amicus filers",
                                              list(href = "/counsel/", label = "Counsel")),
              extra_css = paste0(AMICUS_CSS, if (!is.null(rng)) TERM_PICKER_CSS else ""),
              description = paste(
                "Who files amicus briefs at the Supreme Court: the most active filers,",
                "the petitions they supported, the coalitions they form, the States,",
                "and the lawyers who write the briefs."),
              path = if (isTRUE(rng$part)) NULL else "/counsel/amicus.html",
              extra_head = if (isTRUE(rng$part)) term_range_head("amicus.html", rng$span) else NULL), "\n",
    as.character(body), "\n</html>\n")
  out <- file.path(out_dir, "amicus.html")
  writeLines(enc2utf8(smarten_html(html)), out, useBytes = TRUE)
  invisible(out)
}
