# Reproduce every number in docs/pro-se-grants.md: how often the Court grants
# more than one self-filed petition in a month, and in a Term.
#
#   Rscript .github/scripts/pro_se_grants.R           # ~15 min (classification)
#   PRO_SE_CACHE=/tmp/ps.rds Rscript ...              # cache the classification
#
# No fetch and no CI job. This is an analysis script, kept in-tree so the doc's
# tables can be regenerated after a classifier change rather than trusted.
#
# Env: PRO_SE_CACHE (optional path; written on first run, read on later ones),
#      PRO_SE_EXTRA (comma-separated extra snapshots; default data-raw/snapshot_*.rds).

suppressPackageStartupMessages({library(tidyverse)})
source("R/cert_funnel.R")

cache <- Sys.getenv("PRO_SE_CACHE", unset = "")

# Two sources, and the order between them is load-bearing.
#
# `data-raw/ot_*.rds` is the committed archive, OT2017-OT2024. `snapshot_*` is
# NOT part of that glob on purpose: dropping an ot_2025.rds into data-raw would
# silently re-base the Cert Funnel, the Counsel Table and the model's training
# corpus, all of which glob `ot_*.rds`. The snapshots carry OT2025, OT2026 and a
# re-fetch of every petition the archives still had at `pending` -- an OT2024
# petition granted after the July 2025 archive snapshot is invisible otherwise,
# and ~22% of a Term's grants are of a petition docketed the Term before.
#
# Snapshots load LAST and win: keep the final occurrence of a docket, not the
# first, so a refreshed docket overrides its stale archive copy.
arch <- sort(Sys.glob("data-raw/ot_*.rds"))
if (!length(arch)) stop("no archives matched data-raw/ot_*.rds")
extra <- Sys.getenv("PRO_SE_EXTRA", unset = "")
extra <- if (nzchar(extra)) trimws(unlist(strsplit(extra, ","))) else
         sort(Sys.glob("data-raw/snapshot_*.rds"))
extra <- extra[file.exists(extra)]

cases <- map_dfr(c(arch, extra), read_rds)
cases <- cases[!duplicated(cases$dkt, fromLast = TRUE), ]
cat("archives:", paste(str_extract(basename(arch), "\\d{4}"), collapse = ", "), "\n")
cat("snapshots:", if (length(extra)) paste(basename(extra), collapse = ", ") else "(none)", "\n")
cat("dockets:", nrow(cases), "\n")

# ---- pro se ------------------------------------------------------------------
# The petitioner appears as their OWN attorney anywhere on the petitioner side --
# any petitioner row, any attorney entry, not just counsel of record.
#
# Not petitioner_pro_se() from cert_model.R, which tests counsel of record and
# therefore returns zero pro se grants. The party table is a current snapshot: a
# self-filed petition that is granted acquires merits counsel, that lawyer takes
# counsel of record, and the filer drops to a second entry (20-5279: "Allon Kedem
# (Counsel of Record) || William Dale Wooden"). The label flips on exactly the
# petitions the question is about. See docs/pro-se-grants.md.
.toks <- function(x) {
  t <- str_split(str_squish(str_replace_all(str_to_lower(x %||% ""), "[^a-z ]", " ")), " ")[[1]]
  t[nchar(t) > 1]
}
same_person <- function(party, atty) {
  pn <- .toks(party); at <- .toks(atty)
  if (!length(pn) || !length(at)) return(FALSE)
  all(pn %in% at) || all(at %in% pn)
}
petitioner_self_filed <- function(parties) {
  if (!is.data.frame(parties) || !nrow(parties)) return(FALSE)
  role <- if ("type" %in% names(parties)) parties$type
          else if ("party" %in% names(parties)) parties$party else return(FALSE)
  role <- as.character(role); role[is.na(role)] <- ""
  for (k in which(str_detect(role, regex("petition|appellant|applicant", ignore_case = TRUE)))) {
    pn <- if ("names" %in% names(parties)) paste(unlist(parties$names[[k]]), collapse = " ") else ""
    av <- unlist(parties$attys[[k]], use.names = FALSE)
    av <- str_squish(str_remove(as.character(av %||% character()),
                                regex("\\s*Counsel of Record.*$", ignore_case = TRUE)))
    for (one in av) if (same_person(pn, one)) return(TRUE)
  }
  FALSE
}

# ---- October Term boundaries --------------------------------------------------
# First Monday in October, not 1 October. Ten of the 34 pro se dispositions land
# in the first week of an October -- all of them on or after that Term's first
# Monday, so a 1-October rule agrees on this corpus by luck. The gap runs up to
# six days (2019: 1-6 Oct), and an order inside it belongs to the Term ending.
#
# The offset is (8 - %u) %% 7, NOT (9 - %u) %% 7: %u is Monday = 1, so a 1
# October that IS a Monday must move zero days. The off-by-one version pushes
# every Monday-the-1st to the 8th and reads Tuesday-the-1st as the first Monday,
# which mislabelled six of these 34 rows.
first_monday_oct <- function(y) {
  d1 <- as.Date(sprintf("%d-10-01", y))
  d1 + ((8 - as.integer(format(d1, "%u"))) %% 7)   # %u: Monday = 1
}
ot_of <- function(dt) {
  y <- as.integer(format(dt, "%Y"))
  ifelse(is.na(dt), NA_integer_, ifelse(dt >= first_monday_oct(y), y, y - 1L))
}

# ---- assemble -----------------------------------------------------------------
if (nzchar(cache) && file.exists(cache)) {
  cat("classification: reusing", cache, "\n"); cls <- read_rds(cache)
} else {
  cat("classifying (~12 min)...\n"); cls <- classify_petitions(cases)
  if (nzchar(cache)) write_rds(cls, cache)
}

d <- cls |>
  left_join(tibble(dkt = cases$dkt,
                   pro_se = map_lgl(cases$parties, petitioner_self_filed)), by = "dkt") |>
  mutate(dkt_term = 2000L + as.integer(str_extract(dkt, "^\\d+")),
         ord_term = ot_of(outcome_date),
         month    = if_else(is.na(outcome_date), as.Date(NA), floor_date(outcome_date, "month")))

# Which Terms exist, and which of them can carry a "did it happen twice" verdict.
#
# LAST_COMPLETE is a judgment, not a date arithmetic: a Term formally runs to the
# day before its successor opens, so OT2025 does not "end" until 4 Oct 2026 -- but
# the Court's last order list of a Term lands in late June or early July, and
# nothing further issues under that Term's number. A Term whose June sitting has
# passed is complete for the purpose of counting grants. OT2026 is not: its
# petitions are being docketed but the Term has not opened, so it contributes
# denominators and no grants, and would drag every fraction down if pooled in.
TERMS <- sort(unique(d$dkt_term))
LAST_COMPLETE <- suppressWarnings(as.integer(Sys.getenv("PRO_SE_LAST_COMPLETE", "2025")))
cat("Terms present: OT", paste(TERMS, collapse = ", OT"),
    " | complete through OT", LAST_COMPLETE, "\n", sep = "")

allg      <- d |> filter(outcome %in% c("granted", "gvr"), !is.na(month))
months_all <- seq(min(allg$month), max(allg$month), by = "month")   # every calendar month
months_ord <- sort(unique(allg$month))                              # months with any grant
cat("window:", format(min(months_all)), "..", format(max(months_all)),
    "| calendar months:", length(months_all), "| order months:", length(months_ord), "\n")

per_month <- function(g, months) {
  tibble(month = months) |>
    left_join(count(g, month, name = "k"), by = "month") |>
    mutate(k = replace_na(k, 0L))
}
line <- function(g, months, lbl) {
  k <- per_month(g, months)$k
  cat(sprintf("  %-16s n=%3d | >=1 %2d (%4.1f%%) | >=2 %2d (%4.1f%%) | >=3 %2d (%4.1f%%) | max %d\n",
              lbl, length(months), sum(k >= 1), 100*mean(k >= 1), sum(k >= 2), 100*mean(k >= 2),
              sum(k >= 3), 100*mean(k >= 3), max(k)))
}

for (nm in c("plenary grants", "plenary + GVR")) {
  oc <- if (nm == "plenary grants") "granted" else c("granted", "gvr")
  g  <- d |> filter(pro_se, outcome %in% oc, !is.na(month))
  cat("\n==", nm, "-- total", nrow(g), "==\n(a) months\n")
  line(g, months_all, "calendar months"); line(g, months_ord, "order months")
  m2 <- per_month(g, months_all) |> filter(k >= 2)
  if (nrow(m2)) cat("  >=2 in:", paste0(format(m2$month, "%b %Y"), " (", m2$k, ")", collapse = ", "), "\n")

  cat("(b) Terms\n")
  for (v in c("ord_term", "dkt_term")) {
    t <- tibble(y = TERMS) |>
      left_join(count(g, y = .data[[v]], name = "k"), by = "y") |> mutate(k = replace_na(k, 0L))
    tc <- filter(t, y <= LAST_COMPLETE)
    cat(sprintf("  %-9s %s | over OT%d-OT%d: >=1 %d/%d | >=2 %d/%d | max %d\n", v,
                paste(sprintf("%2d", t$k), collapse = " "), min(TERMS), LAST_COMPLETE,
                sum(tc$k >= 1), nrow(tc), sum(tc$k >= 2), nrow(tc), max(t$k)))
  }
  inc <- setdiff(TERMS, TERMS[TERMS <= LAST_COMPLETE])
  if (length(inc)) cat("  (OT", paste(inc, collapse = ", "),
                       " excluded from the fractions -- see LAST_COMPLETE)\n", sep = "")
}

cat("\n== base rates (resolved petitions) ==\n")
print(d |> filter(!is.na(outcome_date)) |> group_by(type, pro_se) |>
        summarise(resolved = n(), granted = sum(outcome == "granted"),
                  gvr = sum(outcome == "gvr"), .groups = "drop") |> as.data.frame(), right = FALSE)

roll <- d |> filter(pro_se, outcome %in% c("granted", "gvr")) |> arrange(outcome_date)

cat("\n== the full roll ==\n")
print(roll |> transmute(dkt, date = outcome_date, ot_order = ord_term, ot_docket = dkt_term,
                        type, outcome, caption = str_trunc(caption, 60)) |>
        as.data.frame(), right = FALSE)

# PRO_SE_MD=1 emits the roll as the markdown table docs/pro-se-grants.md carries,
# so the doc is regenerated rather than hand-edited after a re-run.
if (nzchar(Sys.getenv("PRO_SE_MD", ""))) {
  cat("\n== markdown roll ==\n")
  cat("| # | docket | order date | OT (order) | OT (docket) | type | disposition | case |\n")
  cat("| --- | --- | --- | --- | --- | --- | --- | --- |\n")
  for (i in seq_len(nrow(roll))) cat(sprintf(
    "| %d | %s | %s | OT%d | OT%d | %s | %s | %s |\n", i,
    if (roll$outcome[i] == "granted") paste0("**", roll$dkt[i], "**") else roll$dkt[i],
    format(roll$outcome_date[i], "%Y-%m-%d"), roll$ord_term[i], roll$dkt_term[i],
    if (roll$type[i] == "ifp") "IFP" else "Paid",
    if (roll$outcome[i] == "granted") "**plenary grant**" else "GVR / summary",
    str_replace_all(roll$caption[i], "\\|", "-")))
}
