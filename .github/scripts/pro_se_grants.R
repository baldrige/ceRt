# Reproduce every number in docs/pro-se-grants.md: how often the Court grants
# more than one self-filed petition in a month, and in a Term.
#
#   Rscript .github/scripts/pro_se_grants.R           # ~12 min (classification)
#   PRO_SE_CACHE=/tmp/ps.rds Rscript ...              # cache the classification
#
# No fetch and no CI job. This is an analysis script, kept in-tree so the doc's
# tables can be regenerated after a classifier change rather than trusted.
#
# Env: PRO_SE_CACHE (optional path; written on first run, read on later ones).

suppressPackageStartupMessages({library(tidyverse)})
source("R/cert_funnel.R")

cache <- Sys.getenv("PRO_SE_CACHE", unset = "")

arch <- sort(Sys.glob("data-raw/ot_*.rds"))
if (!length(arch)) stop("no archives matched data-raw/ot_*.rds")
cases <- map_dfr(arch, read_rds) |> distinct(dkt, .keep_all = TRUE)
cat("archives:", paste(str_extract(basename(arch), "\\d{4}"), collapse = ", "),
    "| dockets:", nrow(cases), "\n")

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
    t <- tibble(y = 2017:2024) |>
      left_join(count(g, y = .data[[v]], name = "k"), by = "y") |> mutate(k = replace_na(k, 0L))
    cat(sprintf("  %-9s %s | >=1 %d/8 | >=2 %d/8 | max %d\n", v,
                paste(sprintf("%2d", t$k), collapse = " "), sum(t$k >= 1), sum(t$k >= 2), max(t$k)))
  }
}

cat("\n== base rates (resolved petitions) ==\n")
print(d |> filter(!is.na(outcome_date)) |> group_by(type, pro_se) |>
        summarise(resolved = n(), granted = sum(outcome == "granted"),
                  gvr = sum(outcome == "gvr"), .groups = "drop") |> as.data.frame(), right = FALSE)

cat("\n== the full roll ==\n")
print(d |> filter(pro_se, outcome %in% c("granted", "gvr")) |> arrange(outcome_date) |>
        transmute(dkt, date = outcome_date, ot_order = ord_term, ot_docket = dkt_term,
                  type, outcome, caption = str_trunc(caption, 60)) |>
        as.data.frame(), right = FALSE)
