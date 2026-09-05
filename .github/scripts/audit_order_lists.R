# Audit the cert funnel's grammar against the Court's order lists.
#
# The order lists (R/orders_list.R, docs/order-lists.md) are the Court's own
# statement of what it granted, GVR'd, denied and dismissed on each order day.
# classify_petitions() reads the same events off each docket. This script joins
# the two, one row per list entry, and reports every disagreement. Findings and
# the reading rules are written up in docs/order-list-audit-2026-09.md.
#
# Usage, from the repo root, with a gh-pages checkout (or `git archive
# origin/gh-pages orders/orders.json orders/data | tar -x -C <dir>`):
#
#   Rscript .github/scripts/audit_order_lists.R <dir>/orders <report.md>
#
# Reads the historical docket snapshots in data-raw/ (ot_20NN.rds, snapshot_*.rds);
# writes a Markdown report and an .rds of the comparison beside it.
suppressPackageStartupMessages({library(tidyverse); library(jsonlite); library(lubridate)})
args <- commandArgs(trailingOnly = TRUE)
odir <- args[1]; out_md <- args[2]
source("R/cert_funnel.R")
md <- character()
say <- function(...) { line <- paste0(...); cat(line, "\n"); md <<- c(md, line) }
tbl_md <- function(df) {
  df[] <- lapply(df, as.character)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |"),
    vapply(seq_len(nrow(df)), function(i) paste0("| ", paste(df[i, ], collapse = " | "), " |"), character(1)))
}

# ---- the order lists ------------------------------------------------------------
man <- fromJSON(file.path(odir, "orders.json"), simplifyVector = FALSE)
ent <- bind_rows(lapply(names(man), function(stem) {
  f <- file.path(odir, "data", paste0(stem, ".json"))
  if (!file.exists(f)) return(NULL)
  e <- fromJSON(f)
  if (!is.data.frame(e) || !nrow(e)) return(NULL)
  e$stem <- stem; e$list_date <- as.Date(man[[stem]]$date); e$kind <- man[[stem]]$kind
  e$text <- as.character(e$text %||% ""); e$text[is.na(e$text)] <- ""
  e
}))
say("Order entries: ", format(nrow(ent), big.mark = ","), " across ", length(unique(ent$stem)),
    " documents, ", format(min(ent$list_date)), " to ", format(max(ent$list_date)), ".")

# What the list says happened to a PETITION docket, per entry. The section is a
# container; the entry's own text governs. A docket in CERTIORARI DENIED whose
# text is "The motion of petitioner for leave to proceed in forma pauperis is
# denied. Petitioner is allowed until ..." has NOT had its petition denied.
# Sentence by sentence, because "The motion to expedite consideration of the
# petition for a writ of certiorari is granted" is about the motion, and the
# petition's own sentence, if any, comes after it.
# "No. 18-776" and "U. S." carry periods that are not sentence ends; flatten
# them first so a sentence regex can span a docket number.
.flatten <- function(t) str_replace_all(str_replace_all(t, "\\bNo\\.\\s*", "No "), "\\bU\\. S\\.", "US")
.sentences <- function(t) str_squish(unlist(str_split(t, "(?<=[.])\\s+(?=[A-Z])")))
.is_motion_sentence <- function(s) str_detect(s, regex("^(The (joint )?motions?|Petitioners?['’]s? motions?|The applications?)\\b", ignore_case = TRUE))
.any_sentence <- function(t, rx, skip_motions = TRUE) vapply(t, function(x) {
  s <- .sentences(.flatten(x)); if (skip_motions) s <- s[!.is_motion_sentence(s)]
  any(str_detect(s, regex(rx, ignore_case = TRUE)))
}, logical(1), USE.NAMES = FALSE)
# The petition's own grant: the petition phrase opens the sentence or follows
# "and" ("The motion ... and the petition for a writ of certiorari are
# granted"), never "consideration of the petition ... is granted", which is a
# motion to expedite. Noting probable jurisdiction, or postponing it to the
# hearing, is an appeal's grant, and the funnel files those as granted.
.GRANT_RX <- paste0("(^|\\band )the petitions? for (a |the )?writs? of certiorari( before judgment)?( in No [^ ,]+(, No [^ ,]+)*(,? and No [^ ,]+)?)?",
                    "( is| are)( hereby)? granted|probable jurisdiction is noted|question of jurisdiction is postponed",
                    # "... is treated as a petition for a writ of certiorari before
                    # judgment, and the petition is granted": the emergency-docket grant.
                    "|treated as (a )?petitions? for (a )?writs? of certiorari[^.]*and the petitions?( in No [^ ,]+)? (is|are) granted")
pet <- ent |> filter(str_detect(dkt, "^\\d{2}-\\d+$")) |> mutate(
  t = str_squish(text),
  # A grant whose same order vacates and remands is a GVR whatever section it
  # sits in (25-162, Tennessee v. Kennedy, under CERTIORARI GRANTED).
  is_gvr  = section == "gvr" | (str_detect(t, regex("(judgments?|orders?) (is|are) vacated", ignore_case = TRUE)) &
                                str_detect(t, regex("remanded", ignore_case = TRUE))),
  is_grant = (section == "granted" & t == "") | .any_sentence(t, .GRANT_RX, skip_motions = FALSE),
  is_dismissed = .any_sentence(t, "petitions?[^.]{0,80}\\bdismissed"),
  is_denied = (section == "denied" & t == "") |
              .any_sentence(t, "(^|\\band )the petitions? for (a |the )?writs? of certiorari( before judgment)?( in No [^ ,]+)?( is| are) denied"),
  is_motion = !is_gvr & !is_grant & !is_dismissed & !is_denied &
              str_detect(t, regex("^The (joint )?motions?\\b|reconsideration|in forma pauperis[^.]{0,30}\\b(is|are) denied|to expedite", ignore_case = TRUE)),
  said = case_when(is_gvr ~ "gvr", is_grant ~ "granted", is_dismissed ~ "dismissed",
                   is_motion ~ "motion only", is_denied ~ "denied",
                   section == "rehearing" ~ "rehearing denied",
                   section %in% c("habeas", "mandamus", "prohibition") ~ "writ denied",
                   TRUE ~ "other"))
say(""); say("### What the lists say about petition dockets"); say("")
md <- c(md, tbl_md(as.data.frame(table(said = pet$said)) |> arrange(desc(Freq)) |> rename(entries = Freq)))

# ---- the docket data ------------------------------------------------------------
files <- rev(c(list.files("data-raw", pattern = "^ot_20\\d{2}\\.rds$", full.names = TRUE),
               "data-raw/snapshot_refresh.rds", "data-raw/snapshot_25.rds", "data-raw/snapshot_26.rds"))
cases <- bind_rows(lapply(files, function(f) { x <- readRDS(f); x$src <- basename(f); x })) |>
  distinct(dkt, .keep_all = TRUE)
cls <- classify_petitions(cases) |> select(dkt, funnel = outcome, funnel_date = outcome_date)
last_entry <- tibble(dkt = cases$dkt, last = as.Date(vapply(cases$events, function(e)
  if (is.data.frame(e) && nrow(e)) as.numeric(suppressWarnings(max(mdy(e$Date), na.rm = TRUE))) else NA_real_, numeric(1)),
  origin = "1970-01-01"), src = cases$src)
say(""); say("Docket data: ", format(nrow(cases), big.mark = ","), " dockets from the local snapshots (",
    paste(rev(basename(files)), collapse = ", "), ").")

# ---- the comparison ---------------------------------------------------------------
cmp <- pet |> filter(said %in% c("granted", "gvr", "denied", "dismissed")) |>
  mutate(expect = said) |>
  left_join(cls, by = "dkt") |> left_join(last_entry, by = "dkt") |>
  mutate(status = case_when(
    is.na(src) ~ "docket not in the snapshots",
    last < list_date ~ "snapshot predates the order",
    is.na(funnel) ~ "funnel: no row",
    funnel == expect & !is.na(funnel_date) & funnel_date == list_date ~ "agree",
    funnel == expect ~ "agree on outcome; funnel dates it later",
    # A grant the docket later dismissed (Rule 46) is not a disagreement: the
    # funnel reports the last word, the list reports the day's.
    expect == "granted" & funnel == "dismissed" & funnel_date > list_date ~ "list granted; later dismissed (Rule 46)",
    expect == "denied" & funnel == "dismissed" ~ "list denied; funnel dismissed",
    TRUE ~ paste0("disagree: list ", expect, ", funnel ", funnel)))
comparable <- cmp |> filter(!status %in% c("docket not in the snapshots", "snapshot predates the order"))
say(""); say("### Comparison, one row per list entry"); say("")
md <- c(md, tbl_md(as.data.frame(table(status = cmp$status)) |> arrange(desc(Freq)) |> rename(entries = Freq)))
byterm <- comparable |> mutate(term = paste0("OT", str_sub(dkt, 1, 2)), ok = str_detect(status, "^agree")) |>
  group_by(term) |> summarise(compared = n(), agree = sum(ok), `agree %` = sprintf("%.2f", 100 * mean(ok)), .groups = "drop")
say(""); say("### By Term of the docket"); say("")
md <- c(md, tbl_md(as.data.frame(byterm)))
say(""); say("Overall: ", sum(str_detect(comparable$status, "^agree")), " of ", nrow(comparable),
    " comparable entries agree on the outcome (", sprintf("%.2f", 100 * mean(str_detect(comparable$status, "^agree"))), "%); ",
    sum(comparable$status == "agree"), " (", sprintf("%.2f", 100 * mean(comparable$status == "agree")), "%) on the date too.")

dis <- comparable |> filter(str_detect(status, "^disagree|no row"))
say(""); say("### Disagreements: ", nrow(dis)); say("")
if (nrow(dis)) {
  md <- c(md, tbl_md(as.data.frame(table(status = dis$status)) |> arrange(desc(Freq)) |> rename(entries = Freq)))
  say(""); say("Samples:"); say("")
  md <- c(md, tbl_md(dis |> arrange(status, list_date) |> group_by(status) |> slice_head(n = 5) |> ungroup() |>
    transmute(status, docket = dkt, `list date` = format(list_date), `funnel date` = format(funnel_date),
              caption = str_trunc(caption, 40), `list text` = str_trunc(t, 90)) |> as.data.frame()))
}
late <- comparable |> filter(status == "agree on outcome; funnel dates it later") |> mutate(gap = as.integer(funnel_date - list_date))
say(""); say("### Same outcome, later funnel date: ", nrow(late)); say("")
if (nrow(late)) {
  say("Gap in days (funnel minus list): ", paste(sprintf("%d×%s", as.integer(table(late$gap)), names(table(late$gap))), collapse = ", "))
  say(""); md <- c(md, tbl_md(late |> arrange(desc(abs(gap))) |> head(8) |>
    transmute(docket = dkt, said, `list date` = format(list_date), `funnel date` = format(funnel_date), gap, `list text` = str_trunc(t, 80)) |> as.data.frame()))
}
lg <- cmp |> filter(status == "list granted; later dismissed (Rule 46)")
if (nrow(lg)) { say(""); say("Grants the docket later dismissed (the funnel reports the last word): ", paste(lg$dkt, collapse = ", ")) }

# The reverse: funnel grants no list names as a grant.
list_granted <- unique(pet$dkt[pet$said == "granted"])
inwin <- cls |> filter(funnel == "granted", !is.na(funnel_date), funnel_date >= min(ent$list_date), funnel_date <= max(ent$list_date))
# For each such grant, the list entry nearest the funnel's grant date, if any.
nearest <- pet |> select(dkt, said, section, stem, kind, list_date, t)
rev <- inwin |> filter(!dkt %in% list_granted) |>
  left_join(nearest, by = "dkt", relationship = "many-to-many") |>
  mutate(gap = abs(as.integer(list_date - funnel_date))) |>
  group_by(dkt) |> arrange(gap, .by_group = TRUE) |> slice_head(n = 1) |> ungroup() |>
  mutate(said = ifelse(!is.na(gap) & gap > 14, paste0(said, " [", gap, " days off]"), said))
say(""); say("### The reverse: funnel grants that no list names as a grant"); say("")
say(nrow(rev), " of ", nrow(inwin), " funnel grants dated inside the lists' window are not in any granted section or granted text.")
if (nrow(rev)) { say(""); md <- c(md, tbl_md(rev |> transmute(docket = dkt, `funnel grant date` = format(funnel_date),
  `nearest list entry` = ifelse(is.na(said), "none", paste0(said, " (", kind, " ", stem, ")")),
  `its text` = ifelse(is.na(t), "", str_trunc(t, 90))) |> as.data.frame())) }

# Coverage gaps in the docket data, for the record.
nd <- cmp |> filter(status == "docket not in the snapshots")
say(""); say("### Docket data coverage"); say("")
say(nrow(nd), " list entries name a docket the local snapshots do not hold",
    if (nrow(nd)) paste0(" (by Term: ", paste(sprintf("%s×OT%s", as.integer(table(str_sub(nd$dkt, 1, 2))), names(table(str_sub(nd$dkt, 1, 2)))), collapse = ", "), ")") else "", ".")
sp <- cmp |> filter(status == "snapshot predates the order")
say(nrow(sp), " name a docket whose snapshot predates the order.")

saveRDS(list(cmp = cmp, rev = rev, pet = pet), sub("\\.md$", ".rds", out_md))
writeLines(md, out_md)
