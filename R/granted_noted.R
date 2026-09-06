# granted_noted.R ----------------------------------------------------------------
# The Court's Granted & Noted List: one text PDF per Term, one block per case
# set for argument, in the Court's own words. Spec: docs/granted-noted.md.
#
# WHAT THE COURT PUBLISHES. /orders/NNgrantednotedlist.pdf, OT16 to the current
# Term, 10-12 pages, refreshed through the Term ("As of August 4, 2026"):
#
#   24-351    CFX   POSTAL SERVICE V. KONAN
#                   Court: USCA-5                       Granted: 4/21/25
#                   Argument Date: 10/8/25              Decided: 2/24/26
#                   Author: J. Thomas                   Other: Sotomayor (D)
#                   Result: VACATED AND REMANDED
#
# The docket line carries a three-letter case code (C/A/Q for cert, appeal,
# certified question; S/F/T/M/O for the court below; X/Y/H for civil, criminal,
# habeas), the caption, and flags: "*" a unanimous Court, "**" unanimous in
# part, "#" unanimous in judgment, ")" a docket consolidated with the next, and
# ")1" a footnote number keying the docket to its own court on a shared
# "Court: 1USCA-7; 2USCA-2" line. Field lines carry one or two "Label: value"
# pairs; a value can wrap onto the next line ("Granted: 6/30/25 (Amended /
# order - 7/3/25)"; a long "Other:" list), and an order can be logged as
# "Order: 6/27/25 – Cases restored to the calendar for reargument". A
# consolidated run of docket lines shares the block that follows it.
#
# WHAT IT ADDS. Three fields the docket JSON does not carry in any structured
# form: the separate writings and their kind ("Other: Sotomayor (D); Kagan
# (C/J)"), the result as the Court states it, and the unanimity flags. And a
# second, independent statement of the decided date and the author, which is
# what docs/granted-noted-audit-2026-09.md checks the argument grammar against.
#
# WHO WRITES WHAT. render_arguments.R (the weekly) fetches the current and
# prior Terms' lists -- two requests -- plus any Term the manifest lacks, and
# rewrites arguments/granted_noted.json Term by Term. The daily reads it.

suppressPackageStartupMessages({ library(stringr); library(jsonlite) })

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

GN_URL  <- "https://www.supremecourt.gov/orders/%sgrantednotedlist.pdf"
GN_FILE <- "granted_noted.json"   # under arguments/
GN_FIRST_TERM <- 16L               # the earliest list the Court serves

GN_LABELS <- c("Court", "Courts", "Granted", "Noted", "Juris Postponed", "Argument Date",
               "Rescheduled Argument Date", "Reargument Date", "Decided", "Author", "Other",
               "Result", "Order", "Order Date", "Date", "Action", "Made Available", "Note", "NOTE")

.gn_date <- function(x) {
  m <- str_extract(x %||% NA_character_, "\\d{1,2}/\\d{1,2}/\\d{2,4}")
  suppressWarnings(lubridate::mdy(m))
}

# "J. Thomas" -> "Thomas"; "Chief Justice" -> "Roberts, C.J." (every list the
# Court serves is from his Court); "Per Curiam" stays. The docket rows use the
# same forms, so the two sources can be compared and cannot name one author
# two ways.
.gn_author <- function(a) {
  a <- str_squish(a %||% "")
  if (!nzchar(a)) return(NA_character_)
  if (str_detect(a, regex("^chief justice", ignore_case = TRUE))) return("Roberts, C.J.")
  if (str_detect(a, regex("per curiam", ignore_case = TRUE))) return("Per Curiam")
  m <- str_match(a, "^J\\.\\s*([A-Z][A-Za-z'’-]+)")[1, 2]
  if (!is.na(m)) return(m)
  NA_character_
}

# The kind codes the list uses, token by token: "C/P, C/J/P, D/P" is three
# tokens joined by commas or "&", each a stance ("C", "D") with a qualifier
# ("/J" in the judgment, "/P" in part, "/J/P" in the judgment in part).
GN_KIND_TOKENS <- c(
  "D" = "dissenting", "C" = "concurring",
  "C/J" = "concurring in the judgment", "C/P" = "concurring in part",
  "D/P" = "dissenting in part", "D/J" = "dissenting from the judgment",
  "C/J/P" = "concurring in the judgment in part", "D/J/P" = "dissenting from the judgment in part")
.gn_kind_phrase <- function(code) {
  toks <- str_squish(unlist(str_split(code, "\\s*[,&]\\s*")))
  toks <- toks[nzchar(toks)]
  words <- unname(GN_KIND_TOKENS[toks])
  words[is.na(words)] <- tolower(toks[is.na(words)])
  if (length(words) <= 1L) return(paste(words, collapse = ""))
  paste(paste(head(words, -1), collapse = ", "), "and", tail(words, 1))
}

#' "Sotomayor (D); Kagan (C/J)" -> data.frame(justice, code, kind).
gn_others <- function(o) {
  none <- data.frame(justice = character(), code = character(), kind = character(), stringsAsFactors = FALSE)
  o <- str_squish(o %||% "")
  if (!nzchar(o)) return(none)
  # A date-prefixed writing on an order ("6/27/25 – Thomas (D)") is a writing.
  m <- str_match_all(o, "([A-Z][A-Za-z'’-]+)\\s*\\(([A-Z/&, ]+)\\)")[[1]]
  if (!nrow(m)) return(none)
  code <- str_squish(m[, 3])
  data.frame(justice = m[, 2], code = code, kind = vapply(code, .gn_kind_phrase, character(1), USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

#' A one-line phrase: "Thomas and Alito dissenting; Kagan concurring in the judgment".
gn_writings_phrase <- function(o) {
  d <- gn_others(o)
  if (!nrow(d)) return(NA_character_)
  parts <- vapply(unique(d$kind), function(k) {
    js <- d$justice[d$kind == k]
    who <- if (length(js) == 1) js else paste(paste(head(js, -1), collapse = ", "), "and", tail(js, 1))
    paste(who, k)
  }, character(1))
  paste(parts, collapse = "; ")
}

# "VACATED AND REMANDED (NMG – no part)" -> list(result = "Vacated and remanded", no_part = "NMG").
.gn_result <- function(r) {
  r <- str_squish(r %||% "")
  if (!nzchar(r)) return(list(result = NA_character_, no_part = NA_character_))
  np <- str_match(r, "\\(([A-Z]{2,4})\\s*[–-]\\s*no part\\)")[1, 2]
  r <- str_squish(str_remove(r, "\\([A-Z]{2,4}\\s*[–-]\\s*no part\\)"))
  # The Court writes the disposition in capitals; the trailing clause ("with
  # instructions to ...", "—see opinion ...") in sentence case already.
  head_up <- str_extract(r, "^[A-Z][A-Z ,'’-]+")
  if (!is.na(head_up)) r <- paste0(str_to_sentence(head_up), substr(r, nchar(head_up) + 1L, nchar(r)))
  list(result = r, no_part = np)
}

.gn_df <- function() data.frame(
  term = integer(), dkt = character(), code = character(), caption = character(),
  flag = character(), group = integer(), court = character(),
  granted = as.Date(character()), argued = as.Date(character()), argued_first = as.Date(character()),
  decided = as.Date(character()), decided_note = character(), author = character(),
  others = character(), writings = character(), result = character(), no_part = character(),
  orders = character(), as_of = as.Date(character()), stringsAsFactors = FALSE)

# The flags can sit tight against the docket ("24-440#") or a space away
# ("21-432 * CFX"); the code is two to five capitals; the caption follows.
.GN_DOCKET_RX <- "^\\s*(\\d{2}-\\d{1,5}|\\d{2}A\\d{1,4}|22O\\d{1,4}|\\d{2}M\\d{1,4})\\s?([*#]*)\\s?(\\)\\d?)?\\s?([*#]*)\\s+([A-Z]{2,5})\\s+(\\S.*)$"
# A section heading inside or after the list ("DISPOSED OF WITH ARGUMENT:",
# "CASES (ARGUMENTS) FOR 2025 TERM"): all capitals, no docket, and a field
# value must not run on into it.
.GN_HEADING_RX <- "^\\s*[A-Z][A-Z0-9 ()'’&/,-]{6,}:?\\s*$"

#' One Term's list -> one row per docket. `pages` is pdftools::pdf_text() output.
parse_granted_noted <- function(pages, term) {
  lines <- unlist(strsplit(paste(pages, collapse = "\n"), "\n", fixed = TRUE))
  lines <- str_replace_all(lines, " ", " ")
  as_txt <- str_match(paste(head(lines, 20), collapse = " "), regex("As of ([A-Za-z]+ \\d{1,2}, \\d{4})", ignore_case = TRUE))[1, 2]
  as_of <- if (is.na(as_txt)) as.Date(NA) else suppressWarnings(as.Date(as_txt, "%B %d, %Y"))
  blocks <- list(); cur <- NULL; open <- list(); last_label <- NULL; group <- 0L
  label_rx <- paste0("\\b(", paste(GN_LABELS, collapse = "|"), "):")
  flush <- function() {
    if (length(open)) blocks[[length(blocks) + 1L]] <<- list(dockets = open, fields = cur %||% list())
    open <<- list(); cur <<- NULL; last_label <<- NULL
  }
  for (ln in lines) {
    if (str_detect(ln, "CASE CODE KEY")) break
    if (!nzchar(str_trim(ln))) next
    if (str_detect(ln, "^<<<PAGE|^\\s*-\\s*\\d+\\s*-\\s*$|SUPREME COURT OF THE UNITED STATES|GRANTED & NOTED LIST|OCTOBER TERM \\d{4}|^\\s*As of ")) next
    dk <- str_match(ln, .GN_DOCKET_RX)
    if (!is.na(dk[1, 2])) {
      # A docket line after a block's fields starts a new block; one directly
      # after another docket line joins its run (a consolidated case).
      if (!is.null(cur)) flush()
      if (!length(open)) group <- group + 1L
      flags <- paste0(dk[1, 3], dk[1, 5])
      open[[length(open) + 1L]] <- list(dkt = dk[1, 2], flags = flags, foot = str_remove(dk[1, 4] %||% "", "\\)"),
                                        code = dk[1, 6], caption = str_squish(dk[1, 7]), group = group)
      last_label <- "caption"
      next
    }
    if (!length(open)) next
    if (str_detect(ln, label_rx)) {
      # One or two "Label: value" pairs; the second starts after a run of spaces.
      pairs <- str_match_all(ln, paste0("(", paste(GN_LABELS, collapse = "|"), "):\\s*(.*?)(?=\\s{2,}(?:", paste(GN_LABELS, collapse = "|"), "):|$)"))[[1]]
      if (is.null(cur)) cur <- list()
      for (i in seq_len(nrow(pairs))) {
        lab <- pairs[i, 2]; val <- str_squish(pairs[i, 3])
        if (lab %in% c("Order", "Date", "Order Date", "Action", "Note", "NOTE")) {
          cur$orders <- c(cur$orders, paste0(lab, ": ", val))
        } else {
          cur[[lab]] <- if (is.null(cur[[lab]])) val else paste(cur[[lab]], val)
        }
        last_label <- lab
      }
      next
    }
    # A continuation: an all-caps line under the docket line is more caption;
    # an all-caps line under a field is a section heading, and closes the
    # block; anything else extends the last field's value.
    t <- str_squish(ln)
    if (identical(last_label, "caption") && !str_detect(t, "[a-z]")) {
      open[[length(open)]]$caption <- str_squish(paste(open[[length(open)]]$caption, t))
    } else if (!is.null(cur) && str_detect(ln, .GN_HEADING_RX)) {
      flush()
    } else if (!is.null(last_label) && !is.null(cur) && !identical(last_label, "caption")) {
      if (last_label %in% c("Order", "Date", "Order Date", "Action", "Note", "NOTE")) {
        cur$orders[length(cur$orders)] <- paste(cur$orders[length(cur$orders)], t)
      } else cur[[last_label]] <- paste(cur[[last_label]], t)
    }
  }
  flush()
  if (!length(blocks)) return(.gn_df())
  rows <- lapply(blocks, function(b) {
    f <- b$fields
    res <- .gn_result(f$Result)
    arg_dates <- c(.gn_date(f[["Argument Date"]]), .gn_date(f[["Rescheduled Argument Date"]]), .gn_date(f[["Reargument Date"]]))
    arg_dates <- arg_dates[!is.na(arg_dates)]
    dec <- .gn_date(f$Decided)
    dec_note <- str_squish(str_extract(f$Decided %||% "", "\\(.*\\)"))
    do.call(rbind, lapply(b$dockets, function(d) {
      flag <- if (str_detect(d$flags, "\\*\\*")) "unanimous in part" else if (str_detect(d$flags, "\\*")) "unanimous"
              else if (str_detect(d$flags, "#")) "unanimous in judgment" else NA_character_
      data.frame(
        term = as.integer(term), dkt = d$dkt, code = d$code, caption = d$caption, flag = flag,
        group = d$group, court = str_squish(f$Court %||% f$Courts %||% NA_character_),
        granted = .gn_date(f$Granted %||% f$Noted %||% f[["Juris Postponed"]]),
        argued = if (length(arg_dates)) max(arg_dates) else as.Date(NA),
        argued_first = if (length(arg_dates)) min(arg_dates) else as.Date(NA),
        decided = dec, decided_note = if (is.na(dec_note) || !nzchar(dec_note)) NA_character_ else dec_note,
        author = .gn_author(f$Author), others = str_squish(f$Other %||% NA_character_),
        writings = gn_writings_phrase(f$Other), result = res$result, no_part = res$no_part,
        orders = if (length(f$orders)) paste(f$orders, collapse = " | ") else NA_character_,
        as_of = as_of, stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  out[!duplicated(out$dkt), , drop = FALSE]
}

# ---- fetch and manifest --------------------------------------------------------------

.gn_fetch_pdf <- function(url) {
  resp <- if (exists("scotus_perform") && exists("scotus_req"))
    scotus_perform(scotus_req(url))
  else httr2::req_perform(httr2::req_user_agent(httr2::request(url), "ceRt SCOTUS docketing dashboard (httr2)"))
  if (httr2::resp_status(resp) != 200L) stop("HTTP ", httr2::resp_status(resp))
  tmp <- tempfile(fileext = ".pdf"); writeBin(httr2::resp_body_raw(resp), tmp)
  pages <- pdftools::pdf_text(tmp); unlink(tmp); pages
}

#' The lists for `terms` (two-digit), parsed. A Term that cannot be fetched
#' contributes nothing and says so.
fetch_granted_noted <- function(terms) {
  parts <- lapply(unique(terms), function(t) {
    url <- sprintf(GN_URL, t)
    tryCatch(parse_granted_noted(.gn_fetch_pdf(url), as.integer(t)), error = function(e) {
      cat("Granted & Noted List", url, "unavailable:", conditionMessage(e), "\n"); NULL })
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) .gn_df() else do.call(rbind, parts)
}

.gn_path <- function(site_dir) file.path(site_dir, "arguments", GN_FILE)

read_granted_noted <- function(site_dir) {
  p <- .gn_path(site_dir)
  if (!file.exists(p)) return(.gn_df())
  j <- tryCatch(fromJSON(p, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j) || !is.data.frame(j) || !nrow(j)) return(.gn_df())
  chr <- function(nm) if (nm %in% names(j)) { x <- as.character(j[[nm]]); x[!nzchar(x) | x == "NA"] <- NA_character_; x } else rep(NA_character_, nrow(j))
  dt <- function(nm) as.Date(chr(nm))
  data.frame(term = as.integer(j$term), dkt = as.character(j$dkt), code = chr("code"), caption = chr("caption"),
             flag = chr("flag"), group = as.integer(j$group %||% NA), court = chr("court"),
             granted = dt("granted"), argued = dt("argued"), argued_first = dt("argued_first"),
             decided = dt("decided"), decided_note = chr("decided_note"), author = chr("author"),
             others = chr("others"), writings = chr("writings"), result = chr("result"), no_part = chr("no_part"),
             orders = chr("orders"), as_of = dt("as_of"), stringsAsFactors = FALSE)
}

#' Replace the manifest's rows for each Term in `df` (a Term's list is complete
#' every time it is read) and keep the rest. Event dates only, never a build time.
write_granted_noted <- function(site_dir, df) {
  old <- read_granted_noted(site_dir)
  if (nrow(df)) old <- old[!old$term %in% unique(df$term), , drop = FALSE]
  all <- rbind(old, df)
  all <- all[order(-all$term, all$dkt), , drop = FALSE]
  for (col in c("granted", "argued", "argued_first", "decided", "as_of"))
    all[[col]] <- ifelse(is.na(all[[col]]), NA_character_, format(as.Date(all[[col]]), "%Y-%m-%d"))
  p <- .gn_path(site_dir)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  write_json(all, p, auto_unbox = TRUE, dataframe = "rows", na = "null")
  invisible(nrow(all))
}

#' The Terms a weekly run should fetch: the current and prior, plus any Term
#' from GN_FIRST_TERM on that the manifest does not hold yet.
gn_terms_to_fetch <- function(site_dir, as_of = Sys.Date()) {
  y <- as.integer(format(as_of, "%Y")) - as.integer(as.integer(format(as_of, "%m")) < 10L)
  cur <- y %% 100L
  have <- unique(read_granted_noted(site_dir)$term)
  want <- unique(c(cur, cur - 1L, setdiff(GN_FIRST_TERM:cur, have)))
  sprintf("%02d", want)
}
