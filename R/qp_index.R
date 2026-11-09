# qp_index.R -------------------------------------------------------------------
# Search over the Questions Presented, and "similar questions" on a case page.
# Both are built here, from the QP caches the site already publishes, into
# static JSON under /questions/ that /qp.js reads. docs/qp-search.md has the
# measurements behind every constant in this file.
#
#   questions/index.html        the search page (write_qp_page)
#   questions/meta.json         Terms, counts, coverage, the build's parameters
#   questions/text/<tt>.json    one Term's questions: [docket, caption, subject,
#                               granted, text] -- what the search page searches
#   questions/like/<tt>.json    docket -> its neighbours and their cosines, for
#                               the search page's "similar to No. X" view
#   questions/near/<tt>-<b>.json  docket -> its few nearest, WITH captions, in
#                               buckets of 100 docket numbers: what a case page
#                               loads, ~10 KB, to fill its "Similar questions"
#
# Three things are easy to get wrong and are decided here once:
#
#   * WHAT COUNTS AS A QUESTION. About 7% of the cached "QPs" are not: the
#     cover's counsel block, a table of contents, or the OCR of a handwritten
#     form. qp_usable() is the gate, and anything it rejects is in neither the
#     search nor a neighbour list. The case page still prints what the cache
#     holds; that is a separate, older decision.
#   * THE SAME TEXT FEEDS BOTH. qp_plain() is what is searched, what is shown in
#     a result and what the similarity is computed on.
#   * NOTHING HERE IS AN INPUT TO A CASE PAGE. The page carries only a hook
#     (<section id='qpsim'>), and the neighbours arrive at read time. A list
#     baked into the page would go stale the day a newer petition asked the same
#     question, because the daily re-renders only the pages it fetched.

suppressPackageStartupMessages({
  library(stringr)
  library(jsonlite)
})

QP_DIR <- "questions"

# The caches, in render_dockets_for()'s order, so a case page and this index
# read the same text for a docket that sits in more than one.
QP_CACHES <- c("conferences/qp_cache.json", "arguments/qp_cache.json",
               "dashboards/qp_cache.json")

# ---- parameters, each measured (docs/qp-search.md) ---------------------------
QP_MAX_CHARS   <- 4000L   # 57 of 17,046 run longer; all of those ramble
QP_MIN_WORDS   <- 8L      # "Whether McGirt v. Oklahoma ... should be overruled." is 9
QP_MIN_INVOCAB <- 0.88    # share of words the rest of the corpus also uses
QP_SIM_FLOOR   <- 0.22    # below this a neighbour shares a topic, not a question
QP_SIM_SAME    <- 0.75    # at or above: the same question in nearly the same words
QP_SIM_KEEP    <- 25L     # neighbours kept per petition (questions/like)
QP_NEAR_SHOW   <- 6L      # neighbours a case page is handed (questions/near)
QP_NEAR_SAME   <- 3L      # ... of which at most this many are near-identical
QP_BUCKET      <- 100L    # docket numbers per questions/near file

# ---- the corpus ---------------------------------------------------------------
# docket -> raw QP, merged across the caches. Petitions only: an application's
# "question" is the relief it asks for, and the caches hold none anyway.
qp_corpus <- function(site_dir) {
  out <- list()
  for (p in file.path(site_dir, QP_CACHES)) {
    if (!file.exists(p)) next
    qc <- tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) list())
    for (d in names(qc)) {
      q <- qc[[d]]$qp
      if (!is.null(q)) out[[d]] <- paste(unlist(q), collapse = "\n")
    }
  }
  x <- unlist(out)
  if (is.null(x)) return(character())
  x <- x[!is.na(x) & nzchar(x) & x != "-" & grepl("^\\d{2}-\\d+$", names(x))]
  x
}

# One line of prose from a raw QP: the heading gone, a word the PDF broke across
# two lines rejoined, a trailing table of contents cut off, whitespace squashed.
qp_plain <- function(x) {
  vapply(x, function(s) {
    s <- strip_qp_heading(s)
    if (is.na(s)) return("")
    s <- gsub("([A-Za-z])[-­]\\n\\s*([a-z])", "\\1\\2", s, perl = TRUE)
    s <- gsub("­", "", s, fixed = TRUE)
    s <- str_squish(s)
    # Dot leaders are a table of contents. One that FOLLOWS a question is the
    # next page caught by the extractor: keep what came before it.
    lead <- regexpr("(\\.[ .]{5,}\\.)|…{3,}", s, perl = TRUE)
    if (lead > 0) {
      head <- substr(s, 1L, lead - 1L)
      cut <- max(unlist(gregexpr("[.?]", head)))
      s <- if (cut > 0) substr(head, 1L, cut) else ""
    }
    # A lone page folio left at the end ("... the Second Amendment? i").
    s <- sub("\\s+\\(?[ivx]{1,4}\\)?$", "", s, perl = TRUE)
    if (nchar(s) > QP_MAX_CHARS) {
      s <- substr(s, 1L, QP_MAX_CHARS)
      s <- paste0(sub("\\s+\\S*$", "", s), " …")
    }
    s
  }, character(1))
}

.qp_words <- function(plain) str_extract_all(tolower(plain), "[a-z]+")

# The gate. `plain` is qp_plain()'s output, named by docket; returns a logical
# vector with a "why" attribute naming the first rule each rejected text failed.
#
# The vocabulary test is the one doing the work: OCR of handwriting is made of
# strings no other petition contains, so the share of a text's words that at
# least five petitions use separates it from prose almost completely (the
# median real QP scores 0.99). It needs the corpus, which is why the gate is
# vectorised rather than a predicate on one string.
qp_usable <- function(plain) {
  w <- .qp_words(plain)
  nw <- lengths(w)
  df <- table(unlist(lapply(w, unique)))
  vocab <- names(df)[df >= 5L]
  inv <- vapply(w, function(v) if (length(v)) mean(v %in% vocab) else 0, numeric(1))
  marker <- str_detect(plain, regex("\\?|\\bwhether\\b", ignore_case = TRUE))
  # The cover page's counsel block, or the cover itself.
  addr <- str_count(plain, regex(paste0(
    "\\bL\\.?L\\.?P\\b|\\bP\\.?L\\.?L\\.?C\\b|\\bP\\.C\\.|\\bSuite\\b|\\bStreet\\b|\\bAvenue\\b|\\bAve\\.",
    "|Counsel (?:for|of Record)|Attorneys? for|\\(\\d{3}\\)\\s?\\d{3}|\\S+@\\S+\\.\\w{2,}",
    "|Telephone|\\bBar No|\\b[A-Z]{2} \\d{5}\\b"), ignore_case = FALSE))
  counsel <- (addr >= 3L & !marker) | addr >= 8L
  cover <- str_detect(substr(plain, 1L, 400L), regex(
    "in the supreme court of the united states|on petition for (?:a )?writ of certiorari|certificate of (?:compliance|service)|table of (?:contents|authorities|citations)",
    ignore_case = TRUE)) |
    str_detect(substr(plain, 1L, 60L), regex(
      "parties to the proceeding|list of (?:all )?parties|corporate disclosure|^per curiam",
      ignore_case = TRUE))
  why <-ifelse(nw < QP_MIN_WORDS, "short",
         ifelse(counsel, "counsel block",
         ifelse(cover, "cover or contents",
         ifelse(inv < QP_MIN_INVOCAB, "not prose",
         ifelse(nw < 15L & !marker, "short, no question", NA_character_)))))
  structure(is.na(why), why = why, invocab = inv, words = nw)
}

# ---- similarity ---------------------------------------------------------------
QP_STOP <- c(
  "a","an","the","of","to","in","on","for","by","with","at","from","as","and","or","but","nor",
  "is","are","was","were","be","been","being","has","have","had","do","does","did","can","could",
  "may","might","must","shall","should","will","would","that","this","these","those","which","who",
  "whom","whose","what","when","where","why","how","if","it","its","their","his","her","he","she",
  "they","them","him","i","we","us","our","you","your","not","no","any","all","such","than","then",
  "there","here","so","also","only","other","under","into","upon","about","after","before","between",
  "because","while","during","through","over","against","within","without","whether","court","courts",
  "petitioner","petitioners","respondent","respondents","case","question","questions","presented",
  "v","vs","s","u","c","ct","ed","d","f","cir","id","see","e","g","et","al","inc","held","holding",
  "one","two","three","first","second","third","even","both","each","more","most","some","same")

# Tokens for the similarity. Statute sections and reporter cites are the most
# specific words a QP contains, so they are kept whole: "922(g)(1)" is the
# token "922g1", not three tokens of which two are "g" and "1".
qp_tokens <- function(plain) {
  s <- tolower(plain)
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- gsub("u\\.\\s?s\\.\\s?c\\.?", " usc ", s, perl = TRUE)
  s <- gsub("(\\d+[a-z]?)((?:\\([a-z0-9]{1,4}\\))+)", "\\1\\2 ", s, perl = TRUE)
  s <- gsub("(?<=[a-z0-9])\\((?=[a-z0-9]{1,4}\\))|(?<=\\([a-z0-9]|[a-z0-9]{2}|[a-z0-9]{3}|[a-z0-9]{4})\\)(?=\\(|\\s|[.,;:?]|$)", "", s, perl = TRUE)
  toks <- str_extract_all(s, "[a-z0-9]+")
  lapply(toks, function(t) {
    t <- t[!(t %in% QP_STOP) & nchar(t) >= 2L & !grepl("^\\d{1,2}$", t)]
    if (length(t) < 2L) return(t)
    c(t, paste(t[-length(t)], t[-1L]))
  })
}

# Row-normalised TF-IDF over unigrams and adjacent pairs. Sublinear tf, because
# a QP that says "arbitration" nine times is not nine times more about it.
qp_tfidf <- function(toks, min_df = 2L, max_df_share = 0.25) {
  if (!requireNamespace("Matrix", quietly = TRUE)) stop("qp_tfidf() needs the Matrix package")
  n <- length(toks)
  doc <- rep.int(seq_len(n), lengths(toks))
  f <- factor(unlist(toks, use.names = FALSE))
  V <- nlevels(f)
  # (document, term) pairs as one number each, sorted and run-length counted:
  # table() on four million pasted strings took half a minute to do the same.
  r <- rle(sort((doc - 1) * V + (as.integer(f) - 1L)))
  d <- r$values %/% V + 1L; j <- r$values %% V + 1L; cnt <- r$lengths
  df <- tabulate(j, V)
  keep <- which(df >= min_df & df <= max_df_share * n)
  ok <- j %in% keep
  d <- d[ok]; cnt <- cnt[ok]; j <- match(j[ok], keep)
  idf <- log(n / df[keep])
  # A pair is evidence of the same phrase; it should not outvote its two words.
  wt <- ifelse(grepl(" ", levels(f)[keep], fixed = TRUE), 0.7, 1)
  x <- (1 + log(cnt)) * idf[j] * wt[j]
  M <- Matrix::sparseMatrix(i = d, j = j, x = x, dims = c(n, length(keep)))
  nrm <- sqrt(Matrix::rowSums(M^2)); nrm[nrm == 0] <- 1
  Matrix::Diagonal(x = 1 / nrm) %*% M
}

# For each row: `hits`, its `keep` nearest by cosine at or above `floor` as a
# data frame (j, s), best first; `n`, how many cleared the floor in all; and
# `n_same`, how many of those are the same question in nearly the same words.
# Blocked so the dense slab stays ~50 MB.
qp_neighbours <- function(M, floor = QP_SIM_FLOOR, keep = QP_SIM_KEEP,
                          same = QP_SIM_SAME, block = 400L) {
  n <- nrow(M); out <- vector("list", n)
  Mt <- Matrix::t(M)
  for (a in seq(1L, n, by = block)) {
    idx <- a:min(n, a + block - 1L)
    S <- as.matrix(M[idx, , drop = FALSE] %*% Mt)
    for (r in seq_along(idx)) {
      s <- S[r, ]; s[idx[r]] <- 0
      hit <- which(s >= floor)
      n_hit <- length(hit); n_same <- sum(s[hit] >= same)
      hit <- hit[order(-s[hit], hit)]
      if (n_hit > keep) hit <- hit[seq_len(keep)]
      out[[idx[r]]] <- list(hits = data.frame(j = hit, s = s[hit]), n = n_hit, n_same = n_same)
    }
  }
  out
}

# ---- the build ----------------------------------------------------------------
.qp_term <- function(dkt) as.integer(substr(dkt, 1L, 2L))
.qp_seq  <- function(dkt) as.integer(sub("^\\d{2}-", "", dkt))
.qp_ot   <- function(t) paste0("OT", ifelse(t >= 90L, 1900L + t, 2000L + t))
.qp_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  txt <- toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  con <- file(path, open = "wb"); on.exit(close(con))
  writeLines(enc2utf8(as.character(txt)), con, useBytes = TRUE)
}

# Everything under questions/ but the page. Returns the meta list, invisibly.
#
# Skipped when nothing it reads has changed since the last build (the hash in
# meta.json): the two workflows that call this both run several times a day,
# and a rebuild from identical inputs is a minute spent rewriting 400 files to
# the bytes they already hold.
#
# `check_pages = FALSE` is for a scratch site that holds the caches but not the
# 55,000 case pages; on a real site a docket without a page is left out, since
# every result links to one.
build_qp_index <- function(site_dir, check_pages = TRUE, force = FALSE) {
  out_dir <- file.path(site_dir, QP_DIR)
  raw <- qp_corpus(site_dir)
  caps <- tryCatch(unlist(fromJSON(file.path(site_dir, "cases", "search.json"))),
                   error = function(e) character())
  raw <- raw[names(raw) %in% names(caps)]
  if (isTRUE(check_pages))
    raw <- raw[file.exists(file.path(site_dir, "cases", paste0(names(raw), ".html")))]
  if (length(raw) < 20L) {
    message("build_qp_index(): only ", length(raw), " question(s) with a case page -- not building.")
    return(invisible(NULL))
  }
  subj <- tryCatch(fromJSON(file.path(site_dir, "cases", "subjects.json"), simplifyVector = FALSE),
                   error = function(e) list())
  # Review granted, as far as the site can say from what it already publishes:
  # the grants ledger (complete from OT2025) and the argued cases (every Term).
  # A grant that was neither argued nor recent is missing, so the flag is
  # offered as "granted", never read as "the rest were denied".
  granted <- unique(c(
    tryCatch(names(fromJSON(file.path(site_dir, "cases", "grants.json"), simplifyVector = FALSE)),
             error = function(e) character()),
    tryCatch(names(fromJSON(file.path(site_dir, "arguments", "qp_cache.json"), simplifyVector = FALSE)),
             error = function(e) character())))

  plain <- qp_plain(raw)
  ok <- qp_usable(plain)
  n_rejected <- sum(!ok)
  plain <- plain[ok]
  dk <- names(plain)
  o <- order(-.qp_term(dk), .qp_seq(dk)); plain <- plain[o]; dk <- dk[o]
  area <- vapply(dk, function(d) { a <- subj[[d]]$area; if (is.null(a)) NA_character_ else a },
                 character(1))
  cap <- unname(caps[dk]); gr <- dk %in% granted

  # Coverage, per Term and bucket: questions held against petitions docketed.
  all_d <- grep("^\\d{2}-\\d+$", names(caps), value = TRUE)
  cover <- lapply(sort(unique(.qp_term(dk)), decreasing = TRUE), function(t) {
    paid <- function(d) .qp_seq(d) < 5001L
    a <- all_d[.qp_term(all_d) == t]; h <- dk[.qp_term(dk) == t]
    list(t = t, ot = .qp_ot(t), n = length(h),
         paid = c(sum(paid(h)), sum(paid(a))), ifp = c(sum(!paid(h)), sum(!paid(a))))
  })
  params <- list(QP_MAX_CHARS, QP_MIN_WORDS, QP_MIN_INVOCAB, QP_SIM_FLOOR, QP_SIM_SAME,
                 QP_SIM_KEEP, QP_NEAR_SHOW, QP_NEAR_SAME, QP_BUCKET, QP_STOP)
  hash <- digest::digest(list(params, dk, unname(plain), cap, unname(area), gr, cover))
  meta_path <- file.path(out_dir, "meta.json")
  if (!isTRUE(force) && file.exists(meta_path)) {
    old <- tryCatch(fromJSON(meta_path, simplifyVector = FALSE), error = function(e) NULL)
    if (identical(old$hash, hash)) {
      message("questions index: inputs unchanged (", length(dk), " questions) -- kept as is.")
      return(invisible(old))
    }
  }

  M <- qp_tfidf(qp_tokens(plain))
  nb <- qp_neighbours(M)
  terms <- sort(unique(.qp_term(dk)), decreasing = TRUE)
  written <- character()
  put <- function(x, ...) { p <- file.path(out_dir, ...); .qp_json(x, p); written <<- c(written, p) }

  for (t in terms) {
    ix <- which(.qp_term(dk) == t)
    tt <- sprintf("%02d", t)
    put(lapply(ix, function(i) list(dk[i], cap[i], if (is.na(area[i])) "" else area[i],
                                    as.integer(gr[i]), unname(plain[i]))),
        "text", paste0(tt, ".json"))
    has <- ix[vapply(nb[ix], function(x) nrow(x$hits) > 0L, logical(1))]
    like <- lapply(has, function(i) {
      h <- nb[[i]]$hits
      lapply(seq_len(nrow(h)), function(r) list(dk[h$j[r]], as.integer(round(100 * h$s[r]))))
    })
    names(like) <- dk[has]
    put(like, "like", paste0(tt, ".json"))
    bucket <- .qp_seq(dk[has]) %/% QP_BUCKET
    for (b in unique(bucket)) {
      sel <- has[bucket == b]
      near <- lapply(sel, function(i) {
        h <- nb[[i]]$hits
        same <- h$s >= QP_SIM_SAME
        # At most QP_NEAR_SAME of the near-identical, so a petition with forty
        # copies still shows the reader a neighbour that is not one of them.
        take <- which(!same | cumsum(same) <= QP_NEAR_SAME)
        take <- utils::head(take, QP_NEAR_SHOW)
        # n: every neighbour over the floor; k: how many of them like/ keeps.
        list(n = nb[[i]]$n, k = nrow(h), same = nb[[i]]$n_same,
             top = lapply(take, function(r) list(dk[h$j[r]], cap[h$j[r]],
                                                 as.integer(round(100 * h$s[r])),
                                                 # 1 = review granted, 2 = nearly the same words
                                                 as.integer(gr[h$j[r]]) + 2L * as.integer(same[r]))))
      })
      names(near) <- dk[sel]
      put(near, "near", paste0(tt, "-", b, ".json"))
    }
  }

  meta <- list(hash = hash, built = format(Sys.Date()), n = length(dk),
               rejected = n_rejected, with_similar = sum(vapply(nb, function(x) nrow(x$hits) > 0L, logical(1))),
               floor = as.integer(round(100 * QP_SIM_FLOOR)), same = as.integer(round(100 * QP_SIM_SAME)),
               bucket = QP_BUCKET, terms = cover,
               subjects = sort(unique(area[!is.na(area) & nzchar(area)])))
  put(meta, "meta.json")

  # Anything left from an earlier build that this one did not write is a bucket
  # whose petitions no longer have neighbours, or a Term that lost its last
  # question; either way it would be served stale forever.
  have <- list.files(out_dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  stale <- setdiff(normalizePath(have, winslash = "/"), normalizePath(written, winslash = "/"))
  if (length(stale)) unlink(stale)
  message("questions index: ", length(dk), " questions across ", length(terms), " Term(s); ",
          meta$with_similar, " with a similar question; ", n_rejected, " cached text(s) rejected; ",
          length(written), " file(s)", if (length(stale)) paste0(", ", length(stale), " stale removed") else "")
  invisible(meta)
}

# ---- the page -----------------------------------------------------------------
QP_PAGE_CSS <- paste0("
  .qps{margin:0 0 .6rem}
  #qpq{width:100%;font-family:'Newsreader',Georgia,serif;font-size:1.1rem;color:var(--ink);
    background:var(--panel);border:1px solid var(--rule);border-radius:3px;padding:.75rem .9rem}
  #qpq:focus{outline:none;border-color:var(--accent);box-shadow:0 0 0 3px rgba(@accent:rgb@,.1)}
  #qpq::placeholder{color:var(--faint)}
  .qpf{display:flex;flex-wrap:wrap;gap:.5rem 1.1rem;align-items:center;margin:.6rem 0 0;
    font-size:.92rem;color:var(--ink-soft)}
  .qpf select{font:inherit;color:var(--ink);background:var(--panel);border:1px solid var(--rule);
    border-radius:3px;padding:.25rem .4rem;max-width:100%}
  .qpf label{display:inline-flex;align-items:center;gap:.35rem}
  .qphint{margin:.5rem 0 0;font-size:.88rem;color:var(--faint)}
  .qphint code{font-family:inherit;color:var(--ink-soft);background:rgba(@accent:rgb@,.06);
    padding:0 .25em;border-radius:2px}
  #qpstat{margin:1.2rem 0 .2rem;font-size:.92rem;color:var(--faint);min-height:1.3em}
  #qpstat a,.qphint a,.qplike a,.qpcov a{color:var(--link);text-decoration:none;
    border-bottom:1px solid rgba(@link:rgb@,.4)}
  .qplike{margin:1.2rem 0 0;background:var(--panel);border:1px solid var(--rule);
    border-left:4px solid var(--accent);padding:.8rem 1.1rem;font-size:.98rem}
  .qplike p{margin:.3rem 0}
  .qplike .qx{color:var(--ink-soft)}
  #qpres{list-style:none;margin:0;padding:0}
  #qpres li{padding:.95rem 0;border-bottom:1px solid var(--rule)}
  #qpres .qt{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:1.08rem;
    color:var(--ink);text-decoration:none;line-height:1.25}
  #qpres .qt:hover{color:var(--accent)}
  .qm{font-size:.84rem;color:var(--faint);font-variant-numeric:tabular-nums;margin:.15rem 0 0}
  .qm .qg{color:var(--accent);font-weight:600}
  .qx{margin:.4rem 0 0;font-size:.98rem;line-height:1.5;color:var(--ink-soft)}
  .qx mark{background:rgba(@accent:rgb@,.14);color:var(--ink);padding:0 .05em;border-radius:2px}
  #qpmore{margin:1.2rem 0 0;font:inherit;font-size:.95rem;color:var(--link);background:none;
    border:1px solid var(--rule);border-radius:3px;padding:.45rem .9rem;cursor:pointer}
  #qpmore:hover{border-color:var(--accent)}
  .qpcov{margin:2.4rem 0 0;font-size:.92rem;color:var(--ink-soft)}
  .qpcov summary{cursor:pointer;color:var(--link)}
  .qpcov table{border-collapse:collapse;margin:.8rem 0;font-variant-numeric:tabular-nums}
  .qpcov th,.qpcov td{text-align:right;padding:.2rem .9rem .2rem 0;border-bottom:1px solid var(--rule)}
  .qpcov th:first-child,.qpcov td:first-child{text-align:left}
  .qpcov th{font-weight:600;color:var(--faint);font-size:.8rem;letter-spacing:.08em;text-transform:uppercase}
  .qpcov p{max-width:38rem;line-height:1.55}
") |> fill_palette()

# questions/index.html. The shell and the coverage table are written here; the
# searching is /qp.js. `meta` is build_qp_index()'s return value, or what is on
# disk.
write_qp_page <- function(site_dir, meta = NULL) {
  out_dir <- file.path(site_dir, QP_DIR)
  if (is.null(meta))
    meta <- tryCatch(fromJSON(file.path(out_dir, "meta.json"), simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(meta) || !length(meta$terms)) {
    message("write_qp_page(): no questions index -- page not written.")
    return(invisible(NULL))
  }
  num <- function(x) format(x, big.mark = ",")
  pct <- function(v) if (v[[2]] > 0) paste0(round(100 * v[[1]] / v[[2]]), "%") else "&mdash;"
  ots <- vapply(meta$terms, function(t) t$ot, character(1))
  span <- paste0(ots[length(ots)], " to ", ots[1])
  rows <- paste(vapply(meta$terms, function(t) paste0(
    "<tr><td>", t$ot, "</td><td>", num(t$paid[[1]]), " of ", num(t$paid[[2]]), "</td><td>", pct(t$paid),
    "</td><td>", num(t$ifp[[1]]), " of ", num(t$ifp[[2]]), "</td><td>", pct(t$ifp), "</td></tr>"),
    character(1)), collapse = "")
  opt <- function(v, lab) paste0("<option value='", htmltools::htmlEscape(v, attribute = TRUE), "'>", htmltools::htmlEscape(lab), "</option>")
  dek <- paste0("Search the question each petition asks the Court to decide: ", num(meta$n),
                " petitions for certiorari, granted and denied, ", span, ".")
  body <- paste0(
    "<p class='kicker'>Cases</p><h1>Questions presented</h1>",
    "<hr class='brule'><p class='dek'>", dek, "</p>",
    "<div class='qps'><input type='search' id='qpq' autocomplete='off' spellcheck='false' ",
    "placeholder='qualified immunity, &quot;equal protection&quot;, 922(g)(1) &hellip;' ",
    "aria-label='Search the questions presented'>",
    "<div class='qpf'>",
    "<label>Term <select id='qpterm'>", opt("", "All Terms"),
    paste(vapply(meta$terms, function(t) opt(sprintf("%02d", t$t), t$ot), character(1)), collapse = ""),
    "</select></label>",
    "<label>Subject <select id='qpsubj'>", opt("", "Any subject"),
    paste(vapply(unlist(meta$subjects), function(s) opt(s, s), character(1)), collapse = ""),
    "</select></label>",
    "<label><input type='checkbox' id='qpgr'> Review granted</label>",
    "<label>Order <select id='qpsort'>", opt("", "Best match"), opt("new", "Newest first"), "</select></label>",
    "</div>",
    "<p class='qphint'>Every word must appear; a word also matches its longer forms ",
    "(<code>arbitra</code> finds arbitration). Put a phrase in <code>&quot;quotation marks&quot;</code>. ",
    "For a case by name or number, use the <a href='/cases/'>case search</a>.</p></div>",
    "<div id='qplike' class='qplike' hidden></div>",
    "<p id='qpstat' role='status' aria-live='polite'></p>",
    "<ol id='qpres'></ol>",
    "<button type='button' id='qpmore' hidden>Show more</button>",
    "<noscript><p class='qphint'>The search runs in your browser and needs JavaScript.</p></noscript>",
    "<details class='qpcov'><summary>What is and is not here</summary>",
    "<p>The questions are read from the petition each docket links to, and the site has not read every ",
    "petition. Coverage is close to complete for paid petitions and thin for older petitions filed ",
    "<i>in forma pauperis</i>; the table gives the count for each Term.</p>",
    "<table><thead><tr><th>Term</th><th>Paid</th><th></th><th><i>In forma pauperis</i></th><th></th></tr></thead>",
    "<tbody>", rows, "</tbody></table>",
    "<p>A further ", num(meta$rejected), " petitions are left out because what was read from them is not a ",
    "question: the cover&rsquo;s list of counsel, a table of contents, or a handwritten form no machine reads. ",
    "&ldquo;Review granted&rdquo; marks a petition the Court granted and heard, or granted since OT2025; ",
    "a petition without the mark was not necessarily denied.</p>",
    "<p>&ldquo;Similar questions&rdquo; on a case page, and behind each result here, compares the words of ",
    "one question with every other. It finds petitions that ask about the same statute, doctrine or decision ",
    "in like terms. It is a comparison of wording, not a finding that two cases are related.</p></details>",
    "<script>window.SCR_QP={base:'/", QP_DIR, "/',cases:'/cases/'};</script>",
    "<script src='/qp.js' defer></script>")
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n",
    page_head("Questions presented — Supreme Court Report",
              site_breadcrumb_jsonld("Questions presented", CASES_CRUMB),
              extra_css = QP_PAGE_CSS, description = dek, path = paste0("/", QP_DIR, "/")),
    "<body>", site_masthead(),
    "<main class='wrap' id='main'>", site_breadcrumb("Questions presented", CASES_CRUMB),
    body, "</main></body>\n</html>\n")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(html), file.path(out_dir, "index.html"), useBytes = TRUE)
  invisible(file.path(out_dir, "index.html"))
}

# What the build scripts call: index, page, and the script the page and every
# case page load. Never fatal -- a failure here must not cost a run its
# dashboards -- but loud, and audit_site.R checks the result.
write_qp_index <- function(site_dir, ...) {
  tryCatch({
    meta <- build_qp_index(site_dir, ...)
    if (file.exists("qp.js")) file.copy("qp.js", file.path(site_dir, "qp.js"), overwrite = TRUE)
    write_qp_page(site_dir, meta)
  }, error = function(e) {
    message("write_qp_index() failed: ", conditionMessage(e))
    invisible(NULL)
  })
}
