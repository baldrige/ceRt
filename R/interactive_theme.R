# interactive_theme.R ----------------------------------------------------------
# Shared presentation layer for the INTERACTIVE dashboards: applies the site's
# editorial parchment/oxblood law-review look to a gt table, makes it sortable /
# filterable / searchable / paginated via gt::opt_interactive(), and wraps it in
# the site page chrome. Used by scotus_dash() (daily) and conference_dash().
#
# Numeric columns (Grant forecast, Relists) are kept numeric so they sort by
# value; opt_interactive() renders via reactable, so the controls are themed with
# injected CSS targeting reactable's classes.

suppressPackageStartupMessages({ library(gt); library(tidyverse); library(jsonlite) })

SCR_FONTS <- "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,600&family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&display=swap"
.scr_noise <- "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='140' height='140'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='2' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.045'/%3E%3C/svg%3E"

SCR_CSS <- paste0("
:root{--paper:#f3ecdd;--panel:#f7f1e4;--ink:#23262d;--soft:#5f5847;--faint:#8a8271;--ox:#8a2b2b;--rule:#d8cdb4;--sienna:#b5651d}
*{box-sizing:border-box}
body{font-family:'Newsreader',Georgia,serif;color:var(--ink);background:var(--paper);margin:0}
body::before{content:'';position:fixed;inset:0;z-index:-1;pointer-events:none;opacity:.5;mix-blend-mode:multiply;background-image:url(\"", .scr_noise, "\")}
.wrap{width:min(95vw,86rem);max-width:100%;margin:0 auto;padding:2.6rem 1.4rem 4rem}
.kicker{font:600 .74rem/1 'Newsreader';letter-spacing:.22em;text-transform:uppercase;color:var(--ox);margin:0 0 .8rem}
h1{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:clamp(1.9rem,4.5vw,2.9rem);line-height:1.02;letter-spacing:-.015em;margin:0 0 .7rem}
.dek{font-size:1.1rem;line-height:1.5;color:var(--soft);font-style:italic;margin:0 0 1.4rem;max-width:46rem}
.brule{border:0;border-top:2px solid var(--ink);margin:1rem 0 1.4rem;position:relative}
.brule::after{content:'';position:absolute;left:0;top:4px;width:100%;border-top:1px solid var(--rule)}
.foot{margin-top:1.6rem;font-size:.9rem;color:var(--soft);font-style:italic;max-width:52rem}
.foot a,.back a{color:var(--sienna);text-decoration:none;border-bottom:1px solid rgba(181,101,29,.35)}
.back{margin-top:1.4rem;font-size:.95rem}
.gt_table,.reactable{font-family:'Newsreader',Georgia,serif!important}
.rt-table{background:var(--panel);border:1px solid var(--rule);overflow-x:auto}
.wrap .reactable,.wrap .html-widget,.wrap .gt_table{width:100%!important;max-width:100%!important}
.rt-thead .rt-th{background:var(--paper)!important;color:var(--ox)!important;font-weight:600;text-transform:uppercase;letter-spacing:.06em;font-size:.76rem;border-bottom:2px solid var(--ink)!important}
.rt-th,.rt-td{border-color:var(--rule)!important}
.rt-td{font-variant-numeric:tabular-nums;color:var(--ink);font-size:.95rem;padding:.5rem .7rem!important;align-items:center!important;line-height:1.5!important}
.rt-tbody .rt-tr:hover{background:rgba(138,43,43,.06)!important}
.rt-tr-striped{background:#efe7d6!important}
.rt-td a{color:var(--ox);text-decoration:none}
.rt-td a:hover{color:#6f2020}
.rt-td details{font-size:.9rem;line-height:1.5}
.rt-td details summary{color:var(--ox);cursor:pointer;font-style:italic;list-style:none}
.rt-td details summary::-webkit-details-marker{display:none}
.rt-td details[open] summary{margin-bottom:.35rem}
.rt-td details p{margin:.3rem 0;text-align:left}
.rt-th[aria-sort='ascending']{box-shadow:inset 0 3px 0 0 var(--ox)!important}
.rt-th[aria-sort='descending']{box-shadow:inset 0 -3px 0 0 var(--ox)!important}
.rt-search,.rt-filter{font-family:'Newsreader',Georgia,serif!important;background:#fbf7ec!important;border:1px solid var(--rule)!important;border-radius:2px;color:var(--ink)!important;padding:.3rem .5rem}
.rt-search:focus,.rt-filter:focus{outline:none;border-color:var(--ox)!important;box-shadow:0 0 0 2px rgba(138,43,43,.12)}
.rt-search{margin-bottom:.7rem;width:16rem;max-width:100%}
.rt-search::placeholder,.rt-filter::placeholder{color:var(--faint)}
.rt-pagination{border-top:1px solid var(--rule)!important;color:var(--soft);font-size:.9rem}
.rt-page-button{font-family:'Newsreader',serif!important;color:var(--ox)!important}
.rt-page-button:not(:disabled):hover{background:rgba(138,43,43,.08)!important}
.rt-page-button[aria-current='true'],.rt-current-page{color:var(--ink)!important;font-weight:600}
.rt-page-info{color:var(--soft)!important}
.rt-page-size-select{font-family:'Newsreader',Georgia,serif!important;background:#fbf7ec!important;border:1px solid var(--rule)!important;border-radius:2px;color:var(--ink)!important;padding:.15rem 1.3rem .15rem .45rem;margin:0 .35rem}
.rt-page-size-select:focus{outline:none;border-color:var(--ox)!important;box-shadow:0 0 0 2px rgba(138,43,43,.12)}
")

# Document links for a case, selected by kind and shown in that order.
case_documents <- function(ev, kinds) {
  if (!is.data.frame(ev) || nrow(ev) == 0) return("—")
  dcols <- str_subset(names(ev), "^docs_"); lcols <- str_subset(names(ev), "^links_")
  if (!length(lcols)) return("—")
  txt <- ev[["Proceedings and Orders"]] %||% rep("", nrow(ev))
  main_url <- function(i) { ls <- unlist(ev[i, lcols], use.names = FALSE); ls[which(!is.na(ls))][1] }
  found <- character()
  for (i in seq_len(nrow(ev))) {
    t <- txt[i] %||% ""
    if ("Petition" %in% kinds && str_detect(t, "^Petition for a writ of certiorari")) { u <- main_url(i); if (!is.na(u)) found["Petition"] <- u }
    # Applications (26A#) file an "Application (…)" whose link is a generic "Main
    # Document"; match the proceeding text and keep the FIRST one with a link (the
    # filed application, not a later order/response).
    if ("Application" %in% kinds && is.na(found["Application"]) && str_detect(t, "^Application\\b")) { u <- main_url(i); if (!is.na(u)) found["Application"] <- u }
    if ("BIO" %in% kinds && str_detect(t, "in opposition filed")) { u <- main_url(i); if (!is.na(u)) found["BIO"] <- u }
    if ("Reply" %in% kinds && str_detect(t, "^Reply")) { u <- main_url(i); if (!is.na(u)) found["Reply"] <- u }
    if ("Appendix" %in% kinds) {
      if (str_detect(t, "(?i)^(Appendix|Joint appendix)")) { u <- main_url(i); if (!is.na(u)) found["Appendix"] <- u }
      if (length(dcols)) { ds <- unlist(ev[i, dcols], use.names = FALSE); ls <- unlist(ev[i, lcols], use.names = FALSE)
        j <- which(!is.na(ds) & str_detect(ds, "(?i)appendix") & !is.na(ls)); if (length(j)) found["Appendix"] <- ls[j[1]] }
    }
  }
  found <- found[intersect(kinds, names(found))]
  if (!length(found)) return("—")
  paste(sprintf("<a href='%s' target='_blank'>%s</a>", found, names(found)), collapse = "<br>")
}

# Petitioner's counsel of record as "Name<br>Firm" from the JSON parties tibble.
petitioner_counsel_html <- function(parties) {
  if (!is.data.frame(parties) || nrow(parties) == 0 || !"attys" %in% names(parties)) return("—")
  pet <- parties[str_detect(parties$type %||% "", "Petitioner|Applicant|Appellant"), , drop = FALSE]
  if (nrow(pet) == 0) return("—")
  cor <- pet[which(pet$counsel_of_record %in% TRUE), , drop = FALSE]
  row <- if (nrow(cor)) cor[1, ] else pet[1, ]
  # %||% guards against a parties structure that lacks a firm column (the
  # historical scrape), where row$firm would be NULL and break the length-1 &&.
  nm <- row$attys %||% NA_character_; fm <- row$firm %||% NA_character_
  if (is.na(nm) || !nzchar(nm)) return("—")
  if (!is.na(fm) && nzchar(fm)) paste0(nm, "<br>", fm) else nm
}

# gt tab_options that opt_interactive() maps into the reactable theme.
scr_theme_options <- function(gt_tbl) {
  gt_tbl |> tab_options(
    table.font.names = c("Newsreader","Georgia","serif"),
    table.font.size = px(15), table.font.color = "#23262d",
    table.background.color = "#f7f1e4",
    column_labels.background.color = "#f3ecdd",
    column_labels.font.weight = "600", column_labels.text_transform = "uppercase",
    table.border.top.color = "#23262d", table.border.bottom.color = "#23262d",
    table_body.hlines.color = "#d8cdb4",
    row.striping.background_color = "#efe7d6")
}

# Make a themed gt interactive; `n_rows` supplies the "All" page-size option.
scr_interactive <- function(gt_tbl, n_rows, page_size_default = 25) {
  gt_tbl |> scr_theme_options() |>
    opt_interactive(use_search = TRUE, use_filters = TRUE, use_sorting = TRUE,
                    use_pagination = TRUE, use_pagination_info = TRUE,
                    use_page_size_select = TRUE, use_highlight = TRUE,
                    use_compact_mode = TRUE, page_size_default = page_size_default,
                    page_size_values = sort(unique(c(25, 50, 100, n_rows))))
}

# Inline the reactable/React widget dependencies that gtsave writes as external
# `lib/...` files, so each page is fully self-contained (no orphaned lib/ dir).
# Scripts become base64 data-URIs (bulletproof against an embedded </script>);
# stylesheets are inlined as <style>. base_dir is where gtsave wrote `lib/`.
scr_inline_libs <- function(html, base_dir) {
  b64 <- function(f) jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))
  sm <- str_match_all(html, "<script[^>]*\\ssrc=\"(lib/[^\"]+)\"[^>]*>\\s*</script>")[[1]]
  for (k in seq_len(nrow(sm))) {
    f <- file.path(base_dir, sm[k, 2])
    if (file.exists(f))
      html <- sub(sm[k, 1],
        sprintf("<script src=\"data:application/javascript;base64,%s\"></script>", b64(f)),
        html, fixed = TRUE)
  }
  lm <- str_match_all(html, "<link[^>]*\\shref=\"(lib/[^\"]+\\.css)\"[^>]*/?>")[[1]]
  for (k in seq_len(nrow(lm))) {
    f <- file.path(base_dir, lm[k, 2])
    if (file.exists(f))
      html <- sub(lm[k, 1],
        paste0("<style>", paste(readLines(f, warn = FALSE), collapse = "\n"), "</style>"),
        html, fixed = TRUE)
  }
  html
}

# gtsave the widget, then wrap in the page chrome + inject the theme, an "All"
# page-size relabel, and any left-aligned DATA columns (headers stay centered).
scr_write_page <- function(gt_tbl, out_path, kicker, title, dek, n_rows,
                           left_cols = integer(0), footer = "", back = NULL) {
  # Typographic quotes for the page chrome (prose only -- never the widget body,
  # whose JSON payload uses " structurally). smarten() lives in page_style.R,
  # always sourced alongside this module in production; fall back to identity.
  sm <- if (exists("smarten", mode = "function")) smarten else function(z) z
  kicker <- sm(kicker); title <- sm(title); dek <- sm(dek)
  if (nzchar(footer)) footer <- sm(footer)
  if (!is.null(back)) back$label <- sm(back$label)
  # Render into an isolated dir so the widget's `lib/` can be inlined and swept.
  wdir <- tempfile("scrpage"); dir.create(wdir)
  tmp <- file.path(wdir, "widget.html")
  gtsave(gt_tbl, tmp)
  w <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  w <- scr_inline_libs(w, wdir)
  unlink(wdir, recursive = TRUE)
  body_inner <- sub("(?s).*<body[^>]*>(.*)</body>.*", "\\1", w, perl = TRUE)
  head_inner <- sub("(?s).*<head[^>]*>(.*)</head>.*", "\\1", w, perl = TRUE)
  leftcss <- if (length(left_cols))
    paste0(paste0(sprintf(".rt-tbody .rt-td:nth-child(%d)", left_cols),
      "{justify-content:flex-start!important;text-align:left!important}", collapse = "\n"), "\n",
      paste0(sprintf(".rt-tbody .rt-td:nth-child(%d) .rt-td-inner", left_cols),
      "{text-align:left!important}", collapse = "\n")) else ""
  script <- sprintf("<script>(function(){var N=%d;function fix(){document.querySelectorAll('select').forEach(function(s){Array.prototype.forEach.call(s.options,function(o){if(String(o.value)===String(N)&&o.text!=='All'){o.text='All';}});});}new MutationObserver(fix).observe(document.body,{childList:true,subtree:true});setTimeout(fix,250);setTimeout(fix,1000);})();</script>", n_rows)
  back_html <- if (!is.null(back)) paste0("<p class='back'><a href='", back$href, "'>", back$label, "</a></p>") else ""
  page <- paste0(
    "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", title, "</title>",
    "<link rel='preconnect' href='https://fonts.googleapis.com'>",
    "<link rel='stylesheet' href='", SCR_FONTS, "'>",
    head_inner, "<style>", SCR_CSS, leftcss, "</style></head><body><main class='wrap'>",
    "<p class='kicker'>", kicker, "</p><h1>", title, "</h1>",
    "<p class='dek'>", dek, "</p><hr class='brule'>",
    body_inner,
    if (nzchar(footer)) paste0("<p class='foot'>", footer, "</p>") else "",
    back_html, script,
    "</main></body></html>")
  writeLines(enc2utf8(page), out_path, useBytes = TRUE)
  invisible(out_path)
}
