# conference_dash.R -----------------------------------------------------------
# Supreme Court conference reports. SCOTUS records each case's distribution for a
# Conference as a docket entry: "DISTRIBUTED for Conference of M/D/YYYY." These
# functions scan a case tibble (as produced by get_scotus_update() in
# scotus_dash_new.R, or the historical data in data-raw/ot_*.rds) for those
# notations and render one dashboard per conference date.
#
# Only the `events` proceedings text is needed, so this works on both the live
# JSON cases and the historical scrape, and requires no network access.

suppressPackageStartupMessages({
  library(gt)
  library(gtExtras)
  library(tidyverse)
  library(htmltools)
})

CONF_PATTERN <- regex(
  "DISTRIBUTED for Conference of (\\d{1,2}/\\d{1,2}/\\d{4})",
  ignore_case = TRUE
)

# Case type derived from the docket number itself, which is authoritative even
# when the source data's `type` column is missing (older terms in scotus.rds
# predate that column): NNA# = application, NN-5001+ = IFP, otherwise paid.
derive_case_type <- function(dkt) {
  n <- suppressWarnings(as.integer(str_extract(dkt, "\\d+$")))
  case_when(
    str_detect(dkt, "A\\d+$") ~ "app",
    !is.na(n) & n >= 5001 ~ "ifp",
    TRUE ~ "paid"
  )
}

# URL of the petition-for-certiorari PDF from a case's events. Handles both the
# historical scrape (Document_*/links_* columns) and the JSON build_events()
# layout (docs_*/links_*), where description column N pairs with link column N.
find_petition_url <- function(events) {
  if (!is.data.frame(events) || nrow(events) == 0) return(NA_character_)
  desc_cols <- str_subset(names(events), "^(docs_|Document_)")
  link_cols <- str_subset(names(events), "^links_")
  if (length(desc_cols) == 0 || length(link_cols) == 0) return(NA_character_)
  for (i in seq_len(nrow(events))) {
    descs <- unlist(events[i, desc_cols], use.names = FALSE)
    links <- unlist(events[i, link_cols], use.names = FALSE)
    hit <- which(!is.na(descs) & str_detect(descs, regex("^Petition", ignore_case = TRUE)))
    hit <- hit[hit <= length(links)]
    if (length(hit) > 0 && !is.na(links[hit[1]])) return(links[hit[1]])
  }
  NA_character_
}

# The conference dates one case's events were distributed for (sorted, unique).
case_conference_dates <- function(events) {
  if (!is.data.frame(events) || !("Proceedings and Orders" %in% names(events))) {
    return(as.Date(character()))
  }
  m <- str_match(events[["Proceedings and Orders"]], CONF_PATTERN)[, 2]
  sort(unique(lubridate::mdy(m[!is.na(m)])))
}

# Filter the case database to distributions and expand to one row per
# (case, conference date). Adds:
#   conf_date        the Conference the case was distributed for
#   n_distributions  total conferences this case was distributed for
#   distribution_no  1 = first conference, 2+ = a relist
conference_distributions <- function(cases) {
  stopifnot("events" %in% names(cases))
  cases |>
    mutate(
      .cid = row_number(),
      conf_date = map(events, case_conference_dates)
    ) |>
    filter(lengths(conf_date) > 0) |>
    mutate(
      n_distributions = lengths(conf_date),
      petition_url = map_chr(events, find_petition_url)
    ) |>
    unnest_longer(conf_date) |>
    group_by(.cid) |>
    mutate(distribution_no = row_number()) |>
    ungroup() |>
    mutate(type = derive_case_type(dkt)) |> # authoritative; source `type` may be NA
    select(-.cid)
}

# Render one conference's dashboard from a conference_distributions() tibble.
# Returns the output path (invisibly), or NULL if no cases match.
conference_dash <- function(dist, conf_date,
                            out_dir = path.expand("~/public_html/conferences")) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  conf_date <- as.Date(conf_date)

  d <- dist |> filter(conf_date == !!conf_date)
  if (nrow(d) == 0) return(invisible(NULL))

  d <- d |>
    # Keep the case name on one line (no v. break).
    mutate(caption = caption |>
      str_replace_all("\\s+", " ") |>
      str_remove(", Petitioners?") |>
      str_remove(", Respondents?") |>
      str_remove_all(", et al.") |>
      str_trim()) |>
    mutate(lower = str_replace(
      lower, "^United States Court of Appeals for the (.+?Circuit)", "\\1"
    )) |>
    mutate(status = if_else(
      distribution_no == 1, "Initial",
      paste0("Relisted (", distribution_no - 1, "×)")
    )) |>
    mutate(petition = if_else(
      !is.na(petition_url) & petition_url != "",
      str_c("[Petition](", petition_url, ")"), "—"
    )) |>
    # Most-relisted cases first (the interesting ones), then paid -> ifp -> app
    # and docket number, while dkt is still raw.
    arrange(
      desc(distribution_no),
      factor(type, levels = c("paid", "ifp", "app")),
      as.integer(str_extract(dkt, "\\d+$"))
    ) |>
    mutate(dkt = str_c(
      "[", dkt,
      "](https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/",
      dkt, ".html)"
    )) |>
    select(type, caption, dkt, petition, lower, status)

  d |>
    gt() |>
    fmt_markdown(columns = c(caption, dkt, petition)) |>
    tab_header(
      title = paste0("Cases distributed for the Conference of ",
                     format(conf_date, "%B %d, %Y")),
      subtitle = paste0(nrow(d), " case(s)")
    ) |>
    gt_theme_nytimes() |>
    cols_label(
      type = "Type", caption = "Caption", dkt = "Docket No",
      petition = "Petition", lower = "Court Below", status = "Status"
    ) |>
    cols_align(align = "left", columns = caption) |>
    cols_align(align = "center", columns = c(type, dkt, petition, status)) |>
    # Fixed per-type fills: paid = green, ifp = orange, app = blue.
    tab_style(cell_fill(color = "#B2DF8A"),
              cells_body(columns = type, rows = type == "paid")) |>
    tab_style(cell_fill(color = "#FDBF6F"),
              cells_body(columns = type, rows = type == "ifp")) |>
    tab_style(cell_fill(color = "#A6CEE3"),
              cells_body(columns = type, rows = type == "app")) |>
    gtsave(str_c("conf_", conf_date, ".html"), path = out_dir)

  invisible(file.path(out_dir, str_c("conf_", conf_date, ".html")))
}

# Regenerate index.html for the conference directory, listing every conf_*.html
# newest-first with its case count. The count is read back from each page's
# subtitle ("N case(s)"), so the index is always correct across all terms --
# including ones backfilled in a separate run.
conference_index <- function(out_dir = path.expand("~/public_html/conferences")) {
  files <- list.files(out_dir, pattern = "^conf_\\d{4}-\\d{2}-\\d{2}\\.html$")
  if (length(files) == 0) return(invisible(NULL))
  dates <- as.Date(str_extract(files, "\\d{4}-\\d{2}-\\d{2}"))
  ord <- order(dates, decreasing = TRUE)
  files <- files[ord]
  dates <- dates[ord]

  read_count <- function(f) {
    h <- paste(readLines(file.path(out_dir, f), warn = FALSE), collapse = " ")
    m <- str_match(h, "([0-9,]+)\\s+case")[, 2]
    if (is.na(m)) NA_integer_ else as.integer(str_remove_all(m, ","))
  }

  items <- purrr::map2(files, dates, function(f, d) {
    n <- read_count(f)
    count_txt <- if (!is.na(n)) paste0(n, if (n == 1) " case" else " cases") else ""
    tags$li(
      tags$a(href = f, target = "_blank", format(d, "%B %d, %Y")),
      tags$span(class = "count", count_txt)
    )
  })

  doc <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title("Supreme Court Conference Reports"),
      tags$style(HTML(
        "body{font-family:'Source Sans Pro',system-ui,sans-serif;max-width:760px;
           margin:2rem auto;padding:0 1rem;color:#1a1a1a}
         h1{font-weight:600;border-bottom:2px solid #1a1a1a;padding-bottom:.4rem}
         ul{list-style:none;padding:0}
         li{padding:.45rem 0;border-bottom:1px solid #eee;display:flex;
           justify-content:space-between;align-items:baseline}
         a{text-decoration:none;color:#0b3d91;font-weight:500}
         a:hover{text-decoration:underline}
         .count{color:#777;font-size:.9em}"
      ))
    ),
    tags$body(
      tags$h1("Supreme Court Conference Reports"),
      tags$ul(items)
    )
  )
  save_html(doc, file.path(out_dir, "index.html"))
  invisible(file.path(out_dir, "index.html"))
}

# Compute distributions once, render a dashboard for every conference date in
# `cases`, and (re)build the index. Returns the conference dates (invisibly).
conference_dashboards <- function(cases,
                                  out_dir = path.expand("~/public_html/conferences")) {
  dist <- conference_distributions(cases)
  dates <- dist |> distinct(conf_date) |> arrange(conf_date) |> pull(conf_date)
  message("Rendering ", length(dates), " conference date(s) to ", out_dir)
  for (i in seq_along(dates)) {
    conference_dash(dist, dates[i], out_dir = out_dir)
  }
  conference_index(out_dir)
  invisible(dates)
}
