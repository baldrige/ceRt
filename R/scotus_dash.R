library(gt)
library(gtExtras)
library(tidyverse)
library(rvest)
library(httr2)

# A helper function that checks if a given URL (based on year and try_val)
# returns an error message.
is_error <- function(year, sep = "-", try_val) {
  base_url <- "https://www.supremecourt.gov/docket/docketfiles/html/public/"
  url <- paste0(base_url, year, sep, try_val, ".html")

  safe_error <- safely(\(x) req_perform(request(x)))
  if_else(is.null(safe_error(url)$result), TRUE, FALSE)
}

# Function to perform binary search for the largest valid try value.
# lower: starting lower bound for try values.
# upper: starting upper bound for try values.
binary_search_max <- function(year, sep, lower, upper) {
  best_valid <- lower - 1 # to keep track of the best valid try value found

  while (lower <= upper) {
    mid <- floor((lower + upper) / 2)

    # Check if this mid value returns an error
    if (is_error(year, sep, mid)) {
      # If error is found, then mid is too high
      upper <- mid - 1
    } else {
      # No error means the page exists; record it and try higher numbers
      best_valid <- mid
      lower <- mid + 1
    }
  }

  return(best_valid)
}

get_qp <- function(url) {
  qp <- dplyr::case_when(
    is.na(url) ~ "-",
    !is.na(url) ~ tryCatch(
      stringr::str_c(pdftools::pdf_ocr_text(url, pages = 2)),
      error = function(cond) {
        message(paste("Error:", conditionMessage(cond)))
      } # on any error, return "-"
    )
  )
  return(qp)
}


extract_events <- function(html) {
  rows <- html |> html_element("#proceedings") |> html_elements("tr")

  per_row <- purrr::map(rows, function(tr) {
    tds <- tr |> html_elements("td")
    if (length(tds) < 2) return(NULL)

    date <- tds[[1]] |> html_text2() |> str_trim()
    anchors <- tds[[2]] |> html_elements("a")
    doc_names <- anchors |> html_text2() |> str_trim()
    hrefs <- anchors |> html_attr("href")

    text_nodes <- xml2::xml_find_all(tds[[2]], ".//text()[not(ancestor::a)]")
    action <- paste(xml2::xml_text(text_nodes), collapse = " ") |>
      str_replace_all("[\r\n\t]+", " ") |> str_squish()

    if (date == "" && action == "" && length(doc_names) == 0) return(NULL)

    row <- tibble(Date = date, `Proceedings and Orders` = action)
    if (length(doc_names) > 0) {
      docs_cols <- as_tibble(setNames(as.list(doc_names),
                                      paste0("docs_", seq_along(doc_names))))
      link_cols <- as_tibble(setNames(as.list(hrefs),
                                      paste0("links_", seq_along(hrefs))))
      row <- bind_cols(row, docs_cols, link_cols)
    }
    row
  })

  per_row <- purrr::compact(per_row)
  if (length(per_row) == 0) {
    return(tibble(Date = character(), `Proceedings and Orders` = character()))
  }
  bind_rows(per_row)
}

get_scotus_case <- function(dkt) {
  base_url <- "https://www.supremecourt.gov/docket/docketfiles/html/public/"
  url <- paste0(base_url, dkt, ".html")
  html <- read_html(url)
  meta <- html %>% html_table() %>% pluck(1)
  pick <- function(key) {
    v <- meta %>% filter(X1 == key) %>% pull(X2)
    if (length(v) == 0) NA_character_ else v[1]
  }
  title <- pick("Title:")
  date <- as_date(pick("Docketed:"), format = "%m %d, %Y")
  lower <- pick("Lower Ct:")
  lower_dkt_raw <- pick("Case Numbers:")
  lower_dkt <- if (is.na(lower_dkt_raw)) NA_character_ else str_sub(lower_dkt_raw, 2L, -2L)
  lower_date <- as_date(pick("Decision Date:"), format = "%m %d, %Y")
  docs <- extract_events(html)
  parties <- tryCatch(parse_parties(html), error = function(e) return(NA))
  case <- tibble(
    caption = title,
    dkt = dkt,
    date = date,
    lower = lower,
    lower_dkt = lower_dkt,
    lower_date = lower_date,
    parties = list(parties),
    events = list(docs)
  )
  return(case)
}

parse_parties <- function(html) {
  cards <- html |> html_elements("#Contacts") |> html_elements(".card")
  all_parties <- tibble()
  for (i in 1:length(cards)) {
    names <- cards |>
      pluck(i) |>
      html_elements(".card-body") |>
      html_elements(".partyname") |>
      html_text2() |>
      str_remove("Party name: ") |>
      str_trim()
    attorneys <-
      cards |>
      pluck(i) |>
      html_elements(".card-body") |>
      html_elements(".ContactName") |>
      html_text2() |>
      str_remove("Counsel of Record") |>
      str_trim()
    addresses <-
      cards |>
      pluck(i) |>
      html_elements(".card-body") |>
      html_elements(".ContactData2") |>
      html_elements(".address1") |>
      html_text2() |>
      str_trim()
    type <- cards |>
      pluck(i) |>
      html_elements(".card-heading") |>
      html_text2() |>
      str_remove("Attorneys for ") |>
      str_remove(" Attorneys") |>
      str_trim()
    parties <- tibble(
      type = type,
      names = names,
      attys = attorneys,
      address = addresses
    )
    all_parties <- bind_rows(all_parties, parties)
  }
  return(all_parties)
}

get_scotus_update <- function(year) {
  paid <- binary_search_max(year, "-", 0, 2000)
  ifp <- binary_search_max(year, "-", 5001, 10000)
  apps <- binary_search_max(year, "A", 0, 2000)
  docket <- tibble(
    dkt = c((paid - 50):paid, (ifp - 50):ifp, (apps - 50):apps),
    sep = NA,
    type = NA
  )
  docket$sep[1] <- "-"
  docket$sep[53] <- "-"
  docket$sep[103] <- "A"
  docket <- docket %>% fill(sep)
  docket$type[1] <- "paid"
  docket$type[53] <- "ifp"
  docket$type[103] <- "app"
  docket <- docket %>% fill(type)
  docket <- docket %>%
    mutate(dkt = paste0(year, sep, dkt)) %>%
    select(dkt, type)
  cases <- docket$dkt %>%
    map(
      \(x) {
        tryCatch(
          {
            get_scotus_case(dkt = x)
          },
          error = function(msg) {
            return(NA)
          }
        )
      },
      .progress = TRUE
    )
  cases <- do.call("rbind", cases)
  cases <- cases %>% left_join(docket, by = "dkt")
  return(filter(cases, !is.na(caption)))
}

scotus_dash <- function(range = today() - 1, year = "25") {
  out_dir <- path.expand("~/public_html/dashboards")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ot_2024_quick <- get_scotus_update(year = year)

  hits <- ot_2024_quick |> filter(date == range)
  if (nrow(hits) == 0) {
    empty_html <- sprintf(
      '<!DOCTYPE html><html><head><meta charset="utf-8"><title>%s</title></head><body style="font-family:sans-serif;padding:2em"><h2>No petitions or applications docketed on %s.</h2></body></html>',
      format(range, "%B %d, %Y"), format(range, "%B %d, %Y")
    )
    writeLines(empty_html, file.path(out_dir, str_c("dash", "_", range, ".html")))
    return(invisible(NULL))
  }

  table1 <- ot_2024_quick |>
    mutate(caption = caption |>
      str_replace_all("\\s+", " ") |>
      str_remove(", Petitioners?") |>
      str_remove(", Applicants?") |>
      str_remove_all(", et al.") |>
      str_replace("\\s*v\\.\\s+", "\n\nv.\n\n") |>
      str_trim()) |>
    dplyr::mutate(
      lower = str_replace(
        lower,
        "^United States Court of Appeals for the (.+?Circuit)",
        "\\1"
      )
    ) |>
    filter(date == range) |>
    unnest_longer(col = events) |>
    unnest(col = events, names_sep = "_") |>
    rowwise() |>
    mutate(
      doc_links = list(purrr::map2_chr(
        c_across(starts_with("events_docs_")),
        c_across(starts_with("events_links_")),
        ~ if (!is.na(.x) && !is.na(.y)) sprintf("[[%s](%s)]", .x, .y) else ""
      ))
    ) |>
    unnest_wider(col = doc_links, names_sep = "_", simplify = TRUE) |>
    unnest_longer(col = parties) |>
    unnest(col = parties, names_sep = "_") |>
    filter(str_detect(parties_type, "Petitioner|Applicant|Appellant")) |>
    rowwise() |>
    unite(col = doc_links, starts_with("doc_links"), sep = " ") |>
    rowwise() |>
    mutate(
      events = str_c(
        doc_links,
        collapse = " ",
        sep = " "
      )
    ) |>
    rowwise() |>
    mutate(events = str_c(events, sep = " ", collapse = "\n")) |>
    mutate(
      events = str_remove_all(
        events,
        "\\[\\[Motion for Leave to Proceed in Forma Pauperis\\]\\(\\S+\\)\\] "
      )
    ) |>
    mutate(
      events = str_remove_all(
        events,
        "\\[\\[Certificate of Word Count\\]\\(\\S+\\)\\]"
      )
    ) |>
    mutate(
      events = str_remove_all(
        events,
        "\\[\\[Proof of Service\\]\\(\\S+\\)\\]"
      )
    ) |>
    mutate(events = str_remove_all(events, "\\[\\[Other\\]\\(.+\\)\\]")) |>
    ungroup() |>
    group_by(dkt) |>
    mutate(events = str_squish(str_c(events, collapse = " "))) |>
    slice(1) |>
    ungroup() |>
    select(type, caption:parties_address, events) |>
    mutate(
      dkt = paste0(
        "[",
        dkt,
        "](https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/",
        dkt,
        ".html)"
      )
    ) |>
    rowwise() |>
    mutate(lower_date = format(lower_date, format = "%B %d, %Y")) |>
    mutate(
      lower = paste0(
        lower,
        ", No. ",
        lower_dkt,
        "\n\n",
        "Judgment: ",
        lower_date,
        "\n\n"
      )
    ) |>
    mutate(lower = str_replace_all(lower, " NA", " —")) |>
    select(type, caption, dkt, lower, parties_type:parties_address, events) |>
    mutate(pro_se = if_else(parties_names == parties_attys, TRUE, FALSE)) |>
    select(-parties_names, -parties_type)
  qps <- table1 |>
    pluck("events") |>
    str_extract("\\[\\[Petition\\]\\(\\S+\\)\\] ") |>
    str_remove_all("\\[\\[Petition\\]\\(") |>
    str_remove_all("\\)\\]") |>
    str_trim("right") |>
    as_tibble() |>
    pluck("value") |>
    map_if(is.na, \(x) x, .else = get_qp) |>
    unlist()
  table1 |>
    bind_cols(qps = qps) |>
    mutate(qps = str_replace_all(qps, "\\$", "&#36;")) |>
    mutate(
      qps = str_c(
        "<details><summary>Question(s) presented</summary>",
        qps,
        "</details>"
      )
    ) |>
    gt() |>
    fmt_markdown(
      columns = c(caption, lower, dkt, parties_address, events, qps)
    ) |>
    tab_header(
      title = paste0(
        "Petitions and applications docketed on ",
        format(range, "%B %d, %Y")
      )
    ) |>
    gt_theme_nytimes() |>
    cols_label(
      caption = "Caption",
      dkt = "Docket No",
      lower = "Court Below",
      parties_attys = "Petitioner's Counsel",
      parties_address = "Counsel's Address",
      events = "Recent Filings",
      qps = "QP"
    ) |>
    data_color(
      columns = c(pro_se),
      target_columns = "parties_attys",
      method = "factor",
      palette = c("white", "gray"),
      domain = c(TRUE, FALSE)
    ) |>
    cols_width(
      dkt ~ px(80),
      events ~ px(150)
    ) |>
    cols_align(align = "left", columns = c(caption)) |>
    cols_align(align = "center", columns = c(type:events)) |>
    data_color(columns = type, palette = "ggthemes::Classic_10_Light") |>
    cols_hide(columns = c(pro_se)) |>
    gtsave(str_c("dash", "_", range, ".html"), path = out_dir)
}


scotus_dash(range = today(), year = "25")
