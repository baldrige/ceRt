library(tidyverse)
library(rvest)

# A helper function that checks if a given URL (based on year and try_val)
# returns an error message.
is_error <- function(year, sep = "-", try_val) {
  base_url <- "https://www.supremecourt.gov/docket/docketfiles/html/public/"
  url <- paste0(base_url, year, sep, try_val, ".html")

  # Read the HTML from the URL. If there is a network or parsing error,
  # you might want to wrap read_html() in tryCatch() to handle that gracefully.
  html <- read_html(url)
  text <- html %>% html_text2()

  # Returns TRUE if the error message is found (i.e. the try_val is too high)
  return(str_detect(text, "ERROR: File or directory not found."))
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

get_scotus_ot <- function(year) {
  paid <- binary_search_max(year, "-", 0, 2000)
  ifp <- binary_search_max(year, "-", 5001, 10000)
  apps <- binary_search_max(year, "A", 0, 2000)
  docket <- tibble(dkt = c(1:paid, 5001:ifp, 1:apps), sep = NA)
  docket$sep[1] <- "-"
  docket$sep[paid + 1] <- "-"
  docket$sep[length(1:paid) + length(5001:ifp) + 1] <- "A"
  docket <- docket %>% fill(sep)
  docket <- docket %>% mutate(dkt = paste0(year, sep, dkt)) %>% select(dkt)
  cases <- docket$dkt %>%
    map(
      \(x)
        tryCatch(
          {
            get_scotus_case(dkt = x)
          },
          error = function(msg) {
            return(NA)
          }
        ),
      .progress = TRUE
    )
  cases <- do.call("rbind", cases)
  return(filter(cases, !is.na(caption)))
}

get_scotus_case <- function(dkt) {
  base_url <- "https://www.supremecourt.gov/docket/docketfiles/html/public/"
  url <- paste0(base_url, dkt, ".html")
  html <- read_html(url)
  title <- html %>%
    html_table() %>%
    pluck(1) %>%
    filter(X1 == "Title:") %>%
    pull(X2)
  date <- html %>%
    html_table() %>%
    pluck(1) %>%
    filter(X1 == "Docketed:") %>%
    pull(X2) %>%
    as_date(format = "%m %d, %Y")
  lower <- html %>%
    html_table() %>%
    pluck(1) %>%
    filter(X1 == "Lower Ct:") %>%
    pull(X2)
  lower_dkt <- html %>%
    html_table() %>%
    pluck(1) %>%
    filter(X1 == "Case Numbers:") %>%
    pull(X2) %>%
    str_sub(start = 2L, end = -2L)
  lower_date <- html %>%
    html_table() %>%
    pluck(1) %>%
    filter(X1 == "Decision Date:") %>%
    pull(X2) %>%
    as_date(format = "%m %d, %Y")
  events <- html %>%
    html_table(header = TRUE) %>%
    pluck(2) %>%
    filter(Date != "")
  docket_dates <- html %>%
    html_table(header = TRUE) %>%
    pluck(2) %>%
    filter(Date != "") %>%
    select(Date, `Proceedings and Orders`)
  docs <- html %>%
    html_table(header = TRUE) %>%
    pluck(2)
  links1 <- html %>%
    html_element("#proceedings") %>%
    html_elements("tr") %>%
    html_elements(".borderbttm")
  links2 <- links1 %>%
    map(\(x) html_attr(html_children(x), "href"))
  links3 <- tibble(links = links2)
  link <- links3 %>%
    unnest_wider(col = links, names_sep = "_")
  docs <- docs %>%
    bind_cols(link) %>%
    filter(Date == "") %>%
    rename(Document = `Proceedings and Orders`) %>%
    select(-Date)
  docs <- docs %>%
    mutate(Document = str_split(Document, "(?<=[a-z])(?=[A-Z])")) %>%
    unnest_wider(col = Document, names_sep = "_")
  docs <- bind_cols(docket_dates, docs)
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
  ids <- html %>%
    html_table() %>%
    pluck(3, "X1") %>%
    str_which("Attorneys for ")
  extra <- html %>% html_table() %>% pluck(3, "X1") %>% str_which("^Other")
  length <- html %>% html_table() %>% pluck(3, "X1") %>% length() + 1
  ids <- ids %>% append(extra) %>% append(length)
  all_parties <- tibble()
  for (i in 1:(length(ids) - 1)) {
    # party_side <- html %>% html_table() %>% pluck(3) %>% slice(ids[i]:(ids[i+1]-1))
    party <- html %>%
      html_table() %>%
      pluck(3) %>%
      slice(ids[i]:(ids[i + 1] - 1)) %>%
      filter(str_detect(X1, "Attorneys for ") | str_detect(X1, "^Other")) %>%
      pluck("X1") %>%
      str_remove("Attorneys for ")
    names <- html %>%
      html_table() %>%
      pluck(3) %>%
      slice(ids[i]:(ids[i + 1] - 1)) %>%
      filter(str_detect(X1, "Party name: ")) %>%
      pluck("X1") %>%
      str_remove("Party name: ")
    attys <- html %>%
      html_table() %>%
      pluck(3) %>%
      filter(X1 != "") %>%
      slice(ids[i]:(ids[i + 1] - 1)) %>%
      filter(!is.na(X1)) %>%
      filter(
        str_detect(X1, "Attorneys for ", negate = TRUE) &
          str_detect(X1, "Other", negate = TRUE) &
          str_detect(X1, "Party name: ", negate = TRUE)
      ) %>%
      pluck("X1")
    parties <- tibble(names = names, attys = attys) %>%
      nest(.by = names, .key = "attys") %>%
      bind_cols(party = party)
    all_parties <- bind_rows(all_parties, parties)
  }
  return(all_parties)
}

index <- tibble(
  name = c(
    "ot_2017.rds",
    "ot_2018.rds",
    "ot_2019.rds",
    "ot_2020.rds",
    "ot_2021.rds",
    "ot_2022.rds",
    "ot_2023.rds",
    "ot_2024.rds"
  )
)
scotus <- map_df(
  index$name,
  \(x) scotus <- bind_rows(scotus, read_rds(paste0("./data-raw/", x)))
)
save(scotus, file = "./data/scotus.rda")
