granted_cases <- function(url) {
  granted <- pdf_text(url)
  granted |>
    str_extract_all(pattern = "\n\\b\\d{2}-\\d{1,4}\\b") |>
    unlist() |>
    str_sub(start = 2L) |>
    as_tibble() |>
    rename(dkt = value)
  return(granted)
}
