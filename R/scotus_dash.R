ot_2024 |>
  dplyr::mutate(caption = str_remove(caption, ", Petitioner*")) |>
  dplyr::mutate(caption = str_remove_all(caption, ", et al.")) |>
  dplyr::mutate(
    lower = str_replace(
      lower,
      "^United States Court of Appeals for the (.+?Circuit)",
      "\\1"
    )
  ) |>
  filter(date == today() - 1) |>
  select(caption, type, dkt, lower, lower_date) |>
  gt() |>
  tab_header(
    title = paste0(
      "Petitions and applications filed on ",
      format(today() - 1, "%B %d, %Y")
    )
  ) |>
  gt_theme_nytimes() |>
  fmt_date(columns = lower_date, date_style = "m_day_year", ) |>
  cols_label(
    caption = "Caption",
    dkt = "Docket No.",
    lower = "Court Below",
    lower_date = "Judgment Date"
  ) |>
  sub_missing(columns = lower_date, missing_text = "-") |>
  data_color(columns = type, palette = "ggthemes::Classic_10_Light") |>
  gtsave("scotus_dash.html")
