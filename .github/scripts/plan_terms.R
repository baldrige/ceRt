# Decide which terms to fetch and which conferences to render, writing the plan
# to $GITHUB_OUTPUT (matrix, min_conf, skip). Base R only -- no packages needed.
#
# - Manual run with INPUT_TERMS set: use it (INPUT_MIN optional; else derived).
# - Otherwise auto: the current sitting's term + the prior term. OT{Y}
#   conferences run ~Sept Y - June Y+1, so the OT year is this year from July on,
#   else last year. Scheduled runs skip the Jul/Aug recess.

terms <- Sys.getenv("INPUT_TERMS", "")
min_conf <- Sys.getenv("INPUT_MIN", "")
event <- Sys.getenv("EVENT_NAME", "")
skip <- "false"

if (nchar(terms) == 0) {
  today <- Sys.Date()
  y <- as.integer(format(today, "%Y"))
  mo <- as.integer(format(today, "%m"))
  ot <- if (mo >= 7) y else y - 1L
  terms <- paste(sprintf("%02d", ot %% 100), sprintf("%02d", (ot - 1) %% 100), sep = ",")
  if (event == "schedule" && mo %in% c(7L, 8L)) skip <- "true"
}

term_vec <- trimws(strsplit(terms, ",")[[1]])
term_vec <- term_vec[nzchar(term_vec)]
if (nchar(min_conf) == 0) {
  min_conf <- sprintf("20%02d-08-01", max(as.integer(term_vec)))
}

matrix_json <- paste0("[", paste0('"', term_vec, '"', collapse = ","), "]")
out <- Sys.getenv("GITHUB_OUTPUT", unset = "")
lines <- c(
  paste0("matrix=", matrix_json),
  paste0("min_conf=", min_conf),
  paste0("skip=", skip)
)
if (nzchar(out)) cat(lines, sep = "\n", file = out, append = TRUE)
cat("Plan -> terms:", paste(term_vec, collapse = ","),
    "| min_conf:", min_conf, "| skip:", skip, "\n")
