# Subject-matter areas: which of the Supreme Court Database's 14 issue areas a
# case is about, estimated from its caption and questions presented by Jev
# (TypeSafe). The classifier is Python -- the only client for the model is --
# and lives in .github/scripts/subject_area/; it keeps cases/subjects.json
# current. This file only reads that file, and asks the classifier to refresh
# it when the key and Python are available. See docs/subject-areas.md.
#
# Measured before this shipped (2026-09): 77-80% agreement with the Database's
# own coding of 573 decided OT17-25 cases, 81% with a hand-labelled sample of
# 150 ungranted petitions (87% IFP, 75% paid). The confidence runs high -- a 0.9
# is right ~88% of the time -- so the threshold below was read off the measured
# table, not the model's own numbers: at 0.7, ~84% of cases get a label and
# ~84-86% of those labels are right.

SUBJECT_MIN_CONFIDENCE <- 0.7

# docket -> area, for the dockets whose label clears the threshold. Unreadable
# questions presented (OCR noise, a cover page) carry no area and are absent,
# as are low-confidence picks: no label beats a wrong one.
load_subjects <- function(site_dir) {
  p <- file.path(site_dir, "cases", "subjects.json")
  if (!file.exists(p)) return(character())
  s <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) list())
  s[["_meta"]] <- NULL
  keep <- vapply(s, function(v) !is.null(v$area) &&
                   isTRUE((v$confidence %||% 0) >= SUBJECT_MIN_CONFIDENCE), logical(1))
  vapply(s[keep], function(v) v$area, character(1))
}

# Run the classifier over whatever QPs the site now holds. Incremental (only new
# or changed dockets cost a request) and never fatal: no Python, no package, no
# key, a timeout -- each is a message, and the pages render with the labels the
# file already had. `max_new` bounds a run's requests.
refresh_subjects <- function(site_dir, max_new = 2000L, timeout = 900) {
  script <- file.path(".github", "scripts", "subject_area", "classify_site.py")
  if (!file.exists(script)) return(invisible(FALSE))
  if (!nzchar(Sys.getenv("TYPESAFE_API_KEY"))) {
    message("subjects: no TYPESAFE_API_KEY; using the labels already in cases/subjects.json")
    return(invisible(FALSE))
  }
  py <- Sys.getenv("SUBJECT_PYTHON", unset = Sys.which("python3"))
  if (!nzchar(py)) py <- Sys.which("python")
  if (!nzchar(py)) { message("subjects: no python on PATH"); return(invisible(FALSE)) }
  out <- tryCatch(
    system2(py, c(shQuote(normalizePath(script)), "--site", shQuote(normalizePath(site_dir)),
                  "--max-new", as.integer(max_new)),
            stdout = TRUE, stderr = TRUE, timeout = timeout),
    error = function(e) structure(conditionMessage(e), status = 1L), warning = function(w)
      structure(conditionMessage(w), status = 1L))
  message(paste("subjects:", out, collapse = "\n"))
  invisible(is.null(attr(out, "status")))
}

# The Case panel's "Subject area" row. Says where the label comes from, because
# it is an estimate and the rest of the panel is the Court's own record.
docket_subject_html <- function(area) {
  if (is.null(area) || length(area) != 1 || is.na(area) || !nzchar(area)) return("")
  paste0("<p><span class='side'>Subject area</span><br>", htmltools::htmlEscape(area),
         " <span class='amic-side'>(estimated from the questions presented, in the ",
         "<a href='/about.html#subject-areas'>Supreme Court Database&rsquo;s categories</a>)</span></p>")
}
