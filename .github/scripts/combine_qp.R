# Merge the per-term QP caches (artifacts from enrich_qp.R) into the shared
# conference QP cache on gh-pages, which render_dockets_for() already reads. New
# entries fold in; existing ones are preserved. Idempotent.
#
# The per-term ledgers of petitions that yielded no question (qp_tried_*.json,
# docket -> the URL read) fold into TRIED_OUT the same way, so the next dispatch
# does not read them again.
#
# Env: IN_DIR (dir of qp_*.json, default "caches"), OUT (shared cache path),
#      TRIED_OUT (shared ledger path; default beside OUT as qp_tried.json).

suppressPackageStartupMessages(library(jsonlite))
in_dir <- Sys.getenv("IN_DIR", unset = "caches")
out    <- Sys.getenv("OUT", unset = "site/conferences/qp_cache.json")
tout   <- Sys.getenv("TRIED_OUT", unset = file.path(dirname(out), "qp_tried.json"))

read_obj <- function(p) if (file.exists(p))
  tryCatch(fromJSON(p, simplifyVector = FALSE), error = function(e) list()) else list()

base <- read_obj(out)
before <- length(base)

all_files <- list.files(in_dir, pattern = "^qp_.*\\.json$", full.names = TRUE, recursive = TRUE)
is_tried <- grepl("^qp_tried_", basename(all_files))
files <- all_files[!is_tried]
got <- character()
for (f in files) {
  m <- read_obj(f)
  for (d in names(m)) if (!is.null(m[[d]]$qp)) { base[[d]] <- m[[d]]; got <- c(got, d) }
}
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_json(base, out, auto_unbox = TRUE)
cat("merged", length(files), "term cache(s):", before, "->", length(base),
    "entries (+", length(base) - before, ")\n")

tried <- read_obj(tout); t_before <- length(tried)
missed <- character()
for (f in all_files[is_tried]) {
  m <- read_obj(f)
  for (d in names(m)) { tried[[d]] <- unlist(m[[d]])[1]; missed <- c(missed, d) }
}
# Dropped only where this merge brought a question and no Term's ledger still
# lists the docket. One can sit in both: a cached counsel block
# (qp_looks_stale) that was read again and still gave nothing keeps its old
# cache entry, and without the ledger it would be re-read on every run.
tried <- tried[setdiff(names(tried), setdiff(got, missed))]
if (length(tried)) write_json(tried, tout, auto_unbox = TRUE) else writeLines("{}", tout)
cat("ledger of petitions with no question found:", t_before, "->", length(tried), "\n")
