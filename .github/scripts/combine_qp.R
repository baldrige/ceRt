# Merge the per-term QP caches (artifacts from enrich_qp.R) into the shared
# conference QP cache on gh-pages, which render_dockets_for() already reads. New
# entries fold in; existing ones are preserved. Idempotent.
#
# Env: IN_DIR (dir of qp_*.json, default "caches"), OUT (shared cache path).

suppressPackageStartupMessages(library(jsonlite))
in_dir <- Sys.getenv("IN_DIR", unset = "caches")
out    <- Sys.getenv("OUT", unset = "site/conferences/qp_cache.json")

base <- if (file.exists(out))
  tryCatch(fromJSON(out, simplifyVector = FALSE), error = function(e) list()) else list()
before <- length(base)

files <- list.files(in_dir, pattern = "^qp_.*\\.json$", full.names = TRUE, recursive = TRUE)
for (f in files) {
  m <- tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) list())
  for (d in names(m)) if (!is.null(m[[d]]$qp)) base[[d]] <- m[[d]]
}
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_json(base, out, auto_unbox = TRUE)
cat("merged", length(files), "term cache(s):", before, "->", length(base),
    "entries (+", length(base) - before, ")\n")
