# Merge the per-term petition-signal caches (downloaded artifacts) into the
# committed enrichment layer data-raw/petition_signals.json, keyed by docket.
#
# Env: IN_DIR (dir of petition_signals_*.json artifacts), OUT (default
# data-raw/petition_signals.json).

suppressPackageStartupMessages({ library(jsonlite) })
in_dir <- Sys.getenv("IN_DIR", unset = "caches")
out    <- Sys.getenv("OUT", unset = "data-raw/petition_signals.json")

files <- list.files(in_dir, pattern = "petition_signals_.*\\.json$",
                    full.names = TRUE, recursive = TRUE)
if (length(files) == 0) stop("no per-term caches found in ", in_dir)

merged <- if (file.exists(out)) fromJSON(out, simplifyDataFrame = FALSE) else list()
for (f in files) {
  part <- fromJSON(f, simplifyDataFrame = FALSE)
  merged[names(part)] <- part            # union; later files win on conflict
  cat(basename(f), "->", length(part), "entries\n")
}
write_json(merged, out, auto_unbox = TRUE)
cat("merged total:", length(merged), "dockets ->", out, "\n")
