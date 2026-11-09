#!/usr/bin/env Rscript
# Rebuild data/model_facts.json for the DEPLOYED artifacts, without retraining.
#
#   Rscript .github/scripts/make_model_facts.R
#
# train_cert_model.R writes the facts whenever it writes the models; this is for
# the two cases where it did not: the first time (the artifacts predate the
# file), and after a change to compute_model_facts() or MEASURED_FACTS. It needs
# the corpus and panel caches the models were fitted on, and
# compute_model_facts() refuses caches that do not reproduce the artifacts'
# row counts and leave-one-term-out AUCs.
#
# About two minutes: the rolling-origin evaluation refits every model at every
# origin, and without the trainer's leave-one-term-out predictions to reuse it
# has to redo those too.
#
# Env: MODEL_DIR (default "data"), CORPUS_CACHE (default data-raw/cert_corpus.rds),
# PANEL_CACHE (default data-raw/cert_panel.rds), TRAINED_ON (default: the date
# already in the file -- this script describes existing models, so it must not
# restamp them as trained today).

suppressPackageStartupMessages(library(tidyverse))
source("R/cert_funnel.R"); source("R/cert_model.R"); source("R/model_facts.R")

model_dir <- Sys.getenv("MODEL_DIR", "data")
cache     <- Sys.getenv("CORPUS_CACHE", "data-raw/cert_corpus.rds")
pcache    <- Sys.getenv("PANEL_CACHE", "data-raw/cert_panel.rds")
for (f in c(cache, pcache)) if (!file.exists(f))
  stop(f, " does not exist. It is gitignored; train_cert_model.R builds it ",
       "(~minutes), and the facts must come from the frames the models were fitted on.")

path <- file.path(model_dir, MODEL_FACTS_FILE)
trained_on <- Sys.getenv("TRAINED_ON", "")
if (!nzchar(trained_on) && file.exists(path))
  trained_on <- jsonlite::fromJSON(path)$trained_on
if (!nzchar(trained_on))
  stop("No TRAINED_ON given and no existing ", path, " to read it from.")

corpus <- readRDS(cache); panel <- readRDS(pcache)
# The trainer adds counsel_tier after loading the caches; so must this.
ix <- readRDS(file.path(model_dir, "counsel_index.rds"))
corpus$counsel_tier <- counsel_tier(corpus$counsel_key, corpus$date, ix)
panel$counsel_tier  <- counsel_tier(panel$counsel_key,  panel$date,  ix)

ro <- list()
for (k in list(list("baseline", corpus, BASELINE_FEATURES),
               list("enhanced", panel, ATRISK_FEATURES),
               list("gvr", set_target(panel, "gvr"), GVR_FEATURES))) {
  message(format(Sys.time(), "%H:%M:%S"), "  rolling origin: ", k[[1]])
  ro[[k[[1]]]] <- rolling_origin_eval(model_frame(k[[2]], k[[3]]), k[[3]])
}
message(format(Sys.time(), "%H:%M:%S"), "  rolling origin: conference")
conf_oos <- conference_oos_eval(conf_model_frame(panel))
mf <- compute_model_facts(corpus, model_dir, ro, conf_oos, trained_on)
write_model_facts(mf, model_dir)
cat("wrote ", path, ": ", length(mf$facts), " computed + ", length(mf$measured),
    " measured facts for models trained ", mf$trained_on, "\n", sep = "")
