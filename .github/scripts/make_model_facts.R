#!/usr/bin/env Rscript
# Rebuild data/model_facts.json for the DEPLOYED artifacts, without retraining.
#
#   Rscript .github/scripts/make_model_facts.R
#
# train_cert_model.R writes the facts whenever it writes the models; this is for
# the two cases where it did not: the first time (the artifacts predate the
# file), and after a change to compute_model_facts() or MEASURED_FACTS. It needs
# the corpus cache the models were fitted on, and compute_model_facts() refuses
# a cache whose row counts disagree with the baseline artifact.
#
# Env: MODEL_DIR (default "data"), CORPUS_CACHE (default data-raw/cert_corpus.rds),
# TRAINED_ON (default: the date already in the file -- this script describes
# existing models, so it must not restamp them as trained today).

suppressPackageStartupMessages(library(tidyverse))
source("R/cert_funnel.R"); source("R/cert_model.R"); source("R/model_facts.R")

model_dir <- Sys.getenv("MODEL_DIR", "data")
cache     <- Sys.getenv("CORPUS_CACHE", "data-raw/cert_corpus.rds")
if (!file.exists(cache))
  stop(cache, " does not exist. It is gitignored; train_cert_model.R builds it ",
       "(~minutes), and the facts must come from the corpus the models were fitted on.")

path <- file.path(model_dir, MODEL_FACTS_FILE)
trained_on <- Sys.getenv("TRAINED_ON", "")
if (!nzchar(trained_on) && file.exists(path))
  trained_on <- jsonlite::fromJSON(path)$trained_on
if (!nzchar(trained_on))
  stop("No TRAINED_ON given and no existing ", path, " to read it from.")

mf <- compute_model_facts(readRDS(cache), model_dir, trained_on)
write_model_facts(mf, model_dir)
cat("wrote ", path, ": ", length(mf$facts), " computed + ", length(mf$measured),
    " measured facts for models trained ", mf$trained_on, "\n", sep = "")
