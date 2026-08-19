# model_coef_table.R -----------------------------------------------------------
# Pull the regression output -- coefficient, standard error, z, p, odds ratio --
# off a deployed cert-model artifact, and name the variable each design-matrix
# column came from.
#
# Shared by the two documents that publish coefficients: the standalone
# reference (make_model_reference.R) and the Coefficients section of the methods
# note (make_methods_note.R). It lives here rather than in either of them
# because a second copy is how the numbers on two pages start disagreeing --
# this repo has paid for that lesson more than once.
#
# NOT in R/: nothing in the render pipeline needs it, and putting it there would
# load it into every workflow that sources cert_model.R to score a petition.

# Which section of the write-up a variable belongs to.
GROUP <- c(pet_type="Structural", resp_type="Structural", court_below="Structural",
  pro_se="Structural", gap_fast="Structural", gap_na="Structural",
  counsel_tier="Counsel", dissent_below="Petition signal", split_argued="Petition signal",
  relist_bucket="Process", amicus_bucket="Process", cvsg="Process",
  response_requested="Process", response_filed="Process", resp_waiver="Process",
  reply_filed="Process", conf_f="Conference", phase="Conference")

# Map a design-matrix column back to its variable; longest match wins so that
# response_filed is not swallowed by response_requested.
var_of <- function(term, feats) {
  if (term == "(Intercept)") return("(Intercept)")
  hit <- feats[vapply(feats, function(f) startsWith(term, f), logical(1))]
  if (!length(hit)) return(term)
  hit[which.max(nchar(hit))]
}

# One row per coefficient for a logit artifact.
logit_tbl <- function(m, key) {
  g <- m$glm; b <- coef(g)
  # firth_vcov comes off the hand-rolled IRLS with NO dimnames, so it is in
  # coef() order and must be named positionally. Subsetting it by name yields
  # all-NA standard errors and no error anywhere -- exactly the silent-zero
  # failure this codebase keeps relearning. Assert rather than trust.
  if (!is.null(g$firth_vcov)) {
    stopifnot(nrow(g$firth_vcov) == length(b))
    se <- sqrt(diag(g$firth_vcov)); names(se) <- names(b)
  } else se <- sqrt(diag(vcov(g)))[names(b)]
  stopifnot(!anyNA(se))
  z <- b / se
  v <- vapply(names(b), var_of, character(1), feats = m$features)
  data.frame(model = key, term = names(b), variable = v,
             group = unname(ifelse(v == "(Intercept)", "—", GROUP[v])),
             estimate = unname(b), se = unname(se), z = unname(z),
             p = unname(2 * stats::pnorm(-abs(z))), or = unname(exp(b)),
             stringsAsFactors = FALSE, row.names = NULL)
}
