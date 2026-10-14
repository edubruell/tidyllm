make_weights <- function(df, targets, trim = 5) {
  require(survey)
  design <- svydesign(ids = ~1, data = df)
  raked <- rake(design, list(~region, ~age_group), targets)
  w <- weights(raked)
  w[w > trim] <- trim
  w
}

check_weights <- function(w) {
  summary(w)
}
