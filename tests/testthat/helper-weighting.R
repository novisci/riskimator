# Compare product_limit to KM from survival package
compare_km <- function(rcen){
  time <- as_canonical(get_time(rcen))
  evnt <- !as_canonical(get_outcome(rcen))

  km  <- summary(survfit(Surv(time, evnt) ~ 1, timefix = FALSE),
                 censored = TRUE)
  res <- product_limit(rcen)

  # Unique values should always be equal
  expect_equal(unique(rev(sort(res))), unique(km$surv),
               tolerance = 1e-15)

  # Length of res should always be >= km
  expect_gte(length(res), length(km[["time"]]))

  # If there are no duplicates lengths should match
  if (anyDuplicated(time) == 0){
    expect_length(res, length(km[["time"]]))
  }
}
