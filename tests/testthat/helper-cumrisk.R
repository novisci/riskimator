
# Compare the result of cumrisk to complement of KM from survival
compare_km <- function(rcen){
  # browser()
  time <- as_canonical(get_time(rcen))
  evnt <- as_canonical(get_outcome(rcen))

  km <- summary(survfit(Surv(time, evnt) ~ 1))

  res <- cumrisk(rcen, w = product_limit)

  expect_length(res$time, length(res$estimate))

  expect_equal(res$time, km$time)

  expect_equal(unique(res$estimate), unique(1 - km$surv),
               tolerance = 1e-15)
}


