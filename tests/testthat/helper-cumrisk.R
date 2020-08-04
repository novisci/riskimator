

create_rcensor <- function(l){
  ctimes <- list(
    v_event_time(replace(l$tm, which(l$st == 1), NA_real_),
                 internal_name = "cA"))

  otimes <- list(
    v_event_time(replace(l$tm, which(l$st == 0), NA_real_),
                 internal_name = "oA"))

  v_rcensored(outcomes = otimes, censors = ctimes)
}

compare_km <- function(rcen){
  time <- as_canonical(get_time(rcen))
  evnt <- as_canonical(get_outcome(rcen))

  km <- summary(survfit(Surv(time, evnt) ~ 1))

  res <- cumrisk(rcen, w = product_limit(rcen))

  expect_equal(res$time, km$time)
  expect_equal(res$estimate, 1 - km$surv)
}

gen_rcens <- function(n){
  gen.with(
    list(
      tm = gen.sample(1:10000, n),
      st = gen.sample(0:1, n, replace = TRUE)
    ),
    create_rcensor
  )
}
