library(survival, verbose = FALSE)
library(dplyr, verbose = FALSE)

test_that("cumrisk without tied data", {

  zz <- aml %>% filter(!duplicated(time))
  m <- survfit(Surv(time, !status) ~ 1, data = zz)
  zz <- zz %>%
    arrange(time) %>%
    mutate(
      Y    = if_else(status == 1, time, Inf),
      C    = if_else(status == 0, time, Inf),
      PrDel = summary(m, times = time)$surv
    )

  crisk <- make_cumrisk_estimator(purrr::transpose(zz))

  crisk_est <-
    zz %>%
    mutate(hat = crisk()[-1]) %>%
    filter(status == 1) %>%
    group_by(time) %>%
    filter(row_number() == n()) %>%
    pull(hat)


  km    <- summary(survfit(Surv(time, status) ~ 1, data = zz))
  expect_equal(crisk_est, 1 - km$surv, tolerance = 1e-15)
})




test_that("cumrisk with tied data", {

  # works when censoring and outcome events that occur at the same time
  # are jittered.
  dt <-
    aml %>%
    arrange(time, status) %>%
    group_by(time) %>%
    mutate(
      time_jitter = `if`(sum(status == 1) > 0 && sum(status == 0) > 0,
                         time  + c(0.00000000001, 0),
                         time),
    ) %>%
    ungroup()

  m1 <- survfit(Surv(time_jitter, !status) ~ 1, data = dt, timefix = FALSE)
  dt <- dt %>%
    mutate(
      Y    = if_else(status == 1, time, Inf),
      C    = if_else(status == 0, time, Inf),
      PrDel = summary(m1, times = time)$surv
    )

  crisk1 <- make_cumrisk_estimator(purrr::transpose(dt))
  km    <- summary(survfit(Surv(time, status) ~ 1, data = aml))

  crisk_est1 <-
    dt %>%
    mutate(hat = crisk1()[-1]) %>%
    filter(status == 1) %>%
    group_by(time) %>%
    summarise(hat = last(hat)) %>%
    pull(hat)

  expect_equal(crisk_est1, 1 - km$surv, tolerance = 1e-15)
})

test_that("Another survival example", {

  m <- survfit(Surv(stop, !(event == "death")) ~1,  data=mgus1, subset=(start==0))
  dt <- mgus1 %>%
    filter(start == 0) %>%
    arrange(stop) %>%
    mutate(
      Y    = if_else(event == "death", stop, Inf),
      C    = if_else(event != "death", stop, Inf),
      PrDel = summary(m, times = stop)$surv
    )


  crisk <- make_cumrisk_estimator(purrr::transpose(dt))


  dt1 <- dt %>%
    mutate(hat = crisk()[-1]) %>%
    filter(event == "death") %>%
    group_by(Y) %>%
    summarize(hat = max(hat))
  crisk_est <- pull(dt1, hat)


  km <- survfit(Surv(stop, event == "death") ~1,
                data = mgus1, subset=(start==0))

  km <- summary(km, times = dt1$Y)
  expect_equal(crisk_est, 1 - km$surv)


})


