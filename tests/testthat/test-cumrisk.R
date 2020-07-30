library(survival, verbose = FALSE)
library(dplyr, verbose = FALSE)

test_that("check_validStucture works", {

  tester <- list(Y = 1, C = 1, PrDel = 1)
  expect_silent(check_valid_structure(tester))
})

test_that("cumrisk without tied data", {

  dt <- aml %>% filter(!duplicated(time))
  m <- survfit(Surv(time, !status) ~ 1, data = dt)
  dt <- dt %>%
    arrange(time) %>%
    mutate(
      Y    = if_else(status == 1, time, Inf),
      C    = if_else(status == 0, time, Inf)
    )

  crisk_est <- cumrisk(x = dt, w  = summary(m, times = dt$time)$surv)

  km  <- summary(survfit(Surv(time, status) ~ 1, data = dt))
  expect_equal(crisk_est$time, km$time)
  expect_equal(crisk_est$estimate, 1 - km$surv, tolerance = 1e-15)

  # Test at selected times
  crisk_est <- cumrisk(x = dt, w  = summary(m, times = dt$time)$surv,
                       times = c(5, 10, 15, 50))

  km  <- summary(survfit(Surv(time, status) ~ 1, data = dt),
                 times = c(5, 10, 15, 50))
  expect_equal(crisk_est$time, km$time)
  expect_equal(crisk_est$estimate, 1 - km$surv, tolerance = 1e-15)


  # Test at selected times
  crisk_est <- cumrisk(x = dt, w  = summary(m, times = dt$time)$surv,
                       times = c(0, 100))

  km  <- summary(survfit(Surv(time, status) ~ 1, data = dt),
                 times = c(0, 100))
  expect_equal(crisk_est$time, km$time)
  expect_equal(crisk_est$estimate, 1 - km$surv, tolerance = 1e-15)

  # Error on unsorted time vector
  expect_error(cumrisk(x = dt, w  = summary(m, times = dt$time)$surv,
                       times = c(100, 0)))

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
      C    = if_else(status == 0, time, Inf)
    )

  crisk_est <- cumrisk(x = dt, w  = summary(m1, times = dt$time)$surv)
  km    <- summary(survfit(Surv(time, status) ~ 1, data = aml))

  expect_equal(crisk_est$time, km$time)
  expect_equal(crisk_est$estimate, 1 - km$surv, tolerance = 1e-15)
})

test_that("Another survival example", {

  m <- survfit(Surv(stop, !(event == "death")) ~1,  data=mgus1, subset=(start==0))
  dt <- mgus1 %>%
    filter(start == 0) %>%
    arrange(stop) %>%
    mutate(
      Y    = if_else(event == "death", stop, Inf),
      C    = if_else(event != "death", stop, Inf),
      time = pmin(Y, C, na.rm = TRUE)
    )

  crisk_est <- cumrisk(x = dt, w  = summary(m, times = dt$time)$surv)

  km <- survfit(Surv(stop, event == "death") ~1,
                data = mgus1, subset=(start==0))

  km <- summary(km)
  expect_equal(crisk_est$time, km$time)
  expect_equal(crisk_est$estimate, 1 - km$surv, tolerance = 1e-15)
})

