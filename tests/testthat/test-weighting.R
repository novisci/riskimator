library(survival)
library(dplyr)
library(stype)

test_that("product_limit works", {

    dt <- aml %>% filter(!duplicated(time))

    # Form v_rcensored
    ctimes <- list(
      v_event_time(replace(dt$time, which(dt$status == 1), NA_real_),
                   internal_name = "cA"))

    otimes <- list(
      v_event_time(replace(dt$time, which(dt$status == 0), NA_real_),
                   internal_name = "oA"))

    vrc <- v_rcensored(outcomes = otimes, censors = ctimes)

    m <- summary(survfit(Surv(time, !status) ~ 1, data = dt), censored = TRUE)

    expect_equal(
      rev(sort(product_limit(vrc))),
      m$surv
    )

})
