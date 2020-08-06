library(survival)
library(dplyr)
library(hedgehog)
library(stype)

# Create a v_rcensored vector from a list of times and states
create_rcensor <- function(l){
  ctimes <- list(
    v_event_time(replace(l$tm, which(l$st == 1), NA_real_),
                 internal_name = "cA"))

  otimes <- list(
    v_event_time(replace(l$tm, which(l$st == 0), NA_real_),
                 internal_name = "oA"))

  v_rcensored(outcomes = otimes, censors = ctimes, end_time = l$et)
}

# Generate v_rcensored vectors for testing
gen_rcens <- function(n){
  gen.with(
    g = list(
      tm = gen.sample(0:10000, n, replace = TRUE),
      st = gen.sample(0:1,     n, replace = TRUE),
      et = gen.sample(c(0:10000, Inf), 1, replace = TRUE)
    ),
    m = create_rcensor
  )
}
