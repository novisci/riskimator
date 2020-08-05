#' Weighting functions
#'
#' These functions are designed to estimate the probability of being *uncensored*.
#'
#' @name cumrisk-weight
#' @param x the object
#' @param ... additonal arguments passed to weighting functions where needed
#'
#' @section Product-limit:
#'
#' When censoring and outcomes occur at the same time, the `cumrisk` function may
#' give incorrect results when weighting by product-limit survival estimates.
#' To handle this, the `product_limit` function add a small amount (`1e-11`) to
#' any censors at tied times. Uncensoring probabilities for `infinite` times are
#' replaced with the `min` of the estimated probabilities; that is, the
#' probability from the last time is carried forward.
#'
#' @return a `numeric` vector of weights
NULL

#' @describeIn cumrisk-weight used product limit (Kaplan-Meier) estimator
#' @export
setGeneric("product_limit", function(x, ...) standardGeneric("product_limit"))

#' @describeIn cumrisk-weight used product limit (Kaplan-Meier) estimator
#' @importFrom survival survfit
#' @importFrom stype as_Surv get_time
#' @importFrom stats runif
#' @export
setMethod(
  f = "product_limit",
  signature = "v_rcensored",
  definition =  function(x, ...) {
    srv <- stype::as_Surv(x, censor_as_event = TRUE)
    time <- as_canonical(get_time(x))

    # Jitter ties
    if( anyDuplicated(srv[ , 1]) > 0){
      tm <- srv[ , 1]
      dl <- srv[ , 2]
      dp <- duplicated(tm) | duplicated(tm, fromLast = TRUE)
      noise <- 1e-11
      tm[dp] <- tm[dp] + (noise * (1 - dl[dp]))
      srv[ , 1] <- tm
    }

    w <- summary(survival::survfit(srv ~ 1, timefix = FALSE),
                 censored = TRUE,
                 times = time)

    w <- w$surv[match(time, w$time)]
    replace(w, is.infinite(time), min(w, na.rm = TRUE))
  }
)



