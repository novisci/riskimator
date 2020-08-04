#' Weighting functions
#'
#' These functions are designed to estimate the probability of being *uncensored*.
#'
#' @name cumrisk-weight
#' @param x the object
#' @param ... additonal arguments passed to weighting functions where needed
#' @return a `numeric` vector of weights
NULL

#' @describeIn cumrisk-weight used product limit (Kaplan-Meier) estimator
#' @export
setGeneric("product_limit", function(x, ...) standardGeneric("product_limit"))

#' @describeIn cumrisk-weight used product limit (Kaplan-Meier) estimator
#' @importFrom survival survfit
#' @importFrom stype as_Surv get_time
#' @export
setMethod(
  f = "product_limit",
  signature = "v_rcensored",
  definition =  function(x, ...) {
    w <- summary(
      censored = TRUE,
      survival::survfit(stype::as_Surv(x, censor_as_event = TRUE) ~ 1))

    w$surv[match(stype::get_time(x), w$time)]
  }
)

