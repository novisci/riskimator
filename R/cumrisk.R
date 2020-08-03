#' Creates an estimator for cumulative risk
#'
#' @description
#' Creates the following estimator:
#' \deqn{
#' \widehat{\Pr}(Y < t) = \frac{1}{n} \sum_{i = 1}^n \frac{\Delta_i I(Y_i < t)} { \widehat{\Pr}(\Delta_i = 1)}
#' }
#' where \eqn{\Delta_i = I(Y_i < C_i)}, \eqn{Y_i} is the time to the event of
#' interest, and \eqn{C_i} is the time to censoring.
#'
#' @param vsl a `list` of `list`s where each element should contain `Y` (the
#'     event time), `C` (the censoring time), and `PrDel` (the estimated
#'     probability of being uncensored)
#' @importFrom monoidimator make_estimator accumsum_collectsum_seq one
#' @keywords internal
make_cumrisk_estimator <- make_estimator(

  as = list(
    function(vs){ one },
    function(vs){
      d <- gather_cumrisk_data(vs)
      function(...){ d[["Del"]] / d[["PrDel"]] }
    }
  ),

  bs = list(
    function(vs){ one }
  ),

  collector = accumsum_collectsum_seq
)

setClassUnion("maybeTime", c("missing", "NULL", "numeric"))

#' Estimate cumulative risk
#'
#' Estimates cumulative risk of right-censored outcomes using the estimator
#' described in Hubbard, Van der Laan, and Robins (2000):
#' \deqn{
#' \widehat{\Pr}(Y < t) = \frac{1}{n} \sum_{i = 1}^n \frac{\Delta_i I(Y_i < t)} { \widehat{\Pr}(\Delta_i = 1)}
#' }
#' where \eqn{\Delta_i = I(Y_i < C_i)}, \eqn{Y_i} is the time to the event of
#' interest, and \eqn{C_i} is the time to censoring.
#'
#' @name cumrisk
#' @param x the object from which to estimate risk. See details.
#' @param w a vector of weights. See
#'      [`cumrisk_weighting`](`cumrisk_weighting`) for functions provided by the
#'      `riskimator` package.
#' @param times a numeric vector of times at which to estimate risk. Defaults to
#'        the times at which events occurred.
#'
#' @details `cumrisk` accepts three types of inputs: a `list`, a `data.frame`,
#' or a [`v_rcensored()`](`stype::v_rcensored()`) vector. `list` inputs should be a
#' transposed `data.frame` with one element per observation, each element
#' being a `list` containing either:
#'
#' * `Y`, `C`, and `PrDel` (the time to outcome, time to censoring,
#'  and probablity of being uncensored)
#' * `Ymin`, `Del`, and `PrDel` (`min(Y, C)`,  an indicator of being uncensored,
#'  and probablity of being uncensored)
#'
#' `list`s are expected to sorted by `min(Y, C)`; this is not validated.
#'
#' The more user-friendly input formats are `data.frame` and `v_rcensored`. With
#' both these types, a vector of `numeric` weights is required. See
#' [`cumrisk_weighting`](`cumrisk_weighting`) for functions provided by the
#' `riskimator` package. `data.frame` inputs must contain either of the variable
#' set described above. An error is thrown if the data is not sorted by
#' `min(Y, C)`. With `v_rcensored` vectors sorting is handled by `cumrisk`.
#'
#' By default, estimates of cumulative risk at event times are returned. The
#' `times` argument can be used to estimate times at other points.
#'
#' @return a `list` of:
#' * `time`: a vector a times at which cumulative risk was evaluated
#' * `estimate`: the estimated cumulative risk for each time
#'
#' @references Hubbard A, Laan M van der, Robins JM. Nonparametric locally
#' efficient estimation of the treatment specific survival distribution with
#' right censored data and covariates in observational studies.
#' In: Halloran M, Berry D, editors. Statistical models in epidemiology,
#' the environment, and clinical trials. New York, NY: Springer; 2000.
#' @export
setGeneric("cumrisk", function(x, w, times = NULL) standardGeneric("cumrisk"))

#' @rdname cumrisk
#' @importFrom purrr flatten_lgl map
#' @importFrom stats stepfun
#' @export
setMethod(
  f = "cumrisk",
  signature = c("list", "missing", "maybeTime"),
  function(x, w, times){

    check_valid_structure(x[[1]])

    assertthat::assert_that(
      missing(times) || is.null(times) || !is.unsorted(times),
      msg = "times argument must be missing, NULL, or a sorted numeric vector."
    )

    # TODO: check events and censors at the same time

    # run estimator
    res <- make_cumrisk_estimator(x)()
    # subset results to times when events occurred
    ev_pos <- events_lgl(x)
    res    <- res[-1][ev_pos]

    evt <- events(x)
    # pick last time
    last_event <- flatten_lgl(map(
      .x = rle(evt)$lengths,
      .f = ~ c(logical(.x - 1L), TRUE)))

    evt <- evt[last_event]
    res <- res[last_event]

    if (missing(times) || is.null(times) ) {
      times <- evt
    } else {
      f <- stats::stepfun(x = evt, y = c(0, res))
      res <- f(times)
    }

    list(
      time     = times,
      estimate = res
    )
  }
)

#' @rdname cumrisk
#' @importFrom purrr transpose
#' @export
setMethod(
  f = "cumrisk",
  signature = c("data.frame", "numeric", "maybeTime"),
  function(x, w, times){
    x[["PrDel"]] <- w
    x <- validate_cumrisk_df(x)
    cumrisk(x = purrr::transpose(x), times = times)
  }
)

#' @rdname cumrisk
#' @importFrom purrr map set_names transpose modify
#' @importFrom stype as_canonical
#' @export
setMethod(
  f = "cumrisk",
  signature = c("v_rcensored", "numeric", "maybeTime"),
  function(x, w, times){

    x <- purrr::map(
      .x = as_canonical(x)[c("time", "outcome")],
      .f = as_canonical)

    x <- purrr::set_names(x, c("Ymin", "Del"))
    x[["PrDel"]] <- w

    ord <- match(sort(x[["Ymin"]]), x[["Ymin"]])

    x <- purrr::modify(x, .f = ~ .x[ord])
    x <- validate_cumrisk_df(x)

    cumrisk(x = purrr::transpose(x), times = times)
  }
)

#' TODO
#'
#' @param df TODO
#' @keywords internal
validate_cumrisk_df <- function(df){

  check_valid_structure(df)

  Ymin <- `if`(
    "Ymin" %in% names(df),
    df[["Ymin"]],
    pmin(df[["Y"]], df[["C"]], na.rm = TRUE)
  )

  assertthat::assert_that(
    !anyNA(Ymin),
    msg = "min(Y, C) must not contain NA values."
  )

  assertthat::assert_that(
    !anyNA(df[["PrDel"]]),
    msg = "PrDel must not contain NA values."
  )

  assertthat::assert_that(
    !is.unsorted(Ymin),
    msg = "Data must be sorted by min(Y, C) must be sorted."
  )

  df
}


# Input list
valid_structures <- list(

  YC = list(
    match  = function(vs) { all(c("Y", "C", "PrDel") %in% names(vs)) },
    gather = function(vs){
      C <- vs$C
      Y <- vs$Y

      # Handle two data structures:
      # 1) The one of Y or C that is not missing is min(Y, C)
      # 2) Both Y and C are available
      Del <- `if`(
        is.na(Y) || is.na(C),
        c(TRUE, FALSE)[!is.na(c(Y, C))],
        Y < C)

      Ymin  <- pmin(Y, C, na.rm = TRUE)
      PrDel <- vs$PrDel

      list(Ymin = Ymin, Del = Del, PrDel = PrDel)
    }
  ),

  Ymin = list(
    match  = function(vs) { all(c("Ymin", "Del", "PrDel") %in% names(vs)) },
    gather = function(vs) {
      list(Ymin = vs$Ymin, Del = vs$Del, PrDel = vs$PrDel)
    }
  )
)

#' Check that a list-like object has valid structure for
#' @importFrom purrr map_lgl
#' @keywords internal
check_valid_structure <- function(l){
  assertthat::validate_that(
    sum(map_lgl(valid_structures, ~ .x$match(l))) == 1,
    msg = "The names in the data must contain Y, C, PrDel OR Ymin, Del, PrDel"
  )

  invisible(NULL)
}

#' Gathers data necessary in cumrisk estimator from a single variable list
#' element.
#' @param vs a named `list` containing data for the `cumrisk` estimator
#' @importFrom purrr map_lgl
#' @keywords internal
gather_cumrisk_data <- function(vs){
  valid_structures[map_lgl(valid_structures, ~ .x$match(vs))][[1]]$gather(vs)
}

#' Pull event times from a variables list
#' @importFrom purrr map_dbl keep
#' @keywords internal
events <- function(vsl){
  map_dbl(
    .x = keep(vsl, ~ gather_cumrisk_data(.x)[["Del"]]),
    .f = ~gather_cumrisk_data(.x)[["Ymin"]])
}

#' Find positions of events in vsl
#' @keywords internal
events_lgl <- function(vsl){
  map_lgl(vsl, ~ gather_cumrisk_data(.x)[["Del"]])
}

