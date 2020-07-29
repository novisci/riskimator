#' An estimator of cumulative risk
#'
#' @param vsl a `list` of `list`s where each element should contain `Y` (the
#'     event time), `C` (the censoring time), and `PrDel` (the estimated
#'     probability of being uncensored)
#' @importFrom monoidimator make_estimator accumsum_collectsum_seq
#' @export
make_cumrisk_estimator <- make_estimator(
  as = list(
    function(vs){ one },
    function(vs){
      C <- vs$C
      Y <- vs$Y
      # Handle two data structures:
      # 1) The one of Y or C that is not missing is min(Y, C)
      # 2) Both Y and C are available
      Del <- `if`(
        is.na(Y) || is.na(C),
        c(TRUE, FALSE)[!is.na(c(Y, C))],
        Y < C)

      PrDel <- vs$PrDel
      function(...){ Del / PrDel }
    }
  ),
  bs = list(
    function(vs){ one }
  ),
  collector = accumsum_collectsum_seq
)
