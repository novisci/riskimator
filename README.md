# Cumulative Risk

The `riskimator` package provides an estimator of the cumultative distribution of right-censored outcomes:

$$\widehat{\Pr}(Y < t) = \frac{1}{n} \sum_{i = 1}^n \frac{\Delta_i I(Y_i < t)}{ \widehat{\Pr}(\Delta_i = 1)}$$

where $\Delta = I(Y < C)$.

# Install

```
install.packages("riskimator", repos= "http://cran.novisci.com")
```

# Usage

The easiest way to use the package is with the `stype` package's `v_rcensored` type.

```
> ctimes <- list(
     v_event_time(c(5, 6, 10, NA_integer_, 1, NA_integer_, 19),
                  internal_name = "cA"))
> otimes <- list(
     v_event_time(c(2, 6, 11, 12, NA_integer_, NA_integer_, 25),
                  internal_name = "oA"))
> vrc <- v_rcensored(outcomes = otimes, censors = ctimes)
> cumrisk(x = vrc, w =  product_limit(vrc))

$time
[1]  2  6 12

$estimate
[1] 0.1666667 0.3333333 0.5555556
```

See the [`cumrisk` documentation](reference/cumrisk.html) for details.
