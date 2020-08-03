# Cumulative Risk

The `riskimator` package provides an estimator of the cumultative distribution of right-censored outcomes:

$$\widehat{\Pr}(Y < t) = \frac{1}{n} \sum_{i = 1}^n \frac{\Delta_i I(Y_i < t)}{ \widehat{\Pr}(\Delta_i = 1)}$$

where $\Delta = I(Y < C)$.

# Install

```r
install.packages("riskimator", repos="http://cran.novisci.com")
```

# Usage

TODO

