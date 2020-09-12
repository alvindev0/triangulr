
<!-- README.md is generated from README.Rmd. Please edit that file -->

# triangulr <a href="https://irkaal.github.io/triangulr"><img src="man/figures/logo.png" align="right" height="139" /></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/triangulr)](http://cran.r-project.org/web/packages/triangulr)
[![Downloads](http://cranlogs.r-pkg.org/badges/triangulr)](http://cran.rstudio.com/package=triangulr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Introduction

The `triangulr` package provides high-performance triangular
distribution functions which includes density function, distribution
function, quantile function, and random deviates generator for the
triangular distribution.

While there are already existing packages that provide these functions,
`triangulr` aims to provide functions that closely follow the interface
of existing distribution functions from the `stats` library and not
sacrifice on performance.

## Installation

<!-- You can install the released version of triangulr from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("triangulr") -->

<!-- ``` -->

<!-- And the development version from [GitHub](https://github.com/) with: -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("irkaal/triangulr")
```

## Example

These are basic examples of using the included functions:

``` r
library(triangulr)
```

Using the density function, `dtri`.

``` r
x <- seq(0.1, 1, 0.1)

# min, max, and mode will be recycled to the length of x
recycle_d <- dtri(x, min = 0, max = 1, mode = 0.5)

# min, max, and mode with lengths equal to the length of x
d <- dtri(x,
          min  = rep.int(0, 10),
          max  = rep.int(1, 10),
          mode = rep.int(0.5, 10))
```

Using the distribution function, `ptri`.

``` r
q <- seq(0.1, 1, 0.1)

# Upper tail probabilities is supported through the lower_tail argument
upper_p <- ptri(q, lower_tail = FALSE)
p <- ptri(q, lower_tail = TRUE)

# Log probabilities is supported through the log_p argument
log_p <- ptri(q, log_p = TRUE)
p <- ptri(q, log_p = FALSE)
```

Using the quantile function, `qtri`.

``` r
# The same applies to the quantile function
upper_q <- ptri(1 - p, lower_tail = FALSE)
q <- ptri(p, lower_tail = TRUE)

log_q <- qtri(log(p), log_p = TRUE)
q <- qtri(p, log_p = FALSE)
```

Using the random deviates generator, `rtri`.

``` r
n <- 10

set.seed(1)
recycle_r <- rtri(n, min = 0, max = 1, mode = 0.5)

set.seed(1)
r <- rtri(n,
          min  = rep.int(0, 10),
          max  = rep.int(1, 10),
          mode = rep.int(0.5, 10))
```
