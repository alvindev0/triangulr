
<!-- README.md is generated from README.Rmd. Please edit that file -->

# triangulr <a href="https://irkaal.github.io/triangulr"><img src="man/figures/logo.png" align="right" height="139" /></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/triangulr)](http://cran.r-project.org/web/packages/triangulr)
[![Downloads](http://cranlogs.r-pkg.org/badges/triangulr)](http://cran.rstudio.com/package=triangulr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Introduction

The `triangulr` package provides high-performance triangular
distribution functions which includes density function, distribution
function, quantile function, random variate generator, moment generating
function, characteristic function, and expected shortfall function for
the triangular distribution.

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
x <- c(0.1, 0.5, 0.9)

dtri(x, min = 0, max = 1, mode = 0.5)
#> [1] 0.4 2.0 0.4

dtri(x, min  = 0, max  = rep.int(1, 3), mode = 0.5)
#> [1] 0.4 2.0 0.4
```

Using the distribution function, `ptri`.

``` r
q <- c(0.1, 0.5, 0.9)

1 - ptri(q, lower_tail = FALSE)
#> [1] 0.02 0.50 0.98

ptri(q, lower_tail = TRUE)
#> [1] 0.02 0.50 0.98

ptri(q, log_p = TRUE)
#> [1] -3.91202301 -0.69314718 -0.02020271

log(ptri(q, log_p = FALSE))
#> [1] -3.91202301 -0.69314718 -0.02020271
```

Using the quantile function, `qtri`.

``` r
p <- c(0.1, 0.5, 0.9)

ptri(1 - p, lower_tail = FALSE)
#> [1] 0.02 0.50 0.98

ptri(p, lower_tail = TRUE)
#> [1] 0.02 0.50 0.98

qtri(log(p), log_p = TRUE)
#> [1] 0.2236068 0.5000000 0.7763932

qtri(p, log_p = FALSE)
#> [1] 0.2236068 0.5000000 0.7763932
```

Using the random variate generator, `rtri`.

``` r
set.seed(1)
n <- 3

rtri(n, min = 0, max = 1, mode = 0.5)
#> [1] 0.3643547 0.4313490 0.5378601

rtri(n, min  = 0, max  = rep.int(1, 3), 0.5)
#> [1] 0.7857662 0.3175547 0.7746000
```

Using the moment generating function, `mgtri`.

``` r
t <- c(1, 2, 3)

mgtri(t, min = 0, max = 1, mode = 0.5)
#> [1] 1.683357 2.952492 5.387626

mgtri(t, min = rep.int(0, 3), max = 1, mode = 0.5)
#> [1] 1.683357 2.952492 5.387626
```

Using the characteristic function, `ctri`.

``` r
t <- c(1, 2, 3)

ctri(t, min = 0, max = 1, mode = 0.5)
#> [1] 0.8594513+0.4695204i 0.4967514+0.7736445i 0.0584297+0.8239422i

ctri(t, min = rep.int(0, 3), max = 1, mode = 0.5)
#> [1] 0.8594513+0.4695204i 0.4967514+0.7736445i 0.0584297+0.8239422i
```

Using the expected shortfall function, `estri`.

``` r
p <- c(0.1, 0.5, 0.9)

estri(p, min = 0, max = 1, mode = 0.5)
#> [1] 0.1490712 0.3333333 0.4610079

estri(p, min = rep.int(0, 3), max = 1, mode = 0.5)
#> [1] 0.1490712 0.3333333 0.4610079
```
