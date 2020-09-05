
<!-- README.md is generated from README.Rmd. Please edit that file -->

# triangulr <a href="https://irkaal.github.io/triangulr"><img src="man/figures/logo.png" align="right" height="139" /></a>

[![R build
status](https://github.com/irkaal/triangulr/workflows/R-CMD-check/badge.svg)](https://github.com/irkaal/triangulr/actions)
[![Codecov test
coverage](https://codecov.io/gh/irkaal/triangulr/branch/master/graph/badge.svg)](https://codecov.io/gh/irkaal/triangulr?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/triangulr)](http://cran.r-project.org/web/packages/triangulr)
[![Downloads](http://cranlogs.r-pkg.org/badges/triangulr)](http://cran.rstudio.com/package=triangulr)

## Introduction

The `triangulr` package provides high-performance triangular
distribution functions which includes density function, distribution
function, quantile function, and random deviates generator for the
triangular distribution.

While there are already existing packages that provides these functions,
`triangulr` aims to provide triangular distribution functions that
closely follow the interface of other distribution functions from the
stats library and not sacrifice on performance.

## Installation

You can install the released version of triangulr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("triangulr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("irkaal/triangulr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(triangulr)
```
