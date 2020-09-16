library(testthat)
library(triangulr)

################################################################################
## Setup

ctri_test <- function(t, a, b, c) {
  numer <- (b - c) * exp(1i * a * t) -
    (b - a) * exp(1i * c * t) +
    (c - a) * exp(1i * b * t)
  denom <- (b - a) * (c - a) * (b - c) * t^2
  -2 * numer / denom
}

################################################################################
## Test cases for the probability density function

test_that("scalar x, scalar params, symmetric", {
  c <- ctri(0.5, min = 0, max = 1, mode = 0.5)
  expect_equal(c, ctri_test(0.5, 0, 1, 0.5))
})

test_that("scalar x, scalar params, non-symmetric", {
  c <- ctri(0.5, min = 0, max = 1, mode = 0.8)
  expect_equal(c, ctri_test(0.5, 0, 1, 0.8))
})

test_that("vector x, scalar params, symmetric", {
  t <- seq(0, 1, 0.1)
  c <- ctri(t, min = 0, max = 1, mode = 0.5)
  expect_equal(c, ctri_test(t, 0, 1, 0.5))
})

test_that("vector x, scalar params, non-symmetric", {
  t <- seq(0, 1, 0.1)
  c <- ctri(t, min = 0, max = 1, mode = 0.8)
  expect_equal(c, ctri_test(t, 0, 1, 0.8))
})

test_that("vector x, vector params, symmetric", {
  t <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  cf <- ctri(t, min = a, max = b, mode = c)
  expect_equal(cf, ctri_test(t, a, b, c))
})

test_that("vector x, vector params, non-symmetric", {
  t <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(3, 4, 0.1)
  c <- seq(1.8, 3.8, 0.2)
  cf <- ctri(t, min = a, max = b, mode = c)
  expect_equal(cf, ctri_test(t, a, b, c))
})

test_that("vector x, vector params recycled, symmetric", {
  t <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  cf <- ctri(t, min = a, max = b, mode = c)
  expect_equal(cf, ctri_test(t, a, b, c))
})

test_that("vector x, vector params recycled, non-symmetric", {
  t <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1.3, 2.3, 0.1)
  cf <- ctri(t, min = a, max = b, mode = c)
  expect_equal(cf, ctri_test(t, a, b, c))
})

test_that("NaN produced, t == 0", {
  t <- seq(0, 1, 0.1)
  c <- expect_warning(ctri(t, min = 0.4, max = 1.4, mode = 0.9))
  expect_equal(c, ctri_test(t, 0.4, 1.4, 0.9))
  a <- rep(0.4, 11)
  c <- expect_warning(ctri(t, min = a, max = 1.4, mode = 0.9))
  expect_equal(c, ctri_test(t, 0.4, 1.4, 0.9))
})

test_that("NaN produced, Mode at bound, min == mode", {
  t <- seq(0, 1, 0.1)
  c <- expect_warning(ctri(t, min = 0, max = 1, mode = 0))
  expect_equal(c, rep.int(NaN + 0i, 11))
  a <- rep.int(0, 11)
  c <- expect_warning(ctri(t, min = a, max = 1, mode = 0))
  expect_equal(c, rep.int(NaN + 0i, 11))
})

test_that("NaN produced, Mode at bound, max == mode", {
  t <- seq(0, 1, 0.1)
  c <- expect_warning(ctri(t, min = 0, max = 1, mode = 1))
  expect_equal(c, rep.int(NaN + 0i, 11))
  a <- rep.int(0, 11)
  c <- expect_warning(ctri(t, min = a, max = 1, mode = 1))
  expect_equal(c, rep.int(NaN + 0i, 11))
})

test_that("NaN produced, mode < min", {
  t <- 1:3
  c <- expect_warning(ctri(t, min = 0, max = 2, mode = -1))
  expect_equal(c, rep.int(NaN + 0i, 3))
  a <- c(0, 0.5, 1)
  c <- expect_warning(ctri(t, min = 0, max = 2, mode = -1))
  expect_equal(c, rep.int(NaN + 0i, 3))
})

test_that("NaN produced, min == mode == max", {
  t <- 1:3
  c <- expect_warning(ctri(t, min = 2, max = 2, mode = 2))
  expect_equal(c, rep.int(NaN + 0i, 3))
  a <- c(0, 0.5, 1)
  c <- expect_warning(ctri(t, min = a, max = 2, mode = 2))
  expect_equal(c, rep.int(NaN + 0i, 3))
})

test_that("NaN produced, min > max", {
  c <- expect_warning(ctri(1, min = 0, max = -1, mode = 1))
  expect_equal(c, NaN + 0i)
  t <- 1:3
  a <- c(0, 0.5, 1)
  c <- expect_warning(ctri(t, min = a, max = -1, mode = 1))
  expect_equal(c, rep.int(NaN + 0i, 3))
})

test_that("Error, NULL arguments", {
  expect_error(ctri(t = NULL))
  expect_error(ctri(t = 1, min = NULL))
  expect_error(ctri(t = 1, max = NULL))
  expect_error(ctri(t = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(ctri(t = "1"))
  expect_error(ctri(t = 1, min = "0"))
  expect_error(ctri(t = 1, max = "1"))
  expect_error(ctri(t = 1, mode = "0.5"))
})

test_that("Error, illegal recycling", {
  expect_error(ctri(seq(0.1, 1, 0.1), min = 0, max = c(1, 2), mode = 0.5))
  expect_error(ctri(seq(0.1, 1, 0.1), min = c(0, 0.1), max = 1, mode = 0.5))
  expect_error(ctri(seq(0.1, 1, 0.1), min = 0, max = 1, mode = c(0.5, 0.6)))
})
