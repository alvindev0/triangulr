library(testthat)
library(triangulr)

################################################################################
## Setup

# This function approximates the integral value
estri_test <- function(p, min, max, mode) {
  es <- function(x, a, b, c) {
    integrate(qtri, lower = 0, upper = x, min = a, max = b, mode = c)$value / x
  }
  mapply(es, x = p, a = min, b = max, c = mode)
}

expect_equal2 <- function(x, y, r = 5) expect_equal(round(x, r), round(y, r))

################################################################################
## Test cases for the quantile function

test_that("scalar p, scalar params, symmetric", {
  es <- estri(0.5, min = 0, max = 1, mode = 0.5)
  expect_equal2(es, estri_test(0.5, 0, 1, 0.5))
})

test_that("scalar p, scalar params, non-symmetric", {
  es <- estri(0.5, min = 0, max = 1, mode = 0.8)
  expect_equal2(es, estri_test(0.5, 0, 1, 0.8))
})

test_that("scalar p, scalar params, non-symmetric, upper_tail", {
  es <- estri(0.4, min = 0, max = 1, mode = 0.8, lower_tail = FALSE)
  expect_equal2(es, estri_test(1 - 0.4, 0, 1, 0.8))
})

test_that("scalar p, scalar params, non-symmetric, log_p", {
  es <- estri(log(0.5), min = 0, max = 1, mode = 0.8, log_p = TRUE)
  expect_equal2(es, estri_test(0.5, 0, 1, 0.8))
})

test_that("scalar p, scalar params, non-symmetric, upper_tail, log_p", {
  es <- estri(log(0.4), min = 0, max = 1, mode = 0.8, lower_tail = FALSE,
              log_p = TRUE)
  expect_equal2(es, estri_test(1 - 0.4, 0, 1, 0.8))
})

test_that("vector p, scalar params, symmetric", {
  p <- seq(0.1, 1, 0.1)
  es <- estri(p, min = 0, max = 1, mode = 0.5)
  expect_equal2(es, estri_test(p, 0, 1, 0.5))
})

test_that("vector p, scalar params, non-symmetric", {
  p <- seq(0.1, 1, 0.1)
  es <- estri(p, min = 0, max = 1, mode = 0.8)
  expect_equal2(es, estri_test(p, 0, 1, 0.8))
})

test_that("vector p, scalar params, non-symmetric, log_p", {
  p <- seq(0.1, 1, 0.1)
  es <- estri(log(p), min = 0, max = 1, mode = 0.8, log_p = TRUE)
  expect_equal2(es, estri_test(p, 0, 1, 0.8))
})

test_that("vector p, vector params, symmetric", {
  p <- seq(0.1, 1, 0.1)
  a <- seq(0.1, 1, 0.1)
  b <- seq(2.1, 3, 0.1)
  c <- seq(1.1, 2, 0.1)
  es <- estri(p, min = a, max = b, mode = c)
  expect_equal2(es, estri_test(p, a, b, c))
})

test_that("vector p, vector params, non-symmetric", {
  p <- seq(0.1, 1, 0.1)
  a <- seq(0.1, 1, 0.1)
  b <- seq(3.1, 4, 0.1)
  c <- seq(2, 3.8, 0.2)
  es <- estri(p, min = a, max = b, mode = c)
  expect_equal2(es, estri_test(p, a, b, c))
})

test_that("vector p, vector params recycled, symmetric", {
  p <- seq(0.1, 1, 0.1)
  a <- 0
  b <- seq(2.1, 3, 0.1)
  c <- seq(1.1, 2, 0.1)
  es <- estri(p, min = a, max = b, mode = c)
  expect_equal2(es, estri_test(p, a, b, c), r = 4)
})

test_that("vector p, vector params recycled, non-symmetric", {
  p <- seq(0.1, 1, 0.1)
  a <- 0
  b <- seq(2.1, 3, 0.1)
  c <- seq(1.4, 2.3, 0.1)
  es <- estri(p, min = a, max = b, mode = c)
  expect_equal2(es, estri_test(p, a, b, c))
})

test_that("Mode at bound, min == mode", {
  p <- seq(0.1, 1, 0.1)
  es <- estri(p, min = 0, max = 1, mode = 0)
  expect_equal2(es, estri_test(p, 0, 1, 0))
  a <- rep.int(0, 10)
  es <- estri(p, min = a, max = 1, mode = 0)
  expect_equal2(es, estri_test(p, 0, 1, 0))
})

test_that("Mode at bound, max == mode", {
  p <- seq(0.1, 1, 0.1)
  es <- estri(p, min = 0, max = 1, mode = 1)
  expect_equal2(es, estri_test(p, 0, 1, 1))
  a <- rep.int(0, 10)
  es <- estri(p, min = a, max = 1, mode = 1)
  expect_equal2(es, estri_test(p, 0, 1, 1))
})

test_that("NaN produced, p == 0 || p < 0 || p > 1", {
  p <- c(-0.01, 0, 1.01)
  es <- expect_warning(estri(p, min = 0.4, max = 1.4, mode = 0.9))
  expect_equal(es, c(NaN, NaN, NaN))
  a <- c(0.3, 0.4, 0.5)
  es <- expect_warning(estri(p, min = a, max = 1.4, mode = 0.9))
  expect_equal(es, c(NaN, NaN, NaN))
})

test_that("NaN produced, mode < min", {
  es <- expect_warning(estri(0.5, min = 0, max = 2, mode = -1))
  expect_equal(es, NaN)
  p <- c(0.5, 0.6)
  a <- 0
  b <- 2
  c <- c(-1, 1)
  es <- expect_warning(estri(p, min = a, max = b, mode = c))
  expect_equal2(es, c(NaN, 0.7308565))
})

test_that("NaN produced, min == mode == max", {
  es <- expect_warning(estri(0.5, min = 0.5, max = 0.5, mode = 0.5))
  expect_equal2(es, NaN)
  p <- c(0.1, 0.5, 1)
  a <- c(0,   1, 3)
  b <- c(2,   3, 3)
  c <- c(1,   2, 3)
  es <- expect_warning(estri(p, min = a, max = b, mode = c))
  expect_equal2(es, c(0.2981424, 1.6666667, NaN))
})

test_that("NaN produced, min > max", {
  es <- expect_warning(estri(0.5, min = 1, max = 0, mode = 0))
  expect_equal(es, NaN)
  p <- c( 0, 0.5, 1)
  a <- c( 0,   1, 2)
  b <- c(-1,   3, 4)
  c <- c( 1,   2, 3)
  es <- expect_warning(estri(p, min = a, max = b, mode = c))
  expect_equal2(es, c(NaN, 1.666667, 3))
})

test_that("Error, NULL arguments", {
  expect_error(estri(p = NULL))
  expect_error(estri(p = 1, min = NULL))
  expect_error(estri(p = 1, max = NULL))
  expect_error(estri(p = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(estri(p = "1"))
  expect_error(estri(p = 1, min = "0"))
  expect_error(estri(p = 1, max = "1"))
  expect_error(estri(p = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(estri(p = 1, lower_tail = "TRUE"))
  expect_error(estri(p = 1, log_p = "FALSE"))
})

test_that("Error, illegal recycling", {
  p <- seq(0.1, 1, 0.1)
  expect_error(estri(p, min = 0, max = c(1, 2), mode = 0.5))
  expect_error(estri(p, min = c(0, 0.1), max = 1, mode = 0.5))
  expect_error(estri(p, min = 0, max = 1, mode = c(0.5, 0.6)))
})
