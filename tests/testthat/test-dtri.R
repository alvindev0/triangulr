library(testthat)
library(triangulr)

################################################################################
## Setup

dtri_test <- function(x, a, b, c) {
  p <- 2 * (x - a) / ((b - a) * (c - a))
  p[x == c] <- 2 / (b - a)
  m <- c < x & x <= b
  p[m] <- 2 * (b - x[m]) / ((b - a) * (b - c))
  p[x < a | b < x] <- 0
  p
}

dtri_vec_test <- function(x, a, b, c) {
  p <- 2 * (x - a) / ((b - a) * (c - a))
  i <- x == c
  p[i] <- 2 / (b - a[i])
  i <- c < x & x <= b
  p[i] <- 2 * (b - x[i]) / ((b - a[i]) * (b - c))
  p[x < a | b < x] <- 0
  p
}

################################################################################
## Test cases for the probability density function

test_that("scalar x, scalar params, symmetric", {
  d <- dtri(0.5, min = 0, max = 1, mode = 0.5)
  expect_equal(d, dtri_test(0.5, 0, 1, 0.5))
})

test_that("scalar x, scalar params, non-symmetric", {
  d <- dtri(0.5, min = 0, max = 1, mode = 0.8)
  expect_equal(d, dtri_test(0.5, 0, 1, 0.8))
})

test_that("scalar x, scalar params, non-symmetric, log-scale", {
  d <- dtri(0.5, min = 0, max = 1, mode = 0.8, log = TRUE)
  expect_equal(exp(d), dtri_test(0.5, 0, 1, 0.8))
})

test_that("vector x, scalar params, symmetric", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0.5)
  expect_equal(d, dtri_test(x, 0, 1, 0.5))
})

test_that("vector x, scalar params, non-symmetric", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0.8)
  expect_equal(d, dtri_test(x, 0, 1, 0.8))
})

test_that("vector x, scalar params, non-symmetric, log-scale", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0.8, log = TRUE)
  expect_equal(exp(d), dtri_test(x, 0, 1, 0.8))
})

test_that("vector x, vector params, symmetric", {
  x <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(d, dtri_test(x, a, b, c))
})

test_that("vector x, vector params, non-symmetric", {
  x <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(3, 4, 0.1)
  c <- seq(1.8, 3.8, 0.2)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(d, dtri_test(x, a, b, c))
})

test_that("vector x, vector params recycled, symmetric", {
  x <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(d, dtri_test(x, a, b, c))
})

test_that("vector x, vector params recycled, non-symmetric", {
  x <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1.3, 2.3, 0.1)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(d, dtri_test(x, a, b, c))
})

test_that("Mode at bound, min == mode", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0)
  expect_equal(d, dtri_test(x, 0, 1, 0))
  a <- rep.int(0, 11)
  d <- dtri(x, min = a, max = 1, mode = 0)
  expect_equal(d, dtri_vec_test(x, a, 1, 0))
})

test_that("Mode at bound, max == mode", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 1)
  expect_equal(d, dtri_test(x, 0, 1, 1))
  a <- rep.int(0, 11)
  d <- dtri(x, min = a, max = 1, mode = 1)
  expect_equal(d, dtri_vec_test(x, a, 1, 1))
})

test_that("Zeros produced, x < min", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0.4, max = 1.4, mode = 0.9)
  expect_equal(d, dtri_test(x, 0.4, 1.4, 0.9))
  a <- rep.int(0.4, 11)
  d <- dtri(x, min = a, max = 1.4, mode = 0.9)
  expect_equal(d, dtri_vec_test(x, a, 1.4, 0.9))
})

test_that("Zeros produced, x > max", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = -0.5, max = 0.5, mode = 0)
  expect_equal(d, dtri_test(x, -0.5, 0.5, 0))
  a <- rep.int(-0.5, 11)
  d <- dtri(x, min = a, max = 0.5, mode = 0)
  expect_equal(d, dtri_vec_test(x, a, 0.5, 0))
})

test_that("NaN produced, mode < min", {
  d <- expect_warning(dtri(1, min = 0, max = 2, mode = -1))
  expect_equal(d, NaN)
  x <- c(1, 2, 3)
  a <- c(0, 1, 2)
  b <- c(2, 3, 4)
  c <- c(-1, 2, 3)
  d <- expect_warning(dtri(x, min = a, max = b, mode = c))
  expect_equal(d, c(NaN, 1, 1))
})

test_that("NaN produced, min == mode == max", {
  d <- expect_warning(dtri(3, min = 3, max = 3, mode = 3))
  expect_equal(d, NaN)
  x <- c(1, 2, 3)
  a <- c(0, 1, 3)
  b <- c(2, 3, 3)
  c <- c(1, 2, 3)
  d <- expect_warning(dtri(x, min = a, max = b, mode = c))
  expect_equal(d, c(1, 1, NaN))
})

test_that("NaN produced, min > max", {
  d <- expect_warning(dtri(1, min = 0, max = -1, mode = 1))
  expect_equal(d, NaN)
  x <- c(1, 2, 3)
  a <- c(0, 1, 2)
  b <- c(-1, 3, 4)
  c <- c(1, 2, 3)
  d <- expect_warning(dtri(x, min = a, max = b, mode = c))
  expect_equal(d, c(NaN, 1, 1))
})

test_that("Error, NULL arguments", {
  expect_error(dtri(x = NULL))
  expect_error(dtri(x = 1, min = NULL))
  expect_error(dtri(x = 1, max = NULL))
  expect_error(dtri(x = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(dtri(x = "1"))
  expect_error(dtri(x = 1, min = "0"))
  expect_error(dtri(x = 1, max = "1"))
  expect_error(dtri(x = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(dtri(x = 1, log = "FALSE"))
})

test_that("Error, illegal recycling", {
  expect_error(dtri(seq(0.1, 1, 0.1), min = 0, max = c(1, 2), mode = 0.5))
  expect_error(dtri(seq(0.1, 1, 0.1), min = c(0, 0.1), max = 1, mode = 0.5))
  expect_error(dtri(seq(0.1, 1, 0.1), min = 0, max = 1, mode = c(0.5, 0.6)))
})
