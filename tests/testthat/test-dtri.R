library(testthat)
library(triangulr)

################################################################################
## Test cases for the probability density function

test_that("scalar x, scalar params, symmetric", {
  d <- dtri(0.5, min = 0, max = 1, mode = 0.5)
  expect_equal(d, 2)
})

test_that("scalar x, scalar params, non-symmetric", {
  d <- dtri(0.5, min = 0, max = 1, mode = 0.8)
  expect_equal(d, 1.25)
})

test_that("scalar x, scalar params, non-symmetric, log-scale", {
  d <- dtri(0.5, min = 0, max = 1, mode = 0.8, log = TRUE)
  expect_equal(exp(d), 1.25)
})

test_that("vector x, scalar params, symmetric", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0.5)
  expect_equal(d, c(0, 0.4, 0.8, 1.2, 1.6, 2.0, 1.6, 1.2, 0.8, 0.4, 0))
})

test_that("vector x, scalar params, non-symmetric", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0.8)
  expect_equal(d, c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 1, 0))
})

test_that("vector x, scalar params, non-symmetric, log-scale", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0.8, log = TRUE)
  expect_equal(exp(d), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 1, 0))
})

test_that("vector x, vector params, symmetric", {
  x <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(d, rep.int(1, 11))
})

test_that("vector x, vector params, non-symmetric", {
  x <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(3, 4, 0.1)
  c <- seq(1.8, 3.8, 0.2)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(round(d, 7), c(0.3703704, 0.3508772, 0.3333333, 0.3174603,
                              0.3030303, 0.2898551, 0.2777778, 0.2666667,
                              0.2564103, 0.2469136, 0.2380952))
})

test_that("vector x, vector params recycled, symmetric", {
  x <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(round(d, 7), c(        1, 0.9523810, 0.9090909, 0.8695652,
                              0.8333333, 0.8000000, 0.7692308, 0.7407407,
                              0.7142857, 0.6896552, 0.6666667))
})

test_that("vector x, vector params recycled, non-symmetric", {
  x <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1.3, 2.3, 0.1)
  d <- dtri(x, min = a, max = b, mode = c)
  expect_equal(round(d, 7), c(0.7692308, 0.7482993, 0.7272727, 0.7065217,
                              0.6862745, 0.6666667, 0.6477733, 0.6296296,
                              0.6122449, 0.5956113, 0.5797101))
})

test_that("Mode at bound, min == mode", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 0)
  expect_equal(d, c(2, 1.8, 1.6, 1.4, 1.2, 1, 0.8, 0.6, 0.4, 0.2, 0))
})

test_that("Mode at bound, max == mode", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0, max = 1, mode = 1)
  expect_equal(d, c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2))
})

test_that("Zeros produced, x < min", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = 0.4, max = 1.4, mode = 0.9)
  expect_equal(d, c(0, 0, 0, 0, 0, 0.4, 0.8, 1.2, 1.6, 2, 1.6))
})

test_that("Zeros produced, x > max", {
  x <- seq(0, 1, 0.1)
  d <- dtri(x, min = -0.5, max = 0.5, mode = 0)
  expect_equal(d, c(2, 1.6, 1.2, 0.8, 0.4, 0, 0, 0, 0, 0, 0))
})

test_that("NaN produced, mode < min", {
  x <- c(1, 2, 3)
  a <- c(0, 1, 2)
  b <- c(2, 3, 4)
  c <- c(-1, 2, 3)
  d <- expect_warning(dtri(x, min = a, max = b, mode = c))
  expect_equal(d, c(NaN, 1, 1))
})

test_that("NaN produced, min == mode == max", {
  x <- c(1, 2, 3)
  a <- c(0, 1, 3)
  b <- c(2, 3, 3)
  c <- c(1, 2, 3)
  d <- expect_warning(dtri(x, min = a, max = b, mode = c))
  expect_equal(d, c(1, 1, NaN))
})

test_that("NaN produced, min > max", {
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
