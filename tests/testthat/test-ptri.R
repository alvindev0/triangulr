library(testthat)
library(triangulr)

################################################################################
## Test cases for the cumulative distribution function

test_that("scalar q, scalar params, symmetric", {
  p <- ptri(0.5, min = 0, max = 1, mode = 0.5)
  expect_equal(p, 0.5)
})

test_that("scalar q, scalar params, non-symmetric", {
  p <- ptri(0.5, min = 0, max = 1, mode = 0.8)
  expect_equal(p, 0.3125)
})

test_that("scalar q, scalar params, non-symmetric, upper_tail", {
  p <- ptri(0.5, min = 0, max = 1, mode = 0.8, lower_tail = FALSE)
  expect_equal(p, 0.6875)
})

test_that("scalar q, scalar params, non-symmetric, log_p", {
  p <- ptri(0.5, min = 0, max = 1, mode = 0.8, log_p = TRUE)
  expect_equal(exp(p), 0.3125)
})

test_that("scalar q, scalar params, non-symmetric, upper_tail, log_p", {
  p <- ptri(0.5, min = 0, max = 1, mode = 0.8, lower_tail = FALSE, log_p = TRUE)
  expect_equal(exp(p), 0.6875)
})

test_that("vector q, scalar params, symmetric", {
  q <- seq(0, 1, 0.1)
  p <- ptri(q, min = 0, max = 1, mode = 0.5)
  expect_equal(p, c(0, 0.02, 0.08, 0.18, 0.32, 0.5, 0.68, 0.82, 0.92, 0.98, 1))
})

test_that("vector q, scalar params, non-symmetric", {
  q <- seq(0, 1, 0.1)
  p <- ptri(q, min = 0, max = 1, mode = 0.8)
  expect_equal(p, c(   0, 0.0125, 0.05, 0.1125, 0.2, 0.3125, 0.45, 0.6125, 0.8,
                    0.95,      1))
})

test_that("vector q, scalar params, non-symmetric, log_p", {
  q <- seq(0, 1, 0.1)
  p <- ptri(q, min = 0, max = 1, mode = 0.8, log_p = TRUE)
  expect_equal(exp(p), c(  0, 0.0125, 0.05, 0.1125, 0.2, 0.3125, 0.45, 0.6125,
                         0.8,   0.95,    1))
})

test_that("vector q, vector params, symmetric", {
  q <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  p <- ptri(q, min = a, max = b, mode = c)
  expect_equal(p, rep.int(0.5, 11))
})

test_that("vector q, vector params, non-symmetric", {
  q <- seq(1, 2, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(3, 4, 0.1)
  c <- seq(1.8, 3.8, 0.2)
  p <- ptri(q, min = a, max = b, mode = c)
  expect_equal(round(p, 7), c(0.1851852, 0.1754386, 0.1666667, 0.1587302,
                              0.1515152, 0.1449275, 0.1388889, 0.1333333,
                              0.1282051, 0.1234568, 0.1190476))
})

test_that("vector q, vector params recycled, symmetric", {
  q <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  p <- ptri(q, min = a, max = b, mode = c)
  expect_equal(round(p, 7), c(      0.5, 0.5238095, 0.5454545, 0.5652174,
                              0.5833333,       0.6, 0.6153846, 0.6296296,
                              0.6428571, 0.6551724, 0.6666667))
})

test_that("vector q, vector params recycled, non-symmetric", {
  q <- seq(1, 2, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1.3, 2.3, 0.1)
  p <- ptri(q, min = a, max = b, mode = c)
  expect_equal(round(p, 7), c(0.3846154, 0.4115646, 0.4363636, 0.4592391,
                              0.4803922,       0.5, 0.5182186, 0.5351852,
                              0.5510204, 0.5658307, 0.5797101))
})

test_that("Mode at bound, min == mode", {
  q <- seq(0, 1, 0.1)
  p <- ptri(q, min = 0, max = 1, mode = 0)
  expect_equal(p, c(0, 0.19, 0.36, 0.51, 0.64, 0.75, 0.84, 0.91, 0.96, 0.99, 1))
})

test_that("Mode at bound, max == mode", {
  q <- seq(0, 1, 0.1)
  p <- ptri(q, min = 0, max = 1, mode = 1)
  expect_equal(p, c(0, 0.01, 0.04, 0.09, 0.16, 0.25, 0.36, 0.49, 0.64, 0.81, 1))
})

test_that("Zeros produced, x < min", {
  q <- seq(0, 1, 0.1)
  p <- ptri(q, min = 0.4, max = 1.4, mode = 0.9)
  expect_equal(p, c(0, 0, 0, 0, 0, 0.02, 0.08, 0.18, 0.32, 0.5, 0.68))
})

test_that("Ones produced, x > max", {
  q <- seq(0, 1, 0.1)
  p <- ptri(q, min = -0.5, max = 0.5, mode = 0)
  expect_equal(p, c(0.5, 0.68, 0.82, 0.92, 0.98, 1, 1, 1, 1, 1, 1))
})

test_that("NaN produced, mode < min", {
  q <- c(1, 2, 3)
  a <- c(0, 1, 2)
  b <- c(2, 3, 4)
  c <- c(-1, 2, 3)
  p <- expect_warning(ptri(q, min = a, max = b, mode = c))
  expect_equal(p, c(NaN, 0.5, 0.5))
})

test_that("NaN produced, min == mode == max", {
  q <- c(1, 2, 3)
  a <- c(0, 1, 3)
  b <- c(2, 3, 3)
  c <- c(1, 2, 3)
  p <- expect_warning(ptri(q, min = a, max = b, mode = c))
  expect_equal(p, c(0.5, 0.5, NaN))
})

test_that("NaN produced, min > max", {
  q <- c(1, 2, 3)
  a <- c(0, 1, 2)
  b <- c(-1, 3, 4)
  c <- c(1, 2, 3)
  p <- expect_warning(ptri(q, min = a, max = b, mode = c))
  expect_equal(p, c(NaN, 0.5, 0.5))
})

test_that("Error, NULL arguments", {
  expect_error(ptri(q = NULL))
  expect_error(ptri(q = 1, min = NULL))
  expect_error(ptri(q = 1, max = NULL))
  expect_error(ptri(q = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(ptri(q = "1"))
  expect_error(ptri(q = 1, min = "0"))
  expect_error(ptri(q = 1, max = "1"))
  expect_error(ptri(q = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(ptri(q = 1, lower_tail = "TRUE"))
  expect_error(ptri(q = 1, log_p = "FALSE"))
})

test_that("Error, illegal recycling", {
  expect_error(ptri(seq(0.1, 1, 0.1), min = 0, max = c(1, 2), mode = 0.5))
  expect_error(ptri(seq(0.1, 1, 0.1), min = c(0, 0.1), max = 1, mode = 0.5))
  expect_error(ptri(seq(0.1, 1, 0.1), min = 0, max = 1, mode = c(0.5, 0.6)))
})
