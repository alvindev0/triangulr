library(testthat)
library(triangulr)

################################################################################
## Test cases for the quantile function

test_that("scalar p, scalar params, symmetric", {
  q <- qtri(0.5, min = 0, max = 1, mode = 0.5)
  expect_equal(q, 0.5)
})

test_that("scalar p, scalar params, non-symmetric", {
  q <- qtri(0.5, min = 0, max = 1, mode = 0.8)
  expect_equal(round(q, 7), 0.6324555)
})

test_that("scalar p, scalar params, non-symmetric, upper_tail", {
  q <- qtri(0.5, min = 0, max = 1, mode = 0.8, lower_tail = FALSE)
  expect_equal(round(q, 7), 0.6324555)
})

test_that("scalar p, scalar params, non-symmetric, log_p", {
  q <- qtri(log(0.5), min = 0, max = 1, mode = 0.8, log_p = TRUE)
  expect_equal(round(q, 7), 0.6324555)
})

test_that("scalar p, scalar params, non-symmetric, upper_tail, log_p", {
  q <- qtri(log(0.5), min = 0, max = 1, mode = 0.8, lower_tail = FALSE, log_p = TRUE)
  expect_equal(round(q, 7), 0.6324555)
})

test_that("vector p, scalar params, symmetric", {
  p <- seq(0, 1, 0.1)
  q <- qtri(p, min = 0, max = 1, mode = 0.5)
  expect_equal(round(q, 7), c(        0, 0.2236068, 0.3162278, 0.3872983,
                              0.4472136,       0.5, 0.5527864, 0.6127017,
                              0.6837722, 0.7763932,         1))
})

test_that("vector p, scalar params, non-symmetric", {
  p <- seq(0, 1, 0.1)
  q <- qtri(p, min = 0, max = 1, mode = 0.8)
  expect_equal(round(q, 7), c(        0, 0.2828427, 0.4, 0.4898979, 0.5656854,
                              0.6324555, 0.6928203, 0.7483315, 0.8, 0.8585786,
                                      1))
})

test_that("vector p, scalar params, non-symmetric, log_p", {
  p <- seq(0, 1, 0.1)
  q <- qtri(log(p), min = 0, max = 1, mode = 0.8, log_p = TRUE)
  expect_equal(round(q, 7), c(        0, 0.2828427, 0.4, 0.4898979, 0.5656854,
                                      0.6324555, 0.6928203, 0.7483315, 0.8, 0.8585786,
                                      1))
})

test_that("vector p, vector params, symmetric", {
  p <- seq(0, 1, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  q <- qtri(p, min = a, max = b, mode = c)
  expect_equal(q, c(        0, 0.5472136, 0.8324555, 1.0745967, 1.2944272, 1.5,
                    1.7055728, 1.9254033, 2.1675445, 2.4527864, 3.0000000))
})

test_that("vector p, vector params, non-symmetric", {
  p <- seq(0, 1, 0.1)
  a <- seq(0, 1, 0.1)
  b <- seq(3, 4, 0.1)
  c <- seq(1.8, 3.8, 0.2)
  q <- qtri(p, min = a, max = b, mode = c)
  expect_equal(round(q, 7), c(        0, 0.8549834, 1.2954451, 1.6747727,
                              2.0248077, 2.3574176, 2.6784610, 2.9912878,
                              3.2979992,       3.6,         4))
})

test_that("vector p, vector params recycled, symmetric", {
  p <- seq(0, 1, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1, 2, 0.1)
  q <- qtri(p, min = a, max = b, mode = c)
  expect_equal(round(q, 7), c(        0, 0.4806246, 0.7266361, 0.9471008,
                              1.1593101, 1.3693064, 1.5798734,       1.8,
                              2.0516685, 2.3614835,         3))
})

test_that("vector p, vector params recycled, non-symmetric", {
  p <- seq(0, 1, 0.1)
  a <- 0
  b <- seq(2, 3, 0.1)
  c <- seq(1.3, 2.3, 0.1)
  q <- qtri(p, min = a, max = b, mode = c)
  expect_equal(round(q, 7), c(        0, 0.5422177, 0.8124038, 1.0507140,
                              1.2774976,       1.5, 1.7216271, 1.9442222,
                              2.1739010, 2.4494448,         3))
})

test_that("Mode at bound, min == mode", {
  p <- seq(0, 1, 0.1)
  q <- qtri(p, min = 0, max = 1, mode = 0)
  expect_equal(round(q, 7), c(        0, 0.0513167, 0.1055728,   0.16334,
                              0.2254033, 0.2928932, 0.3675445, 0.4522774,
                              0.5527864, 0.6837722,         1))
})

test_that("Mode at bound, max == mode", {
  p <- seq(0, 1, 0.1)
  q <- qtri(p, min = 0, max = 1, mode = 1)
  expect_equal(round(q, 7), c(        0, 0.3162278, 0.4472136, 0.5477226,
                              0.6324555, 0.7071068, 0.7745967,   0.83666,
                              0.8944272, 0.9486833,         1))
})

test_that("NaN produced, p < 0 || p > 1", {
  p <- seq(-0.5, 0.5, 0.1)
  q <- expect_warning(qtri(p, min = 0.4, max = 1.4, mode = 0.9))
  expect_equal(round(q, 7), c(NaN,       NaN,       NaN,       NaN,       NaN,
                              0.4, 0.6236068, 0.7162278, 0.7872983, 0.8472136,
                              0.9))
})

test_that("NaN produced, mode < min", {
  p <- c( 0, 0.5, 1)
  a <- c( 0,   1, 2)
  b <- c( 2,   3, 4)
  c <- c(-1,   2, 3)
  q <- expect_warning(qtri(p, min = a, max = b, mode = c))
  expect_equal(q, c(NaN, 2, 4))
})

test_that("NaN produced, min == mode == max", {
  p <- c(0, 0.5, 1)
  a <- c(0,   1, 3)
  b <- c(2,   3, 3)
  c <- c(1,   2, 3)
  q <- expect_warning(qtri(p, min = a, max = b, mode = c))
  expect_equal(q, c(0, 2, NaN))
})

test_that("NaN produced, min > max", {
  q <- c( 0, 0.5, 1)
  a <- c( 0,   1, 2)
  b <- c(-1,   3, 4)
  c <- c( 1,   2, 3)
  q <- expect_warning(qtri(q, min = a, max = b, mode = c))
  expect_equal(q, c(NaN, 2, 4))
})

test_that("Error, NULL arguments", {
  expect_error(qtri(p = NULL))
  expect_error(qtri(p = 1, min = NULL))
  expect_error(qtri(p = 1, max = NULL))
  expect_error(qtri(p = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(qtri(p = "1"))
  expect_error(qtri(p = 1, min = "0"))
  expect_error(qtri(p = 1, max = "1"))
  expect_error(qtri(p = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(qtri(p = 1, lower_tail = "TRUE"))
  expect_error(qtri(p = 1, log_p = "FALSE"))
})

test_that("Error, illegal recycling", {
  expect_error(qtri(seq(0.1, 1, 0.1), min = 0, max = c(1, 2), mode = 0.5))
  expect_error(qtri(seq(0.1, 1, 0.1), min = c(0, 0.1), max = 1, mode = 0.5))
  expect_error(qtri(seq(0.1, 1, 0.1), min = 0, max = 1, mode = c(0.5, 0.6)))
})
