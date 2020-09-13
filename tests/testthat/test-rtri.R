library(testthat)
library(triangulr)

################################################################################
## Test cases for the random deviates generator function

test_that("n = 1, scalar params, symmetric", {
  set.seed(1)
  r <- rtri(1, min = 0, max = 1, mode = 0.5)
  expect_equal(round(r, 7), 0.3643547)
})

test_that("n = 1, scalar params, non-symmetric", {
  set.seed(1)
  r <- rtri(1, min = 0, max = 1, mode = 0.8)
  expect_equal(round(r, 7), 0.4608763)
})

test_that("vector x, scalar params, symmetric", {
  set.seed(1)
  r <- rtri(10, min = 0, max = 1, mode = 0.5)
  expect_equal(round(r, 7), c(0.3643547, 0.4313490, 0.5378601, 0.7857662,
                              0.3175547,    0.7746, 0.8336799, 0.5881735,
                              0.5693691, 0.1757644))
})

test_that("vector x, scalar params, non-symmetric", {
  set.seed(1)
  r <- rtri(10, min = 0, max = 1, mode = 0.8)
  expect_equal(round(r, 7), c(0.4608763, 0.5456181, 0.6769658, 0.8645067,
                              0.4016784, 0.8574445,   0.89481, 0.7270751,
                              0.7094302, 0.2223264))
})

test_that("vector x, vector params, symmetric", {
  set.seed(1)
  a <- seq(0.1, 1, 0.1)
  b <- seq(2.1, 3, 0.1)
  c <- seq(1.1, 2, 0.1)
  r <- rtri(10, min = a, max = b, mode = c)
  expect_equal(round(r, 7), c(0.8287094,  1.062698, 1.3757201, 1.9715325,
                              1.1351093,    2.1492, 2.3673599, 1.9763469,
                              2.0387382, 1.3515289))
})

test_that("vector x, vector params, non-symmetric", {
  set.seed(1)
  a <- seq(0.1,   1, 0.1)
  b <- seq(3.1,   4, 0.1)
  c <- seq(1.9, 3.8, 0.2)
  r <- rtri(10, min = a, max = b, mode = c)
  expect_equal(round(r, 6), c(1.297392, 1.656402, 2.153947, 2.902166, 1.653733,
                              3.138067,  3.38443, 3.026204, 3.115195, 1.707438))
})

test_that("vector x, vector params recycled, symmetric", {
  set.seed(1)
  a <- 0
  b <- seq(2.1, 3, 0.1)
  c <- seq(1.1, 2, 0.1)
  r <- rtri(10, min = a, max = b, mode = c)
  expect_equal(round(r, 6), c(0.783151, 0.991165,  1.30882, 1.930637, 0.869659,
                              2.086009, 2.313507, 1.825441, 1.861832, 0.608866))
})

test_that("vector x, vector params recycled, non-symmetric", {
  set.seed(1)
  a <- 0
  b <- seq(2.1, 3, 0.1)
  c <- seq(1.4, 2.3, 0.1)
  r <- rtri(10, min = a, max = b, mode = c)
  expect_equal(round(r, 6), c(0.883513, 1.108156,  1.45193, 2.007303, 0.952664,
                              2.169964, 2.376637, 1.971165, 2.003434, 0.652936))
})

test_that("Mode at bound, min == mode", {
  set.seed(1)
  r <- rtri(10, min = 0, max = 1, mode = 0)
  expect_equal(round(r, 6), c(0.142975, 0.207614, 0.346435, 0.697028, 0.106514,
                              0.681236, 0.764788, 0.417589, 0.390996, 0.031386))
})

test_that("Mode at bound, max == mode", {
  set.seed(1)
  r <- rtri(10, min = 0, max = 1, mode = 1)
  expect_equal(round(r, 6), c(0.515275,  0.61002, 0.756871, 0.952999,  0.44909,
                              0.947834, 0.971944, 0.812895, 0.793167, 0.248568))
})

test_that("NaN produced, mode < min", {
  set.seed(1)
  a <- c(0, 1, 2)
  b <- c(2, 3, 4)
  c <- c(-1, 2, 3)
  r <- expect_warning(rtri(3, min = a, max = b, mode = c))
  expect_equal(round(r, 6), c(NaN, 1.862698, 3.07572))
})

test_that("NaN produced, min == mode == max", {
  set.seed(1)
  a <- c(0, 1, 3)
  b <- c(2, 3, 3)
  c <- c(1, 2, 3)
  r <- expect_warning(rtri(3, min = a, max = b, mode = c))
  expect_equal(round(r, 6), c(0.728709, 1.862698, NaN))
})

test_that("NaN produced, min > max", {
  set.seed(1)
  a <- c(0, 1, 2)
  b <- c(-1, 3, 4)
  c <- c(1, 2, 3)
  r <- expect_warning(rtri(3, min = a, max = b, mode = c))
  expect_equal(round(r, 6), c(NaN, 1.862698, 3.07572))
})

test_that("Error, Negative argument", {
  expect_error(rtri(n = -1))
})

test_that("Error, NULL arguments", {
  expect_error(rtri(n = NULL))
  expect_error(rtri(n = 1, min = NULL))
  expect_error(rtri(n = 1, max = NULL))
  expect_error(rtri(n = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(rtri(n = "1"))
  expect_error(rtri(n = 1, min = "0"))
  expect_error(rtri(n = 1, max = "1"))
  expect_error(rtri(n = 1, mode = "0.5"))
})

test_that("Error, illegal recycling", {
  expect_error(rtri(n = 10, min = 0, max = c(1, 2), mode = 0.5))
  expect_error(rtri(n = 10, min = c(0, 0.1), max = 1, mode = 0.5))
  expect_error(rtri(n = 10, min = 0, max = 1, mode = c(0.5, 0.6)))
})
