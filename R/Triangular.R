#' The Triangular Distribution
#'
#' @name Triangular
#'
#' @description These functions provide information about the triangular
#'  distribution on the interval from \code{min} to \code{max} with mode equal
#'  to \code{mode}. \code{ctri} gives the characteristic function, \code{dtri}
#'  gives the density function, \code{estri} gives the expected shortfall,
#'  \code{mgtri} gives the moment generating function, \code{ptri} gives the
#'  distribution function, \code{qtri} gives the quantile function, \code{rtri}
#'  generates random deviates, and \code{vartri} gives the value at risk.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of observations. Must have length of one.
#' @param t Vector of dummy variables.
#' @param min Lower limit of the distribution. Must have \code{min} \eqn{<}
#'  \code{max}.
#' @param max Upper limit of the distribution. Must have \code{max} \eqn{>}
#'  \code{min}.
#' @param mode The mode of the distribution. Must have \code{mode} \eqn{\ge}
#'  \code{min} and \code{mode} \eqn{\le} \code{max}.
#' @param log,log_p logical; if \code{TRUE}, probabilities \code{p} are given as
#'  \code{log(p)}.
#' @param lower_tail logical; if \code{TRUE} (default), probabilities \code{p}
#' are \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.
#'
#' @details
#'  If \code{min}, \code{max}, or \code{mode} are not specified they assume the
#'  default values of \code{0}, \code{1}, and \code{(min + max) / 2}
#'  respectively.
#'
#'  The triangular distribution has density
#'  \deqn{0}{0}
#'  for \eqn{x < min} or \eqn{x > max}
#'  \deqn{f(x) = \frac{2(x - min)}{(max - min)(mode - min)}}{f(x) = 2(x - min) /
#'   (max - min)(mode-min)}
#'  for \eqn{min \le x < mode}, and
#'  \deqn{f(x) = \frac{2(max - x)}{(max - min)(max - mode)}}{E(x) = 2(max - x) /
#'   (max - min)(max - mode)}
#'  for \eqn{mode < x \le max}.
#'
#'  \code{rtri} will not generate either of the extreme values unless
#'  \code{max - min} is small compared to \code{min}, and in particular not for
#'  the default arguments.
#'
#' @return
#'  \code{ctri} gives the characteristic function,
#'  \code{dtri} gives the density function,
#'  \code{estri} gives the expected shortfall,
#'  \code{mgtri} gives the moment generating function,
#'  \code{ptri} gives the distribution function,
#'  \code{qtri} gives the quantile function,
#'  \code{rtri} generates random deviates, and
#'  \code{vartri} gives the value at risk.
#'
#'  The numerical arguments other than \code{n} with values of size one are
#'  recycled to the length of \code{x} for \code{dtri}, the length of
#'  \code{q} for \code{ptri}, the length of \code{p} for \code{qtri}, and
#'  \code{n} for \code{rtri}. This determines the length of the result.
#'
#'  The logical arguments \code{log}, \code{lower_tail}, and \code{log_p} must
#'  be of length one each.
#'
#' @note The characteristics of output from pseudo-random number generators
#'  (such as precision and periodicity) vary widely. See
#'  \code{\link{.Random.seed}} for more information on R's random number
#'  generation algorithms.
#'
#' @seealso
#'  \code{\link{RNG}} about random number generation in \R.
#'
#'  \link{Distributions} for other standard distributions.
#'
#' @useDynLib triangulr
#'
#' @importFrom Rcpp sourceCpp
#' @importFrom vctrs vec_recycle_common
#'
#' @examples
#'
#' x <- c(0, 0.5, 1)
#' # min, max, and mode with lengths equal to the length of x
#' d <- dtri(x, min = c(0, 0, 0), max = c(1, 1, 1), mode = c(0.5, 0.5, 0.5))
#' # min and max will be recycled to the length of x
#' rec_d <- dtri(x, min = 0, max = 1, mode = c(0.5, 0.5, 0.5))
#' all.equal(d, rec_d)
#'
#' n <- 3
#' # min, max, and mode with lengths equal to the length of x
#' set.seed(1)
#' r <- rtri(n, min = c(0, 0, 0), max = c(1, 1, 1), mode = c(0.5, 0.5, 0.5))
#' # min and max will be recycled to the length of n
#' set.seed(1)
#' rec_r <- rtri(n, min = 0, max = 1, mode = c(0.5, 0.5, 0.5))
#' all.equal(r, rec_r)
#'
#' # Log quantiles
#' log_d <- dtri(x, log = TRUE)
#' d <- dtri(x, log = FALSE)
#' all.equal(log(d), log_d)
#'
#' q <- c(0, 0.5, 1)
#' # Upper tail probabilities
#' upper_p <- ptri(q, lower_tail = FALSE)
#' p <- ptri(q, lower_tail = TRUE)
#' all.equal(upper_p, 1 - p)
#'
#' # Log probabilities
#' log_p <- ptri(q, log_p = TRUE)
#' p <- ptri(q, log_p = FALSE)
#' all.equal(upper_p, 1 - p)
#'
#' # The quantile function
#' p <- c(0, 0.5, 1)
#' upper_q <- ptri(1 - p, lower_tail = FALSE)
#' q <- ptri(p, lower_tail = TRUE)
#' all.equal(upper_q, q)
#'
#' log_q <- qtri(log(p), log_p = TRUE)
#' q <- qtri(p, log_p = FALSE)
#' all.equal(log_q, q)
#'
#' t <- c(1, 2, 3)
#' # Moment generating function
#' mgtri(t)
#' # Characteristic function
#' ctri(t)
#'
NULL
