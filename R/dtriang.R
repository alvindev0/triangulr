#' The Triangular Distribution
#'
#' @name Triangular
#'
#' @description These functions provide information about the triangular
#'  distribution on the interval from \code{min} to \code{max} with mode equal
#'  to \code{mode}. \code{dtriang} gives the density function, \code{ptriang}
#'  gives the distribution function, \code{qtriang} gives the quantile function,
#'  and \code{rtriang} generates random deviates.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of observations. Must have length of one.
#' @param min Lower limit of the distribution. Must have \code{min} \eqn{<}
#'  \code{max}. Default value is \code{0}.
#' @param max Upper limit of the distribution. Must have \code{max} \eqn{>}
#'  \code{min}. Default value is \code{1}.
#' @param mode The mode of the distribution. Must have \code{mode} \eqn{\ge}
#'  \code{min} and \code{mode} \eqn{\le} \code{max}. Default value is
#'  \code{0.5}.
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
#'  \code{rtriang} will not generate either of the extreme values unless
#'  \code{max - min} is small compared to \code{min}, and in particular not for
#'  the default arguments.
#'
#' @return
#'  \code{dtriang} gives the density function,
#'  \code{ptriang} gives the distribution function,
#'  \code{qtriang} gives the quantile function, and
#'  \code{rtriang} generates random deviates.
#'
#'  The numerical arguments other than \code{n} with values of size one are
#'  recycled to the length of \code{x} for \code{dtriang}, the length of
#'  \code{q} for \code{ptriang}, the length of \code{p} for \code{qtriang}, and
#'  \code{n} for \code{rtriang}. This determines the length of the result.
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
#' @export
#'
#' @examples
#'  x <- seq(0.1, 1, 0.1)
#'
#'  # min, max, and mode will be recycled to the length of x
#'  recycle_d <- dtriang(x, min = 0, max = 1, mode = 0.5)
#'
#'  # min, max, and mode with lengths equal to the length of x
#'  d <- dtriang(x,
#'               min  = rep.int(0, 10),
#'               max  = rep.int(1, 10),
#'               mode = rep.int(0.5, 10))
#'  all.equal(recycle_d, d)
#'
#'  n <- 10
#'
#'  # min, max, and mode will be recycled to the length of n
#'  set.seed(1)
#'  recycle_r <- rtriang(n, min = 0, max = 1, mode = 0.5)
#'
#'  # min, max, and mode with lengths equal to the length of x
#'  set.seed(1)
#'  r <- rtriang(n,
#'               min  = rep.int(0, 10),
#'               max  = rep.int(1, 10),
#'               mode = rep.int(0.5, 10))
#'  all.equal(recycle_r, r)
#'
#'  # Log quantiles is also supported through the log argument
#'  log_d <- dtriang(x, log = TRUE)
#'  d <- dtriang(x, log = FALSE)
#'  all.equal(log(d), log_d)
#'
#'  q <- x
#'
#'  # Upper tail probabilities is supported through the lower_tail argument
#'  upper_p <- ptriang(q, lower_tail = FALSE)
#'  p <- ptriang(q, lower_tail = TRUE)
#'  all.equal(upper_p, 1 - p)
#'
#'  # Log probabilities is supported through the log_p argument
#'  log_p <- ptriang(q, log_p = TRUE)
#'  p <- ptriang(q, log_p = FALSE)
#'  all.equal(upper_p, 1 - p)
#'
#'  p <- q
#'
#'  # The same applies to the quantile function
#'  upper_q <- ptriang(1 - p, lower_tail = FALSE)
#'  q <- ptriang(p, lower_tail = TRUE)
#'  all.equal(upper_q, q)
#'
#'  log_q <- qtriang(log(p), log_p = TRUE)
#'  q <- qtriang(p, log_p = FALSE)
#'  all.equal(log_q, q)
dtriang <- function(x, min = 0, max = 1, mode = 0.5, log = FALSE) {
  if (is.null(x) || is.null(min) || is.null(max) || is.null(mode)) {
    stop("\nArguments x, min, max, and mode must have non-NULL values.")
  }

  if (!is.numeric(x) || !is.numeric(min) || !is.numeric(max) ||
      !is.numeric(mode)) {
    stop("\nArguments x, min, max, and mode must have numeric values.")
  }

  if (!is.logical(log) || length(log) > 1L) {
    stop(paste0("\nArgument log must have a single logical value."))
  }

  x_size <- length(x)

  tryCatch({
    params <- vctrs::vec_recycle_common(min, max, mode, .size = x_size)
  }, error = function(c) {
    stop(paste0("\nArguments min, max, and mode must be values of length ",
                "equal to length of x or one. \nOnly values of length one are ",
                "recycled."))
  })

  if (x_size == 1L) {
    DTriangC(x, min, max, mode, log)
  } else {
    DTriangC2(x, params[[1]], params[[2]], params[[3]], log)
  }
}
