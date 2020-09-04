#' The Triangular Distribution
#'
#' @name Triangular
#'
#' @description These functions provide information about the triangular
#' distribution on the interval from \code{min} to \code{max} with mode equal to
#' \code{mode}. \code{dtriang} gives the density function, \code{ptriang} gives
#' the distribution function, \code{qtriang} gives the quantile function, and
#' \code{rtriang} generates random deviates.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#'  the length is taken to be the number required.
#' @param min,max lower and upper limits of the distribution. Must be finite.
#' @param mode mode of the distribution. Must be finite.
#' @param log,log.p logical; if \code{TRUE}, probabilities p are given as
#'  \code{log(p)}.
#' @param lower.tail logical; if \code{TRUE} (default), probabilities are
#'  \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.
#'
#' @details
#'  If \code{min}, \code{max}, or \code{mode} are not specified they assume the
#'  default values of \code{0}, \code{1}, and \code{(min+max)/2}
#'  respectively.
#'
#'  The triangular distribution has density
#'  \deqn{0}{0}
#'  for \eqn{x < min} or \eqn{x > max}
#'  \deqn{f(x) = \frac{2(x-min)}{(max-min)(mode-min)}}{f(x) = 2(x-min)/(max-min)
#'   (mode-min)}
#'  for \eqn{min \le x < mode}, and
#'  \deqn{f(x) = \frac{2(max-x)}{(max-min)(max-mode)}}{E(x) = 2(max-x)/(max-min)
#'   (max-mode)}
#'  for \eqn{mode < x \le max}.
#'
#'  TODO: \code{rtriang} will not generate either of the extreme values unless
#'  \code{max == min} or \code{max-min} is small compared to \code{min}, and in
#'  particular not for the default arguments.
#'
#' @return
#'  \code{dtriang} gives the density function,
#'  \code{ptriang} gives the distribution function,
#'  \code{qtriang} gives the quantile function, and
#'  \code{rtriang} generates random deviates.
#'
#'  The length of the result is determined by \code{n} for \code{rtriang}, and
#'  is the maximum of the lengths of the numerical arguments for the other
#'  functions.
#'
#'  TODO:
#'  The numerical arguments other than \code{n} are recycled to the length of
#'  the result.  Only the first elements of the logical arguments are used.
#'
#' @note The characteristics of output from pseudo-random number generators
#'  (such as precision and periodicity) vary widely. See
#'  \code{\link{.Random.seed}} for more information on R's random number
#'  generation algorithms.
#'
#' @references
#'  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
#'  \emph{The New S Language}. Wadsworth & Brooks/Cole.
#'
#' @seealso
#'  \code{\link{RNG}} about random number generation in \R.
#'
#'  \link{Distributions} for other standard distributions.
#'
#' @export
#'
#' @examples
#'  # TODO: Add examples
#'
dtriang <- function(x, min = 0, max = 1, mode = (min+max)/2, log = FALSE) {

  # TODO: The numerical arguments other than n are recycled to the length of the
  # result. Only the first elements of the logical arguments are used.

  if (anyNA(c(x, min, max, mode)) || !is.numeric(c(x, min, max, mode)) ||
      !is.logical(log)) {
    stop("invalid arguments")
  }
  if (length(min) > 1) {
    min <- min[1]
  }
  if (length(max) > 1) {
    max <- max[1]
  }
  if (length(mode) > 1) {
    mode <- mode[1]
  }
  if (length(log) > 1) {
    log <- log[1]
  }
  if (any(is.infinite(c(min, max, mode))) ||
      mode < min || mode > max || min == max) {
    warning("NaNs produced")
    return(rep(NaN, length(x)))
  }

  zero <- x < min | x > max
  lower <- min <= x & x < mode
  mid <- x == mode
  upper <- mode < x & x <= max
  x[zero] <- 0
  x[lower] <- 2 * (x[lower] - min) / (max - min) / (mode - min)
  x[mid] <- 2 / (max - min)
  x[upper] <- 2 * (max - x[upper]) / (max - min) / (max - mode)

  if (log) log(x) else x
}
