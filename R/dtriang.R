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
#' @param min lower limit of the distribution. Must be less than max.
#' @param max upper limit of the distribution. Must be greater than min.
#' @param mode mode of the distribution. Must be greater or equal to min and less than or equal to max.
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
#'  \code{rtriang} depends on \code{\link{runif}} and therefore, will not generate
#'  either of the extreme values unless \code{max == min} or \code{max-min} is
#'  small compared to \code{min}, and in particular not for the default
#'  arguments.
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
#'  The numerical arguments other than \code{n} are recycled to the length of
#'  the result. Only the first elements of the logical arguments are used.
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
#' @useDynLib triangulr
#'
#' @importFrom Rcpp sourceCpp
#'
#' @export
#'
#' @examples
#'  # TODO: Add examples
#'
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
