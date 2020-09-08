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
dtriang <- function(x, min = 0, max = 1, mode = (min + max)/2, log = FALSE) {
  mode <- tryCatch(mode, error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(mode) == "try-error" ||
      is.null(mode) || !is.numeric(mode) ||
      is.null(x) || is.null(min) || is.null(max) ||
      !is.numeric(x) || !is.numeric(min) || !is.numeric(max) ||
      !is.logical(log)) {
    stop("invalid arguments")
  }

  d <- if (length(min) + length(max) + length(mode) == 3L) {
    dtriang_sca(x, min, max, mode)
  } else {
    suppressWarnings(dtriang_vec(x, min, max, mode))
  }

  # If log has length > 1, only the first element will be used
  suppressWarnings(
    if (log) {
      d <- log(d)
    }
  )

  if (anyNA(d)) {
    warning("NaNs produced")
  }
  d
}

dtriang_sca <- function(x, min, max, mode) {
  if (is.infinite(min) || is.infinite(max) || is.infinite(mode) ||
      mode < min || mode > max || min == max) {
    return(.Internal(rep.int(NaN, length(q))))
  }

  d <- 2L * (x - min) / ((max - min) * (mode - min))
  u <- mode < x & x <= max
  d[u] <- 2L * (max - x[u]) / ((max - min) * (max - mode))
  d[x < min | x > max] <- 0L
  d
}

dtriang_vec <- function(x, min, max, mode) {
  lo <- max(length(x), length(min), length(max), length(mode))
  min <- .Internal(rep_len(min, lo))
  max <- .Internal(rep_len(max, lo))
  mode <- .Internal(rep_len(mode, lo))

  d <- 2L * (x - min) / ((max - min) * (mode - min))
  u <- .Internal(which(mode < x))
  d[u] <- (2L * (max - x) / ((max - min) * (max - mode)))[u]
  d[x < min | x > max] <- 0L
  d[mode < min | mode > max | min == max |
      is.infinite(min) | is.infinite(max) | is.infinite(mode) |
      is.na(p)] <- NaN
  d
}
