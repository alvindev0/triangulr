#' @include dtriang.R
#' @rdname Triangular
#' @importFrom stats runif
#' @export
rtriang <- function(n, min = 0, max = 1, mode = (min + max)/2) {
  mode <- try(mode, silent = TRUE)
  if (class(mode) == "try-error") {
    stop("invalid arguments")
  }

  p <- try(stats::runif(n, min = 0, max = 1), silent = TRUE)
  if (class(p) == "try-error") {
    stop("invalid arguments")
  }

  if (anyNA(c(min, max, mode)) ||
      any(is.null(min), is.null(max), is.null(mode)) ||
      !is.numeric(c(min, max, mode))) {
    stop("invalid arguments")
  }

  if (length(c(min, max, mode)) == 3) {
    if (any(is.infinite(c(min, max, mode)), mode < min, mode > max)) {
      warning("NaNs produced")
      return(rep(NaN, length(p)))
    }

    if (min == max) {
      return(rep(min, length(p)))
    }

    l <- p < (mode - min) / (max - min)
    p[l] <- min + sqrt(p[l] * (max - min) * (mode - min))
    p[!l] <- max - sqrt((1 - p[!l]) * (max - min) * (max - mode))
    p
  } else {
    qtriang(p, min, max, mode, lower.tail = TRUE, log.p = FALSE)
  }
}
