#' @include dtriang.R
#' @rdname Triangular
#' @importFrom stats runif
#' @export
rtriang <- function(n, min = 0, max = 1, mode = (min+max)/2) {
  p <- tryCatch(stats::runif(n), error = function(c) NaN)
  if (length(p) == 0 || (length(p) == 1 && is.nan(p)) ||
      anyNA(c(min, max, mode)) || !is.numeric(c(min, max, mode))) {
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
  if (any(is.infinite(c(min, max, mode))) || mode < min || mode > max) {
    warning("NaNs produced")
    return(rep(NaN, length(p)))
  }
  if (min == max) {
    return(rep(min, length(p)))
  }

  lower <- p < (mode - min) / (max - min)
  c(
    min + sqrt(p[lower] * (max - min) * (mode - min)),
    max - sqrt((1 - p[!lower]) * (max - min) * (max - mode))
  )
}
