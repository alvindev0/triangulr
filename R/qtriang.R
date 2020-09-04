#' @include dtriang.R
#' @rdname Triangular
#' @export
qtriang <- function(p, min = 0, max = 1, mode = (min+max)/2, lower.tail = TRUE,
                    log.p = FALSE) {

  # TODO: The numerical arguments other than n are recycled to the length of the
  # result. Only the first elements of the logical arguments are used.

  if (anyNA(c(p, min, max, mode)) || !is.numeric(c(p, min, max, mode)) ||
      !is.logical(c(lower.tail, log.p))) {
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
  if (length(lower.tail) > 1) {
    lower.tail <- lower.tail[1]
  }
  if (length(log.p) > 1) {
    log.p <- log.p[1]
  }
  if (any(is.infinite(c(min, max, mode))) || mode < min || mode > max) {
    warning("NaNs produced")
    return(rep(NaN, length(p)))
  }
  if (min == max) {
    return(rep(min, length(p)))
  }

  if (log.p) {
    p <- exp(p)
  }
  if (!lower.tail) {
    p <- 1 - p
  }

  invalid <- p < 0 || p > 1
  lower <- p < (mode - min) / (max - min)
  upper <- !lower
  suppressWarnings({
    p[lower] <- min + sqrt(p[lower] * (max - min) * (mode - min))
    p[upper] <- max - sqrt((1 - p[upper]) * (max - min) * (max - mode))
  })
  if (any(invalid)) {
    warning("NaNs produced")
    p[invalid] <- NaN
  }
  p
}
