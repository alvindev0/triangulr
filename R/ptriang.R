#' @include dtriang.R
#' @rdname Triangular
#' @export
ptriang <- function(q, min = 0, max = 1, mode = (min+max)/2, lower.tail = TRUE,
                    log.p = FALSE) {

  # TODO: The numerical arguments other than n are recycled to the length of the
  # result. Only the first elements of the logical arguments are used.

  if (anyNA(c(q, min, max, mode)) || !is.numeric(c(q, min, max, mode)) ||
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
  if (any(is.infinite(c(min, max, mode))) ||
      mode < min || mode > max || min == max) {
    warning("NaNs produced")
    return(rep(NaN, length(q)))
  }

  zero <- q <= min
  lower <- min < q & q <= mode
  upper <- mode < q & q < max
  one <- max <= q
  q[zero] <- 0
  q[lower] <- (q[lower] - min)^2 / (max - min) / (mode - min)
  q[upper] <- 1 - (max - q[upper])^2 / (max - min) / (max - mode)
  q[one] <- 1

  if (!lower.tail) {
    q <- 1 - q
  }
  if (log.p) log(q) else q
}
