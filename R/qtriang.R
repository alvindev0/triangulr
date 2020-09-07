#' @include dtriang.R
#' @rdname Triangular
#' @export
qtriang <- function(p, min = 0, max = 1, mode = (min + max)/2,
                    lower.tail = TRUE, log.p = FALSE) {
  mode <- try(mode, silent = TRUE)
  if (class(mode) == "try-error") {
    stop("invalid arguments")
  }

  if (anyNA(c(p, min, max, mode)) ||
      any(is.null(min), is.null(max), is.null(mode)) ||
      !is.numeric(c(p, min, max, mode)) ||
      !is.logical(c(lower.tail, log.p))) {
    stop("invalid arguments")
  }

  # If log.p or lower.tail has length > 1, only the first element will be used
  suppressWarnings({
    if (log.p) {
      p <- exp(p)
    }
    if (!lower.tail) {
      p <- 1 - p
    }
  })

  len_p <- length(p)
  min <- rep(min, length.out = len_p)
  max <- rep(max, length.out = len_p)
  mode <- rep(mode, length.out = len_p)

  p[p < 0 | p > 1] <- NaN
  p[mode < min | mode > max] <- NaN

  e <- min == max & !is.nan(p)

  c <- p < (mode - min) / (max - min)

  p[is.na(c)] <- NaN

  p[e] <- min[e]

  l <- which(c)
  p[l] <- (min[l] + sqrt(p[l] * (max[l] - min[l]) * (mode[l] - min[l])))

  u <- which(!c)
  p[u] <- (max[u] - sqrt((1 - p[u]) * (max[u] - min[u]) * (max[u] - mode[u])))

  if (anyNA(p)) {
    warning("NaNs produced")
  }
  p
}
