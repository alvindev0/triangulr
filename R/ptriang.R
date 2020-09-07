#' @include dtriang.R
#' @rdname Triangular
#' @export
ptriang <- function(q, min = 0, max = 1, mode = (min + max)/2,
                    lower.tail = TRUE, log.p = FALSE) {
  mode <- tryCatch(mode, error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(mode) == "try-error" ||
      anyNA(mode) || is.null(mode) || !is.numeric(mode) ||
      anyNA(q) || anyNA(min) || anyNA(max) ||
      is.null(q) || is.null(min) || is.null(max) ||
      !is.numeric(q) || !is.numeric(min) || !is.numeric(max) ||
      !is.logical(lower.tail) || !is.logical(log.p)) {
    stop("invalid arguments")
  }

  len <- max(length(q), length(min), length(max), length(mode))
  q <- rep(q, length.out = len)
  min <- rep(min, length.out = len)
  max <- rep(max, length.out = len)
  mode <- rep(mode, length.out = len)

  q[is.infinite(min) | is.infinite(max) | is.infinite(mode)] <- NaN
  q[mode < min | mode > max] <- NaN
  q[min == max] <- NaN

  z <- which(q <= min)
  l <- which(min < q & q <= mode)
  u <- which(mode < q & q < max)
  o <- which(max <= q)

  q[z] <- 0
  q[l] <- (q[l] - min[l])^2 / (max[l] - min[l]) / (mode[l] - min[l])
  q[u] <- 1 - (max[u] - q[u])^2 / (max[u] - min[u]) / (max[u] - mode[u])
  q[o] <- 1

  # If lower.tail or log.p has length > 1, only the first element will be used
  suppressWarnings({
    if (!lower.tail) {
      q <- 1 - q
    }
    if (log.p) {
      q <- log(q)
    }
  })


  if (anyNA(q)) {
    warning("NaNs produced")
  }
  q
}
