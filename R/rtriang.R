#' @include dtriang.R
#' @rdname Triangular
#' @importFrom stats runif
#' @export
rtriang <- function(n, min = 0, max = 1, mode = (min + max)/2) {
  mode <- tryCatch(mode, error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(mode) == "try-error" ||
      is.null(mode) || !is.numeric(mode) ||
      is.null(min) || is.null(max) ||
      !is.numeric(min) || !is.numeric(max)) {
    stop("invalid arguments")
  }

  p <- tryCatch(stats::runif(n, min = 0L, max = 1L), error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(p) == "try-error") {
    stop("invalid arguments")
  }

  if (length(min) + length(max) + length(mode) == 3L) {
    if (is.infinite(min) || is.infinite(max) || is.infinite(mode) ||
        mode < min || mode > max) {
      warning("NaNs produced")
      return(.Internal(rep.int(NaN, length(p))))
    }

    if (min == max) {
      return(.Internal(rep.int(min, length(p))))
    }

    w <- max - min
    lw <- mode - min
    r <- max - sqrt((1L - p) * w * (max - mode))
    l <- p < lw / w
    r[l] <- min + sqrt(p[l] * w * lw)
    r
  } else {
    r <- suppressWarnings(qtriang_raw(p, min, max, mode)[seq_along(p)])
    if (anyNA(r)) {
      warning("NaNs produced")
    }
    r
  }
}
