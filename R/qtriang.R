#' @include dtriang.R
#' @rdname Triangular
#' @export
qtriang <- function(p, min = 0, max = 1, mode = (min + max)/2,
                    lower.tail = TRUE, log.p = FALSE) {
  mode <- tryCatch(mode, error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(mode) == "try-error" ||
      is.null(mode) || !is.numeric(mode) ||
      is.null(p) || is.null(min) || is.null(max) ||
      !is.numeric(p) || !is.numeric(min) || !is.numeric(max) ||
      !is.logical(lower.tail) || !is.logical(log.p)) {
    stop("invalid arguments")
  }

  # If log.p or lower.tail has length > 1, only the first element will be used
  suppressWarnings({
    if (log.p) {
      p <- exp(p)
    }
    if (!lower.tail) {
      p <- 1L - p
    }
  })

  q <- if (length(min) + length(max) + length(mode) == 3L) {
    qtriang_sca(p, min, max, mode)
  } else {
    suppressWarnings(qtriang_vec(p, min, max, mode))
  }

  if (anyNA(q)) {
    warning("NaNs produced")
  }
  q
}

qtriang_sca <- function(p, min, max, mode) {
  if (is.infinite(min) || is.infinite(max) || is.infinite(mode) ||
      mode < min || mode > max) {
    return(.Internal(rep.int(NaN, length(p))))
  }
  if (min == max) {
    return(.Internal(rep.int(min, length(p))))
  }

  w <- max - min
  lw <- mode - min

  q <- max - sqrt((1L - p) * w * (max - mode))
  l <- p < lw / w
  q[l] <- min + sqrt(p[l] * w * lw)
  q
}

qtriang_vec <- function(p, min, max, mode) {
  len_q <- max(length(p), length(min), length(max), length(mode))
  min <- .Internal(rep_len(min, len_q))
  mode <- .Internal(rep_len(mode, len_q))

  w <- max - min
  lw <- mode - min

  q <- min + sqrt(p * w * lw)
  c <- .Internal(which(p >= lw / w))
  q[c] <- (max - sqrt((1L - p) * w * (max - mode)))[c]

  c <- .Internal(which(min == max))
  q[c] <- min[c]

  q[p < 0L | p > 1L |
      mode < min | mode > max |
      is.infinite(q) | is.na(q)] <- NaN
  q
}
