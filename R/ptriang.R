#' @include dtriang.R
#' @rdname Triangular
#' @export
ptriang <- function(q, min = 0, max = 1, mode = (min + max)/2,
                    lower.tail = TRUE, log.p = FALSE) {
  mode <- tryCatch(mode, error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(mode) == "try-error" ||
      is.null(mode) || !is.numeric(mode) ||
      is.null(q) || is.null(min) || is.null(max) ||
      !is.numeric(q) || !is.numeric(min) || !is.numeric(max) ||
      !is.logical(lower.tail) || !is.logical(log.p)) {
    stop("invalid arguments")
  }

  p <- if (length(min) + length(max) + length(mode) == 3L) {
    ptriang_sca(q, min, max, mode)
  } else {
    suppressWarnings(ptriang_vec(q, min, max, mode))
  }

  # If lower.tail or log.p has length > 1, only the first element will be used
  suppressWarnings({
    if (!lower.tail) {
      p <- 1L - p
    }
    if (log.p) {
      p <- log(p, base = exp(1))
    }
  })

  if (anyNA(p)) {
    warning("NaNs produced")
  }
  p
}

ptriang_sca <- function(q, min, max, mode) {
  if (is.infinite(min) || is.infinite(max) || is.infinite(mode) ||
      mode < min || mode > max) {
    return(.Internal(rep.int(NaN, length(q))))
  }
  if (min == max) {
    if (x == min) {
      return(.Internal(rep.int(1L, length(q))))
    } else {
      return(.Internal(rep.int(0L, length(q))))
    }
  }

  p <- (q - min)^2 / ((max - min) * (mode - min))
  u <- mode < q & q < max
  p[u] <- (1 - (max - q)^2 / ((max - min) * (max - mode)))[u]
  p[q <= min] <- 0L
  p[max <= q] <- 1L
  p
}

ptriang_vec <- function(q, min, max, mode) {
  lo <- max(length(q), length(min), length(max), length(mode))
  min <- .Internal(rep_len(min, lo))
  max <- .Internal(rep_len(max, lo))
  mode <- .Internal(rep_len(mode, lo))

  p <- (q - min)^2 / ((max - min) * (mode - min))
  u <- .Internal(which(q > mode))
  p[u] <- (1L - (max - q)^2 / ((max - min) * (max - mode)))[u]

  # TODO: Test this
  p[q >= max | (min == max & x == min)] <- 1L
  p[q <= min | (min == max & x != min)] <- 0L

  p[mode < min | mode > max |
      is.infinite(min) | is.infinite(max) | is.infinite(mode) |
      is.na(p)] <- NaN
  p
}

