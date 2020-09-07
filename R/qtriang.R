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

  q <- suppressWarnings(qtriang_raw(p, min, max, mode))
  if (anyNA(q)) {
    warning("NaNs produced")
  }
  q
}

qtriang_raw <- function(p, min, max, mode) {
  len_q <- max(length(p), length(min), length(max), length(mode))
  # This is sufficient to ensure correct recycling of vector elements
  min <- .Internal(rep_len(min, len_q))

  w <- max - min
  lw <- mode - min

  q <- min + sqrt(p * (max * lw - min * lw))
  c <- .Internal(which(p >= lw / w))
  q[c] <- (max - sqrt((1L - p) * (max * w - mode * w)))[c]

  c <- .Internal(which(min == max))
  q[c] <- min[c]

  q[p < 0L | p > 1L | mode < min | mode > max | is.infinite(q)] <- NaN
  q
}

# qtriang_raw_old <- function(p, min, max, mode) {
#   q <- min + sqrt(p * (max - min) * (mode - min))
#   c <- suppressWarnings(.Internal(which(p >= (mode - min) / (max - min))))
#   q[c] <- (max - sqrt((1L - p) * (max - min) * (max - mode)))[c]
#
#   len_q <- length(q)
#   c <- .Internal(which(min == .Internal(rep_len(max, len_q))))
#   q[c] <- min[c]
#
#   q[p < 0L | p > 1L |
#       mode < min |
#       mode > max |
#       is.infinite(q)] <- NaN
#   q
# }
