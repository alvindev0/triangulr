#' @include dtriang.R
#' @rdname Triangular
#' @importFrom vctrs vec_recycle
#' @export
qtriang <- function(p, min = 0, max = 1, mode = 0.5, lower_tail = TRUE,
                    log_p = FALSE) {
  if (is.null(p) || is.null(min) || is.null(max) || is.null(mode)) {
    stop("\nArguments p, min, max, and mode must have non-NULL values.")
  }

  if (!is.numeric(p) || !is.numeric(min) || !is.numeric(max) ||
      !is.numeric(mode)) {
    stop("\nArguments p, min, max, and mode must have numeric values.")
  }

  if (!is.logical(lower.tail) || length(lower.tail) > 1L || !is.logical(log.p)
      || length(log.p) > 1L) {
    stop(paste0("\nArguments lower_tail and log_p must have logical values of ",
                "length one each."))
  }

  p_size <- length(p)

  tryCatch({
    min <- vctrs::vec_recycle(min, p_size)
    max <- vctrs::vec_recycle(max, p_size)
    mode <- vctrs::vec_recycle(mode, p_size)
  }, error = function(c) {
    stop(paste0("\nArguments min, max, and mode must be values of length ",
                "equal to length of p or one. \nOnly values of length one are ",
                "recycled."))
  })

  if (p_size == 1L) {
    QTriangC(p, min, max, mode)
  } else {
    QTriangC2(p, min, max, mode)
  }
}
