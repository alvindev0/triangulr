#' @include dtriang.R
#' @rdname Triangular
#' @export
rtriang <- function(n, min = 0, max = 1, mode = 0.5) {
  if (is.null(n) || is.null(min) || is.null(max) || is.null(mode)) {
    stop("\nArguments n, min, max, and mode must have non-NULL values.")
  }

  if (!is.numeric(n) || !is.numeric(min) || !is.numeric(max) || !is.numeric(mode)) {
    stop("\nArguments n, min, max, and mode must have numeric values.")
  }

  tryCatch({
    min <- vctrs::vec_recycle(min, n)
    max <- vctrs::vec_recycle(max, n)
    mode <- vctrs::vec_recycle(mode, n)
  }, error = function(c) {
    stop(paste0("\nArguments min, max, and mode must be values of length n or ",
                "one. \nOnly values of length one are recycled."))
  })

  if (n == 1L) {
    RTriangC(n, min, max, mode)
  } else {
    RTriangC2(n, min, max, mode)
  }
}
