# Random Variates Generator

#' @include Triangular.R
#' @rdname Triangular
#' @importFrom stats runif
#' @export
rtri <- function(n, min = 0, max = 1, mode = 0.5) {
  if (is.null(n) || is.null(min) || is.null(max) || is.null(mode)) {
    stop("\nArguments n, min, max, and mode must have non-NULL values.")
  }

  if (!is.numeric(n) || !is.numeric(min) || !is.numeric(max) ||
      !is.numeric(mode)) {
    stop("\nArguments n, min, max, and mode must have numeric values.")
  }

  if (length(n) != 1L || n < 0L) {
    stop("\nArgument n must have a single positive numeric value.")
  }

  if (length(min) == 1L && length(max) == 1L && length(mode) == 1L) {
    RTriC(n, min, max, mode)
  } else {
    tryCatch({
      n <- as.integer(n)
      params <- vctrs::vec_recycle_common(min, max, mode, .size = n)
    }, error = function(c) {
      stop(paste0("\nArguments min, max, and mode must be values of length n ",
                  "or one. \nOnly values of length one are recycled."))
    })
    RTriC2(n, params[[1L]], params[[2L]], params[[3L]])
  }
}
