# Probability Density Function

#' @include Triangular.R
#' @rdname Triangular
#' @export
dtri <- function(x, min = 0, max = 1, mode = 0.5, log = FALSE) {
  if (is.null(x) || is.null(min) || is.null(max) || is.null(mode)) {
    stop("\nArguments x, min, max, and mode must have non-NULL values.")
  }

  if (!is.numeric(x) || !is.numeric(min) || !is.numeric(max) ||
      !is.numeric(mode)) {
    stop("\nArguments x, min, max, and mode must have numeric values.")
  }

  if (!is.logical(log) || length(log) > 1L) {
    stop(paste0("\nArgument log must have a single logical value."))
  }

  if (length(min) == 1L && length(max) == 1L && length(mode) == 1L) {
    DTriC(x, min, max, mode, log)
  } else {
    tryCatch({
      params <- vctrs::vec_recycle_common(min, max, mode, .size = length(x))
    }, error = function(c) {
      stop(paste0("\nArguments min, max, and mode must be values of length ",
                  "equal to length of x or one. \nOnly values of length one ",
                  "are recycled."))
    })
    DTriC2(x, params[[1L]], params[[2L]], params[[3L]], log)
  }
}
