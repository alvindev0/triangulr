# Moment Generating Function

#' @include Triangular.R
#' @rdname Triangular
#' @export
mgtri <- function(t, min = 0, max = 1, mode = 0.5) {
  if (is.null(t) || is.null(min) || is.null(max) || is.null(mode)) {
    stop("\nArguments t, min, max, and mode must have non-NULL values.")
  }

  if (!is.numeric(t) || !is.numeric(min) || !is.numeric(max) ||
      !is.numeric(mode)) {
    stop("\nArguments t, min, max, and mode must have numeric values.")
  }

  if (length(min) == 1L && length(max) == 1L && length(mode) == 1L) {
    MGTriC(t, min, max, mode)
  } else {
    tryCatch({
      params <- vctrs::vec_recycle_common(min, max, mode, .size = length(t))
    }, error = function(c) {
      stop(paste0("\nArguments min, max, and mode must be values of length ",
                  "equal to length of t or one. \nOnly values of length one ",
                  "are recycled."))
    })
    MGTriC2(t, params[[1L]], params[[2L]], params[[3L]])
  }
}
