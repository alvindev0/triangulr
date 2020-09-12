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

  x_size <- length(x)
  min_size <- length(min)

  tryCatch({
    params <- vctrs::vec_recycle_common(min, max, mode, .size = x_size)
  }, error = function(c) {
    stop(paste0("\nArguments min, max, and mode must be values of length ",
                "equal to length of x or one. \nOnly values of length one are ",
                "recycled."))
  })

  if (min_size == 1L) {
    DTriC(x, min, max, mode, log)
  } else {
    DTriC2(x, params[[1]], params[[2]], params[[3]], log)
  }
}

#' @export
dtri2 <- function(x, min = 0, max = 1, mode = 0.5, log = FALSE) {
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

  min_size <- length(min)

  if (min_size != 1L || length(max) != 1L || length(mode) != 1L) {
    tryCatch({
      params <- vctrs::vec_recycle_common(min, max, mode, .size = length(x))
    }, error = function(c) {
      stop(paste0("\nArguments min, max, and mode must be values of length ",
                  "equal to length of x or one. \nOnly values of length one are ",
                  "recycled."))
    })
  }

  if (min_size == 1L) {
    DTriC(x, min, max, mode, log)
  } else {
    DTriC2(x, params[[1]], params[[2]], params[[3]], log)
  }
}
