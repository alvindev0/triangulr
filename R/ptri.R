# Cumulative Distribution Function

#' @include Triangular.R
#' @rdname Triangular
#' @export
ptri <- function(q, min = 0, max = 1, mode = 0.5, lower_tail = TRUE,
                    log_p = FALSE) {
  if (is.null(q) || is.null(min) || is.null(max) || is.null(mode)) {
    stop("\nArguments q, min, max, and mode must have non-NULL values.")
  }

  if (!is.numeric(q) || !is.numeric(min) || !is.numeric(max) ||
      !is.numeric(mode)) {
    stop("\nArguments q, min, max, and mode must have numeric values.")
  }

  if (!is.logical(lower_tail) || length(lower_tail) > 1L || !is.logical(log_p)
      || length(log_p) > 1L) {
    stop(paste0("\nArguments lower_tail and log_p must have a single logical ",
                "value each."))
  }

  if (length(min) == 1L && length(max) == 1L && length(mode) == 1L) {
    PTriC(q, min, max, mode, lower_tail, log_p)
  } else {
    tryCatch({
      params <- vctrs::vec_recycle_common(min, max, mode, .size = length(q))
    }, error = function(c) {
      stop(paste0("\nArguments min, max, and mode must be values of length ",
                  "equal to length of q or one. \nOnly values of length one ",
                  "are recycled."))
    })
    PTriC2(q, params[[1L]], params[[2L]], params[[3L]], lower_tail, log_p)
  }
}
