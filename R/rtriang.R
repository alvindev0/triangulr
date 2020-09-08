#' @include dtriang.R
#' @rdname Triangular
#' @importFrom stats runif
#' @export
rtriang <- function(n, min = 0, max = 1, mode = (min + max)/2) {
  mode <- tryCatch(mode, error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(mode) == "try-error" ||
      is.null(mode) || !is.numeric(mode) ||
      is.null(min) || is.null(max) ||
      !is.numeric(min) || !is.numeric(max)) {
    stop("invalid arguments")
  }

  p <- tryCatch(stats::runif(n, min = 0L, max = 1L), error = function(e) {
    invisible(structure(list(), class = "try-error"))
  })
  if (class(p) == "try-error") {
    stop("invalid arguments")
  }

  r <- if (length(min) + length(max) + length(mode) == 3L) {
    qtriang_sca(p, min, max, mode)
  } else {
    suppressWarnings(qtriang_vec(p, min, max, mode))[seq_along(p)]
  }

  if (anyNA(r)) {
    warning("NaNs produced")
  }
  r
}
