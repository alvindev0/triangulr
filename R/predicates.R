check_all <- function(..., .f) {
  all(sapply(list(...), .f))
}

is_scalar_logical <- function(...) {
  check_all(
    ...,
    .f = function(x)
      length(x) == 1 && is.logical(x)
  )
}

is_numeric <- function(...) {
  check_all(..., .f = is.numeric)
}

is_scalar <- function(...) {
  check_all(
    ...,
    .f = function(x)
      length(x) == 1
  )
}
