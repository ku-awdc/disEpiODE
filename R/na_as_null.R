#' Coerce `NA` into `NULL`
#'
#' Note that [rlang::%||%] treats `NA` as a value, i.e. `NA %||% 42 == NA`.
#'
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
na_as_null <- function(x) {
  if (is.null(x) || is.na(x)) {
    NULL
  } else {
    x
  }
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @rdname na_as_null
#' @examples
null_as_na <- function(x) {
  if (is.null(x)) {
    NA
  } else {
    x
  }
}
