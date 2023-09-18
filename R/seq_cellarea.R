#' Generate `cellarea` sequence
#'
#' Geometric sequence of `cellarea` used in [create_grid] based
#' on increasing precision
#'
#' @param precision Numeric between `0` and `1`
#' @param min_cellarea,max_cellarea
#'
#' @return Sequence between `min_cellarea` and `max_cellarea`
#' @export
#'
#' @examples
seq_cellarea <- function(precision, min_cellarea, max_cellarea) {
  # TODO: add an option to specify `n` between min/max_cellarea
  # instead of providing `precision`
  stopifnot(
    0 <= precision && precision <= 1,
    min_cellarea <= max_cellarea
  )
  max_x <- log(min_cellarea / max_cellarea) / log(1 - precision)
  max_x <- floor(max_x)
  max_cellarea * (1 - precision) ** (0:max_x)
}
