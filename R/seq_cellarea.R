#TODO: `seq_cellarea` returns cellareas

#' Generate `cellarea` sequence
#'
#' Geometric sequence of `cellarea` used in [create_grid] based
#' on increasing precision
#'
#' @param min_cellarea,max_cellarea Numeric
#' @param n,precision provide either `n` or `precision`, where `n` entails number of steps between
#' `min_cellarea` and `max_cellarea`, and `precision` is `(1 - precision)^n` sequence
#'
#' @return Sequence of length `n + 1` between `min_cellarea` and `max_cellarea`
#' @export
#'
#' @rdname sequence_cells
#' @examples
seq_cellarea <- function(precision = NULL, n = NULL, min_cellarea, max_cellarea) {
  # TODO: add an option to specify `n` between min/max_cellarea
  # instead of providing `precision`
  stopifnot(
    "`min_cellarea` must be a scalar value" = length(min_cellarea) == 1,
    "`max_cellarea` must be a scalar value" = length(max_cellarea) == 1,
    "either provide `precision` or `n`, but not both or neither"  =
      (is.null(precision) && !is.null(n)) || (!is.null(precision) && is.null(n)),
    "`n` must be greater than 1, and preferably integer" =
      ((!is.null(n)) && n >= 1) || is.null(n), #FIXME: check this
    "`precision` must be between 0 and 1" =
      (!is.null(precision) && 0 < precision && precision <= 1) ||
        is.null(precision),
    "`min_cellarea` must be smaller than or equal to `max_cellarea`" =
      min_cellarea <= max_cellarea
  )
  # max_c = min_c × (1 + precision) ^ n, where n := n_max.
  if (!is.null(precision)) {
    n <- log(max_cellarea / min_cellarea) / log(1 + precision)
    n <- floor(n)
  } else if (!is.null(n)) {
    precision <- (max_cellarea / min_cellarea)**(1 / n) - 1
  } else {
    stop("must provide either `n` or `precision`")
  }
  min_cellarea * (1 + precision) ** (0:n)
}

#TODO: Make a seq_cellarea that takes function(n_min_cells, n_max_cells, by)

#' Title
#'
#' @param n_min_cells
#' @param n_max_cells
#' @inheritDotParams base::seq.default by length.out
#'
#' @return Returns a sequence containing integer values between `n_min_cells`
#' and `n_max_cells`. Note that the length of the returned vector is not guaranteed,
#' as non-integer values will be rounded to nearest integer (odd-rounding), and
#' equal values would be filtered out.
#' @export
#' @rdname sequence_cells
#' @examples
seq_cells <- function(n_min_cells, n_max_cells, ...) {
  seq.default(from = round(n_min_cells),
              to = round(n_max_cells), ...) |>
    round() |>
    unique()
}
