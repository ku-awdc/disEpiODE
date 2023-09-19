#' Generate `cellarea` sequence
#'
#' Geometric sequence of `cellarea` used in [create_grid] based
#' on increasing precision
#'
#' @param min_cellarea,max_cellarea Numeric
#' @param n,precision provide either `n` or `precision`, where `n` entails number of cells between
#' `min_cellarea` and `max_cellarea`, and `precision` is `(1 - precision)^n` sequence
#'
#' @return Sequence of length `n + 1` between `min_cellarea` and `max_cellarea`
#' @export
#'
#' @examples
seq_cellarea <- function(precision = NULL, n = NULL, min_cellarea, max_cellarea) {
  # TODO: add an option to specify `n` between min/max_cellarea
  # instead of providing `precision`
  stopifnot(
    length(min_cellarea) == 1,
    length(max_cellarea) == 1,
    (is.null(precision) && !is.null(n))
    || (!is.null(precision) && is.null(n)),
    ((!is.null(n)) && n >= 1) || TRUE, #check this
    (!is.null(precision) && 0 <= precision && precision <= 1) || TRUE,
    min_cellarea <= max_cellarea
  )
  if (!is.null(precision)) {
    n <- log(min_cellarea / max_cellarea) / log(1 - precision)
    n <- floor(n)
  } else if (!is.null(n)) {
    # min_cellarea * (1 - precision) ** n # n == 0, so this doesn't work
    # instead:
    # max_cellarea = min_cellarea * (1 - precision) ** n
    # max_cellarea / min_cellarea = (1 - precision) ** n
    # (max_cellarea / min_cellarea)**(1/n) = (1 - precision)
    # 1 - (max_cellarea / min_cellarea)**(1/n) = precision
    precision <- 1 - (max_cellarea / min_cellarea)**(1/n)

  } else {
    stop("must provide either `n` or `precision`")
  }
  max_cellarea * (1 - precision) ** (0:n)
}
