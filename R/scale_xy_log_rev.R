#' @title Log-transform and reverse continuous variables
#'
#' @inheritDotParams ggplot2::scale_x_continuous
#'
#' @return
#' @export
#'
#' @examples
#'
#' @aliases scale_y_rev_log
#' @rdname scale_xy_log_rev
scale_x_log_rev <- function(...) {
  ggplot2::scale_x_continuous(..., trans = trans_rev_log)
}

#' @description
#' log-transform variables, and reverse order
#'
#'
#' @inheritDotParams ggplot2::scale_y_continuous
#'
#' @return
#' @export
#'
#' @examples
#' @aliases scale_y_rev_log
#' @rdname scale_xy_log_rev
scale_y_log_rev <- function(...) {
  ggplot2::scale_y_continuous(..., trans = trans_rev_log)
}

trans_rev_log <- scales::trans_new(
  name = "rev_log",
  breaks = scales::log_breaks(base = exp(1)),
  transform = \(x) -log(x),
  inverse = \(x) exp(-x),
  minor_breaks = scales::regular_minor_breaks(reverse = TRUE)
)
#' @inheritDotParams ggplot2::scale_x_continuous
#'
#' @return
#' @export
#'
#' @examples
#'
#' @aliases scale_y_rev_log10
#' @rdname scale_xy_log_rev
scale_x_log10_rev <- function(...) {
  scale_x_continuous(..., trans = trans_rev_log10)
}

#' @description
#' `log10`-transform variables, and reverse order
#'
#'
#' @inheritDotParams ggplot2::scale_y_continuous
#'
#' @return
#' @export
#'
#' @examples
#' @aliases scale_y_rev_log10
#' @rdname scale_xy_log_rev
scale_y_log10_rev <- function(...) {
  ggplot2::scale_y_continuous(..., trans = trans_rev_log10)
}

trans_rev_log10 <- scales::trans_new(
  name = "rev_log10",
  breaks = scales::log_breaks(base = 10),
  transform = \(x) -log10(x),
  inverse = \(x) 10**(-x),
  minor_breaks = scales::regular_minor_breaks(reverse = TRUE)
)

