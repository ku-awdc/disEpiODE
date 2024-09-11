#' Removes background from ggplots.
#'
#' @param ...Does nothing
#'
#' @return Theme
#' @export
#'
#' @examples
theme_blank_background <- function(...) {
  theme(panel.grid = element_blank(), panel.background = element_blank())
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_margins <- function(...) {
  theme(axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)))
}
