#' Title
#'
#' @param scale Width and height
#'
#' @return
#' @rdname create_landscape
#' @export
#'
#' @examples
create_landscape <- function(scale) {

  landscape <- st_polygon(
    x = list(
      scale *
        rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))
    )
  )
  #'
  #

  landscape_scales <- get_landscape_scale(landscape)

  structure(
    list(
      landscape = landscape,
      width = landscape_scales[1],
      height = landscape_scales[2]
    )
  )
}


#' @rdname create_landscape
#' @param landscape
#'
#' @return Named vector with `width` and `height`
#' @noRd
#' @examples
get_landscape_scale <- function(landscape) {

  landscape_bbox <- st_bbox(st_union(landscape))
  landscape_width <- landscape_bbox[c(1, 3)] %>% diff()
  landscape_height <- landscape_bbox[c(1, 3) + 1] %>% diff()

  c(
    width = landscape_width %>% unname(),
    height = landscape_height %>% unname()
  )
}
