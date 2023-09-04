#' Title
#'
#' @param n Number of splits in
#' @param landscape Landscape as `sf` object
#' @param landscape_scale Width / height of the landscape
#'
#' @return
#' @export
#'
#' @examples
create_grid <- function(n, landscape, landscape_scale) {
  stopifnot(
    "must split landscape into something" =
      n > 0,
    "both cellsize in either direction must be provided" =
      length(n) == length(landscape_scale)
  )

  grid <- st_make_grid(landscape, cellsize = landscape_scale / n)

  # fix an issue with `st_make_grid` where the boundary gets an extra
  # set of grids sometimes..
  grid <- st_filter(grid %>% st_sf(), world_landscape,
                    .predicate = st_within)


  grid
}
