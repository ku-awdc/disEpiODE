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
    length(n) == 1,
    length(landscape_scale) == 1,
    "must split landscape into something" =
      n > 0,
    "both cellsize in either direction must be provided" =
      length(n) == length(landscape_scale)
  )

  grid <- st_make_grid(landscape, cellsize = landscape_scale / n)
  grid <- st_sf(grid)

  matched_area <- isTRUE(all.equal(
    st_area(st_union(grid)),
    st_area(landscape)
  ))

  if (!matched_area) {
    # fix an issue with `st_make_grid` where the boundary gets an extra
    # set of grids sometimes..
    grid <- st_filter(grid,
                      landscape,
                      .predicate = st_within)
  }

  # DEBUG PLOT
  # ggplot() +
  #   geom_sf(data = grid, fill = NA) +
  #   geom_sf(data = world_landscape, fill = NA) +
  #   theme_blank_background()

  stopifnot(
    "grid area must equal landscape area" =
      isTRUE(all.equal(
        st_area(st_union(grid)),
        st_area(landscape))))

  st_geometry(grid) <- "geometry"
  grid
}
