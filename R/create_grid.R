#' Title
#'
#' @param n Number of splits in
#' @param landscape Landscape as `sf` object
#' @param landscape_scale Width / height of the landscape
#' @param offset Aligned with initial cell placed at lower left corner
#' @param square
#'
#' @note In `offset`, `"bottom"` means aligned to bottom;
#' Thus bottom middle.
#'
#' ⬛⬛⬛
#' ⚫⚫⬛
#' ⚫⚫⬛
#'
#' @return
#' @export
#'
#' @examples
create_grid <- function(n, landscape, landscape_scale,
                        offset = c("corner", "middle", "bottom", "left"),
                        square = TRUE) {
  stopifnot(
    length(n) == 1,
    length(landscape_scale) == 1,
    "must split landscape into something" =
      n > 0,
    "both cellsize in either direction must be provided" =
      length(n) == length(landscape_scale)
  )
  cellsize <- if (square) {
    landscape_scale / n
  } else {
    c((landscape_scale * 2) / (sqrt(3) * n))
  }
  offset <- match.arg(offset, choices = offset, several.ok = FALSE)

  offset <- st_bbox(landscape)[c("xmin", "ymin")] -
    switch(offset,
           corner = {
             c(0,0)
           },
           middle = {
             c(cellsize, cellsize) / 2
           },
           bottom = {
             c(cellsize/2, 0)
           },
           left = {
             c(0, cellsize/2)
           }
    )

  grid <- st_make_grid(landscape, cellsize = cellsize,
                       offset = offset, square = square,
                       what = "polygons")
  grid <- st_sf(grid)

  grid <- st_intersection(grid, landscape, dimensions = "polygon")

  # for hexagon, sometimes we have linestring here..
  if (!square) {
    grid <- st_collection_extract(grid, type = "POLYGON", warn = FALSE)
  }

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

  # Amend grid with properties useful elsewhere
  st_geometry(grid) <- "geometry"
  grid$area <- st_area(grid$geometry)

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

  grid
}
