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
create_grid <- function(cellarea, landscape, landscape_scale,
                        celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
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
  celltype <- match.arg(celltype, celltype)
  grid <-
    switch(
      cellsize,
      square = {
        cellsize <- sqrt(c(area, area))
        st_make_grid(landscape, cellsize = cellsize,
                     square = TRUE, what = "polygons")

      },
      hexagon = {
        # Solve side-length for hexagons area
        # area = (3×sqrt(3) / 2) × s**2
        # area × 2 / (3×sqrt(3)) = s**2
        # s = sqrt(2×area / (3×sqrt(3)))
        #
        # Documentation for `st_make_grid` states that:
        #
        # s = cellsize / sqrt(3)
        #
        # Insert and solve for `cellsize`
        #
        # => cellsize / sqrt(3) = sqrt(2×area / (3×sqrt(3)))
        # => cellsize = sqrt(2×area / sqrt(3))


        cellsize <- sqrt((2 * area) / sqrt(3))
        # NOTE: see `sf:::make_hex_grid` for more details
        st_make_grid(landscape, cellsize = cellsize,
                     square = FALSE, what = "polygons")

      },
      hexagon_rot = {
        cellsize <- sqrt((2 * area) / 3 * sqrt(3))
        st_make_grid(landscape, cellsize = cellsize,
                     square = FALSE, flat_topped = TRUE, what = "polygons")
      },
      triangle = {
        # cellsize <- rep(sqrt(area * 2L), 2L)
        cellsize <- sqrt(c(2 * area, 2 * area))
        st_make_grid(landscape, cellsize = cellsize,
                     square = TRUE, what = "polygons") %>%
          st_triangulate_constrained() %>%
          st_collection_extract()

      }
    )

  grid <- grid %>%
    st_intersection(landscape) %>%
    st_sf() %>%
    dplyr::filter(st_area(geometry) > 0)

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
  grid[!st_is(grid, "POLYGON"), ] <- NULL

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


#' Title
#'
#' @param n
#' @param cellsize
#' @param landscape
#'
#' @note Will attempt to converge on `n` first, then `cellsize`
#'
#' @return
#'
#'
#' @examples
create_triangle_grid <- function(n, cellsize, landscape) {
  tri_current <- landscape
  cellsize_current <- unique(st_area(tri_current))
  n_current <- length(tri_current)

  repeat {
    tri_next <- st_triangulate(tri_current)
    cellsize_next <- unique(st_area(tri_next))
    n_next <- length(tri_next)

    if (!missing(n) && n_current <= n && n <= n_next) {
      break;
      # tri_current
    }
    if (!missing(cellsize) && cellsize_current <= cellsize && cellsize <= cellsize_next) {
      break;
      # cellsize_current
    }
    tri_current <- tri_next
  }
  tri_current
}
