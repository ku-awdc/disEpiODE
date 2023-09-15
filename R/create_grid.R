#' Title
#'
#' @param landscape Landscape as `sf` object
#' @param cellarea Positive number that is less than or equal to `st_area(landscape)`.
#' @param celltype Cell shapes `c("square", "hexagon", "hexagon_rot", "triangle")`
#' @param offset Aligned with initial cell placed at lower left corner
#'
#' @note In `offset`, `"bottom"` means aligned to bottom;
#' Thus bottom middle.
#'
#' ⬛⬛⬛
#'
#' ⚫⚫⬛
#'
#' ⚫⚫⬛
#'
#' @return
#' @export
#'
#' @examples
create_grid <- function(landscape, cellarea,
                        celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
                        offset = c("corner", "middle", "bottom", "left")) {
  stopifnot(
    "not implemented" = missing(offset),
    "`cellarea must be postivie number" = cellarea > 0,
    "`cellarea` must not exceed provided `landscape` in size" =
      cellarea <= sum(st_area(landscape)),
    "`cellarea` must be of size 1 or 2" =
      1 <= length(cellarea) && length(cellarea) <= 2
  )
  celltype <- match.arg(celltype, celltype)

  # offset <- match.arg(offset, choices = offset, several.ok = FALSE)
  # offset <- st_bbox(landscape)[c("xmin", "ymin")] -
  #   switch(offset,
  #          corner = {
  #            c(0,0)
  #          },
  #          middle = {
  #            c(cellsize, cellsize) / 2
  #          },
  #          bottom = {
  #            c(cellsize/2, 0)
  #          },
  #          left = {
  #            c(0, cellsize/2)
  #          }
  #   )

  # FIXME: `offset` is missing here! it is not enough just to uncomment the above
  grid <-
    switch(
      celltype,
      square = {
        cellsize <- sqrt(c(cellarea, cellarea))
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


        cellsize <- sqrt((2 * cellarea) / sqrt(3))
        # NOTE: see `sf:::make_hex_grid` for more details
        st_make_grid(landscape, cellsize = cellsize,
                     square = FALSE, what = "polygons")

      },
      hexagon_rot = {
        cellsize <- sqrt((2 * cellarea) / 3 * sqrt(3))
        st_make_grid(landscape, cellsize = cellsize,
                     square = FALSE, flat_topped = TRUE, what = "polygons")
      },
      triangle = {
        # cellsize <- rep(sqrt(cellarea * 2L), 2L)
        cellsize <- sqrt(c(2 * cellarea, 2 * cellarea))
        st_make_grid(landscape, cellsize = cellsize,
                     square = TRUE, what = "polygons") %>%
          st_triangulate_constrained() %>%
          st_collection_extract()

      }
    )

  grid <- grid %>%
    st_intersection(landscape, dimensions = "polygon") %>%
    st_sf() %>%
    # note: for hexagon, sometimes we have LINESTRING here..
    dplyr::filter(st_area(geometry) > 0)

  matched_area <- isTRUE(all.equal(
    st_area(st_union(grid)),
    st_area(landscape)
  ))

  if (!matched_area) {
    stop("this shouldn't be necessary anymore")
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

