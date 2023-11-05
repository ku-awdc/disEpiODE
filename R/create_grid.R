#' Title
#'
#' @param landscape Landscape as `sf` object
#' @param cellarea Positive number that is less than or equal to `st_area(landscape)`.
#' @param celltype Cell shapes `c("square", "hexagon", "hexagon_rot", "triangle")`
#' @param offset Aligned with initial cell placed at lower left corner
#' @param n Integer specifying how many cells to place horizontally and vertically.
#' @param middle Offset the grid so equal sized cells are in the middle.
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
create_grid <- function(landscape,
                        cellarea = NULL,
                        n = NULL,
                        middle = FALSE,
                        celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
                        offset = c("corner", "middle", "bottom", "left")) {
  stopifnot(
    "not implemented" = missing(offset),
    "either provide `n` or `cellarea`, not both" =
      xor(is.null(n), is.null(cellarea))
  )

  if (!is.null(cellarea)) {
    # provided cellarea
    stopifnot(
      "`cellarea must be positive number" = cellarea > 0,
      "`cellarea` must not exceed provided `landscape` in size" =
        cellarea <= sum(st_area(landscape)),
      "`cellarea` must be of size 1 or 2" =
        1 <= length(cellarea) && length(cellarea) <= 2
    )
  } else {
    # !is.null(n)
    # ???
    # Warning: if middle is specified, than don't do anything?
  }

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
  landscape_bbox <- st_bbox(landscape)
  landscape_bbox_dim <- landscape_bbox[c(3, 4)] -
    landscape_bbox[c(1, 2)]
  grid <-
    switch(celltype,
      square = {
        if (!is.null(cellarea)) {
          # cellarea provided
          if (middle) {
            # offset to the middle
            cellsize <- sqrt(c(cellarea, cellarea))

            offset <- landscape_bbox_dim %% cellsize
            offset <- cellsize - offset / 2

            st_make_grid(landscape,
              cellsize = cellsize,
              offset = -offset,
              square = TRUE, what = "polygons"
            )
          } else {
            # don't offset to the "middle"
            cellsize <- sqrt(c(cellarea, cellarea))
            st_make_grid(landscape,
              cellsize = cellsize,
              square = TRUE, what = "polygons"
            )
          }
        } else {
          # !is.null(n)
          st_make_grid(landscape,
            n = n,
            square = TRUE, what = "polygons"
          )
        }
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

        if (!is.null(cellarea)) {
          cellsize <- sqrt((2 * cellarea) / sqrt(3))
          # NOTE: see `sf:::make_hex_grid` for more details
          st_make_grid(landscape,
            cellsize = cellsize,
            square = FALSE, what = "polygons"
          )
        } else {
          # !is.null(n)
          st_make_grid(landscape,
            n = n,
            square = FALSE,
            what = "polygons"
          )
        }
      },
      hexagon_rot = {
        if (!is.null(cellarea)) {
          cellsize <- sqrt((2 * cellarea) / 3 * sqrt(3))
          st_make_grid(landscape,
            cellsize = cellsize,
            square = FALSE, flat_topped = TRUE, what = "polygons"
          )
        } else {
          # !is.null(n)
          st_make_grid(landscape,
            n = n, square = FALSE,
            flat_topped = TRUE, what = "polygons"
          )
        }
      },
      triangle = {
        if (!is.null(cellarea)) {
          # cellsize <- rep(sqrt(cellarea * 2L), 2L)
          cellsize <- sqrt(c(2 * cellarea, 2 * cellarea))
          st_make_grid(landscape,
            cellsize = cellsize,
            square = TRUE, what = "polygons"
          ) %>%
            st_triangulate_constrained() %>%
            st_collection_extract()
        } else {
          # !is.null(n)
          st_make_grid(landscape,
            n = pmax(1, round(n / 2)),
            square = TRUE,
            what = "polygons"
          ) %>%
            st_triangulate_constrained() %>%
            st_collection_extract()
        }
      }
    )

  grid <- grid %>%
    st_intersection(landscape, dimensions = "polygon") %>%
    st_sf() %>%
    # note: for hexagon, sometimes we have LINESTRING here..
    # dplyr::filter(st_area(geometry) > 0)
    identity()

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
      .predicate = st_within
    )
  }

  # Amend grid with properties useful elsewhere
  grid <- st_as_sf(grid)
  st_geometry(grid) <- "geometry"
  grid$area <- st_area(grid$geometry)

  # TODO: grid boundary?
  # grid_is_boundary <-
  #   st_touches(grid, st_boundary(landscape), sparse = TRUE) %>%

  #   lengths() %>%
  #   magrittr::is_greater_than(0) %>%
  #   which() %>%

  #   identity()

  # grid <- magrittr::set_attributes(
  #   grid, append(
  #     attributes(grid),
  #     list(is_boundary = grid_is_boundary)
  #   )
  # )

  # DEBUG PLOT
  # ggplot() +
  #   geom_sf(data = grid, fill = NA) +
  #   geom_sf(data = world_landscape, fill = NA) +
  #   theme_blank_background()

  stopifnot(
    "grid area must equal landscape area" =
      isTRUE(all.equal(
        st_area(st_union(grid)),
        st_area(landscape)
      ))
  )

  grid
}

# CONCLUSION: For now, this doesn't work with triangles
