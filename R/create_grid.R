#' Title
#'
#' @param landscape Landscape as `sf` object
#' @param cellarea Positive number that is less than or equal to `st_area(landscape)`.
#' @param celltype Cell shapes `c("square", "hexagon", "hexagon_rot", "triangle")`
#' @param offset Aligned with initial cell placed at lower left corner
#' @param n Integer specifying how many cells to place horizontally and vertically.
#' @param center_as_centroid Ensure that there is a cell, whose centroid is the center of `landscape`.
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
                        patch_area = NULL,
                        n = NULL,
                        middle = FALSE,
                        center_as_centroid = FALSE,
                        grid_type = c("square", "hexagon", "hexagon_rot", "triangle",
                                     "triangulate_constrained"),
                        offset = c("corner", "middle", "bottom", "left"),
                        rotate = 0.0)
  {
  #TODO: Finish "middle" which means all the borders should be the same type
  #
  #TODO: introduce center, which means that the center point of the landscape
  # is contained in a cell, and it is that cell's centroid.

  grid_type <- match.arg(grid_type, several.ok = FALSE)
  celltype <- grid_type
  cellarea <- patch_area

  stopifnot(
    "not implemented" = missing(offset),
    "either provide `n` or `cellarea`, not both" =
      xor(is.null(n), is.null(cellarea)),
    "`middle` doesn't work together with `n`" =
      (middle && is.null(n)) || !middle
  )

  if (!is.null(cellarea)) {
    # provided cellarea
    stopifnot(
      "`cellarea must be positive number" = cellarea > 0,
      "`cellarea` must not exceed provided `landscape` in size" =
        (cellarea <= sum(st_area(landscape))) ||
        isTRUE(all.equal(cellarea, sum(st_area(landscape)))),
      "`cellarea` must be of size 1 or 2" =
        1 <= length(cellarea) && length(cellarea) <= 2
    )
  } else {
    # !is.null(n)
    # ???
    # Warning: if middle is specified, then don't do anything?
  }

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

  ## Rotate the landscape, if necessary:
  landscape_original <- landscape
  if(rotate!=0){
    stopifnot(length(rotate)==1L, !is.na(rotate), rotate >= -360, rotate <= 360)
    ## Convert to radians:
    rotate <- rotate * pi/180
    landscape <- landscape * matrix(c(cos(rotate), -sin(rotate), sin(rotate), cos(rotate)), 2)
  }

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
                 offset <- if (all(offset == 0)) { c(0, 0) } else { cellsize - offset / 2 }

                 st_make_grid(landscape,
                              cellsize = cellsize,
                              offset = -offset,
                              square = TRUE, what = "polygons"
                 ) %>%
                   st_intersection(landscape, dimensions = "polygon")
               } else {
                 # don't offset to the "middle"
                 if (center_as_centroid) {
                   # landscape_center = st_centroid(landscape)
                   dist_center <-
                     landscape %>%
                     st_centroid() %>%
                     st_distance(st_point(c(0,0))) %>%
                     `[`(1,1)
                   cellsize <- sqrt(c(cellarea, cellarea))
                   celldiagonal <- sqrt(sum(cellsize**2))
                   offset <- dist_center %% celldiagonal
                   offset_sign <- offset >= celldiagonal / 2
                   offset <- -offset + celldiagonal / 2
                   offset <- c(1,1) * offset / sqrt(2)
                   offset <- offset_sign * cellsize  + offset

                   st_make_grid(landscape,
                                cellsize = cellsize,
                                offset = -offset,
                                square = TRUE, what = "polygons"
                   ) %>%
                     st_intersection(landscape, dimensions = "polygon")
                 } else {
                   cellsize <- sqrt(c(cellarea, cellarea))
                   st_make_grid(landscape,
                                cellsize = cellsize,
                                square = TRUE, what = "polygons"
                   ) %>%
                     st_intersection(landscape, dimensions = "polygon")
                 }
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
             stopifnot("`middle` not implemented" = !middle)
             if (!is.null(cellarea)) {
              stopifnot("TODO" = !center_as_centroid)
               cellsize <- sqrt((2 * cellarea) / sqrt(3))
               # NOTE: see `sf:::make_hex_grid` for more details
               st_make_grid(landscape,
                            cellsize = cellsize,
                            square = FALSE, what = "polygons"
               ) %>%
                 st_intersection(landscape, dimensions = "polygon")
             } else {
               # !is.null(n)
               st_make_grid(landscape,
                            n = n,
                            square = FALSE,
                            what = "polygons"
               ) %>%
                 st_intersection(landscape, dimensions = "polygon")
             }
           },
           hexagon_rot = {
             stopifnot("TODO" = !center_as_centroid)
             stopifnot("`middle` not implemented" = !middle)
             if (!is.null(cellarea)) {
               cellsize <- sqrt((2 * cellarea) / 3 * sqrt(3))
               st_make_grid(landscape,
                            cellsize = cellsize,
                            square = FALSE, flat_topped = TRUE, what = "polygons"
               ) %>%
                 st_intersection(landscape, dimensions = "polygon")
             } else {
               # !is.null(n)
               st_make_grid(landscape,
                            n = n, square = FALSE,
                            flat_topped = TRUE, what = "polygons"
               )
             }
           },
           triangle = {
             square_grid <-
               if (is.null(n)) {
                 stopifnot("TODO" = !center_as_centroid)
                 # this means that is.null(cellarea) == FALSE
                 cellsize <- sqrt(c(2 * cellarea, 2 * cellarea))

                 square_st_make_grid <- if (middle) {
                   offset <- landscape_bbox_dim %% cellsize
                   offset <- if (all(offset == 0)) { c(0, 0) } else { cellsize - offset / 2 }

                   st_make_grid(landscape,
                                cellsize = cellsize,
                                square = TRUE, what = "polygons",
                                offset = -offset
                   )
                 } else {
                   # middle is false
                   st_make_grid(landscape,
                                cellsize = cellsize,
                                square = TRUE, what = "polygons"
                   )
                 }
                 square_st_make_grid %>%
                   st_intersection(landscape, dimensions = "polygon")
               } else {
                 # note: perfect tessellation, so no need to `st_intersection`...
                 st_make_grid(landscape,
                              n = pmax(1, round(n / 2)),
                              square = TRUE,
                              what = "polygons"
                 )
               }

             top_left <- st_geometry(square_grid) %>%
               lapply(\(square) st_polygon(list(bbox_coords_mat(square)[c(1, 3, 4, 5),]))) %>%
               st_sfc() %>% st_sf() %>%
               mutate(orientation = "bottom_right") %>%
               rowid_to_column("id_square")

             bottom_right <- st_geometry(square_grid) %>%
               lapply(\(square) st_polygon(list(bbox_coords_mat(square)[c(1, 2, 3, 5),]))) %>%
               st_sfc() %>% st_sf() %>%
               mutate(orientation = "bottom_right") %>%
               rowid_to_column("id_square")


             #TODO: Add another orientation here (reformat this code like the above)
             # top_right <- st_polygon(list(xy[c(2,3,4,2), ]))
             # bottom_left <- st_polygon(list(xy[c(1, 2, 4, 5), ]))

             grid <- dplyr::bind_rows(top_left = top_left,
                                      bottom_right = bottom_right)
             # first verticals than horizontal elements
             grid
           },
           triangulate_constrained = {
             # stopifnot("`middle` not implemented" = !middle)

             if (!is.null(cellarea)) {
              stopifnot("TODO" = !center_as_centroid)
               # cellsize <- rep(sqrt(cellarea * 2L), 2L)
               if (middle) {
                 # push cells to the middle
                 cellsize <- sqrt(c(2 * cellarea, 2 * cellarea))
                 offset <- landscape_bbox_dim %% cellsize
                 offset <- if (all(offset == 0)) { c(0, 0) } else { cellsize - offset / 2 }
                 st_make_grid(landscape,
                              cellsize = cellsize,
                              offset = -offset,
                              square = TRUE,
                              what = "polygons"
                 ) %>%
                   st_intersection(landscape, dimensions = "polygon") %>%
                   st_triangulate_constrained() %>%
                   st_collection_extract()
               } else {
                 # don't push to the middle
                 cellsize <- sqrt(c(2 * cellarea, 2 * cellarea))
                 st_make_grid(landscape,
                              cellsize = cellsize,
                              square = TRUE, what = "polygons"
                 ) %>%
                   st_intersection(landscape, dimensions = "polygon") %>%
                   st_triangulate_constrained() %>%
                   st_collection_extract()
               }
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
    # st_intersection(landscape, dimensions = "polygon") %>%
    st_sf() %>%
    # note: for hexagon, sometimes we have LINESTRING here..
    dplyr::filter(st_area(geometry) > 0) %>%
    identity()

  ## Rotate the grid back, if necessary:
  if(rotate!=0){
    grid$geometry <- grid$geometry * matrix(c(cos(-rotate), -sin(-rotate), sin(-rotate), cos(-rotate)), 2)
    grid <- grid %>%
      filter(st_intersects(geometry, landscape_original, sparse=FALSE)[,1L]) %>%
      mutate(geometry = st_intersection(geometry, landscape_original))
  }

  matched_area <- isTRUE(all.equal(
    st_area(st_union(grid)),
    st_area(landscape)
  ))

  if (!matched_area) {
    stop("Grid creation failed for parmeters: ", grid_type, ", ", patch_area, ", ", rotate*180/pi, " degree rotation")

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

  grid <- grid %>% mutate(ID = row_number())

  grid
}

#' Bounding box as a polygon
#'
#' @param polygon
#'
#' @return Coordinate matrix of a bounding box in the form of a polygon
#'
#' @examples
bbox_coords_mat <- function(polygon) {
  bbox <- st_bbox(polygon)
  coords_mat <- rbind(bbox[1:2],
                      bbox[c(3, 2)],
                      bbox[3:4],
                      bbox[c(1, 4)],
                      bbox[1:2])
  colnames(coords_mat) <- c("X","Y")
  coords_mat
}
