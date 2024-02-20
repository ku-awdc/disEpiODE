#' Create triangle grid
#'
#' Due to `sf::st_triangulate` / `sf::st_triangulate_constrained` producing
#' differently oriented triangles on a unstructured square grid, there is a
#' need for an approach, that returns consisting triangles throughout.
#'
#' @note
#' This function is meant to be inner function for [`create_grid`], therefore
#' its parameters are broad.
#'
#' @return
#' @export
#'
#' @examples
create_custom_triangle_grid <- function(
    landscape,
    cellarea = NULL,
    n = NULL,
    middle = FALSE,
    # celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
    celltype = c("triangle"),
    offset = c("corner", "middle", "bottom", "left"),
    orientation = c("diag", "anti")
) {
  stopifnot(
    "must provide `n` or `cellarea`" =
      xor(is.null(cellarea), is.null(n))
  )

  stopifnot(celltype == "triangle");
  orientation = match.arg(orientation, orientation)
  stopifnot("different orientation is not implemented" = orientation == "diag")
  stopifnot("middle is not implemented" = !middle)
  offset <- match.arg(offset, offset)
  stopifnot("unimplemented" = offset == "corner")
  stopifnot(length(celltype) == 1 && length(offset) == 1 && length(orientation) == 1,
            is.null(cellarea) || (!is.null(cellarea) && any(length(cellarea) %in% c(1, 2))),
            is.null(n) || (!is.null(n) && (any(length(n) %in% c(1,2))))
  )

  square_grid <-
    if (is.null(n)) {
      # this means that is.null(cellarea) == FALSE
      cellsize <- sqrt(c(2 * cellarea, 2 * cellarea))
      st_make_grid(landscape,
                   cellsize = cellsize,
                   square = TRUE, what = "polygons"
      ) %>%
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

  grid <- bind_rows(top_left = top_left, bottom_right = bottom_right)

  #FIXME: Should this be added
  #
  #   dplyr::filter(st_area(geometry) > 0) %>%
  #
  # ??


  grid <- st_as_sf(grid)
  st_geometry(grid) <- "geometry"
  grid$area <- st_area(grid$geometry)
  grid
}

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


split_into_triangles <- function(x) {
  xy <- st_coordinates(x)[, c("X", "Y")]

  # coordinate order:
  # bottom-left,
  # bottom-right,
  # top-right,
  # top-left,
  # bottom-left

  top_left <- st_polygon(list(xy[c(1, 3, 4, 5), ]))
  bottom_right <- st_polygon(list(xy[c(1, 2, 3, 5), ]))
  #TODO: try different way of constructing these:
  # example: landscape contains one single polygon
  # landscape_sf[[1]][[1]][c(1, 2, 3, 5),] %>% st_polygon(x = list(.))
  # landscape_sf[[1]][[1]][c(1, 3, 4, 5),] %>% st_polygon(x = list(.))

  #TODO: Add another orientation here
  # top_right <- st_polygon(list(xy[c(2,3,4,2), ]))
  # bottom_left <- st_polygon(list(xy[c(1, 2, 4, 5), ]))

  # ordering, first vertical, then horizontal elements

  list(top_left, bottom_right)
}

