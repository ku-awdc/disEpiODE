
#' @param hex_size radius of outer circle
#' @param flat_top `logical(1)` orientation of hexagons
#'
hex_size <- NA
hex_flat_top <- FALSE
cx <- 0
cy <- 0
hex_width <- 2 * hex_size
hex_height <- sqrt(3) * hex_size

hex_coords_arr <-
  array(
    c(-5:5) %>% rep(each = 7 * 2),
    dim = c(7, 2, 11)
  )
hex_coords_arr

hex_each <- asplit(hex_coords_arr, MARGIN = 3)
hex_each

if (hex_flat_top) {
  spacing_horiz <- 3/2 * hex_size
  spacing_vert <- sqrt(3) * hex_size

  angles_deg <- (1:6) * 60
  angles_deg <- c(angles, angles[1])
  angles_rad <- (pi / 180) * angles_deg
} else {
  # !flat_top
  # pointy_top is true
  spacing_horiz <- sqrt(3) * hex_size
  spacing_vert <- 3/2 * hex_size

  angles_deg <- c(1:6) * 60 - 30
  angles_deg <- c(angles, angles[1])
  angles_rad <- (pi / 180) * angles_deg
}

#' We'd like to tessellate a square-polygon with hexagons. First, we choose
#' to retain all hexagons with centroid within the square-polygon. That, however,
#' doesn't suffice, as then there are parts of the square uncovered by a hexagon.
#' These areas stem from the staggered rows of hexagons, in which there is
#' a jagged part that is part of the square, but not part of hexagon.
#'
#'


landscape_sf <- st_sfc(
  st_polygon(
    rbind(c(0, 0),
          c(0, 1),
          c(1, 1),
          c(1, 0),
          c(0, 0)) %>% list()
  )
)
#'
#'
#' This shows that the resulting hexagon doesn't cover the whole grid because
#' of the presence of offset.
#'
st_make_grid_full_hex <- function(landscape, cellsize = 1/8, offset, padding = TRUE, flat_topped = FALSE) {
  #' if pointy, then missing hexagons in y
  #' if flat, then missing hexagons in x
  stopifnot("todo" = !flat_topped)

  # if (length(cellsize) == 1) {
  #   cellsize <- c(cellsize, cellsize)
  # }
  hex_size <- cellsize / sqrt(3)
  horiz <- sqrt(3) * hex_size
  vert <- (3/2) * hex_size
  if (padding) {
    # browser()
    landscape_bbox <- st_bbox(landscape)
    #' [xmin ymin xmax ymax]
    # landscape_bbox[2] <- landscape_bbox[2] - cellsize[2]
    # landscape_bbox[4] <- landscape_bbox[4] + cellsize[2]
    landscape_bbox <- landscape_bbox + c(-1, -1, 1, 1) * c(vert, horiz / 2, vert, horiz / 2)

    landscape_bbox_lower_left <- landscape_bbox[1:2]
    landscape_bbox <- st_as_sfc(landscape_bbox)

  } else {
    landscape_bbox <- landscape
  }

  if (missing(offset)) {
    offset <- st_bbox(landscape_bbox)[c("xmin", "ymin")]
  }

  chopped_grid <- st_make_grid(
    landscape_bbox,
    cellsize = cellsize,
    flat_topped = flat_topped,
    square = FALSE,
    # offset = offset
      # -landscape_bbox_lower_left +
      # st_centroid(st_geometry(landscape)) %>% unlist(recursive = FALSE)
  ) %>%
    # st_intersection(landscape) %>%
    st_sf() %>%
    dplyr::filter(st_geometry_type(.) == "POLYGON")
  print(
    ggplot() +
      geom_sf(data = chopped_grid, fill = NA) +
      geom_sf(data = landscape, linetype = "dotted", fill = NA) +

      geom_sf(data = st_centroid(landscape), aes(color = "center")) +
      geom_sf(data = st_centroid(landscape_bbox), aes(color = "center of bbox")) +
      geom_sf(data = st_filter(chopped_grid, st_centroid(landscape)), aes(fill = "cell with landscape center"), alpha = 0.2) +
      geom_sf(data = st_centroid(st_filter(chopped_grid, st_centroid(landscape))), aes(color = "center cell centroid")) +

      labs(color = NULL, fill = NULL) +

      theme_grid_plot() +
      NULL
  )
  # chopped_grid %>% {
  #   all_equal_area <- all.equal(st_area(st_union(.)) , st_area(landscape))
  #   message(glue("Grid area vs landscape area: {all_equal_area}"))
  #   stopifnot("grid area == landscape area" = isTRUE(all_equal_area)
  #   )
  # }
  # chopped_grid %>% {
  #   grid_cell_that_has_center <- st_filter(., st_centroid(landscape))
  #   print(grid_cell_that_has_center)
  #   print(st_centroid(grid_cell_that_has_center))
  #   stopifnot(
  #     "landscape center must be a centroid in a grid cell" =
  #       all.equal(
  #         st_centroid(grid_cell_that_has_center) %>% unlist(recursive = FALSE),
  #         st_centroid(landscape) %>% unlist(recursive = FALSE)
  #       )
  #   )
  # }
  chopped_grid
}
#'
st_make_grid_full_hex(landscape_sf, cellsize = 1/2, padding = FALSE)
st_make_grid_full_hex(landscape_sf, cellsize = 1/2, padding = TRUE)

st_make_grid_full_hex(landscape_sf, cellsize = 1/2, offset = sqrt(3)* c(1/2/2,1/2/2))
st_make_grid_full_hex(landscape_sf, cellsize = 1/8, offset =  c(0,0))
st_make_grid_full_hex(landscape_sf, cellsize = 1/8, offset = c(0,1/8))
st_make_grid_full_hex(landscape_sf, offset = -c(0.5, 0.5))

# ggplot() +
#   geom_sf(data = landscape_sf, fill = NA, linetype = "dotted", linewidth = 0.6) +
#   theme_grid_plot() +
#   geom_sf(data = st_make_grid_full_hex(landscape = landscape_sf)) +
#
#   NULL
#
#'

hex_cellsize_f <- function(cellarea) {
  sqrt(cellarea * (2 / sqrt(3)))
}
#'
ggplot() +
  geom_sf(data = landscape_sf, fill = NA, linetype = "dotted", linewidth = 0.6) +
  theme_grid_plot() +

  geom_sf(data =
            st_make_grid(
              landscape_sf,
              cellsize = 1 / 9,
              # cellsize = hex_cellsize_f(1/31),
              flat_topped = FALSE,
              # flat_topped = TRUE,
              square = FALSE,
              offset = c(0.5, 0.5)
            ) %>%
            st_intersection(landscape_sf) %>%
            st_sf() %>%
            dplyr::filter(st_geometry_type(.) == "POLYGON") %>% {
              all_equal_area <- all.equal(st_area(st_union(.)) , st_area(landscape_sf))
              message(glue("Grid area vs landscape area: {all_equal_area}"))
              # stopifnot("grid area == landscape area" = isTRUE(all_equal_area)
              # );
              .
            } %>% {
              grid_cell_that_has_center <- st_filter(., st_centroid(landscape_sf))
              print(grid_cell_that_has_center)
              print(st_centroid(grid_cell_that_has_center));
              .
            } %>%
            identity(),
          fill = NA
  ) +
  geom_sf(data = st_centroid(landscape_sf), aes(color = "center")) +
  # geom_sf(data = st_centroid(landscape_bbox), aes(color = "center of bbox")) +
  # geom_sf(data = st_filter(chopped_grid, st_centroid(landscape)), aes(fill = "cell with landscape center"), alpha = 0.2) +
  # geom_sf(data = st_centroid(st_filter(chopped_grid, st_centroid(landscape))), aes(color = "center cell centroid")) +


  # theme_blank_background() +
  NULL

#
# st_make_grid(
#   landscape_sf, cellsize = 1 / 50,
#   flat_topped = FALSE,
#   square = FALSE,
# )

#' This shows that we don't loose hexagons area no matter the cell-area,
#' but this is done with unset offset.
#'
for (cellarea in seq_cellarea(n = 200,
                              min_cellarea = 1/2000,
                              max_cellarea = 1)) {
  # cellsize := vertical spacing [flat] +
  #   horizontal spacing [pointy]
  # hex_size := cellsize / sqrt(3) (flat top)
  # cellarea := (3/2) * sqrt(3) hex_size**2
  #
  # Conclusion: hex_size != cellsize
  #
  # Given cellarea, we want to return cellsize. Thus,
  # cellarea = (3/2) * sqrt(3) (cellsize / sqrt(3))**2
  # => sqrt(cellarea * (2/sqrt(3))) = cellsize
  # cellsize = sqrt(cellarea * (2/sqrt(3)))
  cellsize <- sqrt(cellarea * (2 / sqrt(3)))
  st_make_grid(
    landscape_sf,
    cellsize = cellsize,
    flat_topped = FALSE,
    square = FALSE
  ) %>% {
    (st_area(.))
    stopifnot(all.equal(st_area(.) %>% zapsmall() %>% unique(), cellarea %>% zapsmall()))
    ;.
  } %>%
    st_intersection(landscape_sf) %>%
    st_sf() %>%
    dplyr::filter(st_geometry_type(.) == "POLYGON") %>% {
      all_equal_area <- all.equal(st_area(st_union(.)) , st_area(landscape_sf))
      # message(glue("Grid area vs landscape area: {all_equal_area}"))
      stopifnot("grid area == landscape area" = isTRUE(all_equal_area)
      );
      .
    } %>%
    identity()
}


