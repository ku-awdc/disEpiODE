devtools::load_all()

landscape <- create_landscape(world_scale <- 1)$landscape

create_grid_with_center <- function(landscape, cellsize) {

  landscape_centroid <- st_centroid(landscape)
  centroid_coords <- st_coordinates(landscape_centroid)
  stopifnot(any(length(cellsize) %in% c(1, 2)))

  relative_pos <- centroid_coords %% cellsize
  offset <- dplyr::if_else(relative_pos < cellsize / 2, relative_pos - cellsize, -relative_pos)
  offset <- offset + cellsize / 2

  grid <- st_make_grid(landscape,
                       cellsize = cellsize,
                       square = TRUE,
                       what = "polygons",
                       offset = offset)
  # grid
  grid %>%
    st_intersection(landscape)
}
#'
#'
ggplot() +
  geom_sf(data = landscape, linetype = "dashed", fill = NA) +
  geom_sf(data = create_grid_with_center(landscape = landscape, cellsize = 1/6),
          fill = NA, size = 2) +

  geom_sf(data = st_point(c(0.5, 0.5)), aes(color = "center")) +

  theme_grid_plot() +
  theme_blank_background()
