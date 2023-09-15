#' ---
#' author: Matt
#' ---
#'

world_landscape
area <- 6
type <- "hexagons"

grid <-
  if (type == "square") {
    st_make_grid(world_landscape,
      cellsize = rep(sqrt(area), 2L),
      square = TRUE, what = "polygons"
    )
  } else if (type == "hexagons") {
    # missing cellsize/sqrt(3) = s derivation, see `create_grid`
    st_make_grid(world_landscape,
      cellsize = sqrt((2 * area) / 3 * sqrt(3)),
      square = FALSE, what = "polygons"
    )
  } else if (type == "hexagons_rot") {
    st_make_grid(world_landscape,
      cellsize = sqrt((2 * area) / 3 * sqrt(3)),
      square = FALSE, flat_topped = TRUE, what = "polygons"
    )
  } else if (type == "triangles") {
    st_make_grid(world_landscape,
      cellsize = rep(sqrt(area * 2L), 2L),
      square = TRUE, what = "polygons"
    ) %>%
      st_triangulate_constrained() %>%
      st_collection_extract()
  }

grid %<>%
  st_intersection(world_landscape) %>%
  st_sf() %>%
  dplyr::filter(st_area(geometry) > 0)


grid %>% ggplot() +
  geom_sf()


grid %>% st_area()
grid %>%
  st_area() %>%
  sum()
st_area(world_landscape)
grid %>%
  st_area() %>%
  density() %>%
  plot()

#
# sf:::make_hex_grid()
#
# dx = 5
# dy = sqrt(3) * dx/2
# dx * dy
