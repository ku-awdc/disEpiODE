sfc <- st_sfc(
  st_polygon(
    list(rbind(c(0,0), c(10,0), c(10,10), c(0, 10), c(0,0)))
  )
)

grid <- st_make_grid(sfc, n = c(10, 10))

split_into_triangles <- function(x){

  xy <- st_coordinates(x)[, c("X", "Y")]

  # coordinate order:
  # bottom-left,
  # bottom-right,
  # top-right,
  # top-left,
  # bottom-left

  #TODO: Add another orientation here
  top_left <- st_polygon(list(xy[c(1, 3, 4, 5), ]))
  bottom_right <- st_polygon(list(xy[c(1, 2, 3, 5), ]))

  list(top_left, bottom_right)
}

triangle_labels <- grid |>
  purrr::map(split_into_triangles) |>
  unlist(recursive = FALSE) |>
  st_sfc() |>
  st_sf() |>
  mutate(
    id = c(1, 1 + seq(10, 190, by = 10)) + rep(0:9, each = 20)
  )
triangle_labels

ggplot() +
  geom_sf(data = triangle_labels, fill = "white") +
  # geom_sf_text(data = triangle_labels, aes(label = id)) +
  theme_void()
