sfc <- st_sfc(
  st_polygon(
    list(rbind(c(0,0), c(10,0), c(10,10), c(0, 10), c(0,0)))
  )
)

grid <- st_make_grid(sfc, n = c(10, 10)) %>% st_sf()
st_make_grid(sfc, n = c(10, 10)) %>% st_coordinates() %>% head()



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

grid %>% st_geometry() %>%
  # purrr::map(split_into_triangles) %>%
  purrr::map(. %>% split_into_triangles %>% st_sfc()) ->
  grid_triangle
grid_triangle %>% transpose() %>% st_sf()

# grid$triangle_geometry <- grid_triangle

# grid %>% unnest_wider(triangle_geometry,names_sep = "_") ->
#   grid_tri

ggplot() +
  geom_sf(data = grid_tri, aes(geometry = triangle_geometry_1), fill = "blue") +
  geom_sf(data = grid_tri, aes(geometry = triangle_geometry_2), fill = "yellow") +
  theme_blank_background() +
  NULL

triangle_labels <- grid |>
  purrr::map(split_into_triangles) |>
  unlist(recursive = FALSE) |>
  st_sfc() |>
  st_sf(sf_column_name = "geometry") |>
  mutate(
    # 10 * seq(1, ncol(x), by = 10) + rep(0:nrow(x), each = ncol(x))
    id = c(1, 1 + seq(10, 190, by = 10)) + rep(0:9, each = 20)
  )
triangle_labels

ggplot() +
  geom_sf(data = triangle_labels, fill = "white") +
  # geom_sf_text(data = triangle_labels, aes(label = id)) +
  theme_void()

#' ## Let's do a whole case study
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
landscape_sf %>% class()
landscape_sf %>% st_geometry() %>% class()
# maybe allocation free approach?
landscape_sf[[1]][[1]][c(1, 2, 3, 5),] %>% st_polygon(x = list(.))
landscape_sf[[1]][[1]][c(1, 3, 4, 5),] %>% st_polygon(x = list(.))

st_sfc(
  landscape_sf[[1]][[1]][c(1, 2, 3, 5),] %>% st_polygon(x = list(.)),
  landscape_sf[[1]][[1]][c(1, 3, 4, 5),] %>% st_polygon(x = list(.))
) %>%
  st_sf() %>% plot()

