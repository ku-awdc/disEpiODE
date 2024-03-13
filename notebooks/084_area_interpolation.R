world_landscape <- create_landscape(1)$landscape

centroid_coords <- c(0.5, 0.5)

seq_grid <- seq_cellarea(n = 150,
                         min_cellarea = 1 / 3000, max_cellarea = 1) %>%
  map(\(cellarea) {
    cellsize <- sqrt(cellarea)
    cellsize <- c(cellsize, cellsize)

    dist_center <-
      landscape %>% st_centroid() %>%
      st_distance(st_point(c(0,0))) %>%
      `[`(1,1)
    cellsize <- sqrt(c(cellarea, cellarea))
    celldiagonal <- sqrt(sum(cellsize**2))
    offset <- dist_center %% celldiagonal
    offset_sign <- offset >= celldiagonal / 2
    offset <- -offset + celldiagonal / 2
    offset <- c(1,1) * offset / sqrt(2)
    offset <- offset_sign * cellsize  + offset
    offset <- -offset

    grid <- st_make_grid(
      world_landscape,
      cellsize = cellsize,
      square = TRUE,
      offset = offset
    ) %>%
      st_intersection(world_landscape) %>%
      st_sf() %>%
      # amend properties of the grid,
      # TODO: could be attributes, instead of duplicated cols
      mutate(cellsize_set = cellsize %>% list(), cellarea_set = cellarea)

    center_cell_centroid <- grid %>%
      st_filter(y = st_centroid(world_landscape)) %>%
      st_centroid() %>%
      st_geometry() %>%
      unlist(recursive = TRUE, use.names = FALSE)
    stopifnot(all.equal(center_cell_centroid, c(0.5,0.5)))
    # print(center_cell_centroid)

    grid
  }
  )

middle_zone <- st_buffer(st_centroid(landscape), dist = 1/7) %>%
  st_sfc() %>%
  st_sf()

seq_grid %>% lengths()
p_grids <- ggplot() +
  geom_sf(data = world_landscape, fill = NA,
          linewidth = 1.2,
          linetype = "dotted") +

  geom_sf(data = middle_zone, fill = NA,
          linewidth = 1.1,
          linetype = "dashed") +

  # geom_sf(data = seq_grid[[4]], fill = NA) +
  geom_sf(data = seq_grid[[7]], fill = NA) +
  # geom_sf(data = seq_grid[[1]], fill = NA) +
  # geom_sf(data = seq_grid[[length(seq_grid)]], fill = NA) +

  geom_sf(
    shape = "x",
    data = st_centroid(world_landscape),
    size = 8
  ) +

  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  theme_blank_background() +
  coord_sf(expand = FALSE)

p_grids

middle_zone %>%
  mutate(infected = st_area(geometry) / 2,
         prevalence = infected / st_area(geometry))
#'
#' prevalence_interpolation <-
#'   seq_grid %>%
#'   # grid -> circle
#'   map(
#'     \(grid)
#'     st_interpolate_aw(
#'       middle_zone %>% mutate(infected = st_area(.) / 2),
#'       grid,
#'       extensive = TRUE
#'     )
#'   ) %>%
#'
#'   # .Last.value -> A
#'
#'   # ggplot() + geom_sf(data = A[[4]], aes(fill = infected))
#'   # what's the prevalence in the grid cells inside of the circle?
#'   map(\(grid) grid %>% mutate(prevalence = infected / st_area(grid))) %>%
#'   # grid(circle) -> circle, what's the prevalence in the circle
#'   map(\(grid) st_interpolate_aw(
#'     grid, middle_zone, extensive = FALSE
#'   )) %>%
#'   # walk(
#'   #   \(grid) {
#'   #     plot(grid)
#'   #   }
#'   # )
#'   # map(\(grid)
#'   #     bind_cols(
#'   #       grid %>% st_drop_geometry() %>% select(infected),
#'   #       st_interpolate_aw(
#'   #         grid %>% select(-infected),
#'   #         middle_zone, extensive = FALSE
#' #       ))) %>%
#' # print() %>%
#' bind_rows(.id = "id_grid") %>%
#'   identity()
#' prevalence_interpolation %>% glimpse()
#' #'
#' #'
#' #'
#' prevalence_interpolation %>%
#'   mutate(id_grid = as.numeric(id_grid)) %>%
#'   st_drop_geometry() %>%
#'   pivot_longer(c(infected, prevalence)) %>%
#'   glimpse() %>%
#'   ggplot() +
#'   aes(id_grid, value, group = name) +
#'   geom_line(aes(color = name)) +
#'
#'   facet_wrap(~name, scales = "free_y") +
#'   theme_blank_background() +
#'   NULL
#'
#'
#' Let's project the prevalence directly
#'
#'

middle_zone_init <-
  middle_zone %>%
  mutate(infected = st_area(geometry) / 2,
         prevalence = infected / st_area(geometry))

id_grid_set_cellarea <- seq_grid %>%
  map(. %>% distinct(cellarea_set)) %>%
  bind_rows(.id = "id_grid")

seq_grid %>%
  map(\(grid) {
    # st_interpolate_aw(
    #   middle_zone_init,
    #   grid,
    #   extensive = FALSE
    # )
    bind_cols(
      st_interpolate_aw(
        middle_zone_init %>% select(infected),
        grid,
        extensive = TRUE
      ) %>% st_drop_geometry(),
      st_interpolate_aw(
        middle_zone_init %>% select(prevalence),
        grid,
        extensive = TRUE
      )
    ) %>% st_sf(sf_column_name = "geometry")
  }) %>%
  map(\(grid) {
    bind_cols(
      st_interpolate_aw(
        grid %>% select(infected),
        middle_zone,
        extensive = FALSE
      ) %>% st_drop_geometry(),
      st_interpolate_aw(
        grid %>% select(prevalence),
        middle_zone,
        extensive = FALSE
      )
    )
  }) %>%
  # identity()
  map(st_drop_geometry) %>%
  bind_rows(.id = "id_grid") %>%
  # View() %>%
  # glimpse()

  left_join(id_grid_set_cellarea) %>%

  mutate(id_grid = as.numeric(id_grid)) %>%
  st_drop_geometry() %>%
  pivot_longer(c(infected, prevalence)) %>%
  glimpse() %>%
  ggplot() +
  # aes(id_grid, value, group = name) +
  aes(cellarea_set, value, group = name) +
  geom_line(aes(color = name)) +

  facet_wrap(~name, scales = "free_y") +

  scale_x_log10() +

  theme_blank_background() +
  NULL


