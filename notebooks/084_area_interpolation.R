world_landscape <- create_landscape(1)$landscape

centroid_coords <- c(0.5, 0.5)

seq_grid <- seq_cellarea(n = 25,
                         min_cellarea = 1 / 2000, max_cellarea = 1) %>%
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
      st_sf()

    center_cell_centroid <- grid %>% st_filter(y=st_centroid(world_landscape)) %>% st_centroid() %>%
      unlist(recursive = TRUE, use.names = FALSE)
    stopifnot(all.equal(center_cell_centroid, c(0.5,0.5)))
    # print(center_cell_centroid)

    grid
  }
  )

# middle_zone <- st_buffer(st_centroid(landscape), dist = 1/7)


seq_grid %>% lengths()
p_grids<- ggplot() +
  geom_sf(data = world_landscape, fill = NA,
          linewidth = 1.2,
          linetype = "dotted") +

  # geom_sf(data = seq_grid[[4]], fill = NA) +
  geom_sf(data = seq_grid[[7]], fill = NA) +
  # geom_sf(data = seq_grid[[1]], fill = NA) +
  # geom_sf(data = seq_grid[[length(seq_grid)]], fill = NA) +

  geom_sf(
    shape = "o",
    data = st_centroid(world_landscape),
    size = 8
  ) +

  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  theme_blank_background() +
  coord_sf(expand = FALSE)
p_grids
