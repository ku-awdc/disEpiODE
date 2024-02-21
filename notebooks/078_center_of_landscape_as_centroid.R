
devtools::load_all()

landscape <- create_landscape(world_scale <- 1)
landscape <- landscape$landscape

common_cellarea <- 1/42
common_cellarea <- 1/22
common_cellarea <- 1/10
# common_cellarea <- 1/11
# DEBUG
# create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "triangle")
# DEBUG
# create_grid(landscape, cellarea = common_cellarea, middle = FALSE, celltype = "triangle")


ggplot() +
  geom_sf(data =
            create_grid(landscape, cellarea = common_cellarea, center_as_centroid = FALSE, celltype = "square"), fill = NA) +
  # geom_sf(data = grids, aes(fill = unique_area_fill), show.legend = FALSE) +
  # geom_sf(data = grids, fill = NA, show.legend = FALSE) +
  # geom_sf(data = grids %>% st_centroid(), fill = NA, show.legend = FALSE, shape = "×") +
  # facet_wrap(center_as_centroid~celltype, labeller = label_both) +
  geom_sf(data = st_centroid(landscape), color = "black", size = 4, shape = "o") +
  theme_grid_plot() +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  # scale_fill_viridis_d(direction = -1) +
  NULL


grids <- bind_rows(
  # triangle = create_grid(landscape, cellarea = common_cellarea, center_as_centroid = TRUE, celltype = "triangle"),
  square = create_grid(landscape, cellarea = common_cellarea, center_as_centroid = TRUE, celltype = "square"),
  # hexagon = create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "hexagon"),
  .id = "celltype"
) %>% mutate(center_as_centroid = TRUE) %>%
  bind_rows(
    (
      bind_rows(
        # triangle = create_grid(landscape, cellarea = common_cellarea, center_as_centroid = FALSE, celltype = "triangle"),
        square = create_grid(landscape, cellarea = common_cellarea, center_as_centroid = FALSE, celltype = "square"),
        # hexagon = create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "hexagon"),
        .id = "celltype"
      ) %>% mutate(center_as_centroid = FALSE)
    )
  ) %>%
  # mutate(unique_area_fill = factor(prettyNum(zapsmall(st_area(geometry))))) %>%
  mutate(celltype = fct_inorder(celltype))

ggplot() +
  # geom_sf(data = grids, aes(fill = unique_area_fill), show.legend = FALSE) +
  geom_sf(data = grids, fill = NA, show.legend = FALSE) +
  geom_sf(data = grids %>% st_centroid, fill = NA, show.legend = FALSE, shape = "×") +
  facet_wrap(center_as_centroid~celltype, labeller = label_both) +
  geom_sf(data = st_centroid(landscape), color = "black", size = 4, shape = "o") +
  theme_grid_plot() +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  # scale_fill_viridis_d(direction = -1) +
  NULL
