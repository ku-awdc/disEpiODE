
devtools::load_all()

landscape <- create_landscape(world_scale <- 1)
landscape <- landscape$landscape

common_cellarea <- 1/42
# DEBUG
# create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "triangle")
# DEBUG
# create_grid(landscape, cellarea = common_cellarea, middle = FALSE, celltype = "triangle")

grids <- bind_rows(
  triangle = create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "triangle"),
  square = create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "square"),
  # hexagon = create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "hexagon"),
  .id = "celltype"
) %>% mutate(middle = TRUE) %>%
  bind_rows(
    (
      bind_rows(
        triangle = create_grid(landscape, cellarea = common_cellarea, middle = FALSE, celltype = "triangle"),
        square = create_grid(landscape, cellarea = common_cellarea, middle = FALSE, celltype = "square"),
        # hexagon = create_grid(landscape, cellarea = common_cellarea, middle = TRUE, celltype = "hexagon"),
        .id = "celltype"
      ) %>% mutate(middle = FALSE)
    )
  ) %>%
  mutate(unique_area_fill = factor(prettyNum(zapsmall(st_area(geometry))))) %>%
  mutate(celltype = fct_inorder(celltype))

ggplot() +
  geom_sf(data = grids, aes(fill = unique_area_fill), show.legend = FALSE) +
  facet_wrap(middle~celltype, labeller = label_both) +
  theme_grid_plot() +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  # scale_fill_viridis_d(direction = -1) +
  NULL

