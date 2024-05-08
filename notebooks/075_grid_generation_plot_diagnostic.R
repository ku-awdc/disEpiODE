devtools::load_all(".")

world <- create_landscape(world_scale <- 1)
world$landscape -> world_landscape

cellarea <- seq_cellarea(
  precision = 0.01,
  min_cellarea = 1 / 2000,
  max_cellarea = world_scale**2
)
cellarea
#' Remove cellareas that correspond to already seen cellareas.
# 1 / cellarea[which(!duplicated(zapsmall(1 / cellarea)))]
# cellarea <- cellarea[which(!duplicated(round(1 / cellarea)))]

generate_grids <- tibble(cellarea) %>%
  expand_grid(celltype = c("triangle", "square", "hexagon")) %>%
  mutate(
    grid =
      map2(
        .progress = TRUE,
        celltype, cellarea, \(celltype, cellarea)
        create_grid(
          world_landscape,
          cellarea = cellarea,
          celltype = celltype
        )
      )
  )
generate_grids <- generate_grids %>%
  mutate(
    n = grid %>% map_dbl(nrow),
    n_unique_area = grid %>% map_dbl(\(grid) {
      st_area(grid) %>%
        zapsmall() %>%
        n_distinct()
    })
  ) %>%
  #' validation purposes
  mutate(
    mean_actual_cellarea = grid %>% map_dbl(\(grid) mean(st_area(grid))),
    median_actual_cellarea = grid %>% map_dbl(\(grid) median(st_area(grid)))
  )

p_ncells_plot <- generate_grids %>%
  mutate(celltype = fct(celltype, c("triangle", "square", "hexagon"))) %>%
  ggplot() +
  aes(cellarea, n) +
  geom_step(aes(color = celltype)) +
  theme(legend.position = "bottom") +
  labs(
    color = NULL,
    x = "Cell area",
    y = "Total number of cells"
  ) +
  theme_blank_background()


p_ncells_plot +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  # stat_function(fun = function(x) log10(x), linetype = "dotted") +
  # aes(y = 1 / n) +
  scale_x_log10_rev() +
  theme_reverse_arrow_x() +
  scale_y_log10() +
  # stat_function(fun = function(x) log1p(-x), linetype = "dotted") +

  labs(x = "Set cellarea") +

  NULL

ggsave(
  device = svglite::svglite,
  scale = 2.5,
  filename = glue("figures/grid_diagnostic_log_log_plot.svg")
)



p_ncells_plot +
  aes(y = mean_actual_cellarea) +
  labs(
    y = "Avg. cell areas in grid",
    x = "Preset cell area"
  ) +

  # geom_abline(intercept = 0, slope = -1, linetype = "dotted") +
  # scale_x_reverse() +

  # stat_function(fun = function(x) x, linetype = "dotted") +
  # scale_x_reverse() +

  stat_function(fun = function(x) x, linetype = "dotted") +
  scale_x_log10_rev() +
  theme_reverse_arrow_x() +

  # identity
  # stat_function(fun = function(x) x, linetype = "dotted") +

  # coord_equal() +
  expand_limits(y = 1) +
  # geom_rug(sides = "b", length = unit(0.03 / 3, "npc")) +

  NULL
# p_ncells_plot
#'
#'
# DEBUG
# ggplot() +
#   geom_sf(data =
#             generate_grids$grid[[424]], fill = NA)

#'
beepr::beep()
