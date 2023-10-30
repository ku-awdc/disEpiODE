
devtools::load_all(".")



world <- create_landscape(world_scale <- 39)
world$landscape -> world_landscape

ggplot() +
  geom_sf(data = world_landscape, fill = NA,
          linetype = "dotted") +

  theme_blank_background() ->
  p_world
#'
#'
#'
#'

create_grid(
  world_landscape,
  # cellarea = world_scale**2,
  cellarea = 5,
  # cellarea = 5**2,
  # split_difference = FALSE,
  split_difference = TRUE,
  celltype = "square"
) %>%
  # st_area() %>% unique()
  identity() %>% {
    p_world +
      geom_sf(data = ., aes(fill = factor(prettyNum(zapsmall(st_area(geometry))))),
              linetype = "dashed") +
      labs(fill = "ndistinct(area)") +
      theme(legend.position = "bottom")
  }



cellarea <- world_scale**2
cellsize <- \(cellarea) sqrt((2 * cellarea) / sqrt(3))

# area = (3×sqrt(3) / 2) × s**2
# s <- cellsize / sqrt(3)
# D <- 2 * s
# d <- sqrt(3) * s
# dx <- cellsize[1]/sqrt(3)
# dy <- sqrt(3) * dx/2

# D ?= sqrt(cellarea) = sqrt((3×sqrt(3) / 2) × s**2)
#   = s × sqrt((3×sqrt(3) / 2) = 2 × s
# =>
#
# D ?= sqrt(cellarea) = s × 2
# cellarea = s**2 × 4
#   (3×sqrt(3) / 2) × s**2 = s**2 × 4
#
#

# cellsize <- world_scale * sqrt(3) / 2
# cellsize <- world_scale
ggplot() +

  # D = 2×s
  # s = cellsize / sqrt(3)
  # D = 2 × cellsize / sqrt(3)
  # =>
  # cellsize = D × sqrt(3) / 2

  geom_sf(data = st_make_grid(world_landscape,
                              square = FALSE,
                              cellsize = cellsize(cellarea = (world_scale**2 / sqrt(3)) / 4),
                              # offset = c(0,0) + c(0)
                              # offset = c(0,0) + sqrt(dx**2+dy**2)
                              # cellsize = c(cellsize, world_scale*sqrt(3)/2),
                              # offset = c(0,-cellsize / sqrt(3) / 2)
                              # offset = c(dx, dy) / 5
                              # offset = c(0,0) + c(world_scale / 2)),
                              # offset = c(0,0) + c(cellsize / sqrt(3), cellsize * sqrt(3) / 2)
  ),
  aes(fill = factor(prettyNum(zapsmall(st_area(geometry))))),
  linetype = "dashed") +
  labs(fill = "ndistinct(area)") +
  theme(legend.position = "bottom") +


  geom_sf(data = world_landscape, fill = NA, linewidth = 2, linetype = "dotted") +
  theme_blank_background()


create_grid(
  world_landscape,
  # cellarea = 5,
  cellarea = world_scale**2,
  # split_difference = TRUE,
  celltype = "hexagon"
) %>%
  # st_area() %>% zapsmall() %>%  table()
  identity() %>% {
    p_world +
      # geom_sf(data = ., fill = NA, linetype = "dashed")
      geom_sf(data = ., aes(fill = factor(prettyNum(zapsmall(st_area(geometry))))),
              linetype = "dashed") +
      labs(fill = "ndistinct(area)") +
      theme(legend.position = "bottom")
  }

message("works?")











create_grid(
  world_landscape,
  # cellarea = world_scale**2,
  cellarea = 5**2 / 2,
  # split_difference = TRUE,
  celltype = "triangle"
) %>%
  # st_area() %>% zapsmall() %>%  table()
  identity() %>% {
    p_world +
      # geom_sf(data = ., fill = NA, linetype = "dashed")
      geom_sf(data = ., aes(fill = factor(prettyNum(zapsmall(st_area(geometry))))),
              linetype = "dashed") +
      labs(fill = "ndistinct(area)") +
      theme(legend.position = "bottom")
  }





create_grid(
  world_landscape,
  cellarea = world_scale**2,
  celltype = "hexagon_rot"
) %>%
  # st_area() %>% zapsmall() %>%  table()
  identity() %>% {
    p_world +
      geom_sf(data = ., fill = NA, linetype = "dashed")
  }

#' ## Illustrate difference between triangle and constrained triangle

# square_grid <- st_make_grid(landscape, n = 4, square = TRUE, what = "polygons")
# triangle_grid <- st_triangulate(square_grid) %>% st_collection_extract(type = "POLYGON")
# triangle_grid
# ggplot() +
#   geom_sf(data = square_grid, fill = NA) +
#   geom_sf(data = triangle_grid, fill = NA)


#' ## Second part: Grid relationships
#'
cellarea <- seq_cellarea(n = 200, min_cellarea = 1, max_cellarea = world_scale**2)

generate_grids <- tibble(cellarea) %>%
  expand_grid(celltype = c("triangle", "square", "hexagon")) %>%
  mutate(grid =
           map2(celltype,  cellarea, \(celltype, cellarea)
                create_grid(
                  world_landscape,
                  cellarea = cellarea,
                  celltype = celltype
                )))
generate_grids <-  generate_grids %>%
  mutate(n = grid %>% map_dbl(nrow),
         n_unique_area = grid %>% map_dbl(\(grid) {
           st_area(grid) %>% zapsmall() %>% n_distinct()
         })) %>%
  #' validation purposes
  mutate(mean_actual_cellarea = grid %>% map_dbl(\(grid) mean(st_area(grid))),
         median_actual_cellarea = grid %>% map_dbl(\(grid) median(st_area(grid)))
  )

generate_grids %>%
  ggplot() +
  aes(cellarea, n) +
  # geom_line(aes(color = celltype)) +
  geom_step(aes(color = celltype)) +

  # stat_function(fun = ~ -.x) +
  # geom_abline(slope = log10(0), slope = 1) +
  # geom_abline(aes(intercept = 0, slope = 1,
  #                 y = stage(after_scale = y))) +

  scale_x_log10_rev() +
  scale_y_log10() +
  theme_reverse_arrow_x() +

  coord_equal() +

  theme_blank_background()

#' For validation
p_grids_cellarea <- generate_grids %>%
  ggplot() +
  aes(cellarea, mean_actual_cellarea, group = celltype) +
  # geom_step(aes(color = celltype)) +
  geom_line(aes(color = celltype)) +

  geom_abline(aes(slope = 1, intercept = 1, color = "Identity"),
              linetype = "dashed") +

  labs(y = "Mean of cellareas") +

  guides(color = guide_legend(override.aes = list(linetype = NULL))) +

  coord_equal() +
  theme_blank_background()
p_grids_cellarea
p_grids_cellarea +
  aes(y = median_actual_cellarea) +
  labs(y = "Median of cellareas")

celltype_colors <- scales::hue_pal()(3)

generate_grids %>%
  mutate(celltype = factor(celltype, c("triangle", "square", "hexagon"))) %>%
  ggplot() +

  aes(cellarea, n_unique_area, group = celltype) +
  # geom_step(aes(color = celltype)) +
  geom_line(aes(color = celltype)) +
  # geom_line() +
  #
  geom_hline(aes(yintercept = 1, color = "perfect tessellation"),
             linetype = "dotted") +

  facet_wrap(~celltype, ncol = 1) +

  scale_color_manual(
    values = c("perfect tessellation" = "black",
               "triangle" = celltype_colors[1],
               "square" = celltype_colors[2],
               "hexagon" = celltype_colors[3]
    )
  ) +
  geom_rug(sides = "b", data = . %>% dplyr::filter(celltype == "hexagon")) +

  labs(y = "Total number of unique cellarea-values") +

  theme_blank_background()




