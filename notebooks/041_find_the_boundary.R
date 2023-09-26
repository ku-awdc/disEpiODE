
devtools::load_all()


world_scale <- 29
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape
# world_area <- st_area(world_landscape)
#

# grid <- create_grid(world_landscape, cellarea = 1.2, celltype = "square")
# grid <- create_grid(world_landscape, cellarea = 1.2, celltype = "hexagon")
grid <- create_grid(world_landscape, cellarea = 1.2, celltype = "triangle")

grid
grid %>% attributes()

grid_is_boundary <-
  st_touches(grid, st_boundary(world_landscape), sparse = TRUE) %>%

  lengths() %>%
  is_greater_than(0) %>%
  which() %>%

  identity()

grid_is_boundary %>% str()
# grid_is_boundary %>%
#   # class()
#   print(n = 50)

ggplot() +
  geom_sf(data = world_landscape, fill = NA, linetype = "dotted") +

  geom_sf(data = grid, fill = NA) +
  geom_sf(data = grid[grid_is_boundary,,drop = FALSE], fill = "red") +

  theme_blank_background() +
  NULL
