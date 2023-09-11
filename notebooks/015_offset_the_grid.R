
library(magrittr)
library(ggplot2)
library(sf)
devtools::load_all()

world_scale <- 29
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape
# world_area <- st_area(world_landscape)

n <- 10
grid <-
  create_grid(n, landscape = world_landscape,
              landscape_scale = world_scale,
              offset = "corner")

p_baseline <- ggplot() +
  theme_blank_background()
grid %>% st_area()
grid$geometry %>% length()


p_baseline +
  geom_sf(data = create_grid(n, landscape = world_landscape,
                             landscape_scale = world_scale,
                             offset = "corner"),
          fill = NA) +
  labs(caption = "corner")

p_baseline +
  geom_sf(data = create_grid(n, landscape = world_landscape,
                             landscape_scale = world_scale,
                             offset = "middle"),
          fill = NA) +
  labs(caption = "middle")

p_baseline +
  geom_sf(data = create_grid(n, landscape = world_landscape,
                             landscape_scale = world_scale,
                             offset = "bottom"),
          fill = NA) +
  labs(caption = "bottom")

p_baseline +
  geom_sf(data = create_grid(n, landscape = world_landscape,
                             landscape_scale = world_scale,
                             offset = "left"),
          fill = NA) +
  labs(caption = "left")

