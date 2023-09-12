
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
              square = FALSE,
              offset = "corner")
for (n in seq_len(50)) {
  for (offset in c("corner", "middle", "bottom", "left"))
    create_grid(n, landscape = world_landscape,
                landscape_scale = world_scale,
                square = FALSE,
                offset = offset)
}
