
library(conflicted)
library(magrittr)
library(tidyverse)
library(sf)

devtools::load_all("~/GitHub/disEpiODE/")

world <- create_landscape(29)
world$landscape -> world_landscape
area <- 1

grid1 <- st_make_grid(world_landscape, square = FALSE, cellsize = sqrt(2*area / (3*sqrt(3))))
grid2 <- st_make_grid(world_landscape, square = FALSE, cellsize = sqrt(2*area / sqrt(3)))

# st_area(grid1)
st_area(grid1) %>% density() %>% plot()
st_area(grid2) %>% density() %>% plot()

bind_rows(`1` = grid1 %>% st_sf(),
          `2` = grid2 %>% st_sf(),
          .id = "id") %>%
  mutate(area = st_area(geometry), .by = "id") %>%
  ggplot() +
  aes(area, group = id, fill = id, color = id) +
  stat_density(n = 250) +
  # stat_bin(bins = 250) +
  # geom_density() +
  theme_blank_background() +
  NULL
