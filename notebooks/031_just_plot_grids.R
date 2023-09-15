

library(magrittr)
library(tidyverse)
library(sf)

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
  cellarea = 1,
  celltype = "square"
) %>%
  # st_area() %>% unique()
  identity() %>% {
  p_world +
    geom_sf(data = ., fill = NA, linetype = "dashed")
}

create_grid(
  world_landscape,
  cellarea = 1,
  celltype = "hexagon"
) %>%
  # st_area() %>% zapsmall() %>%  table()
  identity() %>% {
  p_world +
    geom_sf(data = ., fill = NA, linetype = "dashed")
  }

create_grid(
  world_landscape,
  cellarea = 1,
  celltype = "hexagon_rot"
) %>%
  st_area() %>% zapsmall() %>%  table()
  identity() %>% {
  p_world +
    geom_sf(data = ., fill = NA, linetype = "dashed")
}


create_grid(
  world_landscape,
  cellarea = 1,
  celltype = "triangle"
) %>%
  # st_area() %>% zapsmall() %>%  table()
  identity() %>% {
  p_world +
    geom_sf(data = ., fill = NA, linetype = "dashed")
}












