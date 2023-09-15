

library(magrittr)
library(conflicted)


library(tidyverse)
library(sf)

devtools::load_all()


world <- create_landscape(29)
world$landscape -> world_landscape

grid <- st_make_grid(world_landscape, cellsize = 29 / 5) %>%
  st_sf()
grid <- rowid_to_column(grid, "id")

# grid_yflip <- grid %>%
#   mutate(geometry = geometry - )

ggplot() +
  geom_sf(data = grid, fill = NA) +

  geom_sf_text(data = grid, aes(label = id)) +

  theme_blank_background()
#' ## Conclusion
#'
#' `st_make_grid` returns elements in row-major,
#' meaning that the order they are returned in cannot be
#' used directly in the context of R's matrices, and
#' R's natural vectorisation, as R uses col-major.
#'
