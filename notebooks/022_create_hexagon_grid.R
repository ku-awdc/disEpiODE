
library(magrittr)
library(glue)
# library(ggplot2)
library(tidyverse)
library(sf)
devtools::load_all()

world_scale <- 29
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape
# world_area <- st_area(world_landscape)

# library(future)
# library(furrr)
# plan(multisession(workers = 4))

grids_df <- expand_grid(
  n = seq_len(50),
  offset = c("corner", "middle", "bottom", "left"),
  square = c(TRUE, FALSE)) %>%
  mutate(grid =
           furrr::future_pmap(., function(n, offset, square)
             disEpiODE:::create_grid(n = n,
                                     landscape = world_landscape,
                                     landscape_scale = world_scale,
                                     offset = offset, square = square))
  )
grids_df$grid[[10]] %>% nrow()
grids_df$grid[[10]] %>% st_collection_extract(type = "POLYGON")

grids_df <- grids_df %>%
  mutate(n_grid = grid %>% map_dbl(nrow))
#'
#'
#' Make sure that the grid count is somewhat the same between
#' the two `square == TRUE` vs. `square == FALSE`.
grids_df %>%
  glimpse() %>%
  ggplot() +
  aes(n, n_grid) +
  aes(group = str_c(offset, square)) +
  geom_step(aes(color = offset)) +
  # facet_wrap(offset~square, scales = "free") +
  facet_wrap(~square, scales = "free") +
  theme_blank_background()
#'
#'
#'
#' Inspect randomly grids just to see if they are correct looking..
grids_df %>%
  sample_n(size = 5) %>%
  pmap(function(n, offset, square, grid, n_grid) {
    ggplot() +
      geom_sf(data = world_landscape, fill = NA, linetype = "dotted") +
      geom_sf(data = grid, fill = NA) +
      labs(caption = glue("offset = {offset}, n = {n}, n² = {n**2}, n_grid = {n_grid}")) +
      theme_blank_background()
  })

# VALIDATION: must be empty
grids_df %>%
  filter(grid %>%
           map_lgl(\(x) {
             any(st_is(x, "LINESTRING"))
           })
  ) -> bad_grids

